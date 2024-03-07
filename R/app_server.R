#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom broom glance tidy
#' @importFrom dplyr arrange bind_rows case_when collect everything ends_with
#'     filter group_by inner_join left_join mutate pull select summarise tbl
#'     ungroup distinct pull rename mutate_if row_number starts_with
#' @importFrom fixest fepois fixef
#' @importFrom glue glue
#' @importFrom lubridate day year
#' @importFrom rlang sym :=
#' @importFrom stats as.formula predict quasipoisson sd
#' @importFrom shinyhelper helper observe_helpers
#' @importFrom shinyjs hide show
#' @importFrom utils head
#' @importFrom waiter Waitress
#' @importFrom readr write_csv
#' @importFrom RPostgres dbConnect Postgres dbDisconnect
#' @noRd
app_server <- function(input, output, session) {
  # User inputs ----

  observe_helpers()

  inp_d <- reactive({
    input$d
  })

  inp_y <- reactive({
    y2 <- (min(input$y[1], input$y[2])):(max(input$y[1], input$y[2]))
    y2 <- seq(min(y2), max(y2), by = input$i)
    return(y2)
  })

  inp_yr <- reactive({
    input$yr
  })

  inp_c <- reactive({
    input$c
  })

  inp_cr <- reactive({
    input$cr
  })

  inp_cr0 <- reactive({
    paste0("0-", inp_cr())
  })

  inp_s <- reactive({
    input$s
  })

  # Simulation ----

  wt <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

  ## 1. Model parameters ----

  max_dif <- 1
  sd_dif <- 1
  change_price_i_old <- 0

  ## 2. Filter data ----

  df_dtl <- reactive({
    print("Collecting model data...")
    wt$notify(position = "br")

    wt$inc(0.03)

    if (inp_d() == "agtpa") {
      dtrade <- escapstructuralgravity::agtpa_applications %>%
        select(!!sym("exporter"), !!sym("importer"), !!sym("pair_id"), !!sym("year"), !!sym("trade"), !!sym("dist"), !!sym("cntg"), !!sym("lang"), !!sym("clny"), !!sym("rta")) %>%
        filter(!!sym("year") %in% inp_y())
    } else {
      con <- dbConnect(
        drv = Postgres(), dbname = "structural_gravity",
        host = "localhost", user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
        password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
      )

      dtrade <- tbl(con, "usitc_trade_country") %>%
        select(exporter = !!sym("exporter_dynamic_code"), importer = !!sym("importer_dynamic_code"), !!sym("year"), !!sym("trade")) %>%
        filter(!!sym("year") %in% !!inp_y()) %>%
        inner_join(
          tbl(con, "usitc_gravity") %>%
            select(exporter = !!sym("exporter_dynamic_code"), importer = !!sym("importer_dynamic_code"), !!sym("year"), !!sym("dist"), !!sym("cntg"), !!sym("lang"), !!sym("clny"), !!sym("rta")) %>%
            filter(!!sym("year") %in% !!inp_y())
        ) %>%
        collect()

      dbDisconnect(con)

      # create pair_id

      # escapstructuralgravity::agtpa_applications %>%
      #   distinct(exporter, importer, pair_id)

      dtrade_pair <- dtrade %>%
        distinct(!!sym("exporter"), !!sym("importer")) %>%
        mutate(!!sym("pair_id") := row_number())

      dtrade <- dtrade %>%
        inner_join(dtrade_pair)

      dtrade
    }

    dtrade <- dtrade %>%
      mutate(
        !!sym("dist") := ifelse(is.na(log(!!sym("dist"))) | log(!!sym("dist")) == 0, 100, log(!!sym("dist"))),
        !!sym("log_dist") := log(!!sym("dist")),
        !!sym("intl") := ifelse(!!sym("exporter") != !!sym("importer"), 1, 0),
        !!sym("exporter") := ifelse(!!sym("exporter") == inp_cr(), inp_cr0(), !!sym("exporter")),
        !!sym("importer") := ifelse(!!sym("importer") == inp_cr(), inp_cr0(), !!sym("importer"))
      ) %>%
      # Create Yit
      group_by(!!sym("exporter"), !!sym("year")) %>%
      mutate(!!sym("y") := sum(!!sym("trade"), na.rm = TRUE)) %>%
      # Create Eit
      group_by(!!sym("importer"), !!sym("year")) %>%
      mutate(!!sym("e") := sum(!!sym("trade"), na.rm = TRUE)) %>%
      # Create Er
      group_by(!!sym("year")) %>%
      mutate(!!sym("e_r") := max(ifelse(!!sym("importer") == inp_cr0(), !!sym("e"), NA), na.rm = TRUE)) %>%
      arrange(!!sym("importer"))

    wt$inc(0.03)

    dtrade <- dtrade %>%
      ungroup() %>%
      mutate(
        !!sym("exp_year") := paste0(!!sym("exporter"), !!sym("year")),
        !!sym("imp_year") := paste0(!!sym("importer"), !!sym("year")),
        !!sym("pair_id_2") := as.character(ifelse(!!sym("exporter") == !!sym("importer"), "0-domestic", !!sym("pair_id")))
      )

    wt$inc(0.03)

    dtrade <- dtrade %>%
      group_by(!!sym("pair_id")) %>%
      mutate(!!sym("sum_trade") := sum(!!sym("trade"), na.rm = TRUE)) %>%
      ungroup()

    wt$inc(0.03)

    dtrade
  }) %>%
    bindCache(inp_d(), inp_y(), inp_yr(), inp_cr()) %>%
    bindEvent(input$go)

  ## 3. GE models ----

  ge_sim <- reactive({
    # STEP 1 ----

    dtrade <- df_dtl()

    error_0rows <- data.frame(
      error = "No data available for the selected parameters."
    )

    error_model <- data.frame(
      error = "The model cannot be estimated."
    )

    if (nrow(dtrade) == 0L | !any(inp_yr() %in% inp_y())) {
      return(error_0rows)
    }

    wt$inc(0.03)

    ### Step 1: Solve the baseline gravity model ----

    #### Stage 1: Obtain the estimates of pair fixed effects and the effects of RTAs ----

    # With the steps done before, it is straightforward to obtain the PPML regression shown in box #1 from page 112 in @yotov2016advanced.

    fit_baseline_app2 <- try(fepois(
      trade ~ rta | exp_year + imp_year + pair_id_2,
      data = filter(dtrade, !!sym("sum_trade") > 0),
      glm.iter = 500
    ))

    if (inherits(fit_baseline_app2, "try-error")) {
      wt$close()
      return(error_model)
    }

    wt$inc(0.03)

    # We can construct the variables for !!sym("exporter")-time, !!sym("importer")-time, and internal dyads with the estimated fixed effects from the model.

    dtrade <- dtrade %>%
      mutate(
        fe_exporter_bln = fixef(fit_baseline_app2)$exp_year[!!sym("exp_year")],
        fe_importer_bln = fixef(fit_baseline_app2)$imp_year[!!sym("imp_year")],
        !!sym("fe_pair_id_2_bln") := fixef(fit_baseline_app2)$pair_id_2[!!sym("pair_id_2")]
      )

    wt$inc(0.03)

    #### Stage 2: Regress the estimates of pair fixed effects on gravity variables and country fixed effects ----

    # Box #1 from page 113 in @yotov2016advanced can be divided into smaller chunks.
    # We start by filtering to keep the observation from 1994, and then we compute the !!sym("trade") costs ($\bar{t}_{ij}$ and $t_{ij}^{BLN}$) from the internal dyads fixed effects and the estimated RTA coefficient.

    dtrade <- dtrade %>%
      mutate(
        !!sym("tij_bar") := exp(!!sym("fe_pair_id_2_bln")),
        !!sym("tij_bln") := exp(!!sym("fe_pair_id_2_bln") + fit_baseline_app2$coefficients[!!sym("rta")] * !!sym("rta"))
      )

    wt$inc(0.03)

    # We need to create a table for the !!sym("year") 1994 and international flows, which we will use to predict the !!sym("trade") costs for the observations with zero !!sym("trade") flows. The reason to create a sub-table, instead of filtering observations in the regression function, is that it eases posterior work to predict the costs.

    ch2_application2_2 <- dtrade %>%
      filter(!!sym("year") == inp_yr(), !!sym("exporter") != !!sym("importer"))

    wt$inc(0.03)

    # Now, unlike the book, which duplicates $\overline{t_{ij}}$ by creating $t_{ij}$, we can fit a regression to estimate the costs. This is something that emerges from the small differences between expressing an idea in Stata or R.

    fit_costs_app2 <- try(fepois(
      tij_bar ~ log_dist + cntg + lang + clny | exporter + importer,
      data = ch2_application2_2,
      glm.iter = 500
    ))

    if (inherits(fit_costs_app2, "try-error")) {
      wt$close()
      return(error_model)
    }

    wt$inc(0.03)

    # With the regression, we add the fitted values to the sub-table.

    ch2_application2_2 <- ch2_application2_2 %>%
      mutate(!!sym("tij_no_rta") := predict(fit_costs_app2, ch2_application2_2)) %>%
      select(!!sym("exporter"), !!sym("importer"), !!sym("tij_no_rta"))

    wt$inc(0.03)

    # The final step is to keep the observations for the !!sym("year") 1994 in the original table and replace the missing costs with the predicted values.

    dtrade <- dtrade %>%
      filter(!!sym("year") == inp_yr()) %>%
      left_join(ch2_application2_2, by = c("exporter", "importer")) %>%
      mutate(
        !!sym("tij_bar") := ifelse(is.na(!!sym("tij_bar")), !!sym("tij_no_rta"), !!sym("tij_bar")),
        !!sym("tij_bln") := ifelse(is.na(!!sym("tij_bln")), !!sym("tij_bar") * exp(fit_baseline_app2$coefficients[!!sym("rta")] * !!sym("rta")), !!sym("tij_bln"))
      ) %>%
      select(-!!sym("tij_no_rta"))

    wt$inc(0.03)

    # Box #2 from page 113 in @yotov2016advanced is easier to replicate. The first part of completing Box #2 involves solving the constrained baseline gravity model.

    fit_constrained_app2 <- try(fepois(
      trade ~ 0 | exporter + importer,
      data = dtrade,
      offset = ~ log(tij_bln),
      glm.iter = 500
    ))

    if (inherits(fit_constrained_app2, "try-error")) {
      wt$close()
      return(error_model)
    }

    wt$inc(0.03)

    # With the fitted model, we can add the prediction and the $\xi^{BLN}$ column.

    dtrade <- dtrade %>%
      mutate(!!sym("tradehat_bln") := predict(fit_constrained_app2, dtrade)) %>%
      group_by(!!sym("exporter")) %>%
      mutate(xi_bln = sum(!!sym("tradehat_bln") * (!!sym("exporter") != !!sym("importer")), na.rm = TRUE)) %>%
      ungroup()

    wt$inc(0.03)

    # The book specifies that all other baseline indexes of interest can be obtained by applying the same procedure described in the previous application. Here we will obtain the multilateral resistances terms ($OMR^{BLN}$ and $IMR^{BLN}$) by adding the fixed effects from the constrained model to the data.

    dtrade <- dtrade %>%
      mutate(
        !!sym("fe_exporter_cns") := fixef(fit_constrained_app2)$exporter[!!sym("exporter")],
        !!sym("fe_importer_cns") := fixef(fit_constrained_app2)$importer[!!sym("importer")]
      )

    wt$inc(0.03)

    dtrade <- dtrade %>%
      mutate(
        omr_bln = !!sym("y") * !!sym("e_r") / exp(!!sym("fe_exporter_cns")),
        imr_bln = !!sym("e") / (exp(!!sym("fe_importer_cns")) * !!sym("e_r"))
      )

    wt$inc(0.03)

    ### Step II: Define a counterfactual scenario ----

    # Box #1 from page 114 in @yotov2016advanced is direct and consists in replacing the RTA values by zero if the pairs of countries are NAFTA members.

    dtrade <- dtrade %>%
      mutate(
        !!sym("no_rta") := ifelse(!!sym("exporter") %in% inp_c() & !!sym("importer") %in% inp_c(), 0, !!sym("rta")),
        !!sym("tij_cfl") := !!sym("tij_bar") * exp(fit_baseline_app2$coefficients[!!sym("rta")] * !!sym("no_rta"))
      )

    wt$inc(0.03)

    ### Step III: Solve the counterfactual model ----

    # The same procedure from the previous section applies to obtain the conditional general equilibrium effects and then compute the full endowment general equilibrium effects. We start by fitting a counterfactual model.

    fit_counterfactual_app2 <- try(fepois(
      trade ~ 0 | exporter + importer,
      data = dtrade,
      offset = ~ log(tij_cfl),
      glm.iter = 500
    ))

    if (inherits(fit_counterfactual_app2, "try-error")) {
      wt$close()
      return(error_model)
    }

    wt$inc(0.03)

    # With the fitted model we add the fixed effects.

    dtrade <- dtrade %>%
      mutate(
        !!sym("fe_exporter_cfl") := fixef(fit_counterfactual_app2)$exporter[!!sym("exporter")],
        !!sym("fe_importer_cfl") := fixef(fit_counterfactual_app2)$importer[!!sym("importer")]
      )

    wt$inc(0.03)

    # As we did in stage 2, we compute the multilateral resistance terms.

    dtrade <- dtrade %>%
      mutate(
        !!sym("omr_cfl") := !!sym("y") * !!sym("e_r") / exp(!!sym("fe_exporter_cfl")),
        !!sym("imr_cfl") := !!sym("e") / (exp(!!sym("fe_importer_cfl")) * !!sym("e_r"))
      )

    wt$inc(0.03)

    # Up to this point, we are ready to compute the conditional general equilibrium effects of !!sym("trade").

    dtrade <- dtrade %>%
      mutate(!!sym("tradehat_cfl") := predict(fit_counterfactual_app2, dtrade)) %>%
      group_by(!!sym("exporter")) %>%
      mutate(xi_cfl = sum(!!sym("tradehat_cfl") * (!!sym("exporter") != !!sym("importer")), na.rm = TRUE)) %>%
      ungroup()

    wt$inc(0.03)

    # Now we are going to compute the full endowment general equilibrium effects. We start by repeating the steps to obtain changes in $t_{ij}$ $\phi$ from the last section.

    dtrade <- dtrade %>%
      mutate(
        !!sym("change_tij") := !!sym("tij_cfl") / !!sym("tij_bln"),
        !!sym("phi") := ifelse(!!sym("importer") == !!sym("exporter"), !!sym("e") / !!sym("y"), 0)
      ) %>%
      group_by(!!sym("exporter")) %>%
      mutate(!!sym("phi") := max(!!sym("phi"), na.rm = TRUE)) %>%
      ungroup()

    wt$inc(0.03)

    # Now we compute changes in $p_i$, $p_j$ and $!!sym("trade")^{CFL}$, which is just a repetition of the previous steps with some adaptation.

    dtrade <- dtrade %>%
      group_by(!!sym("exporter")) %>%
      mutate(!!sym("change_p_i") := ((exp(!!sym("fe_exporter_cfl")) / !!sym("e_r")) / (exp(!!sym("fe_exporter_cns")) / !!sym("e_r")))^(1 / (1 - inp_s()))) %>%
      ungroup() %>%
      group_by(!!sym("importer")) %>%
      mutate(
        !!sym("change_p_j") := ifelse(!!sym("importer") == !!sym("exporter"), !!sym("change_p_i"), 0),
        !!sym("change_p_j") := max(!!sym("change_p_j"), na.rm = TRUE)
      ) %>%
      ungroup()

    wt$inc(0.03)

    dtrade <- dtrade %>%
      mutate(trade_cfl = !!sym("tradehat_cfl") * !!sym("change_p_i") * !!sym("change_p_j"))

    wt$inc(0.03)

    # Then we need a `while()` loop, but before, we need to duplicate some columns under new names for the loop operations.

    dtrade <- dtrade %>%
      mutate(
        !!sym("omr_cfl_0") := !!sym("omr_cfl"),
        !!sym("imr_cfl_0") := !!sym("imr_cfl"),
        !!sym("change_imr_full_0") := 1,
        !!sym("change_omr_full_0") := 1,
        !!sym("change_p_i_0") := !!sym("change_p_i"),
        !!sym("change_p_j_0") := !!sym("change_p_j"),
        !!sym("fe_exporter_cfl_0") := !!sym("fe_exporter_cfl"),
        !!sym("fe_importer_cfl_0") := !!sym("fe_importer_cfl"),
        !!sym("tradehat_0") := !!sym("tradehat_cfl"),
        !!sym("e_r_cfl_0") := !!sym("e_r")
      )

    wt$inc(0.03)

    # Furthermore, now we run the loop, where the step $N$ depends on the step $N-1$ as in the previous section.

    i2 <- 1
    while (sd_dif > 1e-3 | max_dif > 1e-3) {
      dtrade <- dtrade %>%
        mutate(trade_1 = !!sym("tradehat_0") * !!sym("change_p_i_0") * !!sym("change_p_j_0") / (!!sym("change_omr_full_0") * !!sym("change_imr_full_0")))

      # repeat the counterfactual model
      fit_counterfactual_app2_2 <- try(fepois(
        trade_1 ~ 0 | exporter + importer,
        data = dtrade,
        offset = ~ log(tij_cfl),
        glm.iter = 500
      ))

      if (inherits(fit_counterfactual_app2_2, "try-error")) {
        wt$close()
        return(error_model)
      }

      dtrade <- dtrade %>%
        mutate(
          !!sym("fe_exporter_cfl") := fixef(fit_counterfactual_app2_2)$exporter[!!sym("exporter")],
          !!sym("fe_importer_cfl") := fixef(fit_counterfactual_app2_2)$importer[!!sym("importer")]
        )

      # compute the conditional general equilibrium effects of !!sym("trade")
      dtrade <- dtrade %>%
        mutate(!!sym("tradehat_1") := predict(fit_counterfactual_app2_2, dtrade)) %>%
        group_by(!!sym("exporter")) %>%
        mutate(!!sym("y_cfl_1") := sum(!!sym("tradehat_1"), na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(!!sym("e_cfl_1") := ifelse(!!sym("importer") == !!sym("exporter"), !!sym("phi") * !!sym("y_cfl_1"), 0)) %>%
        group_by(!!sym("importer")) %>%
        mutate(!!sym("e_cfl_1") := max(!!sym("e_cfl_1"), na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(
          !!sym("e_r_cfl_1") := ifelse(!!sym("importer") == inp_cr0(), !!sym("e_cfl_1"), 0),
          !!sym("e_r_cfl_1") := max(!!sym("e_r_cfl_1"), na.rm = TRUE)
        )

      # compute the change in prices for exporters and importers
      dtrade <- dtrade %>%
        mutate(!!sym("change_p_i_1") := ((exp(!!sym("fe_exporter_cfl")) / !!sym("e_r_cfl_1")) /
          (exp(!!sym("fe_exporter_cfl_0")) / !!sym("e_r_cfl_0")))^(1 / (1 - inp_s())))

      # compute the change in prices for exporters and importers
      dtrade <- dtrade %>%
        group_by(!!sym("importer")) %>%
        mutate(
          !!sym("change_p_j_1") := ifelse(!!sym("importer") == !!sym("exporter"), !!sym("change_p_i_1"), 0),
          !!sym("change_p_j_1") := max(!!sym("change_p_j_1"), na.rm = TRUE)
        ) %>%
        ungroup()

      # compute both outward and inward multilateral resistance
      dtrade <- dtrade %>%
        mutate(
          !!sym("omr_cfl_1") := (!!sym("y_cfl_1") * !!sym("e_r_cfl_1")) / exp(!!sym("fe_exporter_cfl")),
          !!sym("imr_cfl_1") := !!sym("e_cfl_1") / (exp(!!sym("fe_importer_cfl")) * !!sym("e_r_cfl_1"))
        )

      # update the differences
      max_dif <- abs(max(dtrade$change_p_i_0 - change_price_i_old, na.rm = TRUE))
      sd_dif <- sd(dtrade$change_p_i_0 - change_price_i_old)
      change_price_i_old <- dtrade$change_p_i_0

      # compute changes in outward and inward multilateral resistance
      dtrade <- dtrade %>%
        mutate(
          !!sym("change_omr_full_1") := !!sym("omr_cfl_1") / !!sym("omr_cfl_0"),
          !!sym("change_imr_full_1") := !!sym("imr_cfl_1") / !!sym("imr_cfl_0"),
          !!sym("omr_cfl_0") := !!sym("omr_cfl_1"),
          !!sym("imr_cfl_0") := !!sym("imr_cfl_1"),
          !!sym("change_omr_full_0") := !!sym("change_omr_full_1"),
          !!sym("change_imr_full_0") := !!sym("change_imr_full_1"),
          !!sym("change_p_i_0") := !!sym("change_p_i_1"),
          !!sym("change_p_j_0") := !!sym("change_p_j_1"),
          !!sym("fe_exporter_cfl_0") := !!sym("fe_exporter_cfl"),
          !!sym("fe_importer_cfl_0") := !!sym("fe_importer_cfl"),
          !!sym("tradehat_0") := !!sym("tradehat_1"),
          !!sym("e_r_cfl_0") := !!sym("e_r_cfl_1")
        ) %>%
        select(-!!sym("fe_exporter_cfl"), -!!sym("fe_importer_cfl"))

      i2 <- i2 + 1
    }

    wt$inc(0.03)

    # The last loop allows us to obtain changes in $p_i^{full}$, $p_j^{full}$ and $y^{full}$.

    dtrade <- dtrade %>%
      mutate(
        !!sym("change_p_i_full") := ((exp(!!sym("fe_exporter_cfl_0")) / !!sym("e_r_cfl_0")) /
          (exp(!!sym("fe_exporter_cns")) / !!sym("e_r")))^(1 / (1 - inp_s())),
        !!sym("change_p_j_full") := !!sym("change_p_i_full") * (!!sym("exporter") == !!sym("importer"))
      ) %>%
      group_by(!!sym("importer")) %>%
      mutate(!!sym("change_p_j_full") := max(!!sym("change_p_j_full"), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(!!sym("y_full") := !!sym("change_p_i_full") * !!sym("y"))

    wt$inc(0.03)

    # Now we compute $e^{full} and $!!sym("e_r")^{full}$.

    dtrade <- dtrade %>%
      mutate(!!sym("e_full") := !!sym("change_p_j_full") * !!sym("e") * (!!sym("exporter") == !!sym("importer"))) %>%
      group_by(!!sym("importer")) %>%
      mutate(!!sym("e_full") := max(!!sym("e_full"), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(!!sym("e_full_r") := max(!!sym("e_full") * (!!sym("importer") == inp_cr0()), na.rm = TRUE))

    wt$inc(0.03)

    # We also need `!!sym("omr_full")` and `!!sym("imr_full")`. This part of the code needs *attention* because the IMR full term is computed in a different way compared to the previous section. Please see the script *RTAsEffects.do*.

    dtrade <- dtrade %>%
      mutate(
        !!sym("omr_full") := !!sym("y_full") * !!sym("e_r_cfl_0") / exp(!!sym("fe_exporter_cfl_0")),
        !!sym("imr_full") := !!sym("e_cfl_1") / (exp(!!sym("fe_importer_cfl_0")) * !!sym("e_r_cfl_0"))
      )

    wt$inc(0.03)

    # To complete this step, we compute $\xi^{full}$.

    dtrade <- dtrade %>%
      mutate(!!sym("x_full") := (!!sym("y_full") * !!sym("e_full") * !!sym("tij_cfl")) / (!!sym("imr_full") * !!sym("omr_full"))) %>%
      group_by(!!sym("exporter")) %>%
      mutate(xi_full = sum(!!sym("x_full") * (!!sym("importer") != !!sym("exporter")), na.rm = TRUE)) %>%
      ungroup()

    wt$inc(0.03)

    # STEP 4 ----

    indexes_final <- dtrade %>%
      select(
        !!sym("exporter"), starts_with("omr_"), !!sym("change_p_i_full"),
        starts_with("xi_"), !!sym("y"), !!sym("y_full")
      ) %>%
      distinct() %>%
      mutate(!!sym("exporter") := ifelse(!!sym("exporter") == as.character(inp_cr0()), as.character(inp_cr()), !!sym("exporter"))) %>%
      mutate(
        !!sym("change_p_i_full") := (1 - !!sym("change_p_i_full")) * 100,
        change_omr_cfl = ((!!sym("omr_bln") / !!sym("omr_cfl"))^(1 / (1 - as.integer(inp_s()))) - 1) * 100,
        change_omr_full = ((!!sym("omr_bln") / !!sym("omr_full"))^(1 / (1 - as.integer(inp_s()))) - 1) * 100,
        change_xi_cfl = (!!sym("xi_bln") / !!sym("xi_cfl") - 1) * 100,
        change_xi_full = (!!sym("xi_bln") / !!sym("xi_full") - 1) * 100
      ) %>%
      select(!!sym("exporter"), starts_with("change"), starts_with("y")) %>%
      left_join(
        dtrade %>%
          select(!!sym("importer"), !!sym("imr_bln"), !!sym("imr_cfl"), !!sym("imr_full")) %>%
          distinct() %>%
          mutate(!!sym("importer") := ifelse(!!sym("importer") == as.character(inp_cr0()), as.character(inp_cr()), !!sym("importer"))) %>%
          mutate(
            change_imr_cfl = ((!!sym("imr_bln") / !!sym("imr_cfl"))^(1 / (1 - as.integer(inp_s()))) - 1) * 100,
            change_imr_full = ((!!sym("imr_bln") / !!sym("imr_full"))^(1 / (1 - as.integer(inp_s()))) - 1) * 100
          ),
        by = c("exporter" = "importer")
      ) %>%
      mutate(
        rgdp_bln = !!sym("y") / ((!!sym("imr_bln"))^(1 / (1 - as.integer(inp_s())))),
        rgdp_full = !!sym("y_full") / ((!!sym("imr_full"))^(1 / (1 - as.integer(inp_s())))),
        change_rgdp_full = (!!sym("rgdp_bln") / !!sym("rgdp_full") - 1) * 100
      ) %>%
      select(
        !!sym("exporter"), !!sym("change_xi_cfl"), !!sym("change_xi_full"),
        !!sym("change_rgdp_full"), !!sym("change_imr_full"), !!sym("change_omr_full"), !!sym("change_p_i_full")
      ) %>%
      mutate_if(is.numeric, function(x) round(x, 2))

    wt$inc(0.03)

    wt$close()

    indexes_final
  }) %>%
    bindCache(inp_d(), inp_y(), inp_yr(), inp_cr()) %>%
    bindEvent(input$go)

  output$ge_sim <- renderDataTable({
    ge_sim()
  })

  # Cite ----

  site_url <- "https://gravity.tiid.org"

  cite_text <- reactive({
    glue(
      "ESCAP. \"ESCAP STRUCTURAL GRAVITY DASHBOARD\". <i>ESCAP</i>.
        Accessed {months(Sys.Date()) } { day(Sys.Date()) }, { year(Sys.Date()) }. { site_url }/."
    )
  })

  cite_bibtex <- reactive({
    glue("@misc{{escap_structural_gravity_{year(Sys.Date())},
      title = {{ESCAP Structural Gravity Dashboard}},
      url = {{{site_url}}},
      author = {{ESCAP}},
      publisher = {{ESCAP}},
      year = {{2022}},
      month = {{Jul}},
      note = {{Accessed: { months(Sys.Date()) } { day(Sys.Date()) }, { year(Sys.Date()) }}}}}")
  })

  output$citation_text <- renderUI({
    HTML(cite_text())
  })

  output$citation_bibtex <- renderUI({
    pre(cite_bibtex())
  })

  # Download ----

  output$dwn_ge_pre <- downloadHandler(
    filename = function() {
      glue("{ paste(inp_c(), collapse = '_') }_{ inp_cr() }_{ paste0('0', inp_s() * 10) }_{ min(inp_y()) }_{ max(inp_y()) }.csv")
    },
    content = function(filename) {
      write_csv(ge_sim(), filename)
    },
    contentType = "application/csv"
  )

  output$dwn_ge <- renderUI({
    req(input$go)
    downloadButton("dwn_ge_pre", label = "Download GE results")
  })

  # Hide boxes until viz is ready ----

  ## observe the button being pressed
  observeEvent(input$go, {
    if (input$go > 0) {
      show(id = "model_col")
    } else {
      hide(id = "model_col")
    }
  })

  # Footer ----

  output$site_footer <- renderText({
    glue("<center><i>ESCAP {year(Sys.Date())}.</i> Creative Commons BY 4.0 International License.
         The information displayed here is based on
         <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. These figures do not
         include services or foreign direct investment.</center>")
  })

  # Bookmarking ----

  observe({
    # Trigger this observer every time an input changes
    # strip shiny related URL parameters
    rvtl(input)
    setBookmarkExclude(c(
      "shinyhelper-modal_params", "own", "fmt", "sidebarCollapsed", "sidebarItemExpanded",
      "i", "go"
    ))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
