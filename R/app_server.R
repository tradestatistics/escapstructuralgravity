#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import otsshinycommon
#' @importFrom broom glance tidy
#' @importFrom dplyr arrange bind_rows case_when collect everything ends_with
#'     filter group_by inner_join left_join mutate pull select summarise tbl
#'     ungroup distinct pull rename mutate_if
#' @importFrom fixest feols feglm
#' @importFrom glue glue
#' @importFrom ggplot2 aes facet_wrap geom_col ggplot labs scale_fill_viridis_d
#'     theme_minimal
#' @importFrom janitor clean_names
#' @importFrom lubridate day year
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom rio import export
#' @importFrom rlang sym
#' @importFrom stats as.formula predict quasipoisson
#' @importFrom shinyhelper helper observe_helpers
#' @importFrom shinyjs hide show
#' @importFrom tidyr pivot_longer expand_grid
#' @importFrom utils head
#' @importFrom waiter Waitress
#' @importFrom GEGravity ge_gravity
#' @noRd
app_server <- function(input, output, session) {
  # Connect to SQL ----

  con <- sql_con()

  # User inputs ----

  observe_helpers()

  inp_y <- reactive({
    y2 <- (min(input$y[1], input$y[2])):(max(input$y[1], input$y[2]))
    y2 <- seq(min(y2), max(y2), by = input$i)
    return(y2)
  })

  inp_r <- reactive({ sort(input$r) }) # reporter
  inp_p <- reactive({ sort(input$p) }) # partner
  inp_t <- reactive({ input$t }) # model type
  inp_z <- reactive({ input$z }) # drop zeros
  inp_d <- reactive({ input$d }) # adjust dollar
  inp_c <- reactive({ input$c }) # cluster
  inp_s <- reactive({
    s <- input$s

    s2 <- s[nchar(s) == 2]

    s4 <- otsshinycommon::commodities %>%
      filter(
        !(!!sym("section_code") %in% s2),
        !!sym("section_code") %in% s[nchar(s) == 4]
      ) %>%
      pull(!!sym("commodity_code"))

    s6 <- otsshinycommon::commodities %>%
      filter(
        !(!!sym("section_code") %in% s2),
        !(!!sym("section_code") %in% s[nchar(s) == 4]),
        !!sym("section_code") %in% s[nchar(s) == 6]
      ) %>%
      pull(!!sym("commodity_code"))

    return(c(s2, s4, s6))
  }) # section/commodity

  inp_fml <- reactive({ input$fml })
  inp_fmt <- reactive({ input$fmt })

  inp_rc <- reactive({ input$rc }) # rta action
  inp_rm <- reactive({ input$rm }) # rta modification
  inp_ry <- reactive({ input$ry }) # rta year
  inp_re <- reactive({ input$re }) # rta trade elasticity

  # inp_mc <- reactive({ input$mc }) # mfa action
  # inp_mm <- reactive({ input$mm }) # mfa modification
  # inp_my <- reactive({ input$my }) # mfa year

  # Simulation ----

  wt <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

  ## 1. upload custom data ----

  custom_data <- eventReactive(input$go, {
    uploaded_file <- input$own

    if(!is.null(uploaded_file)) {
      inp_data <- import(file = uploaded_file$datapath, format = tools::file_ext(uploaded_file$name)) %>%
        clean_names()

      return(inp_data)
    } else {
      data.frame()
    }
  })

  ## 2. define model formula ----

  # lhs <- eventReactive(input$go, {
  #   lhs <- gsub("\\s+", "", gsub("~.*", "", inp_fml()))
  #   return(lhs)
  # })
  #
  # rhs <- eventReactive(input$go, {
  #   rhs <- unlist(strsplit(gsub("\\s+", "", gsub(".*~", "", inp_fml())), "\\+"))
  #   rhs <- sort(rhs[rhs != "+"])
  #   return(rhs)
  # })
  #
  # raw_lhs <- reactive({
  #   x <- unlist(regmatches(lhs(), gregexpr("(?<=\\().*?(?=\\))", lhs(), perl = T)))
  #   x <- x[x != ""]
  #   return(x)
  # })
  #
  # raw_rhs <- reactive({
  #   x <- unlist(regmatches(rhs(), gregexpr("(?<=\\().*?(?=\\))", rhs(), perl = T)))
  #   x <- x[x != ""]
  #   return(x)
  # })

  fml <- eventReactive(input$go, {
    # fml <- paste0(lhs(), " ~ ", paste(rhs(), collapse = " + "))
    return(inp_fml())
  })

  ## 3. read from SQL ----

  df_dtl <- reactive({
    print("Collecting model data...")
    wt$notify(position = "br")

    ### 3.1. apply filters ----

    tbl_sql <- if (any(inp_s() != "all")) {
      "yrpc"
    } else {
      "yrp"
    }

    d <- tbl(con, tbl_sql)

    d <- d %>%
      filter(
        !!sym("year") %in% !!inp_y() &
          !!sym("reporter_iso") != !!sym("partner_iso")
      )

    if (any(inp_r() != "all")) {
      d <- d %>%
        filter(
          !!sym("reporter_iso") %in% !!inp_r()
        )
    }

    if (any(inp_p() != "all")) {
      d <- d %>%
        filter(
          !!sym("partner_iso") %in% !!inp_p()
        )
    }

    wt$inc(2)

    if (any(inp_s() %in% "vaccine")) {
      d <- d %>%
        left_join(
          tbl(con, "vaccine_inputs")
        ) %>%
        mutate(
          section_code = case_when(
            !!sym("is_vaccine_input") == 1L ~ "vaccine",
            TRUE ~ !!sym("section_code")
          )
        )
    }

    if (any(inp_s() != "all")) {
      sfull <- inp_s()
      s2 <- sfull[nchar(sfull) == 2]
      s4 <- sfull[nchar(sfull) == 4]
      s6 <- sfull[nchar(sfull) == 6]

      d <- d %>%
        filter(
          !!sym("section_code") %in% s2 |
            substr(!!sym("commodity_code"), 1, 4) %in% s4 |
            !!sym("commodity_code") %in% s6
        )
    }

    wt$inc(1)

    #### aggregate data ----

    d <- d %>%
      select(!!sym("year"),
             importer = !!sym("reporter_iso"),
             exporter = !!sym("partner_iso"),
             trade = !!sym("trade_value_usd_imp")) %>%
      group_by(!!sym("year"), !!sym("importer"), !!sym("exporter")) %>%
      summarise(trade = sum(!!sym("trade"), na.rm = T)) %>%
      ungroup()

    if (inp_z() == "yes") {
      d <- d %>%
        filter(!!sym("trade") > 0)
    }

    #### collect data ----

    d <- d %>%
      collect() %>%
      arrange(!!sym("year"), !!sym("importer"), !!sym("exporter"))

    wt$inc(1)

    ### 3.2. add geo dist data ----

    d <- d %>%
      mutate(
        country1 = pmin(!!sym("importer"), !!sym("exporter")),
        country2 = pmax(!!sym("importer"), !!sym("exporter"))
      ) %>%
      inner_join(
        tbl(con, "distances") %>% collect(),
        by = c("country1", "country2")
      ) %>%
      select(-!!sym("country1"),-!!sym("country2"))

    wt$inc(1)

    ### 3.3. add RTA data ----

    if (any(grepl("rta", fml()))) {
      d <- d %>%
        mutate(
          country1 = pmin(!!sym("importer"), !!sym("exporter")),
          country2 = pmax(!!sym("importer"), !!sym("exporter"))
        ) %>%
        left_join(
          tbl(con, "rtas") %>%
            filter(!!sym("year") %in% !!inp_y()) %>%
            collect(),
          by = c("year", "country1", "country2")
        ) %>%
        mutate(
          rta = case_when(
            is.na(!!sym("rta")) ~ 0L,
            TRUE ~ !!sym("rta")
          )
        ) %>%
        select(-!!sym("country1"),-!!sym("country2"))
    }

    wt$inc(2)

    ### 3.4. create fixed effects ----

    d <- d %>%
      mutate(
        importer_year = paste0(!!sym("importer"), !!sym("year")),
        exporter_year = paste0(!!sym("exporter"), !!sym("year"))
      )

    wt$inc(.5)

    ### 3.5. create clustering variable ----

    if (inp_c() == "yes") {
      d <- d %>%
        mutate(imp_exp = paste(!!sym("importer"), !!sym("exporter"), sep = "_"))
    }

    wt$inc(.5)

    ### 3.6. add GDP / GDP percap ----

    if (any(grepl("gdp", fml()))) {
      d <- d %>%
        inner_join(
          tbl(con, "gdp") %>%
            filter(!!sym("year") %in% !!inp_y()) %>%
            select(!!sym("country_iso"), !!sym("year"),
                   gdp_importer = !!sym("gdp"),
                   gdp_percap_importer = !!sym("gdp_percap")) %>%
            collect(),
          by = c("importer" = "country_iso", "year")
        )

      d <- d %>%
        inner_join(
          tbl(con, "gdp") %>%
            filter(!!sym("year") %in% !!inp_y()) %>%
            select(!!sym("country_iso"), !!sym("year"),
                   gdp_exporter = !!sym("gdp"),
                   gdp_percap_exporter = !!sym("gdp_percap")) %>%
            collect(),
          by = c("exporter" = "country_iso", "year")
        )
    }

    ### 3.7. convert dollars in time ----

    if (inp_d() != "No") {
      d <- gdp_deflator_adjustment_model(d, as.integer(inp_d()), sql_con = con)
    }

    ### 3.8. add tariffs data ----

    # here we need the applied tariffs when the product gets to destination

    if (any(grepl("tariff", fml()))) {
      tar <- tbl(con, "tariffs") %>%
        filter(
          !!sym("year") %in% !!inp_y()
        ) %>%
        select(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"),
               !!sym("section_code"), !!sym("commodity_code"),
               !!sym("tariff"))

      trd <- tbl(con, "yrpc") %>%
        filter(
          !!sym("year") %in% !!inp_y()
        ) %>%
        select(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"),
               !!sym("section_code"), !!sym("commodity_code"),
               !!sym("trade_value_usd_imp"))

      if (any(inp_r() != "all")) {
        tar <- tar %>%
          filter(
            !!sym("reporter_iso") %in% !!inp_r()
          )

        trd <- trd %>%
          filter(
            !!sym("reporter_iso") %in% !!inp_r()
          )
      }

      if (any(inp_p() != "all")) {
        tar <- tar %>%
          filter(
            !!sym("partner_iso") %in% !!inp_p()
          )

        trd <- trd %>%
          filter(
            !!sym("partner_iso") %in% !!inp_p()
          )
      }

      if (any(inp_s() %in% "vaccine")) {
        tar <- tar %>%
          left_join(
            tbl(con, "vaccine_inputs")
          ) %>%
          mutate(
            section_code = case_when(
              !!sym("is_vaccine_input") == 1L ~ "vaccine",
              TRUE ~ !!sym("section_code")
            )
          )

        trd <- trd %>%
          left_join(
            tbl(con, "vaccine_inputs")
          ) %>%
          mutate(
            section_code = case_when(
              !!sym("is_vaccine_input") == 1L ~ "vaccine",
              TRUE ~ !!sym("section_code")
            )
          )
      }

      if (any(inp_s() != "all")) {
        tar <- tar %>%
          filter(!!sym("section_code") %in% !!inp_s())

        trd <- trd %>%
          filter(!!sym("section_code") %in% !!inp_s())
      }

      trd <- trd %>%
        filter(
          !!sym("trade_value_usd_imp") > 0
        ) %>%
        inner_join(tar, by = c("year", "reporter_iso", "partner_iso", "commodity_code")) %>%
        select(
          !!sym("year"),
          importer = !!sym("reporter_iso"),
          exporter = !!sym("partner_iso"),
          !!sym("trade_value_usd_imp"),
          !!sym("tariff")
        ) %>%
        mutate(
          tariff = ifelse(is.na(!!sym("tariff")), 0, !!sym("tariff")),
          tariff_x_trade = !!sym("tariff") * !!sym("trade_value_usd_imp")
        ) %>%
        group_by(!!sym("year"), !!sym("importer")) %>%
        summarise(
          # NOT IMPLEMENTED IN POSTGRESQL
          # needs previous line tariff_x_trade
          # tariff = weighted.mean(!!sym("tariff"), !!sym("trade_value_usd_imp"), na.rm = T)
          tariff = (1 / 100) * sum(!!sym("tariff_x_trade"), na.rm = T) /
            sum(!!sym("trade_value_usd_imp"), na.rm = T)
        ) %>%
        ungroup() %>%
        collect()

      rm(tar)

      d <- d %>%
        inner_join(trd)

      rm(trd)
    }

    wt$inc(1)

    gc()

    # return(
    #   # TODO: improve this
    #   # it's not elegant, but works well with polynomials, logs, etc in formulas
    #   d[,
    #     colnames(d) %in%
    #       c("year", "importer", "exporter", "importer_year", "exporter_year",
    #         lhs(), rhs(), raw_lhs(), raw_rhs(),
    #         "imp_exp"
    #       )
    #   ]
    # )

    return(d)
  }) %>%
    bindCache(
      inp_y(), inp_r(), inp_p(), inp_t(), inp_z(),
      inp_d(), inp_c(), inp_s(),
      fml()
      # lhs(), rhs(), raw_lhs(), raw_rhs()
    ) %>%
    bindEvent(input$go)

  df_dtl_2 <- eventReactive(input$go, {
    ### 3.8. join with custom data ----

    if (nrow(custom_data()) > 0) {
      d <- df_dtl() %>% inner_join(custom_data())
    } else {
      d <- df_dtl()
    }

    d <- d %>% select(!!sym("year"), everything())

    return(d)
  })

  ## 4. Fit model ----

  fit <- eventReactive(input$go, {
    print("Fitting model...")

    fml <- as.formula(fml())

    if (any(inp_t() == "ols")) {
      if (inp_c() == "yes") {
        m <- tryCatch(
          feols(fml, df_dtl_2(), cluster = ~imp_exp),
          error = function(e) { custom_regression_error() }
        )
      } else {
        m <- tryCatch(
          feols(fml, df_dtl_2()),
          error = function(e) { custom_regression_error() }
        )
      }
    }

    if (inp_t() == "ppml") {
      if (inp_c() == "yes") {
        m <- tryCatch(
          feglm(fml, df_dtl_2(), cluster = ~imp_exp,
                family = quasipoisson(link = "log")),
          error = function(e) { custom_regression_error() }
        )
      } else {
        m <- tryCatch(
          feglm(fml, df_dtl_2(), family = quasipoisson(link = "log")),
          error = function(e) { custom_regression_error() }
        )
      }
    }

    wt$inc(1)
    wt$close()
    gc()

    return(m)
  })

  ## 5. Simulate ----

  wt2 <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

  pred_trade_table <- reactive({
    wt2$notify(position = "br")
    wt2$inc(1)

    ### model prediction ----

    d1 <-  d <- df_dtl_2() %>%
      filter(!!sym("exporter") %in% !!inp_rc(), !!sym("year") == !!inp_ry())

    if (length(inp_rc()) > 0) {
      d1_1 <- d1 %>%
        select(c("importer", "exporter", "rta")) %>%
        mutate(
          rta = case_when(
            !!sym("exporter") %in% !!inp_rc() & !!sym("importer") %in% !!inp_rc() ~ as.integer(!!inp_rm()),
            !!sym("importer") %in% !!inp_rc() & !!sym("exporter") %in% !!inp_rc() ~ as.integer(!!inp_rm()),
            TRUE ~ !!sym("rta")
          )
        )

      d1 <- d1 %>%
        left_join(d1_1, by = c("importer", "exporter")) %>%
        mutate(
          rta = case_when(
            !is.na(!!sym("rta.y")) ~ !!sym("rta.y"),
            TRUE ~ !!sym("rta.x")
          )
        ) %>%
        select(-ends_with(".x"), -ends_with(".y"))
    }

    d1 <- d1 %>%
      mutate(trade = predict(fit(), newdata = d1)) %>%
      ungroup() %>%
      group_by(!!sym("exporter")) %>%
      summarise(trade = sum(!!sym("trade"), na.rm = T)) %>%
      mutate(observation = "Fitted values")

    wt2$inc(1)

    ### ge prediction ----

    d2 <- df_dtl_2() %>%
      filter(!!sym("year") == inp_ry()) %>%
      select(!!sym("exporter"), !!sym("importer"), !!sym("trade"), !!sym("rta"))

    d2_full <- unique(c(d2 %>% distinct(!!sym("exporter")) %>% pull(),
        d2 %>% distinct(!!sym("importer")) %>% pull()))

    d2 <- expand_grid(exporter = d2_full, importer = d2_full) %>%
      arrange(!!sym("exporter"), !!sym("importer")) %>%
      left_join(d2) %>%
      mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))

    rm(d2_full)

    wt2$inc(1)

    if (length(inp_rc()) > 0) {
      d2_1 <- d2 %>%
        select(c("importer", "exporter", "rta")) %>%
        mutate(
          rta = case_when(
            !!sym("exporter") %in% !!inp_rc() & !!sym("importer") %in% !!inp_rc() ~ as.integer(!!inp_rm()),
            !!sym("importer") %in% !!inp_rc() & !!sym("exporter") %in% !!inp_rc() ~ as.integer(!!inp_rm()),
            TRUE ~ !!sym("rta")
          )
        )

      d2 <- d2 %>%
        left_join(d2_1, by = c("importer", "exporter")) %>%
        mutate(
          rta = case_when(
            !is.na(!!sym("rta.y")) ~ !!sym("rta.y"),
            TRUE ~ !!sym("rta.x")
          )
        ) %>%
        select(-ends_with(".x"), -ends_with(".y"))
    }

    wt2$inc(1)

    d2 <- d2 %>%
      mutate(rta_effect = !!sym("rta") * fit()$coefficients["rta"])

    d2_1 <- ge_gravity(
      exp_id = d2$exporter,     # Origin country associated with each observation
      imp_id = d2$importer,     # Destination country associated with each observation
      flows  = d2$trade + 1000, # Observed trade flows in the data for the year being used as the baseline
      beta   = d2$rta_effect,   # "Partial" change in trade, obtained as coefficient from gravity estimation
      theta  = inp_re(),        # Trade elasticity
      mult   = FALSE,           # Assume trade balance is an additive component of national expenditure
      data   = d2 %>%
        select(!!sym("exporter"), !!sym("importer"))
    )

    d2_1 <- d2_1 %>%
      filter(!!sym("exporter") %in% !!inp_rc()) %>%
      rename(trade = !!sym("new_trade")) %>%
      group_by(!!sym("exporter")) %>%
      summarise(trade = sum(!!sym("trade"), na.rm = T)) %>%
      mutate(observation = "GE simulation")

    d2 <- d2 %>%
      filter(!!sym("exporter") %in% !!inp_rc()) %>%
      group_by(!!sym("exporter")) %>%
      summarise(trade = sum(!!sym("trade"), na.rm = T)) %>%
      mutate(observation = "Observed trade") %>%
      bind_rows(d2_1) %>%
      bind_rows(d1)

    print(d2)

    wt2$inc(1)

    wt2$close()

    return(d2)
  }) %>%
    bindCache(
      inp_y(), inp_r(), inp_p(), inp_t(), inp_z(),
      inp_d(), inp_c(), inp_s(),
      fml(),
      inp_rc(), inp_rm(), inp_ry(), inp_re()
    ) %>%
    bindEvent(input$go2)

  pred_trade_plot <- reactive({
    g <- ggplot(data = pred_trade_table()) +
      geom_col(aes(x = toupper(!!sym("exporter")), y = !!sym("trade"), fill = !!sym("observation")), position = "dodge2") +
      labs(x = "Exporter", y = "USD", title = "Observed vs simulated data") +
      scale_fill_viridis_d() +
      theme_minimal()

    ggplotly(g)
  }) %>%
    bindCache(
      inp_y(), inp_r(), inp_p(), inp_t(), inp_z(),
      inp_d(), inp_c(), inp_s(),
      fml(),
      inp_rc(), inp_rm(), inp_ry()
    ) %>%
    bindEvent(input$go2)

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
      note = {{Accessed: { months(Sys.Date()) } { day(Sys.Date()) }, { year(Sys.Date()) }}}}}"
    )
  })

  # Outputs ----

  ## Dynamic / Server side selectors ----

  updateSelectizeInput(session, "s",
                       choices = list(
                         "All Products" = available_all(),
                         "Vaccine Inputs" = available_vaccine(),
                         "HS Sections" = available_sections_code(),
                         "HS Commodities (4-digits)" = available_commodities_short_code(),
                         "HS Commodities (6-digits)" = available_commodities_code()
                       ),
                       selected = "all",
                       server = TRUE
  )

  output$rc <- renderUI({
    # here we update the RTA modification selection after inp_r+inp_p
    # rp <- unique(as.character(inp_r()), as.character(inp_p()))
    # if (any(rp == "all")) { rp <- rp[rp != "all"] }
    # if (length(rp) == 0) { rp <- "all" }
    rp <- as.character(inp_p())

    selectInput(
      "rc",
      "Alter RTAs situation for",
      choices = available_reporters_iso(),
      selected = rp,
      selectize = TRUE,
      width = "100%",
      multiple = TRUE
    ) %>%
      helper(
        type = "inline",
        title = "Alter RTAs situation for",
        content = c("This corresponds to a 'what if' situation, for example, what would have happened (according
                              to the model) if the countries you've chosen dropped or subscribed their RTA starting in
                              a certain year (i.e. what if Chile and China would have subscribed their RTA back in 2002
                              instead of 2006).",
                    "",
                    "<b>References</b>",
                    "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
        buttonLabel = "Got it!",
        easyClose = FALSE,
        fade = TRUE,
        size = "s"
      )
  })

  output$mc <- renderUI({
    # here we update the MFA modification selection after inp_r+inp_p
    rp <- unique(as.character(inp_r()), as.character(inp_p()))
    if (any(rp == "all")) { rp <- rp[rp != "all"] }
    if (length(rp) == 0) { rp <- "all" }

    selectInput(
      "mc",
      "Alter MFNs situation for",
      choices = available_reporters_iso(),
      selected = rp,
      selectize = TRUE,
      width = "100%",
      multiple = TRUE
    ) %>%
      helper(
        type = "inline",
        title = "Alter MFNs situation for",
        content = c("This corresponds to a 'what if' situation, for example, what would have happened (according
                              to the model) if the countries you've chosen increased/decreased their MFN avg rate starting in
                              a certain year (i.e. what if Chile would have increased their MFN rates to an avg
                              of 25% since 2020).",
                    "",
                    "<b>References</b>",
                    "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
        buttonLabel = "Got it!",
        easyClose = FALSE,
        fade = TRUE,
        size = "s"
      )
  })

  ## Model ----

  hdata_stl <- eventReactive(input$go, { "Data preview" })
  fit_stl <- eventReactive(input$go, { "Model summary" })
  pred_stl <- eventReactive(input$go2, { "Model simulation" })

  output$hdata_stl <- renderText({ hdata_stl() })
  output$hdata_dtl <- renderTable({ head(df_dtl_2()) })
  output$fit_stl <- renderText({ fit_stl() })
  output$fit_tidy <- renderTable({ tidy(fit()) })
  output$fit_glance <- renderTable({ glance(fit()) })
  output$fit_cat <- renderPrint({ fit() })
  output$pred_stl <- renderText({ pred_stl() })
  output$pred_trade_table <- renderTable({ pred_trade_table() })
  output$pred_trade_plot <- renderPlotly({ pred_trade_plot() })

  ## Download ----

  dwn_stl <- eventReactive(input$go, { "Download model data" })

  dwn_txt <- eventReactive(input$go, {
    "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
  })

  dwn_fmt <- eventReactive(input$go, {
    selectInput(
      "fmt",
      "Download data as:",
      choices = available_formats(),
      selected = NULL,
      selectize = TRUE
    )
  })

  output$dwn_dtl_pre <- downloadHandler(
    filename = function() {
      glue("{ inp_t() }_{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }.{ inp_fmt() }")
    },
    content = function(filename) {
      export(df_dtl(), filename)
    },
    contentType = "application/zip"
  )

  output$dwn_fit_pre <- downloadHandler(
    filename = function() {
      glue("{ inp_t() }_{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }.rds")
    },
    content = function(filename) {
      saveRDS(fit(), filename)
    },
    contentType = "application/zip"
  )

  output$dwn_sim_pre <- downloadHandler(
    filename = function() {
      glue("simulation.{ inp_fmt() }")
    },
    content = function(filename) {
      export(pred_trade_table(), filename)
    },
    contentType = "application/zip"
  )

  output$dwn_stl <- renderText({ dwn_stl() })
  output$dwn_txt <- renderText({ dwn_txt() })
  output$dwn_fmt <- renderUI({ dwn_fmt() })

  output$dwn_dtl <- renderUI({
    req(input$go)
    downloadButton('dwn_dtl_pre', label = 'Detailed data')
  })

  output$dwn_fit <- renderUI({
    req(input$go)
    downloadButton('dwn_fit_pre', label = 'Fitted model')
  })

  output$dwn_sim <- renderUI({
    req(input$go2)
    downloadButton('dwn_sim_pre', label = 'Simulation results')
  })

  ## Cite ----

  output$citation_text <- renderUI({
    req(input$go)
    HTML(cite_text())
  })

  output$citation_bibtex <- renderUI({
    req(input$go)
    pre(cite_bibtex())
  })

  # Hide boxes until viz is ready ----

  ## observe the button being pressed
  observeEvent(input$go, {
    if (input$go > 0) {
      show(id = "model_col")
      show(id = "simulate_col")
    } else {
      hide(id = "model_col")
      hide(id = "simulate_col")
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
      "i", ".clientValue-default-plotlyCrosstalkOpts", "plotly_hover-A", "plotly_afterplot-A",
      "go", "go2"
    ))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
