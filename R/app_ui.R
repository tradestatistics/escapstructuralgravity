#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import otsshinycommon
#' @importFrom highcharter highchartOutput
#' @importFrom shinyhelper helper
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter useWaitress
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      skin = styles$skin_color,
      theme = styles$css_files,

      dashboardHeader(title = "UN-TTC-SGD"),

      dashboardSidebar(
        useShinyjs(),
        useWaitress(),
        sidebarMenu(
          menuItem("Welcome", tabName = "welcome"),
          menuItem("Model", tabName = "model"),
          menuItem("Simulate", tabName = "simulate"),
          menuItem("Download", tabName = "download"),
          menuItem("Cite", tabName = "cite")
        )
      ),

      dashboardBody(
        fluidRow(
          col_12(
            HTML("<h1>UN Transport and Trade Connectivity Structural Gravity Dashboard</h1>"),
            HTML("The information displayed here is based on
            <a href='https://comtrade.un.org/'>UN Comtrade</a> datasets. This tool is released
            under Creative Commons BY 4.0 International License. These figures do not
            include services or foreign direct investment.</p>")
          )
        ),

        tabItems(
          # Welcome ----

          tabItem(
            tabName = "welcome",
            h1("Welcome"),
            p("The United Nations Economic and Social Commission for Asia and
            the Pacific (ESCAP), in collaboration with the United Nations
            Conference on Trade and Development (UNCTAD) and other four UN
            regional commissions, are implementing a Development Account Project
            on Transport and Trade Connectivity in the Age of Pandemics."),
            p("This project responds to a call for action to tackle the many
            social and economic dimensions of the COVID-19 crisis. In
            particular, this tools provides structural gravity modelling tools
            to estimate the impact of trade policies, such as tariff reductions
            or trade agreement provisions."),
            p("The structural gravity model is the workhorse of international
            trade analysis. The gravity model of trade is a structural model
            with solid theoretical foundations. This property makes the gravity
            framework particularly appropriate for counterfactual analysis,
            such as quantifying the effects of trade policy.")
          ),

          # Model ----

          tabItem(
            tabName = "model",
            fluidRow(
              col_12(
                HTML("<h1>Model</h1>")
              ),

              ## Variables ----

              col_12(
                hr(),
                h2("Filter")
              ),

              col_6(
                sliderInput(
                  "y",
                  "Years",
                  min = available_yrs_min(),
                  max = available_yrs_max(),
                  value = c(2002, 2014),
                  sep = "",
                  step = 1,
                  ticks = FALSE,
                  width = "100%"
                )
              ),

              col_6(
                sliderInput(
                  "i",
                  "Interval of years",
                  min = 1,
                  max = 5,
                  value = 4,
                  sep = "",
                  step = 1,
                  ticks = FALSE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Interval of years",
                    content = c("Yotov et al. (2016) suggest to use intervals of four years in gravity estimation.",
                                "",
                                "<b>References</b>",
                                "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                selectInput(
                  "r",
                  "Importer",
                  choices = available_reporters_iso(),
                  selected = c("can", "mex", "usa"),
                  selectize = TRUE,
                  width = "100%",
                  multiple = TRUE
                ) %>%
                  helper(
                    type = "inline",
                    title = "Select importers",
                    content = "You can select more than one importer. For example, to estimate effects for NAFTA, choose the US, Canada and Mexico.",
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                selectInput(
                  "p",
                  "Exporter",
                  choices = available_reporters_iso(),
                  selected = "all",
                  selectize = TRUE,
                  width = "100%",
                  multiple = TRUE
                ) %>%
                  helper(
                    type = "inline",
                    title = "Select exporters",
                    content = "You can select more than one exporter.",
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                selectInput(
                  "t",
                  "Model type",
                  choices = available_models(),
                  selected = "ppml",
                  selectize = TRUE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Available models",
                    content = c("Yotov et al. (2016) propose that PPML is an estimation methods that is consistent with the economic theory.",
                                "",
                                "<b>References</b>",
                                "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                selectInput(
                  "z",
                  "Drop zero flows",
                  choices = available_logicals(),
                  selected = "no",
                  selectize = TRUE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Drop zero flows",
                    content = c("You should set this to 'yes' for OLS. For PPML, being it a zero inflated model, can be set to 'no'."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                selectInput(
                  "d",
                  "Convert dollars to a fixed year",
                  choices = c("No", 2002:2019),
                  selected = "",
                  selectize = TRUE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Convert to dollars of the year",
                    content = c("Uses present value and/or future value equations to adjust money value
                              by yearly changes in GDP deflator. The source for the GDP deflator data is The World Bank."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                selectInput(
                  "c",
                  "Use country pairs for clustering",
                  choices = available_logicals(),
                  selected = "no",
                  selectize = TRUE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Country pairs for clustering",
                    content = c("Yotov et al. (2016) propose that in the event of violation of assumptions, we should cluster the standard errors to account for an accurate margin of error.",
                                "",
                                "<b>References</b>",
                                "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                selectInput(
                  "s",
                  "Section/Commodity",
                  choices = NULL,
                  selected = NULL,
                  selectize = TRUE,
                  width = "100%",
                  multiple = TRUE
                ) %>%
                  helper(
                    type = "inline",
                    title = "Section/Commodity",
                    content = c("Subset the data for a custom category (i.e. vaccine inputs is our own subset),
                              or for any official section or commodity in the Harmonised System.",
                                "",
                                "<b>References</b>",
                                "Hossain, K. and Nyirongo, V.<i> HS 2002 Classification by Section </i>. UN Stats Wiki, 2021."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_3(
                fileInput(
                  'own',
                  'Upload your own data',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    '.csv',
                    '.tsv',
                    '.xlsx',
                    '.sav',
                    '.dta'
                  ),
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Upload your own data",
                    content = c("Select any CSV/TSV/XLSX (Excel) or SAV/DTA (SPSS/Stata) file.",
                                "In order to join you own data to the data in our API, you need at least to provide year, reporter
                              and partner. See this
                              <a href='https://raw.githubusercontent.com/pachadotdev/tradestatistics-visualization-with-shiny/master/custom_variables_for_modelling_demo.csv'>example</a>
                              that uses ISO-3 codes for reporters and partners.",
                                "Any rows that doesn't match will be dropped and the data for the analysis will be the <b>intersection</b> between yours and ours."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_12(
                textInput(
                  "fml",
                  "Model formula",
                  "trade ~ log(dist) + log(gdp_exporter) + colony + comlang_off + contig + rta + tariff",
                  width = "100%",
                  placeholder = "Any valid R formula"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Model formula",
                    content = c("Write a valid formula in the context of the fixest package. The fixed effects (if any) are added automatically, so don't write those.",
                                "",
                                "<h4>Gravity variables in our database</h4>",
                                "<b>Exports and Imports (LHS)</b>
                              <ul>
                               <li>trade: Bilateral trade (exports, reported at destination) in USD of each year</li>
                              </ul>",
                                "<b>Distance for modelling (RHS)</b>
                              <ul>
                               <li>dist: Simple distance between most populated cities in km</li>
                               <li>distcap: Simple distance between capitals in km</li>
                              </ul>",
                                "<b>Additional variables for modelling</b>
                              <ul>
                                <li>gdp_exporter: GDP (in current USD) for the exporters</li>
                                <li>gdp_importer: GDP (in current USD) for the importers</li>
                                <li>gdp_percap_exporter: GDP per capita (in current USD) for the exporters</li>
                                <li>gdp_percap_importer: GDP per capita (in current USD) for the importers</li>
                                <li>colony: The two countries are/were in a colonial relation</li>
                                <li>comlang_ethno: The two countries have at least 9% of their population speaking the same language</li>
                                <li>comlang_off: The two countries share the same official language</li>
                                <li>contig: The two countries are next to each other</li>
                                <li>rta: The two countries are in a trade agreement</li>
                                <li>smctry: The two countries were or are the same country</li>
                                <li>tariff: Weighted by imports mean for the minimum values between Most Favoured Nation (MFN) tariff and Preferential Rate at origin-destination-product level</li>
                              </ul>",
                                "<b>References</b>",
                                "Berge, L. and McDermott, G.<i><a href='https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#14_Other_estimation_functions'>Fast Fixed-Effects Estimation: Short introduction</a></i>. CRAN, 2021."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "l"
                  )
              ),

              ## Model results ----

              col_12(
                align="center",
                hr(),
                actionButton(
                  "go",
                  "Give me the results for this model",
                  class = "btn-primary"
                )
              ),

              div(
                id = "model_col",
                col_12(
                  htmlOutput("hdata_stl", container = tags$h2),
                  tableOutput("hdata_dtl"),
                  htmlOutput("fit_stl", container = tags$h2),
                  tableOutput("fit_tidy"),
                  tableOutput("fit_glance")
                )
              )
            )
          ),

          # Simulate ----

          tabItem(
            tabName = "simulate",
            fluidRow(

              ## RTA changes ----

              col_12(
                hr(),
                h2("RTA modification")
              ),

              col_4(
                uiOutput("rc")
              ),

              col_4(
                selectInput(
                  "rm",
                  "RTA modification",
                  choices = c("Drop RTA" = 0L, "Subscribe RTA" = 1L),
                  selected = 0L,
                  selectize = TRUE,
                  width = "100%",
                  multiple = FALSE
                ) %>%
                  helper(
                    type = "inline",
                    title = "RTA action",
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
              ),

              col_4(
                sliderInput(
                  "ry",
                  "Since year",
                  min = available_yrs_min(),
                  max = available_yrs_max(),
                  value = 2002,
                  sep = "",
                  step = 1,
                  ticks = FALSE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Since year",
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
              ),

              ## Tariff changes ----

              col_12(
                hr(),
                h2("Tariff modification")
              ),

              col_4(
                uiOutput("mc")
              ),

              col_4(
                sliderInput(
                  "mm",
                  "Tariff modification (%)",
                  min = 0,
                  max = 30,
                  value = 10,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Alter tarriff situation for",
                    content = c("This corresponds to a 'what if' situation, for example, what would have happened (according
                              to the model) if the countries you've chosen increased/decreased their MFN avg rate starting in
                              a certain year (i.e. what if Chile would have increased their weighted average MFN/PFR rate to
                              25% since 2020).",
                                "",
                                "<b>References</b>",
                                "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              col_4(
                sliderInput(
                  "my",
                  "Since year",
                  min = available_yrs_min(),
                  max = available_yrs_max(),
                  value = 2002,
                  sep = "",
                  step = 1,
                  ticks = FALSE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Alter tarriff situation for",
                    content = c("This corresponds to a 'what if' situation, for example, what would have happened (according
                              to the model) if the countries you've chosen increased/decreased their MFN avg rate starting in
                              a certain year (i.e. what if Chile would have increased their weighted average MFN/PFR rate to
                              25% since 2020).",
                                "",
                                "<b>References</b>",
                                "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              ## Simulation results ----
              col_12(
                align="center",
                hr(),
                actionButton(
                  "go2",
                  "Give me the results for this simulation",
                  class = "btn-primary"
                )
              ),

              div(
                id = "simulate_col",
                col_12(
                  htmlOutput("pred_stl", container = tags$h2)
                ),
                col_6(
                  tableOutput("pred_trade_table")
                )
                # col_6(
                #   highchartOutput("pred_trade_plot")
                # )
              )
            )
          ),

          # Download ----

          tabItem(
            tabName = "download",
            fluidRow(
              col_12(
                htmlOutput("dwn_stl", container = tags$h2),
                htmlOutput("dwn_txt", container = tags$p),
                uiOutput("dwn_fmt"),
                uiOutput("dwn_dtl"),
                uiOutput("dwn_fit")
              )
            )
          ),

          # Cite ----

          tabItem(
            tabName = "cite",
            fluidRow(
              col_12(
                h2("Citation"),
                uiOutput("citation_text"),
                uiOutput("citation_bibtex")
              )
            )
          )
        ),

        # Footer ----

        fluidRow(
          col_12(
            hr(),
            htmlOutput("site_footer", container = tags$p)
          )
        )
      ),

      uiOutput(outputId = "dynamicUI")
    ),

    tags$footer(
      tags$link(rel = "shortcut icon", href = "img/favicon.ico")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "UN ESCAP - TTC Structural Gravity"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
