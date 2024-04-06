#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
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
      dashboardHeader(title = "ESCAP SGD"),
      dashboardSidebar(
        useShinyjs(),
        useWaitress(),
        sidebarMenu(
          menuItem("Welcome", tabName = "welcome"),
          menuItem("Model", tabName = "model")
          # menuItem("Cite", tabName = "cite")
        )
      ),
      dashboardBody(
        fluidRow(
          col_12(
            HTML("<h1>ESCAP Structural Gravity Dashboard</h1>")
          )
        ),
        tabItems(
          # Acknowledgement ----
          tabItem(
            tabName = "welcome",
            h1("Welcome"),
            p("The structural gravity model is the workhorse of international
            trade analysis. The gravity model of trade is a structural model
            with solid theoretical foundations. This property makes the gravity
            framework particularly appropriate for counterfactual analysis,
            such as quantifying the effects of trade policy, as it would be
            a backlash against free trade."),
            p(" "),
            h1("How to use this tool"),
            HTML("<p>We provide detailed
                 <a href='https://shiny.tradestatistics.io/docs/escapstructuralgravity.pdf'>instructions</a>
                 in PDF format.</p>"),
            h1("Technical details"),
            HTML("<p>This tool allows to run General Equilibrium counterfactual analysis based on the recommentations
              in Yotov et al. (2016). The idea is to provide a sort of scientific calculator to change the countries, years, and elasticity of substitution to explore the simulated effects of dropping a Regional Trade Agreement (RTA) in the past.</p>"),
            HTML("<p>Please click play on the video to see a short demonstration on how to use this tool.</p>"),
            HTML("<video controls width='800'>
                 <source src='https://shiny.tradestatistics.io/images/escapstructuralgravity.mp4' type='video/mp4'>
                 Your browser does not support the video tag.
                 </video>"),
            h1("References"),
            p("The main references for this work were the next materials."),
            HTML("<p>Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016.</p>"),
            HTML("<p>Vargas Sepulveda, M. <i><a href='https://pacha.dev/tradepolicy'>Solutions Manual for An Advanced Guide to Trade Policy Analysis in R</a></i>. UN ESCAP, 2024.</p>"),
            HTML("<p>UN ESCAP. <i><a href='https://r.tiid.org/'>ESCAP Online Training on Using R for Trade Analysis</a></i>. UN ESCAP, 2020.</p>"),
            p("Note: You can get a certificate if you finish the ESCAP online training.")
          ),

          # Model ----

          tabItem(
            tabName = "model",
            fluidRow(
              col_12(
                HTML("<h1>General equilibrium trade policy analysis with structural gravity</h1>")
              ),

              ## Variables ----

              col_12(
                selectInput(
                  "d",
                  "Dataset",
                  # choices = c("Advanced Guide to Trade Policy Analysis" = "agtpa", "International Trade and Production Database" = "itpd"),
                  choices = c("Advanced Guide to Trade Policy Analysis" = "agtpa"),
                  selected = "agtpa",
                  selectize = TRUE,
                  width = "100%",
                  multiple = FALSE
                )
              ),
              col_6(
                sliderInput(
                  "y",
                  "Years",
                  min = 1986L,
                  max = 2019L,
                  value = c(1986L, 2006L),
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
                    content = c(
                      "Yotov et al. (2016) suggest to use intervals of four years in gravity estimation.",
                      "",
                      "<b>References</b>",
                      "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."
                    ),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),
              col_6(
                selectInput(
                  "c",
                  "Countries",
                  choices = escapstructuralgravity::params$c,
                  selected = c("CAN", "USA", "MEX"),
                  selectize = TRUE,
                  width = "100%",
                  multiple = TRUE
                ) %>%
                  helper(
                    type = "inline",
                    title = "Select countries",
                    content = "You can select one or more countries. For example, to estimate effects for NAFTA, choose the US, Canada and Mexico.",
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),
              col_6(
                selectInput(
                  "cr",
                  "Reference country",
                  choices = escapstructuralgravity::params$c,
                  selected = "DEU",
                  selectize = TRUE,
                  width = "100%",
                  multiple = FALSE
                ) %>%
                  helper(
                    type = "inline",
                    title = "Select reference country",
                    content = "You can select one countries. For example, Germany.",
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),
              col_6(
                sliderInput(
                  "yr",
                  "RTA subscription year",
                  min = min(escapstructuralgravity::params$y),
                  max = max(escapstructuralgravity::params$y),
                  value = 1994L,
                  sep = "",
                  step = 1,
                  ticks = FALSE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Select RTA susbscription year",
                    content = "You can select one year. For example, 1994 for NAFTA.",
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),
              col_6(
                sliderInput(
                  "s",
                  "Elasticity of substitution",
                  min = 7,
                  max = 14,
                  value = 7,
                  sep = "",
                  step = 0.1,
                  ticks = FALSE,
                  width = "100%"
                ) %>%
                  helper(
                    type = "inline",
                    title = "Elasticity",
                    content = c(
                      "Yotov et al. (2016) suggest to use a value of seven.",
                      "",
                      "<b>References</b>",
                      "Yotov, Y. V., Piermartini, R., and Larch, M. <i><a href='https://www.wto.org/english/res_e/publications_e/advancedguide2016_e.htm'>An Advanced Guide to Trade Policy Analysis: The Structural Gravity Model</a></i>. WTO iLibrary, 2016."
                    ),
                    buttonLabel = "Got it!",
                    easyClose = FALSE,
                    fade = TRUE,
                    size = "s"
                  )
              ),

              # col_3(
              #   selectInput(
              #     "s",
              #     "Sector",
              #     choices = escapstructuralgravity::params$s,
              #     selected = "ALL",
              #     selectize = TRUE,
              #     width = "100%",
              #     multiple = TRUE
              #   )
              # ),
              # col_3(
              #   selectInput(
              #     "i",
              #     "Industry",
              #     choices = escapstructuralgravity::params$i,
              #     selected = "ALL",
              #     selectize = TRUE,
              #     width = "100%",
              #     multiple = TRUE
              #   )
              # ),

              ## Model results ----

              col_12(
                align = "center",
                hr(),
                actionButton(
                  "go",
                  "Compute the GE simulation",
                  class = "btn-primary"
                )
              ),
              div(
                id = "model_col",
                col_12(
                  hr(),
                  uiOutput("dwn_ge"),
                  hr(),
                  dataTableOutput("ge_sim")
                )
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
      app_title = "ESCAP SGD"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
