################################################################################
# This is a Shiny module template
#
# To be copied in the UI:
#   mod_template_ui("template_ui_1")
#
# To be copied in the server:
#   mod_template_server("template_ui_1")
################################################################################

#' Module UI
#'
#' A Shiny UI module.
#'
#' @param id Namespace Id
#'
#' @export
mod_template_ui <- function(id) {
    ns <- NS(id)

    # Initial salary
    initial_annual_salary <- 50000
    initial_hourly_salary <- round(initial_annual_salary / 2080, 0)

    # Remember to wrap your reactive variable IDs in ns()
    tagList(

        fluidRow(
            column(12,
                sliderInput(
                    ns("salary_hourly"),
                    label = "Salary -- Hourly",
                    value = initial_hourly_salary,
                    min = 0,
                    max = 50,
                    step = 1,
                    width = "100%")
            ),
        ),

        fluidRow(
            column(12,
                   sliderInput(
                       ns("salary_annual"),
                       label = "Salary -- Annual",
                       value = initial_annual_salary,
                       min = 0,
                       max = 100000,
                       step = 1000,
                       width = "100%")
            ),
        ),

        fluidRow(
            column(12,
                sliderInput(
                    ns("distribution_401k"),
                    label = "401k Distribution",
                    value = 50000,
                    min = 0,
                    max = 150000,
                    step = 5000,
                    width = "100%"
                )
            ),
        ),

        fluidRow(
            column(1, offset = 5,
                actionButton(ns("calc_button"), label = "Calculate")
            )
        ),

        hr(),

        fluidRow(
            column(12, reactable::reactableOutput(ns("calc_results")))
        )
    )
}


#' Module server
#'
#' A Shiny server module.
#'
#' @param id Namespace Id
#'
#' @export
mod_template_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {

            # When the hourly salary slider changes, update the annual salary slider
            observeEvent(input$salary_hourly,  {
                updateSliderInput(
                    session = session,
                    inputId = "salary_annual",
                    value = round(input$salary_hourly * 2080, -3)
                )
            })

            # When the annual salary slider changes, update the hourly salary slider
            observeEvent(input$salary_annual,  {
                updateSliderInput(
                    session = session,
                    inputId = "salary_hourly",
                    value = round(input$salary_annual / 2080, 0)
                )
            })

            calc_result <- eventReactive(input$calc_button, {
                income_salary <- input$salary_annual
                income_401k <- input$distribution_401k
                calc_tax_table(income_salary, income_401k)
            })


            # Display results
            output$calc_results <- reactable::renderReactable(
                reactable::reactable(
                    calc_result(),
                    defaultPageSize = 15,
                    columns = list(
                        type = reactable::colDef(
                            name = "Type"
                        ),
                        monthly_amount = reactable::colDef(
                            name = "Monthly Amount",
                            format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 2)
                        ),
                        yearly_amounts = reactable::colDef(
                            name = "Yearly Amount",
                            format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 2)
                        ),
                        tax_bracket = reactable::colDef(
                            name = "Tax Bracket",
                            format = reactable::colFormat(percent = TRUE, digits = 0)
                        ),
                        effective_tax_rates = reactable::colDef(
                            name = "Effective Tax Rate",
                            format = reactable::colFormat(percent = TRUE, digits = 1)
                        )
                    )
                )
            )
        }
    )
}
