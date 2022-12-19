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
                box(
                    title = "Salary",
                    status = "success",
                    solidHeader = TRUE,
                    width = "100%",
                    sliderInput(
                        ns("salary_hourly"),
                        label = "Hourly",
                        value = initial_hourly_salary,
                        min = 0,
                        max = 120,
                        step = 1,
                        width = "100%"
                    ),
                    sliderInput(
                        ns("salary_annual"),
                        label = "Annual",
                        value = initial_annual_salary,
                        min = 0,
                        max = 250000,
                        step = 5000,
                        width = "100%"
                    )
                ),
            )
        ),

        fluidRow(
            column(12,
                box(
                    title = "401k Distribution",
                    status = "success",
                    solidHeader = TRUE,
                    width = "100%",
                    sliderInput(
                        ns("distribution_401k"),
                        label = NULL,
                        value = 50000,
                        min = 0,
                        max = 150000,
                        step = 5000,
                        width = "100%"
                    )
                )
            ),
        ),

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

            # Display results
            output$calc_results <- reactable::renderReactable({
                df <- calc_tax_table(input$salary_annual, input$distribution_401k)

                reactable::reactable(
                    df,
                    defaultPageSize = 15,
                    compact = TRUE,
                    columns = list(
                        line_item = reactable::colDef(
                            name = "Line Item"
                        ),
                        monthly_amount = reactable::colDef(
                            name = "Monthly Amount",
                            format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 2)
                        ),
                        yearly_amount = reactable::colDef(
                            name = "Yearly Amount",
                            format = reactable::colFormat(prefix = "$", separators = TRUE, digits = 2)
                        ),
                        tax_bracket = reactable::colDef(
                            name = "Tax Bracket",
                            format = reactable::colFormat(percent = TRUE, digits = 0)
                        ),
                        effective_tax_rate = reactable::colDef(
                            name = "Effective Tax Rate",
                            format = reactable::colFormat(percent = TRUE, digits = 1)
                        )
                    ),
                    rowStyle = function(index) {
                        line_item = df[[index, "line_item"]]

                        if (!is.na(line_item) & stringr::str_starts(line_item, "Total")) {
                            list(background = "rgba(0, 0, 0, 0.05)")
                        } else if (!is.na(line_item) & line_item %in% c("Gross Income", "Net Income")) {
                            list(background = "rgba(102, 204, 0, 0.05)")
                        }
                    }
                )
            })
        }
    )
}
