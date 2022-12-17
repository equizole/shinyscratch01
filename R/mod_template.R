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

    # Remember to wrap your reactive variable IDs in ns()
    tagList(
        fluidRow(
            column(4, textInput(ns("name"), label = NULL, placeholder = "Your name")),
            column(2, actionButton(ns("hello_button"), label = "Say Hello"))
        ),

        hr(),

        fluidRow(
            column(12, textOutput(ns("hello_name")))
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

            # When the user clicks on the "Hello" button update the reactive variable "name"
            name <- eventReactive(input$hello_button, {
                return(input$name)
            })

            # Say Hello!
            output$hello_name <- renderText({
                if (name() %in% "") {
                    return("Hello World!")
                } else {
                    return(paste0("Hello ", name(), "!"))
                }
            })

        }
    )
}
