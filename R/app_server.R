#' The main application server
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
app_server <- function(input, output, session) {

    # List the first level server modules here
    mod_template_server("template_ui_1")
}
