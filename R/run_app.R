#' Run the Shiny Application
#'
#' @export
run_app <- function() {
    options(shiny.autoload.r = FALSE)
    shinyApp(ui = app_ui(), server = app_server)
}
