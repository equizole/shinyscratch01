#' The main application UI
app_ui <- function() {

    tagList(
        add_resources(),

        dashboardPage(
            skin = "green",

            dashboardHeader(title = "Hello World!"),

            dashboardSidebar(),

            dashboardBody(
                mod_template_ui("template_ui_1")
            )
        )
    )
}

#' Add external resources in the www folder
add_resources <- function() {

    # TODO - Update package name
    addResourcePath("www", system.file("app/www", package = "shdackr"))

    tags$head(
        # Add external resources here
        # For example, if you have a custom.css in the inst/app/www:
        # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
        # Or for example, you can add shinyalert::useShinyalert() here

        # TODO - Update favicon (default is a white letter S on a black background)
        tags$link(href = "www/favicon.ico", rel = "shortcut icon")
    )
}
