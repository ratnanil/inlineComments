#' Insert Inline Comment
#'
#' Call this function as an inline comment
#'
#' @export
inlinecomment <- function(){
  # cont <- rstudioapi::getActiveDocumentContext()
  # filepath <- cont$path
  # rootpath <- here::here()
  # filepath <- stringr::str_remove(filepath, rootpath)
  # firstline <- cont$selection[[1]]$range$start[[1]]
  # lastline <- cont$selection[[1]]$range$end[[1]]
  # remote_url <- git2r::remote_url()
  # remote_url <- gh_username_repo(remote_url)
  # sha <- git2r::commits(n = 1)[[1]]$sha

  # return(glue::glue("{remote_url}/blob/{sha}{filepath}#L{firstline}-L{lastline}"))
  ui <- shiny::fluidPage(
    shiny::textInput("title", "Issue Title", width = "100%"),
    shiny::textAreaInput("body", "Issue Text",  width = "100%"),
  )
  server <- function(input, output) {
    output$value <- shiny::renderText({input$title})
  }
  shiny::shinyApp(ui, server)
}
