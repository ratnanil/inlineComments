inlinecomment <- function(){
  cont <- rstudioapi::getActiveDocumentContext()
  filepath <- cont$path
  rootpath <- here::here()
  filepath <- stringr::str_remove(filepath, rootpath)
  firstline <- cont$selection[[1]]$range$start[[1]]
  lastline <- cont$selection[[1]]$range$end[[1]]
  remote_url <- git2r::remote_url()
  remote_url <- gh_username_repo(remote_url)
  sha <- git2r::commits(n = 1)[[1]]$sha

  lineref <- glue::glue("{remote_url}/blob/{sha}{filepath}#L{firstline}-L{lastline}")
  ui <- shiny::fluidPage(
    shiny::actionButton("done", "Create issue"),
    shiny::textInput("title", "Issue Title", width = "100%"),
    shinyAce::aceEditor("body", mode = "markdown", height = "200px"),

  )
  server <- function(input, output) {
    observeEvent(input$done,{
      res <- system(glue::glue('gh issue create --title "{input$title}" --body "{lineref}<br/><br/>{input$body}"'))

      print(res)
      shiny::stopApp()
    })
  }
  shiny::shinyApp(ui, server)
}
