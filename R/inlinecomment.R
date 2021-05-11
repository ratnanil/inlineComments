inlinecomment <- function(){
  cont <- rstudioapi::getActiveDocumentContext()
  filepath <- cont$path
  rootpath <- rprojroot::find_root(rprojroot::is_git_root)
  filepath <- stringr::str_remove(filepath, rootpath)
  firstline <- cont$selection[[1]]$range$start[[1]]
  lastline <- cont$selection[[1]]$range$end[[1]]
  remote_url <- git2r::remote_url()
  remote_addr <- gh_username_repo(remote_url)
  remote_server <- remote_addr$server
  remote_user <- remote_addr$username
  remote_repo <- remote_addr$repo

  sha <- git2r::commits(n = 1)[[1]]$sha

  lineref <- glue::glue("https://{remote_server}/{remote_user}/{remote_repo}/blob/{sha}{filepath}#L{firstline}-L{lastline}")
  ui <- shiny::fluidPage(
    shiny::actionButton("done", "Create issue"),
    shiny::textInput("title", "Issue Title", width = "100%"),
    shinyAce::aceEditor("body", mode = "markdown", height = "200px"),

  )
  server <- function(input, output) {
    observeEvent(input$done,{
      respo <- gh::gh("POST /repos/{owner}/{repo}/issues", owner = remote_user, repo = remote_repo, title = input$title, body = glue::glue("{lineref}<br/><br/>{input$body}"))
      Sys.sleep(1)
      shiny::showModal(shiny::modalDialog(title = "Issue created",glue::glue('See {respo$html_url}'),easyClose = TRUE))
      # shiny::stopApp()
    })
  }
  shiny::shinyApp(ui, server)
}
