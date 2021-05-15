inlinecomment <- function(remote_url = NULL){
  cont <- rstudioapi::getActiveDocumentContext()
  filepath <- cont$path
  rootpath <- rprojroot::find_root(rprojroot::is_git_root)
  filepath <- stringr::str_remove(filepath, rootpath)
  firstline <- cont$selection[[1]]$range$start[[1]]
  lastline <- cont$selection[[1]]$range$end[[1]]
  if(is.null(remote_url)) remote_url <- git2r::remote_url()
  remote_addr <- gh_username_repo(remote_url)
  remote_server <- remote_addr$server
  remote_user <- remote_addr$username
  remote_repo <- remote_addr$repo

  sha <- git2r::commits(n = 1)[[1]]$sha

  lineref <- glue::glue("https://{remote_server}/{remote_user}/{remote_repo}/blob/{sha}{filepath}#L{firstline}-L{lastline}")
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
               shiny::actionButton("done", "Create issue"),
               # shiny::selectInput("remote","",remote_url,selected = remote_url[1], width = "60%")
             )



    ),
    shiny::fluidRow(
      shiny::column(12,
             shiny::textInput("title",label = "", width = "60%",placeholder = "Issue Title"),
             shiny::HTML(glue::glue('<a href = "{lineref}">{lineref}</a>')),
             shinyAce::aceEditor("body", mode = "markdown", height = "200px", wordWrap = TRUE),
      )

    )


  )
  server <- function(input, output) {
    shiny::observeEvent(input$done,{
      if(stringr::str_length(input$title) == 0){
        showModal(modalDialog("You need to provide a title",title = "Title is empty",easyClose = TRUE))
      } else{
        respo <- gh::gh("POST /repos/{owner}/{repo}/issues", owner = remote_user, repo = remote_repo, title = input$title, body = glue::glue("{lineref}<br/><br/>{input$body}"))
        Sys.sleep(0.5)
        shiny::showModal(
          shiny::modalDialog(
            title = "Issue created",
            shiny::HTML(glue::glue('Issue created here: <a href = "{respo$html_url}">{respo$html_url}</a>')),
            footer = shiny::actionButton("ok", "OK"),
            easyClose = TRUE),
        )
      }
    })
    shiny::observeEvent(input$ok, {shiny::stopApp()})
  }
  shiny::shinyApp(ui, server)
}
