inlinecomment <- function(){
  cont <- rstudioapi::getActiveDocumentContext()
  filepath <- cont$path
  rootpath <- rprojroot::find_root(rprojroot::is_git_root)
  filepath <- stringr::str_remove(filepath, rootpath)
  firstline <- cont$selection[[1]]$range$start[[1]]
  lastline <- cont$selection[[1]]$range$end[[1]]
  remote_url <- git2r::remote_url()
  if(length(remote_url)>1){
    remote_url <- remote_url[1]
    multiple_remotes <- TRUE
  }
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
      shiny::textInput("title",label = "", width = "60%",placeholder = "Issue Title"),
      shiny::HTML(glue::glue('Referencing line: <a href = "{lineref}">{lineref}</a>'))
    ),
    shiny::fluidRow(
      shiny::tabsetPanel(id = "mytabset",
        shiny::tabPanel("Editor",{
          shiny::column(12,
                        shinyAce::aceEditor("body", mode = "markdown", height = "200px", wordWrap = TRUE),
                        shiny::HTML("<b style='color:red;'>Warning: multiple remotes detected. Currently, you cannot choose which remote to push to.</b>")

          )

        }),
        shiny::tabPanel("Preview",{
          htmlOutput("knitDoc")
        })
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

    output$knitDoc <- shiny::renderUI({
      input$mytabset
      shiny::HTML(knitr::knit2html(text = isolate(input$body), fragment.only = TRUE, quiet = TRUE))
    })
  }
  shiny::shinyApp(ui, server)
}
