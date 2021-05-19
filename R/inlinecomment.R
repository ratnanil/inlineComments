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
    multiple_remotes <- "<b style='color:red;'>Warning: multiple remotes detected. Currently, you cannot choose which remote to push to.</b>"
  } else{
    multiple_remotes <- ""
  }

  remote_addr <- gh_username_repo(remote_url)
  remote_server <- remote_addr$server
  remote_user <- remote_addr$username
  remote_repo <- remote_addr$repo

  sha <- git2r::commits(n = 1)[[1]]$sha

  markdown_examples <- "

You can use syntax highlighting with markdown within your issue.

<ul>
<li>Wrapping words inbetween <code>*single asterix*</code> will render your text cursive, like <i>this</i>.</li>
<li> Using <code>**double asterix**</code> will render your text bold, like <b>this</b> </li>
<li>  To make code examples, place the code in between two lines containg three
backticks (<code>```</code>), like so:</li>
</ul>


<pre>
  <code>
    ```
    your code example
    ```
  </code>
</pre>

For more information, checkout the <a href = 'https://guides.github.com/pdfs/markdown-cheatsheet-online.pdf'>markdown cheatsheet</a>
"

  lineref <- glue::glue("https://{remote_server}/{remote_user}/{remote_repo}/blob/{sha}{filepath}#L{firstline}-L{lastline}")
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton("done", "Create issue"),
                    shiny::actionButton("cancel", "Cancel"),
                    # shiny::selectInput("remote","",remote_url,selected = remote_url[1], width = "60%")
      )



    ),
    shiny::fluidRow(
      shiny::textInput("title",label = "", width = "60%",placeholder = "Issue Title"),
      shiny::HTML(glue::glue('Referencing line(s): <a href = "{lineref}">{lineref}</a>'))
    ),
    shiny::fluidRow(
      shiny::tabsetPanel(id = "mytabset",
        shiny::tabPanel("Editor",{
          shiny::column(12,
                        shinyAce::aceEditor("body", mode = "markdown", height = "200px", wordWrap = TRUE),
                        shiny::HTML(multiple_remotes)

          )

        }),
        shiny::tabPanel("Preview",{
          shiny::htmlOutput("knitDoc")
        }),
        shiny::tabPanel("Help",{
          # shiny::HTML(knitr::knit2html(text = isolate(markdown_examples), fragment.only = TRUE, quiet = TRUE))
          shiny::HTML(markdown_examples)

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
    shiny::observeEvent(input$cancel, {shiny::stopApp()})


    output$knitDoc <- shiny::renderUI({
      input$mytabset
      shiny::HTML(knitr::knit2html(text = isolate(input$body), fragment.only = TRUE, quiet = TRUE))
    })
  }
  shiny::shinyApp(ui, server)
}
