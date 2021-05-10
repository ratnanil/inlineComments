gh_username_repo <- function(gh_remote){

  if(startsWith(gh_remote, "https://github")){
    matches <- stringr::str_match(gh_remote, "https://(github.+)/(.+)/(.+).git")
  } else if(startsWith(gh_remote,"git@github")){
    matches <- stringr::str_match(gh_remote, "git@(github.+):(.+)/(.+)")
  } else{
    stop("input does not seem to be a valid github remote")
  }
  gh_server <- matches[,2]
  username <- matches[,3]
  repo <- matches[,4]
  return(glue::glue("https://{gh_server}/{username}/{repo}"))
}
