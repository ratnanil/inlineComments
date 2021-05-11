gh_username_repo <- function(gh_remote){

  if(startsWith(gh_remote, "https://github")){
    matches <- stringr::str_match(gh_remote, "https://(github.+)/(.+)/(.+)")
  } else if(startsWith(gh_remote,"git@github")){
    matches <- stringr::str_match(gh_remote, "git@(github.+):(.+)/(.+)")
  } else{
    stop("input does not seem to be a valid github remote")
  }
  gh_server <- matches[,2]
  username <- matches[,3]
  repo <- stringr::str_remove(matches[,4], "\\.git$")

  return(list(server = gh_server, username = username, repo = repo))

}
