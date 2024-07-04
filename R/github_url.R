#' Get GitHub URL Components
#'
#' @param urls A string vector with GitHub URLs
#' @return A dataframe containing the components of a GitHub URL
#' @importFrom stringr str_match
#' @examples
#'  quartoextexp:::github_url_parts("http://github.com/tidyverse/dplyr")
#'  quartoextexp:::github_url_parts("https://github.com/tidyverse/dplyr.git")
github_url_parts <- function(urls) {
  tmp <- stringr::str_match(urls,'.*http[s]?://github.com/(([^/]+)/([^/]+)).*')
  tmp <- data.frame(tmp, stringsAsFactors = FALSE)
  colnames(tmp) <- c('url', 'user_repo', 'user', 'repo')
  tmp$user_repo <- sub('\\.git$','', tmp$user_repo)
  tmp$repo <- sub('\\.git$','', tmp$repo)
  tmp
}

#' Extract User and Repository from GitHub URL
#'
#' @param url The GitHub URL to extract the user and repository name from
#' @return A character vector containing the user and repository name
#' @examples
#' quartoextexp:::github_url_to_repo("http://github.com/tidyverse/dplyr")
#' quartoextexp:::github_url_to_repo("https://github.com/tidyverse/dplyr.git")
github_url_to_repo <- function(url) {
  github_url_parts(url)[["user_repo"]]
}

#' Extract Username from GitHub URL
#'
#' @param url The GitHub URL to extract the user name from
#' @return A character vector containing the user name
#' @examples
#' quartoextexp:::github_url_to_user("http://github.com/tidyverse/dplyr")
#' quartoextexp:::github_url_to_user("https://github.com/tidyverse/dplyr.git")
github_url_to_user <- function(url){
  github_url_parts(url)$user
}
