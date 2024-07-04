#' Fetch and Parse Quarto Extension Listing YAML Files from GitHub
#'
#' @return A dataframe that represents the parsed content of the extension YAML files
#' and includes the file name as an additional column.
#' @importFrom httr GET status_code content
#' @importFrom purrr map
#' @importFrom yaml yaml.load
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr bind_rows filter
#' @examples
#' \dontrun{
#'  get_extensions()
#' }
get_extensions <- function() {
  url_base <- "https://raw.githubusercontent.com/quarto-dev/quarto-web/main/docs/extensions/listings/"
  extensions <- c(
    "shortcodes-and-filters.yml",
    "journal-articles.yml",
    "custom-formats.yml",
    "revealjs.yml",
    "revealjs-formats.yml"
  )
  extension_list <- purrr::map(extensions, function(ext) {
    url <- paste0(url_base, ext)
    res <- httr::GET(url)
    if (httr::status_code(res) == 200) {
      content_list <- httr::content(res, "text") %>%
        yaml::yaml.load()
      content_list <- purrr::map(content_list, function(item) {
        item$file_name <- tools::file_path_sans_ext(ext)
        return(item)
      })
      return(content_list)
    } else {
      return(NULL)
    }
  })
  dplyr::bind_rows(extension_list) %>% dplyr::filter(!is.null(name))
}
