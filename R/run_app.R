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

#' Get GitHub URL Components
#'
#' @param urls A string vector with GitHub URLs
#' @return A dataframe containing the components of a GitHub URL
#' @importFrom stringr str_match
#' @examples
#'  quartoextexp:::githubURLParts("http://github.com/tidyverse/dplyr")
#'  quartoextexp:::githubURLParts("https://github.com/tidyverse/dplyr.git")
githubURLParts <- function(urls) {
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
  githubURLParts(url)[["user_repo"]]
}

#' Run Quarto Extension Explorer Shiny App
#'
#' @param install_dir The installation directory for the Quarto extension
#'  by default this is the current working directory
#' @import DT
#' @import shiny
#' @import keys
#' @import miniUI
#' @import dplyr
#' @import glue
#' @import quarto
#' @examples
#' \dontrun{
#'  run_app()
#' }
#' @export
run_app <- function(install_dir = getwd()) {
  # Fetch extensions once at the start
  extensions <- get_extensions()
  
  # Lookup table for extension categories
  category_lookup <- data.frame(
    file_name = c("shortcodes-and-filters", "journal-articles", "custom-formats", "revealjs", "revealjs-formats"),
    category = c("Shortcode/Filter", "Journal Articles", "Custom Formats", "Revealjs", "Revealjs")
  )
  
  # Lookup table for extension category descriptions
  lookup_df <- data.frame(
    file_name = c("shortcodes-and-filters.yml", "journal-articles.yml", "custom-formats.yml", "revealjs.yml", "revealjs-formats.yml"),
    description = c(
      "Special markdown directives that generate various types of httr::content and/or add new markdown rendering behaviors",
      "Enable authoring of professional Journal articles using markdown, and produce both LaTeX (PDF) and HTML versions of the articles.",
      "Create new output formats by bundling together document options, templates, stylesheets, and other httr::content.",
      "Extend the capabilities of HTML presentations created with Revealjs.",
      "Custom Revealjs output formats / templates."
    ),
    stringsAsFactors = FALSE # Avoid converting strings to factors
  )
  lookup_df$file_name <- tools::file_path_sans_ext(lookup_df$file_name)
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      shiny::div("Quarto Extension Explorer")
    ),
    keys::useKeys(),
    keys::keysInput("keys", c("enter", "shift+enter")),
    miniUI::miniContentPanel(
    shiny::tags$style(type='text/css', '#install_status {white-space: pre-wrap;}'),
    shiny::fluidRow(
      shiny::column(12, 
             shiny::selectInput(
               "setup_type",
               "Way to install Quarto extension",
               choices = c(
                 "quarto add extension" = "quarto_add_extension"
                 # "quarto use template" = "quarto_use_template"
                 ),
               selected = "quarto use"
             )
           )
    ),
    shiny::fluidRow(
      shiny::column(12, 
             shiny::textInput("install_dir", "Installation Directory:", value = file.path(install_dir), width = "100%"),
             )
    ),
    shiny::fluidRow(
     shiny::textOutput("selectedDir"),
      shiny::column(12, 
             shiny::actionButton("setup_btn", "Setup R Code",  icon = shiny::icon("person-running"), class = "btn btn-primary btn-lg btn-block"),
             shiny::actionButton("run_btn", "Install Selected Extensions", icon = shiny::icon("download"), class = "btn btn-success btn-lg btn-block"),
             shiny::br(),
             shiny::verbatimTextOutput("install_status")
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(12, 
             shiny::textOutput("category_description"))
    ),
    shiny::fluidRow(
      shiny::column(3, shiny::actionButton("btn_shortcode", "Shortcode/Filter", icon = shiny::icon("cog"), class = "btn btn-primary btn-lg btn-block")),
      shiny::column(3, shiny::actionButton("btn_journal", "Journal Articles", icon = shiny::icon("file-alt"), class = "btn btn-primary btn-lg btn-block")),
      shiny::column(3, shiny::actionButton("btn_custom", "Custom Formats", icon = shiny::icon("file-code"), class = "btn btn-primary btn-lg btn-block")),
      shiny::column(3, shiny::actionButton("btn_reveal", "Revealjs", icon = shiny::icon("slideshare"), class = "btn btn-primary btn-lg btn-block"))
    ),
    shiny::hr(),
    DT::DTOutput("extensions_table")
  ))
  
  server <- function(input, output, session) {
    filtered_extensions <- shiny::reactiveVal(extensions)
    
    shiny::observeEvent(input$btn_shortcode, {
      type <- "shortcodes-and-filters"
      filtered_extensions(extensions %>% dplyr::filter(file_name == type))
      output$category_description <- shiny::renderText({
        lookup_df$description[lookup_df$file_name == type]
      })
    })
    
    shiny::observeEvent(input$btn_journal, {
      type <- "journal-articles"
      filtered_extensions(extensions %>% dplyr::filter(file_name == type))
      output$category_description <- shiny::renderText({
        lookup_df$description[lookup_df$file_name == type]
      })
    })
    
    shiny::observeEvent(input$btn_custom, {
      type <- "custom-formats"
      filtered_extensions(extensions %>% dplyr::filter(file_name == type))
      output$category_description <- shiny::renderText({
        lookup_df$description[lookup_df$file_name == type]
      })
    })
    
    shiny::observeEvent(input$btn_reveal, {
      type <- c("revealjs", "revealjs-formats")
      filtered_extensions(extensions %>% dplyr::filter(file_name %in% type))
      output$category_description <- shiny::renderText({
        lookup_df$description[lookup_df$file_name %in% type]
      })
    })
    
    output$extensions_table <- DT::renderDT({
      DT::datatable(
        filtered_extensions() %>% 
          dplyr::select(name, description, author) %>%
          dplyr::mutate(description = purrr::map(description, ~shiny::includeMarkdown(.x))) %>%
          dplyr::mutate(author = purrr::map(author, ~shiny::includeMarkdown(.x)))
        ,
        selection = 'multiple',
        rownames = FALSE,
        options = list(pageLength = 10)
      )
    }
    # escape = FALSE
    )
    
    install_commands <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$setup_btn | input$keys == "enter", {
      selected_rows <- input$extensions_table_rows_selected
      if (length(selected_rows) > 0) {
        selected_extensions <- filtered_extensions()[selected_rows, ]
        install_dir <- input$install_dir
        
        commands <- sapply(selected_extensions$path, function(repo_path) {
          paste0(input$setup_type, glue::glue('("{github_url_to_repo(repo_path)}", no_prompt = TRUE)'))
        })
        install_commands(commands)
      
        output$install_status <- shiny::renderText({
          paste0(
            "R setup code:\n ", paste(install_commands(), collapse = "\n"), paste("\n\nWill it at:\n", install_dir, collapse="\n")
          )
        })
      } else {
        output$install_status <- shiny::renderText("No extensions selected for installation.")
      }
    })
  
    shiny::observeEvent(input$run_btn | input$keys == "shift+enter",{
      shiny::req(install_commands())
      shiny::req(input$run_btn | input$keys == "shift+enter")
      message("Installing extensions...")
      tmp <- setwd(input$install_dir)
      sapply(install_commands(), function(x){
        message("Running...\n", x)
        eval(parse(text=x))
      })
      setwd(tmp)
      message("Done!")
    })
    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
        shiny::stopApp(returnValue)
    })
  }
  
  shiny::runGadget(ui, server)
}
