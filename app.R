# app.R
library(shiny)
library(yaml)
library(httr)
library(purrr)
library(dplyr)
library(DT)
library(shinyFiles)
library(quarto)

# Function to fetch and parse YAML files from GitHub
get_extensions <- function() {
  url_base <- "https://raw.githubusercontent.com/quarto-dev/quarto-web/main/docs/extensions/listings/"
  extensions <- c(
    "shortcodes-and-filters.yml",
    "journal-articles.yml",
    "custom-formats.yml",
    "revealjs.yml",
    "revealjs-formats.yml"
  )
  extension_list <- map(extensions, function(ext) {
    url <- paste0(url_base, ext)
    res <- GET(url)
    if (status_code(res) == 200) {
      content_list <- content(res, "text") %>%
        yaml.load()
      content_list <- map(content_list, function(item) {
        item$file_name <- tools::file_path_sans_ext(ext)
        return(item)
      })
      return(content_list)
    } else {
      return(NULL)
    }
  })
  bind_rows(extension_list) %>% filter(!is.null(name))
}

# Function to parse a Github URL
githubURLParts <- function(urls) {
  tmp <- stringr::str_match(urls,'.*http[s]?://github.com/(([^/]+)/([^/]+)).*')
  tmp <- data.frame(tmp, stringsAsFactors = FALSE)
  colnames(tmp) <- c('url', 'user_repo', 'user', 'repo')
  tmp$user_repo <- sub('\\.git$','', tmp$user_repo)
  tmp$repo <- sub('\\.git$','', tmp$repo)
  tmp
}

# Function to extract the user/repo from a Github URL
github_url_to_repo <- function(url){
  githubURLParts(url)[["user_repo"]]
}

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
    "Special markdown directives that generate various types of content and/or add new markdown rendering behaviors",
    "Enable authoring of professional Journal articles using markdown, and produce both LaTeX (PDF) and HTML versions of the articles.",
    "Create new output formats by bundling together document options, templates, stylesheets, and other content.",
    "Extend the capabilities of HTML presentations created with Revealjs.",
    "Custom Revealjs output formats / templates."
  ),
  stringsAsFactors = FALSE # Avoid converting strings to factors
)
lookup_df$file_name <- tools::file_path_sans_ext(lookup_df$file_name)

ui <- fluidPage(
  tags$style(type='text/css', '#install_status {white-space: pre-wrap;}'),
  titlePanel("Quarto Extension Explorer"),
  fluidRow(
    column(12, 
           selectInput(
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
  fluidRow(
    column(12, 
           textInput("install_dir", "Installation Directory:", value = file.path(getwd()), width = "100%"),
           )
  ),
  fluidRow(
   textOutput("selectedDir"),
    column(12, 
           actionButton("setup_btn", "Setup R Code", icon = icon("person-running"), class = "btn btn-primary btn-lg btn-block"),
           actionButton("run_btn", "Install Selected Extensions",icon = icon("download"), class = "btn btn-success btn-lg btn-block"),
           br(),
           verbatimTextOutput("install_status")
    )
  ),
  hr(),
  fluidRow(
    column(12, 
           textOutput("category_description"))
  ),
  fluidRow(
    column(3, actionButton("btn_shortcode", "Shortcode/Filter", icon = icon("cog"), class = "btn btn-primary btn-lg btn-block")),
    column(3, actionButton("btn_journal", "Journal Articles", icon = icon("file-alt"), class = "btn btn-primary btn-lg btn-block")),
    column(3, actionButton("btn_custom", "Custom Formats", icon = icon("file-code"), class = "btn btn-primary btn-lg btn-block")),
    column(3, actionButton("btn_reveal", "Revealjs", icon = icon("slideshare"), class = "btn btn-primary btn-lg btn-block"))
  ),
  hr(),
  DTOutput("extensions_table")
)

server <- function(input, output, session) {
  filtered_extensions <- reactiveVal(extensions)
  
  observeEvent(input$btn_shortcode, {
    type <- "shortcodes-and-filters"
    filtered_extensions(extensions %>% filter(file_name == type))
    output$category_description <- renderText({
      lookup_df$description[lookup_df$file_name == type]
    })
  })
  
  observeEvent(input$btn_journal, {
    type <- "journal-articles"
    filtered_extensions(extensions %>% filter(file_name == type))
    output$category_description <- renderText({
      lookup_df$description[lookup_df$file_name == type]
    })
  })
  
  observeEvent(input$btn_custom, {
    type <- "custom-formats"
    filtered_extensions(extensions %>% filter(file_name == type))
    output$category_description <- renderText({
      lookup_df$description[lookup_df$file_name == type]
    })
  })
  
  observeEvent(input$btn_reveal, {
    type <- c("revealjs", "revealjs-formats")
    filtered_extensions(extensions %>% filter(file_name %in% type))
    output$category_description <- renderText({
      lookup_df$description[lookup_df$file_name %in% type]
    })
  })
  
  output$extensions_table <- renderDT({
    datatable(
      filtered_extensions() %>% 
        select(name, description, author) %>%
        mutate(description = map(description, ~includeMarkdown(.x))) %>%
        mutate(author = map(author, ~includeMarkdown(.x)))
      ,
      selection = 'multiple',
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  }
  # escape = FALSE
  )
  
  install_commands <- reactiveVal(NULL)
  
  observeEvent(input$setup_btn, {
    selected_rows <- input$extensions_table_rows_selected
    if (length(selected_rows) > 0) {
      selected_extensions <- filtered_extensions()[selected_rows, ]
      install_dir <- input$install_dir
      
      commands <- sapply(selected_extensions$path, function(repo_path) {
        paste0(input$setup_type, glue::glue('("{github_url_to_repo(repo_path)}", no_prompt = TRUE)'))
      })
      install_commands(commands)
    
      output$install_status <- renderText({
        paste0(
          "R setup code:\n ", paste(install_commands(), collapse = "\n"), paste("\n\nWill it at:\n", install_dir, collapse="\n")
        )
      })
    } else {
      output$install_status <- renderText("No extensions selected for installation.")
    }
  })

    observeEvent(input$run_btn,{
      message("Installing extensions...")
      tmp <- setwd(input$install_dir)
      sapply(install_commands(), function(x){
        message("Running...\n", x)
        eval(parse(text=x))
      })
      setwd(tmp)
      message("Done!")
  })

}

shinyApp(ui, server)
