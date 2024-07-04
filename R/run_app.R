#' Run Quarto Extension Explorer Shiny App
#'
#' @param install_dir The installation directory for the Quarto extension by 
#'  default this is the current working directory
#' @param modal_on_startup Show the informational modal dialog on startup
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import keys
#' @import miniUI
#' @import dplyr
#' @import glue
#' @import quarto
#' @import DT
#' @examples
#' \dontrun{
#'  run_app()
#' }
#' @export
run_app <- function(install_dir = getwd(), modal_on_startup = TRUE) {
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
               "Way to handle Quarto extension:",
               choices = c(
                 "quarto add extension" = "quarto::quarto_add_extension",
                 "quarto use template" = "quarto::quarto_use_template",
                 "quarto update" = "quarto_update_extension",
                 "quarto remove" = "quarto_remove_extension"
                 ),
               selected = "quarto::quarto_add_extension"
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
             shiny::actionButton("run_btn", "Apply on Selected Extensions", icon = shiny::icon("download"), class = "btn btn-success btn-lg btn-block"),
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
    shiny::h3("Available Extensions"),
    DT::DTOutput("extensions_table"),
    shiny::h3("Installed Extensions"),
    DT::DTOutput("installed_table")
  ))
  
  server <- function(input, output, session) {
    
    if(modal_on_startup){
      showModal(modalDialog(
        title = "Warning",
        "Quarto extensions may execute code when documents are rendered. If you do not trust the author(s) of the Quarto extensions, do not install or use the selected Quarto extensions. Only install Quarto extensions from sources you trust.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))
    }
    
    installed_extensions <- reactive({
      # rerun on each install
      input$setup_btn
      input$run_btn
      return(quarto_list_extensions())
    })
    
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
    
    output$installed_table <- DT::renderDataTable({
      installed_df <- installed_extensions() 
      DT::datatable(
        installed_df,
        selection = 'none',
        rownames = FALSE,
        options = list(pageLength = 10)
      )
    })
    
    output$extensions_table <- DT::renderDataTable({
      extensions_df <- filtered_extensions() %>% 
        dplyr::mutate(id = glue::glue("{github_url_to_user(path)}/{name}")) %>%
        dplyr::mutate(installed = id %in% installed_extensions()$Id) %>%
        dplyr::mutate(name = glue::glue("[{name}]({path})")) %>%
        dplyr::select(name, description, author, installed) %>%
        dplyr::mutate(name = purrr::map(name, ~shiny::includeMarkdown(.x))) %>%
        dplyr::mutate(description = purrr::map(description, ~shiny::includeMarkdown(.x))) %>%
        dplyr::mutate(author = purrr::map(author, ~shiny::includeMarkdown(.x)))
      DT::datatable(
        extensions_df,
        selection = 'multiple',
        rownames = FALSE,
        options = list(pageLength = 10)
      )
    },
    escape = FALSE,
    server = FALSE
    )
    
    install_commands <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$setup_btn | input$keys == "enter", {
      selected_rows <- input$extensions_table_rows_selected
      if (length(selected_rows) > 0) {
        selected_extensions <- filtered_extensions()[selected_rows, ]
        install_dir <- input$install_dir
        
        commands <- purrr::map2(selected_extensions$path, selected_extensions$name, function(path, name) {
          id <- github_url_to_repo(path)
          if(input$setup_type == "quarto_remove_extension"){
            id <- glue::glue("{github_url_to_user(path)}/{name}")
          }
          paste0(input$setup_type, glue::glue('("{id}", no_prompt = TRUE)'))
        })
        install_commands(commands)
      
        output$install_status <- shiny::renderText({
          paste0(
            "R setup code:\n", paste(install_commands(), collapse = "\n"), paste("\n\nWill run it at:\n", install_dir, collapse="\n")
          )
        })
      } else {
        output$install_status <- shiny::renderText("No extensions selected for installation.")
      }
    })
  
    shiny::observeEvent(input$run_btn | input$keys == "shift+enter",{
      shiny::req(length(install_commands()) > 0)
      shiny::req(input$run_btn > 0 || input$keys == "shift+enter")
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
