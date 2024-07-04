quarto_list_extensions <- function(quarto_args = NULL){
  quarto_bin <- quarto:::find_quarto()
  
  args <- "extensions"  
  
  x <- quarto_list(args, quarto_bin = quarto_bin)
  
  # Clean the stderr output to remove extra spaces and ensure consistent formatting
  stderr_cleaned <- gsub("\\s+$", "", x$stderr)
  
  read.table(text = stderr_cleaned, header = TRUE, fill = TRUE, sep = "", stringsAsFactors = FALSE)

}

quarto_list <- function(args = character(), ...){
  quarto:::quarto_run_what("list", args = args, ...)
}
