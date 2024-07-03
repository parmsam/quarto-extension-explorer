quarto_list_extensions <- function(quarto_args = NULL){
  quarto_bin <- quarto:::find_quarto()
  
  args <- "extensions"  
  
  x <- quarto_list(args, quarto_bin = quarto_bin)
  
  readr::read_table(x$stderr)
}


quarto_list <- function(args = character(), ...){
  quarto:::quarto_run_what("list", args = args, ...)
}
