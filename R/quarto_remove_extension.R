quarto_remove_extension <- function(extension = NULL, no_prompt = FALSE, quiet = FALSE, quarto_args = NULL) {
  rlang::check_required(extension)
  
  quarto_bin <- quarto:::find_quarto()
  
  # This will ask for approval or stop installation
  approval <- quarto:::check_extension_approval(no_prompt, "Quarto extensions", "https://quarto.org/docs/extensions/managing.html")
  if(is.null(approval)) approval <- TRUE
  
  if (approval) {
    args <- c(extension, "--no-prompt", if (quiet) quarto:::cli_arg_quiet(), quarto_args)
    quarto_remove(args, quarto_bin = quarto_bin, echo = TRUE)
  }
  
  invisible()
}

quarto_remove <- function(args = character(), ...) {
  quarto:::quarto_run_what("remove", args = args, ...)
}
