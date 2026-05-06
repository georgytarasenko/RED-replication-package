# Function: run a script and print stage headers during execution to trace progress

run_with_stage_progress <- function(file,
                                    # Matching the section style, i.e. lines like: "# 2. Combine the years -----"
                                    header_regex = "^\\s*#.*-{4,}\\s*$",
                                    label_cleanup = function(x) sub("^\\s*#\\s*", "", x
                                    )) {
  
  # Read original script
  lines <- readLines(file, warn = FALSE)
  
  # Find header lines
  hdr_idx <- grep(header_regex, lines)
  
  # If no headers matched, just source the original script
  if (length(hdr_idx) == 0L) {
    # Fallback: just source the file at top-level
    message("No headers matched; sourcing as-is: ", file)
    sys.source(file, envir = .GlobalEnv, keep.source = TRUE)
    return(invisible(NULL))
  }
  
  # After each header comment, insert a message
  out <- character(0L)
  hdr_set <- rep(FALSE, length(lines))
  hdr_set[hdr_idx] <- TRUE
  
  for (i in seq_along(lines)) {
    out <- c(out, lines[i])
    if (hdr_set[i]) {
      label <- label_cleanup(lines[i])
      # Print to stderr so it appears live
      out <- c(out,
               sprintf('message("\\n==> %s")', label))
    }
  }
  
  # Write to a temp file 
  tf <- tempfile(pattern = paste0(basename(file), "_instrumented_"), fileext = ".R")
  writeLines(out, tf, useBytes = TRUE)
  
  # Source the instrumented script at top-level
  sys.source(tf, envir = .GlobalEnv, keep.source = TRUE)
  
  invisible(NULL)
}
