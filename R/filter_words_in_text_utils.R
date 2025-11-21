#' Process a Single Text File
#' @keywords internal
process_single_file <- function(filepath, model, upos_tags, rewrite, dir_filtered,
                                min_word_count = 2000) {

  tryCatch({
    # Read file
    text <- paste(readLines(filepath, encoding = "UTF-8", warn = FALSE),
                  collapse = " ")

    # Clean text (basic cleaning)
    text <- clean_text(text)

    # Count words
    word_count <- length(strsplit(text, "\\s+")[[1]])

    # Check minimum word count
    if (word_count < min_word_count) {
      return(list(
        file = basename(filepath),
        status = "skipped",
        reason = sprintf("Less than %d words", min_word_count),
        word_count = word_count,
        pos_proportion = NA
      ))
    }

    # Annotate with UDPipe
    annotated <- udpipe::udpipe_annotate(model, x = text)
    tokens_df <- as.data.frame(annotated)

    # Filter by POS tags
    filtered_mask <- tokens_df$upos %in% upos_tags
    filtered_tokens <- tokens_df$token[filtered_mask]

    # Calculate proportion (for informational purposes)
    # Count only content words (not punctuation, symbols, etc.)
    content_words_mask <- tokens_df$upos %in% c("NOUN", "PROPN", "VERB",
                                                "ADJ", "ADV", "NUM")
    content_words_count <- sum(content_words_mask)

    if (content_words_count > 0) {
      pos_proportion <- length(filtered_tokens) / content_words_count
    } else {
      pos_proportion <- 0
    }

    # Create filtered text
    filtered_text <- paste(filtered_tokens, collapse = " ")

    # Remove email addresses
    filtered_text <- gsub("\\S+@\\S+\\.\\S+", "", filtered_text)

    # Remove single letter words (with or without periods)
    filtered_text <- gsub("\\b[a-z]\\.?\\b", "", filtered_text, ignore.case = TRUE)

    # Remove standalone punctuation and special characters
    # This catches: . , ; : ! ? - _ / \ | etc.
    filtered_text <- gsub("\\s+[[:punct:]]+\\s+", " ", filtered_text)

    # Remove punctuation at start or end of the text
    filtered_text <- gsub("^[[:punct:]]+\\s*", "", filtered_text)
    filtered_text <- gsub("\\s*[[:punct:]]+$", "", filtered_text)

    # Clean up extra whitespace
    filtered_text <- gsub("\\s+", " ", filtered_text)
    filtered_text <- trimws(filtered_text)

    # Save filtered file
    base_name <- tools::file_path_sans_ext(basename(filepath))
    out_name <- paste0(base_name, "-filtered.txt")

    if (rewrite) {
      out_path <- filepath  # Overwrite original
    } else {
      out_path <- file.path(dir_filtered, out_name)
    }

    writeLines(filtered_text, out_path, useBytes = TRUE)

    return(list(
      file = basename(filepath),
      status = "processed",
      word_count = word_count,
      filtered_word_count = length(filtered_tokens),
      pos_proportion = pos_proportion,  # Still included for info
      output_file = basename(out_path)
    ))

  }, error = function(e) {
    return(list(
      file = basename(filepath),
      status = "error",
      error = as.character(e$message)
    ))
  })
}

#' Clean Text
#' @keywords internal
clean_text <- function(txt) {
  txt <- tolower(txt)
  txt <- gsub("[0-9]+", "", txt)  # Remove numbers
  txt <- gsub("\\s+", " ", txt)   # Normalize whitespace
  txt <- trimws(txt)
  txt
}

#' Convert Penn Treebank to UPOS Tags
#' @keywords internal
penn_to_upos <- function(penn_tags) {

  mapping <- list(
    # Nouns
    "NN" = "NOUN", "NNS" = "NOUN",
    "NNP" = "PROPN", "NNPS" = "PROPN",
    # Verbs
    "VB" = "VERB", "VBD" = "VERB", "VBG" = "VERB",
    "VBN" = "VERB", "VBP" = "VERB", "VBZ" = "VERB",
    # Adjectives
    "JJ" = "ADJ", "JJR" = "ADJ", "JJS" = "ADJ",
    # Adverbs
    "RB" = "ADV", "RBR" = "ADV", "RBS" = "ADV"
  )

  upos <- unique(unlist(mapping[toupper(penn_tags)]))

  if (length(upos) == 0) {
    warning("No valid POS tags provided. Using NOUN and PROPN as default.")
    upos <- c("NOUN", "PROPN")
  }

  upos
}

# Create package environment for caching
.textfilter_env <- new.env(parent = emptyenv())

#' Load UDPipe Model with Caching
#' @keywords internal
load_udpipe_model <- function(language = "english") {

  # Try package-level cache first
  if (exists("model", envir = .textfilter_env)) {
    model <- get("model", envir = .textfilter_env)
    if (inherits(model, "udpipe_model")) {
      return(model)
    }
  }

  # Setup model directory
  model_dir <- file.path(Sys.getenv("HOME"), ".udpipe_models")
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

  # Look for existing model file
  model_files <- list.files(model_dir, pattern = "\\.udpipe$", full.names = TRUE)

  if (length(model_files) > 0) {
    message("Loading cached UDPipe model...")
    model <- udpipe::udpipe_load_model(model_files[1])
  } else {
    message("Downloading UDPipe model (one-time setup)...")
    message("This may take a minute...")

    model_info <- udpipe::udpipe_download_model(
      language = language,
      model_dir = model_dir
    )
    model <- udpipe::udpipe_load_model(model_info$file_model)
  }

  # Cache in package environment
  assign("model", model, envir = .textfilter_env)

  model
}

#' Print Processing Summary
#' @keywords internal
print_processing_summary <- function(results) {

  total <- length(results)
  processed <- sum(sapply(results, function(x) x$status == "processed"))
  skipped <- sum(sapply(results, function(x) x$status == "skipped"))
  errors <- sum(sapply(results, function(x) x$status == "error"))

  cat("\n", rep("=", 40), "\n", sep = "")
  cat("Processing Summary\n")
  cat(rep("=", 40), "\n", sep = "")
  cat("Total files:    ", total, "\n")
  cat("+ Processed:    ", processed, "\n")
  cat("- Skipped:      ", skipped, "\n")

  if (errors > 0) {
    cat("! Errors:       ", errors, "\n")
  }

  if (skipped > 0) {
    cat("\nSkipped files:\n")
    for (r in results) {
      if (r$status == "skipped") {
        cat(sprintf("  - %s: %s\n", r$file, r$reason))
      }
    }
  }

  if (errors > 0) {
    cat("\nErrors:\n")
    for (r in results) {
      if (r$status == "error") {
        cat(sprintf("  - %s: %s\n", r$file, substr(r$error, 1, 50)))
      }
    }
  }

  if (processed > 0) {
    cat("\n+ Filtered files saved to 'filtered' subdirectory\n")
  }

  cat(rep("=", 40), "\n", sep = "")
}
