# Process a Single Text File
# Helper functions for filter_words_in_text()

#' Get UDPipe model path (downloads if needed)
#' @keywords internal
get_udpipe_model_path <- function() {
  model_dir <- file.path(tempdir(), "udpipe_models")
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  model_files <- list.files(model_dir, pattern = "english.*\\.udpipe$", full.names = TRUE)

  # Only use existing files that actually exist and have size > 0
  valid_models <- model_files[file.exists(model_files) & file.size(model_files) > 0]

  if (length(valid_models) == 0) {
    dl <- tryCatch(
      udpipe::udpipe_download_model("english", model_dir = model_dir),
      error = function(e) NULL
    )
    if (is.null(dl) || is.na(dl$file_model) || !file.exists(dl$file_model)) {
      stop("Could not download the UDPipe English model. ",
           "This function requires an internet connection on first use.",
           call. = FALSE)
    }
    return(dl$file_model)
  }

  valid_models[1]
}

#' Load UDPipe model with retry on failure
#' @keywords internal
load_udpipe_model <- function(model_path) {
  # Helper: load and validate model actually works
  load_and_validate <- function(path) {
    model <- udpipe::udpipe_load_model(path)
    # Test that model works (catches "external pointer is not valid")
    udpipe::udpipe_annotate(model, x = "test")
    model
  }

  model <- tryCatch(load_and_validate(model_path), error = function(e) NULL)

  # If loading/validation failed, re-download and try again
  if (is.null(model)) {
    model_dir <- dirname(model_path)
    dl <- udpipe::udpipe_download_model("english", model_dir = model_dir, overwrite = TRUE)
    model <- load_and_validate(dl$file_model)
  }

  model
}

#' Basic text cleaning
#' @keywords internal
clean_text <- function(text) {
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

#' Print processing summary
#' @keywords internal
print_processing_summary <- function(results) {
  status <- sapply(results, `[[`, "status")
  message(sprintf("Done: %d processed, %d skipped, %d errors",
                  sum(status == "processed"),
                  sum(status == "skipped"),
                  sum(status == "error")))
}

#' Process a Single Text File
#' @keywords internal
process_single_file <- function(filepath, model_path, pos_tag, rewrite, dir_filtered,
                                min_word_count = 2000, use_lemma = TRUE) {
  tryCatch({
    text <- paste(readLines(filepath, encoding = "UTF-8", warn = FALSE), collapse = " ")
    text <- clean_text(text)
    word_count <- length(strsplit(text, "\\s+")[[1]])

    if (word_count < min_word_count) {
      return(list(
        file = basename(filepath),
        status = "skipped",
        reason = sprintf("Less than %d words", min_word_count),
        word_count = word_count,
        pos_proportion = NA
      ))
    }

    # Load model with retry logic for external pointer issues
    model <- load_udpipe_model(model_path)

    # Annotate and filter by POS
    tokens_df <- as.data.frame(udpipe::udpipe_annotate(model, x = text))
    pos_mask <- tokens_df$xpos %in% pos_tag
    clean_mask <- !grepl("^[[:punct:]0-9]+$", tokens_df$token)
    subset_df <- tokens_df[pos_mask & clean_mask, ]

    # Use lemmas or original tokens
    if (use_lemma) {
      words <- ifelse(is.na(subset_df$lemma), subset_df$token, subset_df$lemma)
    } else {
      words <- subset_df$token
    }
    words <- words[nchar(words) > 1]

    # Calculate proportion
    content_count <- sum(tokens_df$upos %in% c("NOUN", "PROPN", "VERB", "ADJ", "ADV"))
    pos_proportion <- if (content_count > 0) length(words) / content_count else 0

    # Save output
    out_path <- if (rewrite) filepath else file.path(dir_filtered, paste0(tools::file_path_sans_ext(basename(filepath)), "-filtered.txt"))
    writeLines(paste(words, collapse = " "), out_path, useBytes = TRUE)

    list(
      file = basename(filepath),
      status = "processed",
      word_count = word_count,
      filtered_word_count = length(words),
      pos_proportion = pos_proportion,
      output_file = basename(out_path)
    )
  }, error = function(e) {
    list(file = basename(filepath), status = "error", error = e$message)
  })
}
