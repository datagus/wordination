# Process a Single Text File
# Helper functions for filter_words_in_text()

#' Convert Penn Treebank tags to Universal POS tags
#' @keywords internal
penn_to_upos <- function(penn_tags) {
  mapping <- c(
    "NN" = "NOUN", "NNS" = "NOUN", "NNP" = "PROPN", "NNPS" = "PROPN",
    "VB" = "VERB", "VBD" = "VERB", "VBG" = "VERB", "VBN" = "VERB",
    "VBP" = "VERB", "VBZ" = "VERB",
    "JJ" = "ADJ", "JJR" = "ADJ", "JJS" = "ADJ",
    "RB" = "ADV", "RBR" = "ADV", "RBS" = "ADV"
  )
  unique(unname(mapping[penn_tags]))
}

#' Get UDPipe model path (downloads if needed)
#' @keywords internal
get_udpipe_model_path <- function() {
  model_dir <- file.path(tempdir(), "udpipe_models")
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  model_files <- list.files(model_dir, pattern = "english.*\\.udpipe$", full.names = TRUE)

  if (length(model_files) == 0) {
    dl <- udpipe::udpipe_download_model("english", model_dir = model_dir)
    return(dl$file_model)
  }
  model_files[1]
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
process_single_file <- function(filepath, model_path, upos_tags, rewrite, dir_filtered,
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

    # Load model fresh for each file
    model <- udpipe::udpipe_load_model(model_path)

    # Annotate and filter by POS
    tokens_df <- as.data.frame(udpipe::udpipe_annotate(model, x = text))
    pos_mask <- tokens_df$upos %in% upos_tags
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
