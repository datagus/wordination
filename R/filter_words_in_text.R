#' Filter Words in Text Files by POS Tags
#'
#' Filters text files in a directory to keep only specified word types based on
#' part-of-speech tagging. Processes all .txt files that meet the minimum word
#' count requirement.
#'
#' @param dir_in Character string. Working directory containing text files.
#' @param pos_tag Character vector of POS tags to keep. Default is c("NN", "NNP") for nouns.
#' @param rewrite Logical. If FALSE (default), creates "filtered" subfolder;
#'   if TRUE, overwrites files in the original directory.
#' @param min_word_count Integer. Minimum number of words required in input files (default 2000).
#'
#' @return Invisibly returns a list of processing results for each file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Filter only nouns from text files (default settings)
#' filter_words_in_text("./documents")
#'
#' # Filter with custom word count threshold
#' filter_words_in_text("./documents",
#'                     pos_tag = c("NN", "NNP"),
#'                     min_word_count = 1000)
#' }
filter_words_in_text <- function(dir_in,
                                 pos_tag = c("NN", "NNP"),
                                 rewrite = FALSE,
                                 min_word_count = 2000) {

  # Input validation
  if (!dir.exists(dir_in)) {
    stop("Input directory does not exist: ", dir_in)
  }

  # Validate numeric parameters
  if (!is.numeric(min_word_count) || min_word_count <= 0) {
    stop("min_word_count must be a positive number")
  }

  # Convert Penn Treebank tags to UPOS
  upos_tags <- penn_to_upos(pos_tag)

  # Setup output directory
  if (!rewrite) {
    dir_filtered <- file.path(dir_in, "filtered")
    if (!dir.exists(dir_filtered)) {
      dir.create(dir_filtered, recursive = TRUE)
    }
  }

  # Get all .txt files (excluding subdirectories)
  files <- list.files(path = dir_in, pattern = "\\.txt$",
                      full.names = TRUE, recursive = FALSE)

  if (length(files) == 0) {
    message("No .txt files found in: ", dir_in)
    return(invisible(list()))
  }

  message("Found ", length(files), " text file(s) to process")
  message("Settings: min_word_count = ", min_word_count)

  # Load UDPipe model (with caching)
  model <- load_udpipe_model()

  # Process each file with progress bar if available
  if (requireNamespace("pbapply", quietly = TRUE)) {
    results <- pbapply::pblapply(files, process_single_file,
                                 model = model,
                                 upos_tags = upos_tags,
                                 rewrite = rewrite,
                                 dir_filtered = if(!rewrite) dir_filtered else NULL,
                                 min_word_count = min_word_count)
  } else {
    message("Install 'pbapply' package for progress bar")
    results <- lapply(files, process_single_file,
                      model = model,
                      upos_tags = upos_tags,
                      rewrite = rewrite,
                      dir_filtered = if(!rewrite) dir_filtered else NULL,
                      min_word_count = min_word_count)
  }

  # Print summary
  print_processing_summary(results)

  invisible(results)
}

#' Show Available POS Tags
#'
#' Display a table of common Penn Treebank POS tags with descriptions and examples.
#' Helps users understand which tags to use for filtering.
#'
#' @return A data frame of POS tags (invisibly)
#' @export
#' @examples
#' show_pos_tags()
show_pos_tags <- function() {
  tags <- data.frame(
    Tag = c("NN", "NNS", "NNP", "NNPS", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ",
            "JJ", "JJR", "JJS", "RB", "RBR", "RBS"),
    Type = c(rep("Noun", 4), rep("Verb", 6), rep("Adjective", 3), rep("Adverb", 3)),
    Description = c(
      "Noun, singular", "Noun, plural",
      "Proper noun, singular", "Proper noun, plural",
      "Verb, base form", "Verb, past tense", "Verb, gerund/present participle",
      "Verb, past participle", "Verb, non-3rd person singular present",
      "Verb, 3rd person singular present",
      "Adjective", "Adjective, comparative", "Adjective, superlative",
      "Adverb", "Adverb, comparative", "Adverb, superlative"
    ),
    Example = c(
      "cat, idea", "cats, ideas",
      "London, Mary", "Americas, Johns",
      "run, eat", "ran, ate", "running, eating",
      "run, eaten", "run, eat", "runs, eats",
      "big, blue", "bigger", "biggest",
      "quickly, very", "faster", "fastest"
    ),
    stringsAsFactors = FALSE
  )

  print(tags, row.names = FALSE)
  invisible(tags)
}
