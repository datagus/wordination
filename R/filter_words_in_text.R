#' Filter Words in Text Files by POS Tags
#'
#' Filters text files in a directory to keep only specified word types based on
#' part-of-speech tagging using UDPipe. Useful for preparing text corpora for
#' analysis by extracting only nouns, verbs, or other word classes.
#'
#' @param dir_in Character string. Directory containing text files to process.
#' @param pos_tag Character vector of Penn Treebank POS tags to keep.
#'   Default is \code{c("NN", "NNP")} for singular nouns and proper nouns.
#'   Use \code{show_pos_tags()} to see all available tags.
#' @param rewrite Logical. If \code{FALSE} (default), creates a "filtered"
#'   subfolder for output files. If \code{TRUE}, overwrites original files.
#' @param min_word_count Integer. Minimum word count required to process a file.
#'   Files with fewer words are skipped. Default is 2000.
#' @param use_lemma Logical. If \code{TRUE} (default), outputs lemmatized forms
#'   (e.g., "running" becomes "run"). If \code{FALSE}, keeps original word forms.
#'
#' @return Invisibly returns a list of results, one per file, each containing:
#' \describe{
#'   \item{file}{Filename processed}
#'   \item{status}{"processed", "skipped", or "error"}
#'   \item{word_count}{Total words in input file}
#'   \item{filtered_word_count}{Words remaining after filtering}
#'   \item{pos_proportion}{Proportion of content words retained}
#'   \item{output_file}{Name of output file (if processed)}
#' }
#'
#' @details
#' This function uses UDPipe for natural language processing. On first run,
#' it downloads the English language model (~16MB) which is cached for future use.
#'
#' The filtering workflow:
#' \enumerate{
#'   \item Read each .txt file from the input directory
#'   \item Skip files below the minimum word count
#'   \item Annotate text with UDPipe (tokenization, POS tagging, lemmatization)
#'   \item Keep only words matching the specified POS tags
#'   \item Remove punctuation, numbers, and single-character tokens
#'   \item Save filtered words to output file
#' }
#'
#' @seealso \code{\link{show_pos_tags}} for available POS tags
#'
#' @importFrom udpipe udpipe_annotate udpipe_download_model udpipe_load_model
#' @importFrom tools file_path_sans_ext
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage: extract nouns from all text files
#' results <- filter_words_in_text("path/to/documents")
#'
#' # Extract only verbs with lower word threshold
#' results <- filter_words_in_text(
#'   dir_in = "path/to/documents",
#'   pos_tag = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
#'   min_word_count = 1000
#' )
#'
#' # Keep original word forms (no lemmatization)
#' results <- filter_words_in_text(
#'   dir_in = "path/to/documents",
#'   use_lemma = FALSE
#' )
#'
#' # Extract nouns and adjectives, overwrite original files
#' results <- filter_words_in_text(
#'   dir_in = "path/to/documents",
#'   pos_tag = c("NN", "NNS", "JJ"),
#'   rewrite = TRUE
#' )
#'
#' # Check available POS tags
#' show_pos_tags()
#' }
filter_words_in_text <- function(dir_in,
                                 pos_tag = c("NN", "NNP"),
                                 rewrite = FALSE,
                                 min_word_count = 2000,
                                 use_lemma = TRUE) {

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
  model_path <- get_udpipe_model_path()

  # Process each file with progress bar if available
  if (requireNamespace("pbapply", quietly = TRUE)) {
    results <- pbapply::pblapply(files, process_single_file,
                                 model_path = model_path,
                                 upos_tags = upos_tags,
                                 rewrite = rewrite,
                                 dir_filtered = if(!rewrite) dir_filtered else NULL,
                                 min_word_count = min_word_count,
                                 use_lemma = use_lemma)
  } else {
    message("Install 'pbapply' package for progress bar")
    results <- lapply(files, process_single_file,
                      model_path = model_path,
                      upos_tags = upos_tags,
                      rewrite = rewrite,
                      dir_filtered = if(!rewrite) dir_filtered else NULL,
                      min_word_count = min_word_count,
                      use_lemma = use_lemma)
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
