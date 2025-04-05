clean_text <- function(text) {
  text <- tolower(text)
  text <- str_replace_all(text, "http\\S+", "")
  text <- str_replace_all(text, "[[:punct:]]", " ")
  text <- str_replace_all(text, "[[:digit:]]", "")
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)

  text
}


get_sentiment_by_words <- function(tokens, lexicon = "bing") {
  sentiment_words <- tokens %>%
    inner_join(get_sentiments(lexicon), by = "word")

  sentiment_words
}

tokenize_text <- function(text) {
  tibble(text = text) %>%
    unnest_tokens(word, text)
}

get_top_words <- function(tokens, n = 50, exclude_stopwords = TRUE) {
  if (exclude_stopwords) {
    tokens <- tokens %>%
      anti_join(stop_words, by = "word")
  }

  top_words <- tokens %>%
    count(word, sort = TRUE) %>%
    head(n)

  top_words
}
