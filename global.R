library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(textdata)
library(syuzhet)
library(plotly)
library(wordcloud2)
library(tm)
library(stringr)
library(DT)

MAX_FILE_SIZE <- 5 * 1024^2 # 5MB max file size

# Prep text for analysis
clean_text <- function(text) {
  # Convert to lowercase
  text <- tolower(text)

  # Remove URLs
  text <- str_replace_all(text, "http\\S+", "")

  # Remove punctuation
  text <- str_replace_all(text, "[[:punct:]]", " ")

  # Remove numbers
  text <- str_replace_all(text, "[[:digit:]]", "")

  # Remove extra whitespace
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)

  text
}

tokenize_text <- function(text) {
  tibble(text = text) %>%
    unnest_tokens(word, text)
}

# Analyze sentiment using different lexicons
analyze_sentiment <- function(text) {
  clean <- clean_text(text)
  tokens <- tokenize_text(clean)

  # Get sentiment scores using different lexicons

  # AFINN lexicon (numeric scores)
  afinn_scores <- tokens %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(
      afinn_score = sum(value),
      afinn_mean = mean(value),
      words_matched = n()
    )

  # Bing lexicon (positive/negative)
  bing_counts <- tokens %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(sentiment)

  # Make sure both positive and negative exist
  if (!"positive" %in% bing_counts$sentiment) {
    bing_counts <- rbind(bing_counts, data.frame(sentiment = "positive", n = 0))
  }
  if (!"negative" %in% bing_counts$sentiment) {
    bing_counts <- rbind(bing_counts, data.frame(sentiment = "negative", n = 0))
  }

  bing_scores <- bing_counts %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(
      bing_ratio = ifelse(positive + negative > 0, positive / (positive + negative), 0.5),
      total_bing = positive + negative
    )

  # NRC lexicon (emotions)
  nrc_scores <- tokens %>%
    inner_join(get_sentiments("nrc"), by = "word") %>%
    count(sentiment) %>%
    mutate(proportion = n / sum(n))

  # Syuzhet method (alternative approach)
  syuzhet_scores <- get_sentiment(clean, method = "syuzhet")
  syuzhet_score <- mean(syuzhet_scores)

  # Calculate an overall sentiment score (normalized between -1 and 1)
  overall_sentiment <- mean(c(
    afinn_scores$afinn_mean / 5, # Normalize AFINN to -1 to 1
    (bing_scores$bing_ratio - 0.5) * 2, # Normalize Bing to -1 to 1
    syuzhet_score / 5 # Normalize Syuzhet to -1 to 1
  ), na.rm = TRUE)

  results <- list(
    afinn = afinn_scores,
    bing = bing_scores,
    nrc = nrc_scores,
    syuzhet = syuzhet_score,
    overall = overall_sentiment,
    tokens = tokens
  )

  results
}

# Get top words by frequency
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

# Get sentiment by words
get_sentiment_by_words <- function(tokens, lexicon = "bing") {
  sentiment_words <- tokens %>%
    inner_join(get_sentiments(lexicon), by = "word")

  sentiment_words
}

# Load lexicons in advance to avoid first-run delay
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
