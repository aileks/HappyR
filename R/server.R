server <- function(input, output, session) {
  results <- reactiveVal(NULL)

  getText <- reactive({
    if (input$inputType == "text") {
      text <- input$textInput
    } else if (input$inputType == "file" && !is.null(input$fileInput)) {
      ext <- tools::file_ext(input$fileInput$name)

      if (ext == "csv") {
        df <- read.csv(input$fileInput$datapath, stringsAsFactors = FALSE)
        text <- paste(df[, 1], collapse = " ")
      } else {
        text <- readLines(input$fileInput$datapath, warn = FALSE)
        text <- paste(text, collapse = " ")
      }
    } else {
      text <- ""
    }

    # Handle example text selection
    if (input$exampleText == "positive") {
      file_path <- file.path("data", "sample-pos.txt")
      text <- readLines(file_path, warn = FALSE)
      text <- paste(text, collapse = "\n")
      text <- gsub("\n\n+", " PARAGRAPH_BREAK ", text)
      text <- gsub("\n", " ", text)
      text <- gsub("PARAGRAPH_BREAK", "\n\n", text)
    } else if (input$exampleText == "negative") {
      file_path <- file.path("data", "sample-neg.txt")
      text <- readLines(file_path, warn = FALSE)
      text <- paste(text, collapse = "\n")
      text <- gsub("\n\n+", " PARAGRAPH_BREAK ", text)
      text <- gsub("\n", " ", text)
      text <- gsub("PARAGRAPH_BREAK", "\n\n", text)
    } else if (input$exampleText == "neutral") {
      file_path <- file.path("data", "sample-neut.txt")
      text <- readLines(file_path, warn = FALSE)
      text <- paste(text, collapse = "\n")
      text <- gsub("\n\n+", " PARAGRAPH_BREAK ", text)
      text <- gsub("\n", " ", text)
      text <- gsub("PARAGRAPH_BREAK", "\n\n", text)
    }

    text
  })

  # Preview the input text
  output$textPreview <- renderText({
    text <- getText()
    if (nchar(text) > 1000) {
      paste0(substr(text, 1, 1000), "... (text truncated for preview)")
    } else {
      text
    }
  })

  # Analyze button action
  observeEvent(input$analyzeBtn, {
    text <- getText()

    if (nchar(text) < 10) {
      showNotification("Please enter more text to analyze", type = "error")
      NULL
    }

    # Show progress notification
    id <- showNotification("Analyzing text...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id))

    # Perform analysis
    analysis_results <- analyze_sentiment(text)
    results(analysis_results)

    # Switch to analysis tab
    updateTabItems(session, "sidebar", "analysis")
  })

  # Overall sentiment gauge
  output$sentimentGauge <- renderPlotly({
    req(results())

    score <- results()$overall

    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = score,
      title = list(text = "Sentiment Score"),
      gauge = list(
        axis = list(range = list(-1, 1)),
        bar = list(color = ifelse(score >= 0, "#2C82E5", "#E53935")),
        steps = list(
          list(range = c(-1, -0.6), color = "#E53935"),
          list(range = c(-0.6, -0.2), color = "#FB8C00"),
          list(range = c(-0.2, 0.2), color = "#FFEB3B"),
          list(range = c(0.2, 0.6), color = "#8BC34A"),
          list(range = c(0.6, 1), color = "#43A047")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 0.75,
          value = score
        )
      )
    )
  })

  # Sentiment summary box
  output$sentimentSummary <- renderValueBox({
    req(results())

    score <- results()$overall

    if (score >= 0.5) {
      sentiment <- "Very Positive"
      color <- "green"
    } else if (score >= 0.1) {
      sentiment <- "Positive"
      color <- "olive"
    } else if (score > -0.1) {
      sentiment <- "Neutral"
      color <- "yellow"
    } else if (score > -0.5) {
      sentiment <- "Negative"
      color <- "orange"
    } else {
      sentiment <- "Very Negative"
      color <- "red"
    }

    valueBox(
      sentiment,
      paste("Overall Sentiment: ", round(score, 2)),
      icon = icon("face-smile"),
      color = color
    )
  })

  # Sentiment breakdown bar chart
  output$sentimentBreakdown <- renderPlotly({
    req(results())

    bing <- results()$bing

    df <- data.frame(
      Category = c("Positive", "Negative"),
      Count = c(bing$positive, bing$negative)
    )

    plot_ly(df,
      x = ~Category, y = ~Count, type = "bar",
      marker = list(color = c("#43A047", "#E53935"))
    ) %>%
      layout(
        title = "Positive vs Negative Words",
        xaxis = list(title = ""),
        yaxis = list(title = "Word Count")
      )
  })

  # Emotion plot from NRC lexicon
  output$emotionPlot <- renderPlotly({
    req(results())

    nrc <- results()$nrc

    # Filter to just emotions (exclude positive/negative)
    emotions <- nrc %>%
      filter(!sentiment %in% c("positive", "negative")) %>%
      arrange(desc(n))

    # Colors for emotions
    emotion_colors <- c(
      "anger" = "#E53935",
      "anticipation" = "#FB8C00",
      "disgust" = "#8DB85F",
      "fear" = "#7B1FA2",
      "joy" = "#FFD600",
      "sadness" = "#42A5F5",
      "surprise" = "#26A69A",
      "trust" = "#C7C7C7"
    )

    colors <- emotion_colors[emotions$sentiment]

    plot_ly(emotions,
      x = ~sentiment, y = ~n, type = "bar",
      marker = list(color = colors)
    ) %>%
      layout(
        title = "Emotion Analysis",
        xaxis = list(title = "", categoryorder = "total descending"),
        yaxis = list(title = "Word Count")
      )
  })

  # Top words plot
  output$topWordsPlot <- renderPlotly({
    req(results())

    tokens <- results()$tokens
    exclude <- input$excludeStopwords

    top_words <- get_top_words(tokens, n = 10, exclude_stopwords = exclude)

    plot_ly(top_words,
      x = ~n, y = ~ reorder(word, n), type = "bar",
      orientation = "h",
      marker = list(color = "#2C82E5")
    ) %>%
      layout(
        title = "Most Frequent Words",
        xaxis = list(title = "Frequency"),
        yaxis = list(title = "")
      )
  })

  # Word cloud
  output$wordCloud <- renderWordcloud2({
    req(results())

    tokens <- results()$tokens
    num_words <- input$numWords
    shape <- input$cloudShape

    # Get word frequencies
    word_freqs <- get_top_words(tokens, n = num_words, exclude_stopwords = TRUE)

    # Create word cloud
    if (nrow(word_freqs) > 0) {
      wordcloud2(word_freqs, size = 0.6, shape = shape)
    }
  })

  # About content
  output$aboutContent <- renderUI({
    HTML("
      <div style='font-size: 16px;'>
        <p>This Text Sentiment Analyzer app uses Natural Language Processing techniques to analyze the sentiment and emotion of text.</p>

        <h4>Features:</h4>
        <ul>
          <li>Overall sentiment scoring</li>
          <li>Positive/negative word breakdown</li>
          <li>Emotion analysis</li>
          <li>Word frequency visualization</li>
          <li>Interactive word cloud</li>
        </ul>

        <h4>How It Works:</h4>
        <p>The app uses multiple sentiment lexicons:</p>
        <ul>
          <li><strong>AFINN</strong>: Words scored from -5 (negative) to +5 (positive)</li>
          <li><strong>Bing</strong>: Binary positive/negative classifications</li>
          <li><strong>NRC</strong>: Emotions (anger, fear, joy, etc.) and sentiment</li>
          <li><strong>Syuzhet</strong>: Alternative sentiment analysis method</li>
        </ul>

        <h4>Lexicon Citations:</h4>
        <p><strong>AFINN Lexicon:</strong><br>
        Finn Årup Nielsen. (2011). A new ANEW: Evaluation of a word list for sentiment analysis in microblogs.
        <i>Proceedings of the ESWC2011 Workshop on 'Making Sense of Microposts': Big things come in small packages.</i>
        Volume 718 in CEUR Workshop Proceedings: 93-98.</p>

        <p><strong>Bing Lexicon:</strong><br>
        Minqing Hu and Bing Liu. (2004). Mining and summarizing customer reviews.
        <i>Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining</i> (KDD-2004).</p>

        <p><strong>NRC Emotion Lexicon:</strong><br>
        Saif M. Mohammad and Peter D. Turney. (2013). Crowdsourcing a Word-Emotion Association Lexicon.
        <i>Computational Intelligence</i>, 29(3): 436-465.</p>

        <p><strong>Syuzhet:</strong><br>
        Matthew Jockers. (2017). Syuzhet: Extract Sentiment and Plot Arcs from Text.
        <a href='https://github.com/mjockers/syuzhet' target='_blank'>https://github.com/mjockers/syuzhet</a></p>

        <p><strong>R Packages:</strong><br>
        This app uses the tidytext, textdata, and syuzhet R packages for sentiment analysis.</p>

        <hr>
        <p>Created with R Shiny.</p>
      </div>
    ")
  })
}
