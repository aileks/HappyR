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

# Load lexicons from local directory
get_local_sentiments <- function(lexicon_name) {
  if (lexicon_name == "afinn") {
    readRDS("lexicons/afinn/afinn.rds")
  } else if (lexicon_name == "nrc") {
    readRDS("lexicons/nrc/NRCWordEmotion.rds")
  } else {
    stop("Unsupported lexicon name")
  }
}

# Load lexicons in advance to avoid first-run delay
get_sentiments("bing")
# DEV VERSION
# get_sentiments("afinn")
# get_sentiments("nrc")
# PROD VERSION
get_local_sentiments("afinn")
get_local_sentiments("nrc")

# ---------------------------------------------------- #

ui <- dashboardPage(
  title = "HappyR - Text Sentiment Analysis",

  # Header with enhanced styling
  dashboardHeader(
    title = span(icon("chart-line", class = "mr-2"), "HappyR"),
    titleWidth = 300
  ),

  # Sidebar with improved menu items
  dashboardSidebar(
    width = 300,
    tags$div(
      class = "text-center", style = "padding: 10px; color: #ecf0f1;",
      "Analyze text sentiment and emotions"
    ),
    sidebarMenu(
      id = "sidebar",
      menuItem("Text Input", tabName = "input", icon = icon("keyboard")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    tags$div(
      class = "sidebar-footer", style = "position: absolute; bottom: 0; padding: 15px; width: 100%; text-align: center; color: #7f8c8d;",
      "© ", format(Sys.Date(), "%Y"), "HappyR"
    )
  ),

  # Main content area with enhanced styling
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500&display=swap"),
      tags$style(HTML("
        /* Additional inline styles */
        .content-wrapper {background-color: #f5f7fa;}
        .tab-content {padding-top: 20px;}
        .btn {border-radius: 4px; text-transform: uppercase; font-weight: 500; letter-spacing: 0.5px;}
        .nav-tabs-custom {border-radius: 8px; overflow: hidden; box-shadow: 0 2px 10px rgba(0,0,0,0.05);}
      "))
    ),
    tabItems(
      # Text Input Tab - Improved layout
      tabItem(
        tabName = "input",
        fluidRow(
          column(
            width = 5,
            box(
              title = span(icon("edit"), "Text Input Options"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,

              # Input method selection with better styling
              radioButtons("inputType", "Input Method:",
                choices = c(
                  "Text Input" = "text",
                  "Load File" = "file"
                ),
                selected = "text",
                inline = TRUE
              ),

              # Text area for direct input with enhanced styling
              conditionalPanel(
                condition = "input.inputType == 'text'",
                textAreaInput("textInput", "Enter Text:",
                  height = "230px",
                  placeholder = "Paste or type your text here for sentiment analysis..."
                )
              ),

              # File upload with better file type indicators
              conditionalPanel(
                condition = "input.inputType == 'file'",
                fileInput("fileInput", "Upload Text File:",
                  accept = c(".txt", ".csv", ".md")
                ),
                tags$div(
                  class = "file-types",
                  tags$span(class = "label label-info", ".txt"),
                  tags$span(class = "label label-info", ".csv"),
                  tags$span(class = "label label-info", ".md"),
                  style = "margin-top: -15px; margin-bottom: 15px;"
                ),
                helpText("Max file size: 5MB")
              ),

              # Analysis button with enhanced styling
              tags$div(
                style = "margin-top: 20px;",
                actionButton("analyzeBtn", "Analyze Sentiment",
                  icon = icon("search"),
                  class = "btn-primary",
                  width = "100%",
                  style = "padding: 10px;"
                )
              ),

              # Example text dropdown with better styling
              div(
                style = "margin-top: 20px;",
                selectInput("exampleText", "Or try an example:",
                  choices = c(
                    "None" = "",
                    "Positive Review" = "positive",
                    "Negative Review" = "negative",
                    "Neutral Text" = "neutral"
                  ),
                  width = "100%"
                )
              )
            )
          ),
          column(
            width = 7,
            box(
              title = span(icon("eye"), "Input Preview"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              div(
                style = "min-height: 400px;",
                verbatimTextOutput("textPreview")
              ),
              footer = div(
                class = "text-muted", style = "font-size: 0.9em;",
                "The preview shows how your text will be processed for analysis."
              )
            )
          )
        )
      ),

      # Analysis Tab - Enhanced visualization layout
      tabItem(
        tabName = "analysis",
        fluidRow(
          column(
            width = 12,
            h2("Sentiment Analysis Results", class = "text-primary", style = "margin-bottom: 20px;")
          )
        ),
        fluidRow(
          box(
            title = span(icon("gauge-high"), "Overall Sentiment"),
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            div(
              class = "text-center",
              plotlyOutput("sentimentGauge", height = "200px")
            ),
            valueBoxOutput("sentimentSummary", width = 12)
          ),
          box(
            title = span(icon("balance-scale"), "Sentiment Breakdown"),
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("sentimentBreakdown", height = "250px")
          )
        ),
        fluidRow(
          box(
            title = span(icon("face-smile"), "Emotion Analysis"),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("emotionPlot", height = "300px"),
            footer = div(
              class = "text-muted", style = "font-size: 0.9em;",
              "Based on the NRC Emotion Lexicon"
            )
          ),
          box(
            title = span(icon("list-ol"), "Top Words"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("topWordsPlot", height = "300px"),
            div(
              style = "padding: 10px 0;",
              checkboxInput("excludeStopwords", "Exclude Common Words (stop words)", value = TRUE)
            ),
            footer = div(
              class = "text-muted", style = "font-size: 0.9em;",
              "Most frequently occurring words in your text"
            )
          )
        )
      ),

      # Word Cloud Tab - Enhanced styling
      tabItem(
        tabName = "wordcloud",
        fluidRow(
          column(
            width = 12,
            h2("Word Cloud Visualization", class = "text-primary", style = "margin-bottom: 20px;")
          )
        ),
        fluidRow(
          box(
            title = span(icon("cloud"), "Interactive Word Cloud"),
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            div(
              style = "background-color: #ffffff; padding: 15px; border-radius: 8px; min-height: 450px;",
              wordcloud2Output("wordCloud", height = "450px")
            ),
            footer = div(
              class = "text-muted", style = "font-size: 0.9em;",
              "Words sized by frequency in the text"
            )
          ),
          box(
            title = span(icon("sliders"), "Word Cloud Settings"),
            status = "info",
            solidHeader = TRUE,
            width = 4,
            div(
              style = "padding: 15px 5px;",
              sliderInput("numWords", "Number of Words:",
                min = 10, max = 200, value = 100, step = 10
              )
            ),
            div(
              style = "padding: 15px 5px;",
              selectInput("cloudShape", "Cloud Shape:",
                choices = c(
                  "Circle" = "circle",
                  "Rectangle" = "rectangle",
                  "Star" = "star"
                )
              )
            ),
            div(
              class = "alert alert-info",
              icon("info-circle"),
              "Adjust the settings to customize your word cloud visualization"
            )
          )
        )
      ),

      # About Tab - Enhanced with better typography
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = span(icon("circle-info"), "About This App"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              class = "about-content",
              style = "padding: 10px;",
              uiOutput("aboutContent")
            ),
            footer = div(
              class = "text-center text-muted", style = "padding-top: 15px;",
              "Built with ", icon("heart"), " using R Shiny"
            )
          )
        )
      )
    )
  )
)


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
      return(NULL)
    }

    # Show progress notification with improved styling
    withProgress(message = "Analyzing text", value = 0, {
      # Add an animation effect
      for (i in 1:5) {
        incProgress(1 / 5, detail = paste("Step", i))
        Sys.sleep(0.1)
      }

      # Perform analysis
      analysis_results <- analyze_sentiment(text)
      results(analysis_results)
    })

    # Show success notification
    showNotification("Analysis complete! Viewing results.", type = "message", duration = 3)

    # Switch to analysis tab
    updateTabItems(session, "sidebar", "analysis")
  })

  # Overall sentiment gauge with improved styling
  output$sentimentGauge <- renderPlotly({
    req(results())

    score <- results()$overall

    # Custom color based on sentiment score
    gauge_color <- ifelse(
      score >= 0.5, "#43A047", # Very positive
      ifelse(score >= 0.1, "#8BC34A", # Positive
        ifelse(score > -0.1, "#FFEB3B", # Neutral
          ifelse(score > -0.5, "#FB8C00", # Negative
            "#E53935"
          ) # Very negative
        )
      )
    )

    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = score,
      title = list(text = "Sentiment Score", font = list(size = 16)),
      gauge = list(
        axis = list(range = list(-1, 1), tickwidth = 1, tickcolor = "darkgrey"),
        bar = list(color = gauge_color, thickness = 0.7),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "lightgrey",
        steps = list(
          list(range = c(-1, -0.6), color = "#FFCDD2"), # Light red
          list(range = c(-0.6, -0.2), color = "#FFE0B2"), # Light orange
          list(range = c(-0.2, 0.2), color = "#FFF9C4"), # Light yellow
          list(range = c(0.2, 0.6), color = "#DCEDC8"), # Light green
          list(range = c(0.6, 1), color = "#C8E6C9") # Lighter green
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 0.75,
          value = score
        )
      ),
      number = list(
        font = list(size = 20, color = gauge_color),
        suffix = "",
        valueformat = ".2f"
      )
    ) %>%
      layout(
        margin = list(l = 30, r = 30, t = 50, b = 20),
        height = 200
      )
  })

  # Sentiment summary box with improved styling
  output$sentimentSummary <- renderValueBox({
    req(results())

    score <- results()$overall

    if (score >= 0.5) {
      sentiment <- "Very Positive"
      color <- "green"
      icon_name <- "face-grin-stars"
    } else if (score >= 0.1) {
      sentiment <- "Positive"
      color <- "olive"
      icon_name <- "face-smile"
    } else if (score > -0.1) {
      sentiment <- "Neutral"
      color <- "yellow"
      icon_name <- "face-meh"
    } else if (score > -0.5) {
      sentiment <- "Negative"
      color <- "orange"
      icon_name <- "face-frown"
    } else {
      sentiment <- "Very Negative"
      color <- "red"
      icon_name <- "face-angry"
    }

    valueBox(
      sentiment,
      paste("Overall Sentiment Score:", round(score, 2)),
      icon = icon(icon_name),
      color = color
    )
  })

  # Sentiment breakdown bar chart with improved styling
  output$sentimentBreakdown <- renderPlotly({
    req(results())

    bing <- results()$bing

    df <- data.frame(
      Category = c("Positive", "Negative"),
      Count = c(bing$positive, bing$negative)
    )

    # Calculate percentages
    total <- sum(df$Count)
    df$Percentage <- ifelse(total > 0, round(df$Count / total * 100, 1), 0)
    df$Label <- paste0(df$Count, " (", df$Percentage, "%)")

    plot_ly(df,
      x = ~Category, y = ~Count, type = "bar",
      marker = list(
        color = c("#43A047", "#E53935"),
        line = list(color = c("#388E3C", "#D32F2F"), width = 1.5)
      ),
      text = ~Label,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~ paste(Category, "Words:", Count, "<br>Percentage:", Percentage, "%")
    ) %>%
      layout(
        title = list(
          text = "Positive vs Negative Words",
          font = list(size = 18, family = "Roboto")
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 14)
        ),
        yaxis = list(
          title = "Word Count",
          titlefont = list(size = 14),
          tickfont = list(size = 12),
          gridcolor = "#f5f5f5"
        ),
        bargap = 0.4,
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 50, t = 60, b = 40),
        hoverlabel = list(
          bgcolor = "#333",
          font = list(size = 14, color = "white")
        )
      )
  })

  # Emotion plot from NRC lexicon with improved styling
  output$emotionPlot <- renderPlotly({
    req(results())

    nrc <- results()$nrc

    # Filter to just emotions (exclude positive/negative)
    emotions <- nrc %>%
      filter(!sentiment %in% c("positive", "negative")) %>%
      arrange(desc(n))

    # Improved colors for emotions with better accessibility
    emotion_colors <- c(
      "anger" = "#E53935", # Red
      "anticipation" = "#FB8C00", # Orange
      "disgust" = "#8E24AA", # Purple
      "fear" = "#7B1FA2", # Deep Purple
      "joy" = "#FFD600", # Yellow
      "sadness" = "#42A5F5", # Blue
      "surprise" = "#26A69A", # Teal
      "trust" = "#66BB6A" # Green
    )

    # Extract colors in the right order
    colors <- emotion_colors[emotions$sentiment]

    plot_ly(emotions,
      x = ~sentiment, y = ~n, type = "bar",
      marker = list(
        color = colors,
        line = list(color = "rgba(0,0,0,0.3)", width = 1)
      ),
      text = ~n,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~ paste(sentiment, ":", n, "words")
    ) %>%
      layout(
        title = list(
          text = "Emotion Analysis",
          font = list(size = 18, family = "Roboto")
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 14),
          categoryorder = "total descending",
          tickangle = -30
        ),
        yaxis = list(
          title = "Word Count",
          titlefont = list(size = 14),
          tickfont = list(size = 12),
          gridcolor = "#f5f5f5"
        ),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 50, t = 60, b = 100),
        hoverlabel = list(
          bgcolor = "#333",
          font = list(size = 14, color = "white")
        )
      )
  })

  # Top words plot with improved styling
  output$topWordsPlot <- renderPlotly({
    req(results())

    tokens <- results()$tokens
    exclude <- input$excludeStopwords

    top_words <- get_top_words(tokens, n = 12, exclude_stopwords = exclude)

    plot_ly(top_words,
      x = ~n, y = ~ reorder(word, n), type = "bar",
      orientation = "h",
      marker = list(
        color = "#2C82E5",
        line = list(color = "#1A237E", width = 1),
        gradient = list(
          type = "horizontal",
          color = "#64B5F6"
        )
      ),
      text = ~n,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~ paste(word, ":", n, "occurrences")
    ) %>%
      layout(
        title = list(
          text = "Most Frequent Words",
          font = list(size = 18, family = "Roboto")
        ),
        xaxis = list(
          title = "Frequency",
          titlefont = list(size = 14),
          tickfont = list(size = 12),
          gridcolor = "#f5f5f5",
          zeroline = TRUE,
          zerolinecolor = "#e0e0e0"
        ),
        yaxis = list(
          title = "",
          titlefont = list(size = 14),
          tickfont = list(size = 12)
        ),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FFFFFF",
        margin = list(l = 110, r = 40, t = 60, b = 40),
        hoverlabel = list(
          bgcolor = "#333",
          font = list(size = 14, color = "white")
        )
      )
  })

  # Word cloud with improved styling
  output$wordCloud <- renderWordcloud2({
    req(results())

    tokens <- results()$tokens
    num_words <- input$numWords
    shape <- input$cloudShape

    # Get word frequencies
    word_freqs <- get_top_words(tokens, n = num_words, exclude_stopwords = TRUE)

    # Create word cloud with enhanced options
    if (nrow(word_freqs) > 0) {
      wordcloud2(
        word_freqs,
        size = 0.6,
        shape = shape,
        color = "random-dark",
        backgroundColor = "white",
        rotateRatio = 0.3,
        minRotation = -pi / 4,
        maxRotation = pi / 4,
        fontFamily = "Roboto"
      )
    }
  })

  # About content with enhanced styling
  output$aboutContent <- renderUI({
    HTML("
      <div style='font-size: 16px; line-height: 1.6;'>
        <p class='lead' style='font-size: 18px; color: #3498db;'>HappR uses Natural Language Processing techniques to analyze the sentiment and emotion of text.</p>

        <h4 style='margin-top: 25px; color: #3498db; font-weight: 500;'>Features:</h4>
        <ul style='list-style-type: none; padding-left: 5px;'>
          <li><i class='fa fa-check-circle' style='color: #27ae60; margin-right: 10px;'></i>Overall sentiment scoring</li>
          <li><i class='fa fa-check-circle' style='color: #27ae60; margin-right: 10px;'></i>Positive/negative word breakdown</li>
          <li><i class='fa fa-check-circle' style='color: #27ae60; margin-right: 10px;'></i>Emotion analysis</li>
          <li><i class='fa fa-check-circle' style='color: #27ae60; margin-right: 10px;'></i>Word frequency visualization</li>
          <li><i class='fa fa-check-circle' style='color: #27ae60; margin-right: 10px;'></i>Interactive word cloud</li>
        </ul>

        <h4 style='margin-top: 25px; color: #3498db; font-weight: 500;'>How It Works:</h4>
        <p>The app uses multiple sentiment lexicons:</p>
        <div class='row' style='display: flex; flex-wrap: wrap; margin: 0 -15px;'>
          <div class='col-md-6' style='padding: 10px;'>
            <div class='panel panel-default' style='border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>
              <div class='panel-heading' style='background-color: #3498db; color: white; border-radius: 8px 8px 0 0; padding: 10px;'>
                <strong>AFINN</strong>
              </div>
              <div class='panel-body' style='padding: 15px;'>
                Words scored from -5 (negative) to +5 (positive)
              </div>
            </div>
          </div>
          <div class='col-md-6' style='padding: 10px;'>
            <div class='panel panel-default' style='border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>
              <div class='panel-heading' style='background-color: #3498db; color: white; border-radius: 8px 8px 0 0; padding: 10px;'>
                <strong>Bing</strong>
              </div>
              <div class='panel-body' style='padding: 15px;'>
                Binary positive/negative classifications
              </div>
            </div>
          </div>
          <div class='col-md-6' style='padding: 10px;'>
            <div class='panel panel-default' style='border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>
              <div class='panel-heading' style='background-color: #3498db; color: white; border-radius: 8px 8px 0 0; padding: 10px;'>
                <strong>NRC</strong>
              </div>
              <div class='panel-body' style='padding: 15px;'>
                Emotions (anger, fear, joy, etc.) and sentiment
              </div>
            </div>
          </div>
          <div class='col-md-6' style='padding: 10px;'>
            <div class='panel panel-default' style='border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>
              <div class='panel-heading' style='background-color: #3498db; color: white; border-radius: 8px 8px 0 0; padding: 10px;'>
                <strong>Syuzhet</strong>
              </div>
              <div class='panel-body' style='padding: 15px;'>
                Alternative sentiment analysis method
              </div>
            </div>
          </div>
        </div>

        <h4 style='margin-top: 25px; color: #3498db; font-weight: 500;'>Lexicon Citations:</h4>
        <div class='well' style='background-color: #f9f9f9; border-radius: 8px; margin-top: 15px; padding: 15px;'>
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
        </div>

        <p style='margin-top: 20px;'><strong>R Packages:</strong><br>
        This app uses the tidytext, textdata, and syuzhet R packages for sentiment analysis.</p>
      </div>
    ")
  })
}

shinyApp(ui, server)
