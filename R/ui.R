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
