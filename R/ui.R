ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "Text Sentiment Analyzer",
    titleWidth = 300
  ),

  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Text Input", tabName = "input", icon = icon("keyboard")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),

  # Main content area
  dashboardBody(
    # Include custom CSS
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")),
    tabItems(
      # Text Input Tab
      tabItem(
        tabName = "input",
        fluidRow(
          column(
            width = 4,
            box(
              title = "Text Input Options",
              status = "primary",
              solidHeader = TRUE,
              width = 12,

              # Input method selection
              radioButtons("inputType", "Input Method:",
                choices = c(
                  "Text Input" = "text",
                  "Load File" = "file"
                ),
                selected = "text"
              ),

              # Text area for direct input
              conditionalPanel(
                condition = "input.inputType == 'text'",
                textAreaInput("textInput", "Enter Text:",
                  height = "200px",
                  placeholder = "Paste or type your text here..."
                )
              ),

              # File upload
              conditionalPanel(
                condition = "input.inputType == 'file'",
                fileInput("fileInput", "Upload Text File:",
                  accept = c(".txt", ".csv", ".md")
                ),
                helpText("Max file size: 5MB")
              ),

              # Analysis buttons
              actionButton("analyzeBtn", "Analyze Sentiment",
                icon = icon("play"),
                class = "btn-primary",
                width = "100%"
              ),

              # Example text dropdown
              selectInput("exampleText", "Or try an example:",
                choices = c(
                  "None" = "",
                  "Positive Text" = "positive",
                  "Negative Text" = "negative",
                  "Neutral Text" = "neutral"
                )
              )
            )
          ),
          column(
            width = 8,
            box(
              title = "Input Preview",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              verbatimTextOutput("textPreview")
            )
          )
        )
      ),

      # Analysis Tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Overall Sentiment",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("sentimentGauge", height = "200px"),
            valueBoxOutput("sentimentSummary", width = 12)
          ),
          box(
            title = "Sentiment Breakdown",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("sentimentBreakdown", height = "200px")
          )
        ),
        fluidRow(
          box(
            title = "Emotion Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("emotionPlot", height = "250px")
          ),
          box(
            title = "Top Words",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("topWordsPlot", height = "250px"),
            checkboxInput("excludeStopwords", "Exclude Stop Words", value = TRUE)
          )
        )
      ),

      # Word Cloud Tab
      tabItem(
        tabName = "wordcloud",
        fluidRow(
          box(
            title = "Word Cloud",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            wordcloud2Output("wordCloud", height = "400px"),
            helpText("Words sized by frequency")
          ),
          box(
            title = "Word Cloud Settings",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            sliderInput("numWords", "Number of Words:",
              min = 10, max = 200, value = 100
            ),
            selectInput("cloudShape", "Cloud Shape:",
              choices = c(
                "Circle" = "circle",
                "Rectangle" = "rectangle",
                "Star" = "star"
              )
            )
          )
        )
      ),

      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This App",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("aboutContent")
          )
        )
      )
    )
  )
)
