source("global.R")
source("R/ui.R")
source("R/server.R")

addResourcePath("www", "www")

options(shiny.fullstacktrace = TRUE)
options(shiny.reactlog = TRUE)
shinyApp(ui, server)
