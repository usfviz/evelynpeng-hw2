library(shiny)
library(ggplot2)

rm(list=ls())
cat("\014")
setwd('/Users/evelyn/Documents/2017Spring/MSAN622/assignment2')

life <- read.csv('life.csv', header=T)
life <- life[-c(109), ]
fertility <- read.csv('fertility.csv', header=T)
fertility <- fertility[-c(109), ]
population <- read.csv('population.csv', header=T)
population <- population[-c(109), ]
metadata <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', header = T)

## Only run examples in interactive R sessions
if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    uiOutput("hover_info"),
    selectInput("variable", "Select Region:",
                c('All Region' = 'All',
                  "Latin America & Caribbean" = "Latin America & Caribbean",
                  "South Asia" = "South Asia",
                  "Sub-Saharan Africa" = "Sub-Saharan Africa",
                  "Europe & Central Asia" = "Europe & Central Asia",
                  "Middle East & North Africa" = "Middle East & North Africa",
                  "East Asia & Pacific" = "East Asia & Pacific",
                  'North America' = "North America"), selected = NULL), 
    plotOutput("distPlot", click = "plot_click", hover = "plot_hover", height = "400px"),
    
    sliderInput("year", "Year of dataset:",
                min = 1960, max = 2014, value = 1, animate = TRUE,
                width = '90%'
    )

  )
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      
      colname <- paste('X',input$year, sep="")
      #colname <- paste('X',1960, sep="")
      assign("df", data.frame(region=metadata$Region, life = life[[colname]], fertility = fertility[[colname]], population = population[[colname]]*100), envir = .GlobalEnv) 
      #data <- data.frame(life = life[[colname]], fertility = fertility[[colname]], population = population[[colname]])
      row.names(df) <- life$Country.Name
      remove <- c("World","IDA & IBRD total","Low & middle income","Middle income","IBRD only","Upper middle income","Late-demographic dividend","East Asia & Pacific","Early-demographic dividend","Lower middle income","East Asia & Pacific (excluding high income)","East Asia & Pacific (IDA & IBRD countries)","OECD members","High income","Post-demographic dividend","Europe & Central Asia","South Asia","South Asia (IDA & IBRD)","IDA total","European Union","Europe & Central Asia (IDA & IBRD countries)","Europe & Central Asia (excluding high income)","IDA only","Euro area","Least developed countries: UN classification","Sub-Saharan Africa","Sub-Saharan Africa (IDA & IBRD countries)","Sub-Saharan Africa (excluding high income)","Latin America & Caribbean","Latin America & the Caribbean (IDA & IBRD countries)","Latin America & Caribbean (excluding high income)","North America","Pre-demographic dividend","IDA blend","Heavily indebted poor countries (HIPC)","Low income","Russian Federation","Fragile and conflict affected situations","Middle East & North Africa","Middle East & North Africa (excluding high income)","Middle East & North Africa (IDA & IBRD countries)")
      df <- df[!rownames(df) %in% remove, ]
      df <- subset(df, region != '')
      df <- na.omit(df)
      
      if (input$variable == "All"){
        ggplot(df, aes(x=life, y=fertility, fill = factor(region), size = population)) + geom_point(pch=21) + scale_size(range = c(2, 40)) + guides(size=FALSE)+ theme(text = element_text(size=20)) + xlab("Life expectancy") + ylab("Fertility Rate") + ggtitle("Life expectancy vs Fertility Rate 1960-2014") + labs(fill='Region') 
      }
      else{
        d_bg <- df[df$region == input$variable, ]
        ggplot(df, aes(x=life, y=fertility, color = factor(region), size = population, alpha = 0.4)) + geom_point() + geom_jitter(data = d_bg, aes(alpha=0.6)) + scale_size(range = c(2, 40)) + guides(size=FALSE, alpha = FALSE) + theme(text = element_text(size=20)) + xlab("Life expectancy") + ylab("Fertility Rate") + ggtitle("Life expectancy vs Fertility Rate 1960-2014") + labs(color='Region')  
      }
})
    output$hover_info <- renderUI({
      row.names(df) <- life$Country.Name
      hover <- input$plot_hover
      point <- nearPoints(df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Country: </b>", rownames(point), "<br/>", "Population: </b>", point[,c('population')], "<br/>", "Fertility Rate: </b>", point[,c('fertility')], "<br/>")))
      )
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}

