library(shiny)
library(ggplot2)
library(dplyr)

bcl<-read.csv("bcl-results.csv",stringsAsFactors = FALSE)

#Define UI for application
ui<-fluidPage(
  titlePanel("BC Liquor Store Prices"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput","Price",
                  min=0,max=100,
                  value = c(25,40),
                  pre = "$"),
                    radioButtons("typeInput","product type",
                                 choices = c("BEER","REFRESHMENT","SPIRITS","WINE"),
                                 selected = "WINE"),
                   uiOutput("countryOutput"),

                    ),
    mainPanel(plotOutput("coolplot"),
                 br(),br(),
                 tableOutput("cooltable"))
  )
)

#Define server logic
server <- function(input,output){
  
  output$countryOutput<-renderUI({
    selectInput("countryInput","Country",sort(unique(bcl$Country)),
                                              selected = "CANADA")
   })
  
  bcl_filter<-reactive({
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
  })
  output$coolplot<-renderPlot({

      ggplot(bcl_filter(),aes(Alcohol_Content))+
      geom_histogram()
  })
  output$cooltable<-renderTable({
    bcl_filter() %>%
      arrange(Price)
     
  })
}

#Run the application
shinyApp(ui = ui,server = server)