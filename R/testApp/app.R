#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("../multiWidgetModule.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           uiOutput("faithfulPlots")
        )
    )
)

plotFunc <- function(input,output,session,bins){
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
}


plotFunc2 <- function(input,output,session,binCount,name){
    # generate bins based on input$bins from ui.R
    ns <- session$ns
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = binCount + 1)

    # draw the histogram with the specified number of bins

    output[[name]] <- renderPlot({
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    tagList(p(name),plotOutput(ns(name)))
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    binCounts <- reactive({
        lapply(list(1,5,10,15),function(i){
            list(binCount=input$bins+i,name=paste(input$bins+i,'bins'))
        })
        #list(input$bins+1,input$bins+5,input$bins+10,input$bins+15)
    })

    output$faithfulPlots <- renderUI({
        faithfulPlots <- callModule(multiWidgetModule,"faithfulPlots",plotFunc2,binCounts(),
                                    list(ui = uiOutput, server = renderUI),nColumns=2)

        multiWidgetModuleUI("faithfulPlots")
    })


    output$distPlot <- renderPlot({
        plotFunc(input,output,session,input$bins)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
