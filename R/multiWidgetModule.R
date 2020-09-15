library(shiny)

widgetList <- list(
  plot = list(ui = plotOutput, server = renderPlot),
  table = list(ui = uiOutput, server = renderUI),
  ui = list(ui = tableOutput, server = renderTable)
)


#' @export
multiWidgetModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("widgetSet"))
}

#' @export
#function_inputs must be a list of
multiWidgetModule <- function(input, output, session,
                              func, func_inputs,
                              widgetFuncs = list(ui = uiOutput, server = renderUI),
                              nColumns = 1) {
  # if(!widget %in% names(widgetList)){
  #   stop('widget must be from known list ')
  # }
  #set ns for use in renderUI outputs
  ns <- session$ns
  # print("HOWDY!!!")
  # renderWidget <- widgetList[widget]$server
  # widgetOutput <- widgetList[widget]$ui
  if(!is.numeric(nColumns)){
    stop("nColumns must be numeric")
  }
  nColumns = floor(abs(nColumns))


  nWidgets <- length(func_inputs)
  # print(nWidgets)
  # print(func_inputs)
  for(i in 1:nWidgets){
    local({
      #renderWidget <- widgetList[widget]$server
      my_i <- i
      #nButtons: paste0("button",my_i)
      # print(length(func_inputs[[my_i]]))
      # print(func_inputs[[my_i]])
      args <- c(list(input,output,session),func_inputs[[my_i]])
      # print(typeof(args))
      # print(length(args))
      # print(names(args))
      # print(args)
      output[[my_i]] <- widgetFuncs$server({
        do.call(func,args)
        })
    })
  }

  #nButtons: paste0("button",i)
  #inline: no br()s

  widget_grid <- lapply(1:ceiling(nWidgets/nColumns),function(i){
    fluidRow(tagList(lapply((1+(i-1)*nColumns):min(i*nColumns,nWidgets),function(j){
      tagList(column(width=12/nColumns,widgetFuncs$ui(ns(j))))
    })))
  })

  widget_list <- lapply(1:nWidgets, function(i) {
    tagList(widgetFuncs$ui(ns(i)),br())
  })

  output$widgetSet <- renderUI({
    widget_grid
  })


  #ret <- reactiveValues()

  # ret <- reactive({
  #   print("hello")
  #   print(names(input))
  #   names(input)
  # })

  # TODO return reactive containing input
  # For now, it's best if you know the names of the inputs based on whatever data used to
  # call your func
  #print(input)
  return(1)
}
