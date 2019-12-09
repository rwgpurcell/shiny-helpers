
widgetList <- list(
  plot = list(ui = plotOutput, server = renderPlot),
  table = list(ui = uiOutput, server = renderUI),
  ui = list(ui = tableOutput, server = renderTable)
)



multiWidgetModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("widgetSet"))
}

#function_inputs must be a list of
multiWidgetModule <- function(input, output, session, func, func_inputs, widgetFuncs) {
  # if(!widget %in% names(widgetList)){
  #   stop('widget must be from known list ')
  # }
  #set ns for use in renderUI outputs
  ns <- session$ns
  print("HOWDY!!!")
  # renderWidget <- widgetList[widget]$server
  # widgetOutput <- widgetList[widget]$ui

  nWidgets <- length(func_inputs)

  for(i in 1:nWidgets){
    local({
      #renderWidget <- widgetList[widget]$server
      my_i <- i
      #nButtons: paste0("button",my_i)
      print(func_inputs[[my_i]])
      output[[my_i]] <- widgetFuncs$server({func(func_inputs[[my_i]])})
    })
  }

  #nButtons: paste0("button",i)
  #inline: no br()s
  widget_list <- lapply(1:nWidgets, function(i) {
    tagList(widgetFuncs$ui(ns(i)),br())
  })

  output$widgetSet <- renderUI({
    widget_list
  })

  return(1)
}
