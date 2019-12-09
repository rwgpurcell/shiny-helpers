
widgetList <- list(
  plot = list(ui = renderPlot, server = plotOutput),
  table = list(ui = renderUI, server = uiOutput),
  ui = list(ui = renderTable, server = tableOutput)
)



multiWidgetModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("multiWidget"))
}

#function_inputs must be a list of
multiWidgetModule <- function(input, output, session, func, func_inputs, widget) {
  if(!widget %in% names(widgetList)){
    stop('widget must be from known list ')
  }
  #set ns for use in renderUI outputs
  ns <- session$ns
  renderWidget <- widgetList[widget]$server
  widgetOutput <- widgetList[widget]$ui

  nWidgets <- length(widgetList)

  for(i in 1:nWidgets){
    local({
      my_i <- i
      #nButtons: paste0("button",my_i)
      output[[my_i]] <- renderWidget(func(func_inputs[my_i]))
    })
  }


  if(is.null(choices) & is.null(n_buttons)){
    stop("One of 'choices' and 'n_buttons' must be provided")
  } else if (is.null(choices)){
    choices = 1:n_buttons
  }



  buttonData <- reactiveValues(presses=0,lastPress = NULL)

  nButtons <- length(choices)
  #print(nButtons)
  if(is.null(names(choices))){
    names(choices)=1:n_buttons
  }

  #print(names(choices))
  for(i in 1:nButtons){
    local({
      my_i <- i
      #nButtons: paste0("button",my_i)
      observeEvent(input[[names(choices)[my_i]]], {
        buttonData$presses <- buttonData$presses + 1
        buttonData$lastPress <- choices[my_i]
      })
    })
  }

  #nButtons: paste0("button",i)
  #inline: no br()s
  button_list <- lapply(1:nButtons, function(i) {
    tagList(actionButton(ns(names(choices)[i]), names(choices)[i]),br())
  })

  output$buttonSet <- renderUI({
    button_list
  })

  return(buttonData)
}
