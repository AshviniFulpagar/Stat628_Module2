rm(list=ls())
library(shiny)


# calculate body fat

Bodyfat = function(x){
  if (sum(is.na(x))==0){
    out = -13.683991+0.028202*x[3]-0.075871*x[1]-0.053628*x[4]+
      0.175768*x[5]-0.275745*x[6]-0.008257*x[8]+0.819270*x[2]-1.112699*x[7]
    return(out)
  } else if(sum(is.na(x[1:4]))==0){
    out = -32.617318-0.009708*x[3]-0.125244*x[1]-0.117822*x[4]+0.893289*x[2]
    return(out)
  }else{
    return("ERROR: please input at least the first four variables!")
  }
}


ui = fluidPage(
  # title
  titlePanel("Body fat calculator"),
  # input
  numericInput("x1",label=h3("Weight(lbs):"),value=NA),
  numericInput("x2",label=h3("Abdomen 2 circumference(cm):"),value=NA),
  numericInput("x3",label=h3("Age(years):"),value=NA),
  numericInput("x4",label=h3("Height(inches):"),value=NA),
  numericInput("x5",label=h3("Adioposity(bmi):"),value=NA),
  numericInput("x6",label=h3("Neck(cm):"),value=NA),
  numericInput("x7",label=h3("Wrist(cm):"),value=NA),
  numericInput("x8",label=h3("Chest(cm):"),value=NA),
  hr(),
  verbatimTextOutput("value")
)

server = function(input,output){
  # `value` will in the output
  output$value = renderPrint({
    Bodyfat(c(input$x1,input$x2,input$x3,input$x4,input$x5,
              input$x6,input$x7,input$x8))
  })
}

shinyApp(ui = ui, server = server)


