### This code is for "body fat calculator - shiny app" , it takes various inputsfrom the user and calculates the body fat
### please install "shinythemes" package before running the code
rm(list=ls())
library(shiny)
library(shinythemes)

#calculate the result
Bodyfat = function(x){
  if (sum(is.na(x))==0){
    fat = -13.683991+0.028202*x[3]-0.075871*x[1]-0.053628*x[4]+
      0.175768*x[5]-0.275745*x[6]-0.008257*x[8]+0.819270*x[2]-1.112699*x[7]
  }else if(sum(is.na(x[1:4]))==0){
    fat = -32.617318-0.009708*x[3]-0.125244*x[1]-0.117822*x[4]+0.893289*x[2]
  }
  
  if (sum(is.na(x[1:4]))>0){
    value = "Please input at least the first four variables!"
    suggest = "Please input at least the first four variables!"
  }else if(fat<=0){
    value = "ERROR: Please reenter the correct input!"
    suggest = "ERROR: Please reenter the correct input!"
  }else if(fat<2 | fat>45){
    value = paste0("Warning: Your body fat percentage is ",fat,
                   ". You have entered incorrect inputs as this body fat is unusual")
    suggest = paste0("Warning: Your body fat percentage is ",fat,
                     ". You have entered incorrect inputs as this body fat is unusual ")
  }else{
    value = paste0("Your body fat percentage is: ",fat)
    if(fat<5){
      suggest = "You are underweight. Take more nutrition and exercise."
    }else if (fat<11){
      suggest = "You have lesser than the recommended body fat, unless you are an Athelet"
    }else if (fat<21){
      suggest = "You are healthy. Just keep going."
    }else if (fat<28){
      suggest = "You are overweight, consult your dietitian!"
    }else {
      suggest = "You are obese. Please consult your dietitian."
    }
  }
  return(c(value,suggest))
}

# ui
ui = fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Body Fat Calculator"),
  sidebarPanel(
      #width=10,
      h6("Note: Items with * are required. Fill all the iteams can give a more acuurate results."),
      hr(),
      #input weight, abdomen, age, height, bmi, neck, wrist, chest
      numericInput("x1",label=h6("Weight(lbs):*"),value=NA),
      numericInput("x2",label=h6("Abdomen circumference(cm):*"),value=NA),
      numericInput("x3",label=h6("Age(years):*"),value=NA),
      numericInput("x4",label=h6("Height(inches):*"),value=NA),
      numericInput("x5",label=h6("Adioposity(bmi):"),value=NA),
      numericInput("x6",label=h6("Neck(cm):"),value=NA),
      numericInput("x7",label=h6("Wrist(cm):"),value=NA),
      numericInput("x8",label=h6("Chest(cm):"),value=NA),
      submitButton("Calculate"), #click the button to predict
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Your body fat(%)",
               h4("Your body fat percentage:"),
               verbatimTextOutput("value")),
      tabPanel("Suggestion",
               h4("Suggestion from us based on your body fat:"),
               verbatimTextOutput("suggestion")),
      tabPanel("Introduction",h6("This is the introduction page.")),
      tabPanel("Contact information",
               h6("This app is build by group 17. If you meet any problem, please contact any of the creators:"),
               h6("Luyang Fang: lfang45@wisc.edu"),
               h6("Ashvini Fulpagar:"),
               h6("Hongyi Liu:"))
  ),
 )
)


# server
server = function(input, output,session) {
  #print result of the bodyfat
  output$value = renderPrint({
    Bodyfat(c(input$x1,input$x2,input$x3,input$x4,input$x5,
              input$x6,input$x7,input$x8))[1]
  })
  #print health suggestion according to the bodyfat result
  output$suggestion <- renderText({
    Bodyfat(c(input$x1,input$x2,input$x3,input$x4,input$x5,
                    input$x6,input$x7,input$x8))[2]
  })
}

shinyApp(ui, server)

