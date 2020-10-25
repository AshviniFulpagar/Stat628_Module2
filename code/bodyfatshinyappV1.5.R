### This code is for "body fat calculator - shiny app" , it takes various inputsfrom the user and calculates the body fat
### please install "shiny" and "shinythemes" package before running the code


rm(list=ls())
library(shiny)
library(shinythemes)

#calculate the result
Bodyfat = function(x){
  # when the user give us all 8 varaibles, we use model 2 to predict
  if (sum(is.na(x))==0){
    fat = -40.86809-0.11583*x[1]+0.73*x[2]+0.06391*x[3]+
      0.26441*x[4]+0.54095*x[5]-0.31450*x[6]-1.32608*x[7]+0.23050*x[8]
  }else if(sum(is.na(x[1:4]))==0){  # when we don't have all 8 variables but have the most important 4 varisbles, use model 1
    fat = -33.965020-0.09474*x[1]+0.843770*x[2]-0.004409*x[3]-0.116907*x[4]
  }
  # if the user didn't give the four most important variables, output error information
  if (sum(is.na(x[1:4]))>0){
    value = "Please input at least the first four variables!"
    suggest = "Please input at least the first four variables!"
  }else if(fat<=0){ 
    # Body fat rate cannot be <0 so there must be somthing wrong with the input, then tell the user about this
    # 'value' is used to store the estimation of body fat
    # 'suggest' is used to store the suggestion we will give to the users based on their body fat
    value = "ERROR: Wrong input, Please enter your inputs again!"
    suggest = "ERROR: Wrong input, Please enter your inputs again!"
  }else if(fat<2 | fat>45){
    # when the body fat lies in this interval, it is almost impossible, 
    # so we will output the result but also give the warning information.
    value = paste0("Warning: Your body fat percentage is ",fat,
                   ". You have entered incorrect inputs as this body fat is unusual.")
    suggest = paste0("Warning: Your body fat percentage is ",fat,
                     ". You have entered incorrect inputs as this body fat is unusual.")
  }else{
    # in other situation, we will consider inputs as good and do prediction
    value = paste0("Your body fat percentage is: ",fat)
    # we will give different suggestions based on user's body fat estimation
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
  theme = shinytheme("cyborg"), # theme
  titlePanel("Bodyfat Calculator"),
  sidebarPanel(
    h6("Note: Items with * are required. Fill all the iteams can give a more acuurate results."),
    hr(),
    #input weight, abdomen, age, height, bmi, neck, wrist, thigh
    numericInput("x1",label=h6("Weight(lbs):*"),value=NA),
    numericInput("x2",label=h6("Abdomen circumference(cm):*"),value=NA),
    numericInput("x3",label=h6("Age(years):*"),value=NA),
    numericInput("x4",label=h6("Height(inches):*"),value=NA),
    numericInput("x5",label=h6("Adioposity(bmi):"),value=NA),
    numericInput("x6",label=h6("Neck circumference(cm):"),value=NA),
    numericInput("x7",label=h6("Wrist circumference(cm):"),value=NA),
    numericInput("x8",label=h6("Thigh circumference(cm):"),value=NA),
    submitButton("Calculate"), #click the button to predict
  ),
  
  mainPanel(
    tabsetPanel(
      # estimation of user's body fat
      tabPanel("Your body fat(%)",
               h4("Your body fat percentage:"),
               verbatimTextOutput("value")),
      # our suggestion
      tabPanel("Suggestion",
               h4("Suggestion from us based on your body fat:"),
               verbatimTextOutput("suggestion")),
      # introduction of our bodyat calculator app
      tabPanel("Introduction",
               h6("This app predicts your body fat percentage based on some easy measurements"),
               h6("Please enter measurements of weight(lbs), Abdomen circumference(cm), 
                  Age(years), Height(inches), Adioposity(bmi), Neck circumference(cm), 
                  Wrist circumference(cm), Thigh circumference(cm) to get the most accurate result! "),
               h6("It is also sufficient to enter measurements of only weight(lbs), Abdomen circumference(cm), Age(years),and Height(inches) to get an estimation of your body fat."),
               h6("Once you enter correct measurements, the application will calculate body fat percentage and give you suggestion
                  based on your body fat. For example, if your body fat percentage is in the range of 21 to 28, then 
                  you may probabily overweight and we will suggest you to consult your dietitian. The standard is 
                  from https://www.calculator.net/body-fat-calculator.html")),
      # contact information
      tabPanel("Contact information",
               h6("Please contact us for further queries."),
               h6("Luyang Fang: lfang45@wisc.edu"),
               h6("Ashvini Fulpagar: fulpagar@wisc.edu"),
               h6("Hongyi Liu: hliu557@wisc.edu"))
    ),
  ),
)


# server
server = function(input, output,session) {
  #print result of the bodyfat
  output$value = renderPrint({
    Bodyfat(c(input$x1,input$x2,input$x3,input$x4,input$x5,
              input$x6,input$x7,input$x8))[1] # 'value'
  })
  #print health suggestion according to the bodyfat result
  output$suggestion <- renderText({
    Bodyfat(c(input$x1,input$x2,input$x3,input$x4,input$x5,
              input$x6,input$x7,input$x8))[2] #'suggest'
  })
}

shinyApp(ui, server)

