#server.R
shinyServer(function(input,output)
{
  
  sliderValues <- reactive({
    
    # Compose data frame
    testdata <- data.frame(
      Survived=NA,
      Pclass=factor(input$Pclass,levels=c("1","2","3")),
      Sex=factor(input$Sex,levels=c("male","female")),
      Age=input$Age,
      Fare=input$Fare,
      Embarked=factor(input$Embarked, levels=c("C","Q","S")),
      Title=factor(input$Title,levels= c(" Capt"," Col"," Don"," Dona",
                                         " Dr" ," Jonkheer"," Lady"," Major",      
                                         " Master"," Miss"," Mlle"," Mme",         
                                         " Mr"," Mrs"," Ms"," Rev",         
                                         " Sir"," the Countess")),
      FamilySize= input$FamilySize,
      SibSp=input$SibSp)
          
    
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
  output$text<- renderText({
    paste("乘客生还预测结果（1为生还，0为未生还）：", predict(rf_model22,sliderValues()))
  })
 
})