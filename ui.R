library(shiny)
shinyUI(pageWithSidebar (
  headerPanel("Titanic: Machine Learning from Disaster"),
  sidebarPanel (
    selectInput ("Pclass","choose the Pclass:",choices = c("1","2","3")),
    selectInput ("Sex","choose the sex:",choices = c("female","male")),
    sliderInput("Age","choose the age:",
                min=0,max=80,value = 40,step = 1),
    sliderInput("Fare","choose the Fare:",
                min=0,max=520,value = 35,step = 0.1),
    selectInput ("Embarked","choose the Embarked:",choices = c("C","Q","S")),
    selectInput ("Title","choose the Title:",choices =c(" Capt"," Col"," Don"," Dona",
                                                        " Dr" ," Jonkheer"," Lady"," Major",      
                                                        " Master"," Miss"," Mlle"," Mme",         
                                                        " Mr"," Mrs"," Ms"," Rev",         
                                                        " Sir"," the Countess")),
    sliderInput("FamilySize","choose the FamilySize:",
                min=1,max=11,value = 3,step = 1),
    sliderInput("SibSp","choose the SibSp:",
                min=0,max = 8,value = 1,step = 1)
                
  ),
  mainPanel(
    tableOutput("values"),
    textOutput("text")
  )
)
        
        
        
        )