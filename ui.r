## ui.R ##
## server.R ##
library(shiny)
library(DMwR)
library(readxl)
library(shinydashboard)
library(plotly)
library(dplyr) #dplyr ne s'utilise qu'avec des dataframes
library(questionr) # pour des tableaux
library(shinythemes)
library(dplyr)
library(rpart)
library(rpart.plot)
library(nnet)
library(NeuralNetTools)
library(kernlab)
library(ROCR)
library(pROC)
library(dashboardthemes)
library(DT)
library(plotly)
library(ggpubr)
library(car)
#setwd("D:/EAD/kouki/Projet_Econometrie")
German_health_care_data <- read_excel("German-health-care_data.xlsx")

German_health_care_data$FEMALE<-ifelse(German_health_care_data$FEMALE==2,0,1)
German_health_care_data$FEMALE=factor(German_health_care_data$FEMALE)

German_health_care_data$DOCTOR<-ifelse(German_health_care_data$DOCTOR==2,0,1)
German_health_care_data$DOCTOR=factor(German_health_care_data$DOCTOR)


German_health_care_data$HHKIDS<-ifelse(German_health_care_data$HHKIDS==2,0,1)
German_health_care_data$HHKIDS=factor(German_health_care_data$HHKIDS)


German_health_care_data$WORKING<-ifelse(German_health_care_data$WORKING==2,0,1)
German_health_care_data$WORKING=factor(German_health_care_data$WORKING)



data=German_health_care_data
data=subset(German_health_care_data,German_health_care_data$YEAR==1994)
data=data[,-c(1,3)]

dataQuali=data[,c(1,4,6,10)]
dataQuanti=data[,-c(1,4,6,10)]

data=data.frame(data)
attach(data)




######################################################

ui <- dashboardPage(
  dashboardHeader(
    title=h3(span(strong("dataScience+"), style = "color:white"))),
    #title = "dataScience+"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      strong("UPLOAD YOUR DATA"),
      menuItem(strong("Welcome & Upload Data"), tabName = "WelcomeUpload", icon = icon("door-open")),
      
      strong("DATA EXPLORATION"),
      menuItem(strong("Univariate Analysis"), tabName = "univariate", icon = icon("server")),
      menuItem(strong("Bivariate Analysis"), tabName = "multivariate", icon = icon("lightbulb")),

      strong("REGRESSION MODELS"),
      menuItem(strong("Preparing Data"), tabName = "Acknowledgements", icon = icon("file-alt")),
      menuItem(strong("Regression"), tabName = "logisticRegression", icon = icon("cube")),
      menuItem(strong("Performances"), tabName = "performance", icon = icon("chart-line")),
      
      strong("New Client"),
      menuItem(strong("Formulaire"), tabName = "formulaire", icon = icon("file-alt")),
      #menuItem("Acknowledgements", tabName = "Acknowledgements", icon = icon("file-alt")),
      #strong(""),
      strong("About the App"),
      menuItem(strong("Informations"), tabName = "sendAmessage", icon =icon("question-circle"))
    )
  )
  ,
  ## Body content
  dashboardBody(
    shinyDashboardThemes(
      theme = "black"
    ),
    tabItems(
      
      tabItem(tabName = "WelcomeUpload",
              fluidRow(
                       box(title = "The Year to be Studied",
                           status = 'primary',
                           #solidHeader = T,
                           width = 12,
                           collapsible = T,
                           numericInput('Year', 'Year', value =1994,min=1884,max=1994))),
              fluidRow(
                tabBox(width = 12,
                     # The id lets us use input$tabset1 on the server to find the current tab
                     id = "tabset1", height = "250px",
                     tabPanel("Your Holl Dataset",
                              div(style = 'overflow-x: scroll',dataTableOutput('tableau'))
                              ),
                     tabPanel("The Data to be studied",
                              div(style = 'overflow-x: scroll',dataTableOutput('tableauPartielle'))
                     ),
                     tabPanel("Data Structure",
                              h4("Data Structure."),
                              verbatimTextOutput("DataStructure"))
                     
                     ))),
      tabItem(tabName = "univariate",
              fluidRow(
                box(title = "Variable Quantitative",
                    status = 'primary',
                    #solidHeader = T,
                    width = 3,
                    collapsible = T,
                    selectInput("vquanti","Choisir la variable",
                                choices = colnames(dataQuanti)),
                    ),
                tabBox(width = 9,
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset580",
                       tabPanel("Summary",verbatimTextOutput("summaryVar")),
                       
                       tabPanel("Boxplot",
                                h4("Boxplot "),
                                plotlyOutput("Boxplot")),
                       tabPanel("Histogramme",
                                h4("Histogramme"),
                                plotlyOutput('Histogramme')),
                       tabPanel("DensityPlot",
                                h4(" DensityPlot"),
                                plotlyOutput("DensityPlot")),
                       tabPanel("QQPLOT",
                                h4(" QQPLOT"),
                                plotOutput("QQPLOT")),
                       tabPanel("Normality Test",
                                h4("Normality Test"),
                                verbatimTextOutput('testNorm')))
                ),
              br(),
              br(),
              
              fluidRow(
                box(title = "Variable Quanlitatives",
                    status = 'primary',
                    #solidHeader = T,
                    width = 3,
                    collapsible = T,
                    selectInput("vquali","Choisir la variable",
                                choices = colnames(dataQuali))),
                tabBox(width = 8,
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset2",
                       tabPanel("Table de FrÃ©quences",verbatimTextOutput("Contingtab")),
                       
                       tabPanel("PieChart",
                                h4(" PieChart"),
                                plotlyOutput("PieChart")),
                       tabPanel("BarPlot",
                                h4("BarPlot"),
                                plotlyOutput('BarPlot')))
                )
              
    ),
    tabItem(tabName = "multivariate",
            box(title = "Variable Quantitative",
                status = 'primary',
                #solidHeader = T,
                width = 12,
                collapsible = T,
                selectInput("vCible","Variable Cible(Factor)",choices = colnames(dataQuali)),
                selectInput("vExplic","Variable Explicatives",choices = colnames(data))),
            
            tabBox(width = 12,
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset2", 
                   #height = "200px",
                   tabPanel("Resume",verbatimTextOutput("resume"),tableOutput("resume2"),verbatimTextOutput("resume3")),
                   
                   tabPanel("Boxplot / BarPlot",
                            plotOutput('boxMultiv'),
                            h4("")),
                   
                   tabPanel("Mosaique Plot",plotOutput('mosaique'))
                   
            ))
    ,
    tabItem(tabName = "logisticRegression",
            tabsetPanel(
              
              tabPanel("Regression sur DOCTOR",
                       
                       fluidRow(
                         box(title = "Definition du modele",
                             status = 'primary',
                             #solidHeader = T,
                             width = 6,
                             collapsible = T,
                             #h4(" La variable cible ici est le facteur: DOCTOR"),
                             selectInput("LaCibl","Variable Cible",choices = c('DOCTOR')),
                             #br(),
                             selectInput("meThode","Methode/Noyau",
                                         choices = c('Logistic Regression','Decision Trees','Neural Network')),
                             #br(),
                             sliderInput("slider", label = h4("Choisir la taille de << Train Data >> "), min = 0, 
                                         max = dim(data)[1], value = round(dim(data)[1]*0.75)))
                         ,
                         box(title = "Variable explicatives",
                             status = 'primary',
                             #solidHeader = T,
                             width = 6,
                             collapsible = T,
                             checkboxGroupInput("checkGroup", 
                                                label = "", 
                                                choices=colnames(data[,-7]),
                                                selected=colnames(data[,-7])),
                             tags$hr(),
                             checkboxInput('includeClasses','Inclure les Classes dans le modele')
                         )),
                       #tags$hr(),
                       #br(),
                       
                       fluidRow(
                         box(title = "                MODELE           ",
                             status = 'primary',
                             #solidHeader = T,
                             width = 12,
                             collapsible = F,
                             verbatimTextOutput("textModelLogReg"))),
                       
                       fluidRow(
                         tabBox(width = 12,
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset2806", 
                                tabPanel("Resume",
                                         h4("Summary")
                                         ,verbatimTextOutput("textLogReg")
                                         ,div(style = 'overflow-x: scroll',dataTableOutput('intConfcoef'))
                                         ,verbatimTextOutput("waldTest")
                                ),
                                
                                tabPanel("ROC Plot"
                                         ,plotOutput('rocPlot3')),
                                tabPanel('Lift Curve',
                                         plotOutput("liftPlot3.1"),plotOutput("liftPlot3.2"))
                                          
                                
                         )
                       )
                       
                       
                       
                       
                       ),
              tabPanel("Regression sur DOCVIS",
                       
                       fluidRow(
                         box(title = "Definition du modele",
                             status = 'primary',
                             #solidHeader = T,
                             width = 6,
                             collapsible = T,
                             #h4(" La variable cible ici est le facteur: DOCTOR"),
                             selectInput("LaCiblDOCVIS","Variable Cible",choices = c('DOCVIS')),
                             #br(),
                             selectInput("meThodDOCVIS","Methode/Noyau",
                                         choices = c('Poisson Regression','Negative Binomial','Decision Trees')),
                             #br(),
                             sliderInput("sliderDOCVIS", label = h4("Choisir la taille de << Train Data >> "), min = 0, 
                                         max = dim(data)[1], value = round(dim(data)[1]*0.75)))
                         ,
                         box(title = "Variable explicatives",
                             status = 'primary',
                             #solidHeader = T,
                             width = 6,
                             collapsible = T,
                             checkboxGroupInput("checkGroupDOCVIS", 
                                                label = "", 
                                                choices=colnames(data[,-10]),
                                                selected=colnames(data[,-10])),
                             tags$hr(),
                             checkboxInput('includeClassesDOCVIS','Inclure les Classes dans le modele')
                         )),
                       #tags$hr(),
                       #br(),
                       
                       fluidRow(
                         box(title = "                MODELE           ",
                             status = 'primary',
                             #solidHeader = T,
                             width = 12,
                             collapsible = F,
                             verbatimTextOutput("textModelDOCVIS"))),
                       
                       fluidRow(
                         tabBox(width = 12,
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabsetDOCVIS2806", 
                                tabPanel("Resume",
                                         h4("Summary")
                                         ,verbatimTextOutput("textLogRegDOCVIS")
                                         ,div(style = 'overflow-x: scroll',dataTableOutput('intConfcoefDOCVIS'))
                                         ,verbatimTextOutput("waldTestDOCVIS")
                                ),
                                
                                tabPanel("Plot of Observe and Predicted Values on Test data"
                                         ,plotOutput('rocPlot3DOCVIS')),
                                tabPanel("Evaluation du Modele",
                                         verbatimTextOutput("chose1"),
                                         verbatimTextOutput("chose2"),
                                         plotOutput("chose3")
                                         
                                         )
                                
                         )
                       )
                       
                       
                       
                       
                       
                       
                       
                       )
                        
                        )
                        
                                
            
            )
    ,
    tabItem(tabName = "performance",
            #'Logistic Regression','Decision Trees','Neural Network'
            fluidRow(
              valueBoxOutput("LR"),
              valueBoxOutput("DT"),
              valueBoxOutput("NN")),
            br(),
            tags$hr(),
            
            fluidRow(
              box(title = "Cumulatives ROCs on Test data ",
                  status = 'primary',
                  #solidHeader = T,
                  width = 12,
                  collapsible = T,
                  plotOutput("cumRocPlotTest"))),
            br(),
            br(),
            
            fluidRow(
              box(title = "Indicateurs de Performance",
                  status = 'primary',
                  #solidHeader = T,
                  width = 12,
                  collapsible = T,
                  tableOutput("tabResrComparaison"))
              )
            
            
            )
    ,
    tabItem(tabName = "formulaire",
      fluidRow(
          box(title = "Values Entering ",
                  status = 'primary',
                  #solidHeader = T,
                  width = 3,
                  collapsible = T,
              numericInput('female', 'FEMALE', value = 1, min = 0, max = 1),
              numericInput('age', 'Age', value = round(mean(data$AGE)), min = 0, max = 100),
              numericInput('hhninc', 'HHNINC', value = 2, min = 0, max = 3),
              numericInput('hhkids', 'HHKIDS', value =1, min = 0, max = 1),
              numericInput('educ', 'EDUC', value = round(mean(data$EDUC)), min = 0, max = 30),
              numericInput('working', 'WORKING', value = 1, min = 0, max = 1),
              numericInput('hospvis', 'HOSPVIS', value = round(mean(data$Health_Satisfaction_Index)), min = 0, max = 200),
              numericInput('hsi', 'Health Satisfaction Index', value = round(mean(data$Health_Satisfaction_Index)), min = 0, max = 100)
                  ),
          tabBox(
            width = 9,
            title = "Predict New Client issues",
            tabPanel("Doctor",
                     fluidRow(
                       h4('  Predicted Doctor value for the client below'),
                       valueBoxOutput("PredireNvo.LR")
                       ,valueBoxOutput("PredireNvo.DT")
                       ,valueBoxOutput("PredireNvo.NN")
                     ),
                     
                     fluidRow(
                       box(title = "RÃ©sultats",
                           status = 'primary',
                           #solidHeader = T,
                           width = 12,
                           collapsible = T
                           ,h4("Recapitulatif des infos sur le nouveau client")
                           ,div(style = 'overflow-x: scroll',dataTableOutput('tableNvoClient'))
                           ,tableOutput("predNvoClient")
                           ,
                           br(),
                           br()
                           ,tableOutput("textNvoClient"))) 
                     ),
            tabPanel("DOCVIS",
                     fluidRow(
                       h4('  Predicted DOCVIS number for the client below'),
                       valueBoxOutput("PredireNvoDOCVIS.PM")
                       ,valueBoxOutput("PredireNvoDOCVIS.DT")
                     ),
                     
                     fluidRow(
                       box(title = "RÃ©sultats",
                           status = 'primary',
                           #solidHeader = T,
                           width = 12,
                           collapsible = T
                           ,h4("Recapitulatif des infos sur le nouveau client")
                           ,div(style = 'overflow-x: scroll',dataTableOutput('tableNvoClientDOCVIS'))
                           ,tableOutput("predNvoClientDOCVIS")
                           ,
                           br(),
                           br()
                           ,tableOutput("textNvoClientDOCVIS")))
                     
                     )
          )
            
      )      
    )
    ,
    tabItem(tabName = "Acknowledgements",
            
            fluidRow(
              box(title = "Correlation des Variables",
                  status = 'primary',
                  #solidHeader = T,
                  width = 10,
                  #collapsible = T,
                  plotOutput('corr1')
              ),
              box(title = "Creer des Classes de Variable",
                  status = 'primary',
                  #solidHeader = T,
                  width = 2,
                  #collapsible = T,
                  textInput('clasAGE', 
                            'clasAGE', 
                            "0,0,0"),
                  textInput('clasHHNINC', 
                            'clasHHNINC', 
                            "0,0,0"),
                  #textInput('clasDOCVIS', 
                  #          'clasDOCVIS', 
                  #          "0,0,0"),
                  textInput('clasHOSPVIS', 
                            'clasHOSPVIS', 
                            "0,0,0"),
                  textInput('clasEDUC', 
                            'clasEDUC', 
                            "0,0,0"),
                  textInput('clasHSI', 
                            'clasHSI', 
                            "0,0,0")
              )
              ),
            fluidRow(
              tabBox(width = 9,
                     # The id lets us use input$tabset1 on the server to find the current tab
                     id = "tabset580",
                     
                     tabPanel("Afficher la table",
                              h4(" Table Modifiee")
                              ,div(style = 'overflow-x: scroll',dataTableOutput('Q1'))),
                     tabPanel("Structure",
                              h4(""),
                              verbatimTextOutput('t1')))
            ),
            br(),
            br()
            
            
            
            )
    ,
    tabItem(tabName = "sendAmessage",
            
            h2(span(strong("A propos de l'application "), style = "color:maroon")),                    
            
            p("Dans ce projet , nous avons essayé de prédire le comportement d'un client en se basant sur ses informations saisies notemment le nombre d'enfants,
 le nombre d'années d'éducation, l'age etc... .
 Plus précisemment , nous avons prédit dans un premier lieu et à l'aide de trois modèles conçus, la possibilité d'un client de revenir à un hopital privé après l'avoir 
visité. Dans un second lieu, nous avons prédit le nombre de visite qu'un client peut éffectuer, si jamais il décide de revenir à l'hôpital privé. ",style = "font-family: 'times'; font-si16pt"),
            
            br(),    
            h2(span(strong("Présentation de la base de Données"), style = "color:maroon")),                    
            
            span(p(" Notre travail était fait sur le jeu de données German Health Care. Cette base présente 
            quatre variables qualitatives qui sont les suivantes:,
,FEMALE (qui prend 1 si le client et un homme et 2 si le client est une femme),
                   HHKIDS(qui prend 1 si le client a un enfants et 2 si non ), 
Working(elle prend 1 si le client est au chomage et 0 s'il a un travail) et 
DOCTOR(elle prend 1 s'il compte  revenir à l'hopital privé et 2 si non ).
et 6 variables quantitatives telle que : l'année , l'AGE, EDUC( qui présente le nombre d'années d'étude du client),DOCVIS(Donne le nombre de visite d'un hôpital privé),
Health_satisfaction_index(Elle prend ses valeur dans l'intervalle 0:10 et présente la satisfaction du client vis à vis le service de l'hôpital privé) 		
 
    ",strong("SEANCE"),",",strong(" LIB_VAL")," (Contenant les noms des diverses actions cotees) et ",strong("CLOTURE")," (la cloture des 
    actions). 
    Charger la base de donnee de la bourse de valeurs mobilieres de Tunisie pour une annee donnee. 
    Ensuite vous pouvez dans un premier temps decrire chacune des actions isolees ou la combinaison 
    de plusieurs actions a la fois a travers la partie Exploration et Analyse Descriptive proposee 
    dans cette interface.
    Ensuite, vous pouvez disposer d'un portefeuille et en fonction de votre aversion ou repulsion
    du risque vous interessez a l'aspect rentabilite journaliere du portefeuille et ou du niveau 
    de risque encouru dans la detention de ce portefeuille et comparer egalement plusieurs 
    portefeuilles en terme de risque et de rentabilite.",style = "font-family: 'times'; font-si16pt"),style = "color:olive"),
            
            
            br(),
            h2(span(strong("Contact"), style = "color:maroon")),                    
            p("Vos suggestions, commentaires, plaintes ou compliments sont tres apprecies et nous guideront pour
  ameliorer le tableau de bord en continu. Veuillez les envoyer par courrier electronique a ",
              strong(span("abussabuk@gmail.com ou encore missiano.aymen@gmail.com",style = "color:maroon")),style = "font-family: 'times'; font-si16pt"),
            
            
            
            
    )
#*************    
   
)))