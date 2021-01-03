## server.R ##
#update.packages(ask = FALSE, checkBuilt = TRUE)
library(DMwR)
library(shiny)
library(xtable)
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
library(MASS)
library(DT)
library(plotly)
library(ggpubr)
library(aod)
library(corrplot)
library(psych)

set.seed(sample(1:100,1))
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
str(data)

train_id=sample(dim(data)[1],round(dim(data)[1]*0.75))
data_train=data[train_id,]
data_test=data[-train_id,]

logit=glm(DOCTOR~.,data = data_train,family = binomial)
prob_test=predict(logit,data_test,type = "response")
pred_test=prediction(prob_test,data_test$DOCTOR)
perf_roc_test=performance(pred_test,measure = "tpr",x.measure ="fpr")

mycontrol = rpart.control(cp = 0, xval = 10)
model_ar = rpart(DOCTOR~ .,method = "class" ,control = mycontrol, data=data_train)
prob_test_ar=predict(model_ar,data_test,type = "prob")[,2]
pred_test_ar=prediction(prob_test_ar,data_test$DOCTOR)
perf_roc_test_ar=performance(pred_test_ar,measure = "tpr",x.measure ="fpr")

Neural = nnet(DOCTOR~ .,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
fitNeural = predict(Neural,newdata=data_test)
prednn = prediction( fitNeural, data_test$DOCTOR)
perfnn = performance(prednn, "tpr", "fpr")
######################################################
######################################################

server <- function(input, output,session) {
  
  output$tableau <- renderDataTable({
    if(is.null(data)){return ()}
    else {return(German_health_care_data)}
  })
  
  #-----------------------------------DISPLAY--------------------------------------------------
  output$tableau <- renderDataTable({
    if(is.null(data)){return ()}
    else {return(German_health_care_data)}
  })
  
  output$tableauPartielle <- renderDataTable({
    if(is.null(data)){return ()}
    else {return(data)}
  })
  
  output$DataStructure<-renderPrint({
    str(data)
  })
  
  vquanti<-reactive({
    x=data[,input$vquanti]
    return(x)
  })
  
  vquali<-reactive({
    x=data[,input$vquali]
    return(x)
    })
  
  
  
  output$summaryVar<-renderPrint({summary(vquanti())})
  

  output$Boxplot<-renderPlotly({
    Boxplot.Graph(data,input$vquanti,' ')
  })

  output$Histogramme<-renderPlotly({
    Histogramme.Graph(data,input$vquanti,' ')
    })
  
  output$DensityPlot=renderPlotly({
    Density.Graph(data,input$vquanti,' ')
    })
  
  output$QQPLOT=renderPlot({
    
    ggqqplot(vquanti())
    })
  
  output$testNorm=renderPrint({shapiro.test(vquanti())})
  
  output$Contingtab<-renderPrint({summary(vquali())})
  
  output$PieChart<-renderPlotly({
    if(input$vquali=='FEMALE'){modalite=c("FEMME","HOMME")}
    else{modalite=c("OUI","NON")}
    PieGraph(data,input$vquali,modalite=modalite,input$vquali)
    })
  
  output$BarPlot<-renderPlotly({
    
    if(input$vquali=='FEMME'){modalite=c("FEMME","HOMME")}
    else{modalite=c("OUI","NON")}
    BarGraph(data,input$vquali,modalite=modalite,input$vquali)
    })
  
  
  mosa<-reactive({
    y=data[,input$vExplic]
    x=data[,input$vCible]
    x=unlist(x)
    y=unlist(y)
    mosaicplot(x ~ y,xlab = input$vCible,ylab = input$vExplic,shade = TRUE, main = "Graphe en mosaique")
  })
  
  output$mosaique<-renderPlot({mosa()})
  
  output$resume = renderPrint({
    y=data[,input$vExplic]
    x=data[,input$vCible]
    x=unlist(x)
    y = unlist(y)
    if(is.factor(y)){
      x = table(x,y)
      x=lprop(x, digits = 2, percent = TRUE)
      x
    }
  })
  
# ************  Multivariate Analysis *************************  
  boxMultiv = reactive({
    y=data[,input$vExplic]
    x=data[,input$vCible]
    x=unlist(x)
    y = unlist(y)
    if(is.numeric(y)){
      x=unlist(x)
      y = unlist(y)
      b=boxplot(y~x,xlab =input$vCible,ylab =input$vExplic,col = "#3399FF",border = "#cc0099",main = paste("Boxplot de ",input$vExplic,'~',input$vCible))
      return(b)
    }
    else{  
      t = table(x,y)
      b=barplot(t,xlab =input$vCible,ylab =input$vExplic,col = c("tomato","lightskyblue") ,legend = levels(x), main = paste(input$vExplic,"Selon ",input$vCible))
      return(b)
      }
  })
  
  output$boxMultiv <-renderPlot({boxMultiv()})
  
  
  output$resume3=renderPrint({
    y=data[,input$vExplic]
    x=data[,input$vCible]
    x=unlist(x)
    y = unlist(y)
    if(is.numeric(y)){
    print(t.test(y~x))}
  })

#------------------------- Text Regression  DOCTOR --------------------------------------------------
  ModelLogReg = reactive({
    if(input$includeClasses) {
      data=ndata()
      form = paste("DOCTOR~",paste(c(colnames(data[,!colnames(data) %in% 'DOCVIS'])), collapse= "+"))
    }
    else{
      form = paste("DOCTOR~", paste(c(input$checkGroup), collapse= "+"))
    }
    modl = as.character(unlist(form))
    modl
  })
  
  #------------------------- Text Regression  DOCVIS --------------------------------------------------
  ModelLogRegDOCVIS = reactive({
    if(input$includeClassesDOCVIS) {
      data=ndata()
      form = paste("DOCVIS~",paste(c(colnames(data[,!colnames(data) %in% 'DOCTOR'])), collapse= "+"))
    }
    else{
      form = paste("DOCVIS~", paste(c(input$checkGroupDOCVIS), collapse= "+"))
    }
    modl = as.character(unlist(form))
    modl
  })
  #------------------------------ Texte regression DOCTOR ------------------
  output$textModelLogReg = renderText({
    ModelLogReg()
  })
  #-----------------------------
  output$textModelDOCVIS = renderText({
    ModelLogRegDOCVIS()
  })
  #-------------------------------    Regression DOCTOR-----------------------
  regreccion = reactive({
    form=ModelLogReg()
    data1=data
    if(input$includeClasses){
    data1=ndata()}
    id = unlist(input$slider)
    train_id = sample(dim(data1)[1],id)
    train_data = data1[train_id,]
    train_data = SMOTE(DOCTOR~.,train_data)
    test_data = data1[-train_id,]
    
    mycontrol = rpart.control(cp = 0, xval = 10)
    
    if(input$meThode == 'Logistic Regression'){
        Logit = glm(formula(form),data = train_data,family = "binomial")
        Logit=step(Logit,direction = 'both',trace = 0)
    }
    else if(input$meThode == 'Decision Trees'){
        Logit = rpart(formula(form),data = train_data,method = "class" ,control = mycontrol)
    }
    else{
        Logit= nnet(formula(form),data = train_data,size=10,maxit=500,decay=.001, linout=F, trace = F)
    }
    return(Logit)
  })
  
  
  #-------------------------------    Regression DOCVIS-----------------------
  regreccionDOCVIS = reactive({
    form=ModelLogRegDOCVIS()
    if(input$includeClassesDOCVIS){data1=ndata()}
    else{data1=data}
      
      id = unlist(input$sliderDOCVIS)
      train_id = sample(dim(data1)[1],id)
      train_data = data1[train_id,]
      train_data = SMOTE(DOCTOR~.,train_data)
      test_data = data1[-train_id,]
      #mycontrol = rpart.control(cp = 0, xval = 10)
      
      if(input$meThodDOCVIS == 'Poisson Regression'){
        Logit = glm(formula(form),data = train_data,family = "poisson")
        Logit=step(Logit,direction = 'both',trace = 0)
      }
      else if(input$meThodDOCVIS == 'Negative Binomial'){
        Logit = glm.nb(formula(form),data = train_data)
        Logit=step(Logit,direction = 'both',trace = 0)
      }
      else {
        Logit = rpart(formula(form),data = train_data,method = "poisson")
      }
      
    return(Logit)
  })
  
  
 #------------------------------------summary de regression logistic---------------------------------- 
  output$textLogReg = renderPrint({
    if(input$meThode=='Logistic Regression'){
        return(summary(regreccion()))
    }
    else{
      return(regreccion())}
  })
  #------------------------------------------------
  output$textLogRegDOCVIS = renderPrint({
    if(input$meThodDOCVIS=='Poisson Regression' || input$meThodDOCVIS=='Negative Binomial' ){
      return(summary(regreccionDOCVIS()))
    }
    else{
      return(regreccionDOCVIS())}
  })
  
  
 #------------------------------- coefficient de la regression logistic     ------------------------------------------------------------------------------- 
  
  output$intConfcoef = renderDataTable({
    L=regreccion()
    if(input$meThode == 'Logistic Regression'){
    ci=confint(L)
    or=coef(L)
    round(exp(cbind(or,ci)),digits = 2)}
  })
  
  output$intConfcoefDOCVIS = renderDataTable({
    L=regreccionDOCVIS()
    if(input$meThode == 'Poisson Regression' ){ # || input$meThode == 'Negative Binomial'){
      ci=confint(L)
      or=coef(L)
      round(exp(cbind(or,ci)),digits = 2)}
  })
  
  output$waldTest = renderText({
    if(input$meThode == 'Logistic Regression'){
    r=regreccion()
    w=wald.test(b = coef(r), Sigma = vcov(r), Terms = 2:r$rank)
    paste("Significativite Globale  P-value: ",w$result$chi2[3])}
  })
  
  output$waldTestDOCVIS = renderText({
    if(input$meThode == 'Poisson Regression' ){ #|| input$meThode == 'Negative Binomial'){
      r=regreccionDOCVIS()
      w=wald.test(b = coef(r), Sigma = vcov(r), Terms = 2:r$rank)
      paste("Significativite Globale  P-value: ",w$result$chi2[3])}
  })
  
  output$rocPlot3 = renderPlot({
    
    if(input$includeClasses){
      data1=ndata()}
    else{data1=data}
    
    id = unlist(input$slider)
    train_id = sample(dim(data1)[1],id)
    data_train=data1[train_id,]
    data_train=SMOTE(DOCTOR~.,data_train)
    data_test=data1[-train_id,]
    #form=ModelLogReg()
    L=regreccion()
    
    if(input$meThode == 'Logistic Regression'){
     # L=step(glm(DOCTOR~.,data = data_train,family = binomial),direction = 'both',trace = 0)
      
      prob_train <- predict(L,newdata=data_train)
      pred_train = prediction( prob_train, data_train$DOCTOR)
      perf_roc_train = performance(pred_train, "tpr", "fpr")
      auc_train = performance(pred_train,measure="auc")
      
      prob_test <- predict(L, newdata=data_test)
      pred_test = prediction(prob_test, data_test$DOCTOR)
      perf_roc_test <- performance(pred_test, "tpr", "fpr")
      auc_test=performance(pred_test,measure="auc")
    }
    
    else if(input$meThode == 'Decision Trees'){
      #mycontrol = rpart.control(cp = 0, xval = 10)
      #L = rpart(DOCTOR~ .,method = "class" ,control = mycontrol, data=data_train)
      
      prob_train <- predict(L,newdata=data_train)[,2]
      pred_train = prediction( prob_train, data_train$DOCTOR)
      perf_roc_train = performance(pred_train, "tpr", "fpr")
      auc_train = performance(pred_train,measure="auc")
      
      prob_test <- predict(L, newdata=data_test)[,2]
      pred_test = prediction(prob_test, data_test$DOCTOR)
      perf_roc_test <- performance(pred_test, "tpr", "fpr")
      auc_test=performance(pred_test,measure="auc")
    }
    
    else{

     
      #L = nnet(DOCTOR~ .,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
      
      #Logit<- nnet(data_train$DOCTOR~.,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
      
      prob_train <- predict(L,newdata=data_train)
      pred_train = prediction( prob_train, data_train$DOCTOR)
      perf_roc_train = performance(pred_train, "tpr", "fpr")
      auc_train = performance(pred_train,measure="auc")
      
      prob_test <- predict(L, newdata=data_test)
      pred_test = prediction(prob_test, data_test$DOCTOR)
      perf_roc_test <- performance(pred_test, "tpr", "fpr")
      auc_test=performance(pred_test,measure="auc")
    }
    

    
    precision=abs(auc_test@y.values[[1]]/auc_train@y.values[[1]])
    
    plot(perf_roc_train,col="red",print.cutoffs.at=seq(0,1,by=0.25),lwd=2)
    abline(0,1,lty=3)
    par(new=TRUE)
    plot(perf_roc_test,col="blue",print.cutoffs.at=seq(0,1,by=0.25),lwd=2)
    abline(0,1,lty=3)
    #text(0.9,0.6,paste("Precision = ",round(precision,digits = 3)),col ="black")
    text(x=0.9,y=0.45,paste("auc(train)=",round(auc_train@y.values[[1]],digits = 3)),col ="red")
    text(x=0.9,y=0.3,paste("auc(test)=",round(auc_test@y.values[[1]],digits = 3)),col ="blue")
    
  })
  
  output$liftPlot3.1<-renderPlot({
    if(input$includeClasses){
      data1=ndata()}
    else{data1=data}
    
    id = unlist(input$slider)
    train_id = sample(dim(data1)[1],id)
    data_train=data1[train_id,]
    data_train=SMOTE(DOCTOR~.,data_train)
    data_test=data1[-train_id,]
    #form=ModelLogReg()
    L=regreccion()
    
    if(input$meThode == 'Logistic Regression'){
      prob_train <- predict(L,newdata=data_train)
      pred_train = prediction( prob_train, data_train$DOCTOR)
      
      prob_test <- predict(L, newdata=data_test)
      pred_test = prediction(prob_test, data_test$DOCTOR)
   
    }
    
    else if(input$meThode == 'Decision Trees'){
      prob_train <- predict(L,newdata=data_train)[,2]
      pred_train = prediction( prob_train, data_train$DOCTOR)
      
      prob_test <- predict(L, newdata=data_test)[,2]
      pred_test = prediction(prob_test, data_test$DOCTOR)
    }
    
    else{
      
      prob_train <- predict(L,newdata=data_train)
      pred_train = prediction( prob_train, data_train$DOCTOR)
       
      prob_test <- predict(L, newdata=data_test)
      pred_test = prediction(prob_test, data_test$DOCTOR)
     
    }
   
    
    perf_lift1_train <- performance(pred_train, measure = "lift", x.measure = "rpp") #courbe de Lift
    perf_lift2_train <- performance(pred_train, measure = "tpr", x.measure = "rpp") #courbe de Lift
    
    perf_lift1_test <- performance(pred_test, measure = "lift", x.measure = "rpp") #courbe de Lift
    perf_lift2_test <- performance(pred_test, measure = "tpr", x.measure = "rpp") #courbe de Lift

    #mettre les deux de Lift ensemble
    layout(matrix(1:2,ncol = 2))
    plot(perf_lift1_train, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilité (tpr)",
         bg="white",cex.main=1,cex.lab=1) 
    
    lines(perf_lift1_test@x.values[[1]],perf_lift1_test@y.values[[1]],col="red") 
    text(1,.25,labels="__ train",adj=1,col = "blue")
    text(1,.15,labels="__ test",adj=1,col = "red")
    #legend(x=1,y=1,c("Train","Test"),col=c('blue','red'))
    
    plot(perf_lift2_train, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilité (tpr)",
         bg="white",cex.main=1,cex.lab=1) 
    
    lines(perf_lift2_test@x.values[[1]],perf_lift2_test@y.values[[1]],col="red") 
    text(1,.25,labels="__ train",adj=1,col = "blue")
    text(1,.15,labels="__ test",adj=1,col = "red")
  })
  
 
#-------------------------*****************************************
  output$rocPlot3DOCVIS=renderPlot({
    form=ModelLogRegDOCVIS()
    if(input$includeClassesDOCVIS){data1=ndata()}
    else{data1=data}
    
    id = unlist(input$sliderDOCVIS)
    train_id = sample(dim(data1)[1],id)
    train_data = data1[train_id,]
    test_data = data1[-train_id,]

    
      LogitNB = glm.nb(formula(form),data = train_data)
      #LogitNB=step(LogitNB,direction = 'both',trace = 0)
    NBp=predict(LogitNB,test_data)
      
      LogitRP = rpart(formula(form),data = train_data,method = "poisson")
    RPp=predict(LogitRP,test_data)
    
    plot(test_data$DOCVIS,pch='o',col='blue',xlim=c(0,900),ylim=c(-1,50))
    points(NBp,pch='p',col='red')
    points(RPp,pch='r',col='yellow')
    legend(810,40,c("Obs","Pred PR","Pred RF"),pch = c('o','p','r'),
           col = c('blue','red','yellow'))
  })
  
  Chose3=reactive({
    
    set.seed(1234) # permet de simuler toujours les mêmes comptages.
    theoretic_count <-rpois(248,mean(data$DOCVIS))
    
    # on incorpore ces comptages théoriques dans un data frame
    tc_df <-data.frame(theoretic_count)
  
    # on plot simultanémaent les comptages observés et les comptages théoriques
    g=ggplot(data,aes(DOCVIS))+
      geom_bar(fill="#1E90FF")+
      geom_bar(data=tc_df, aes(theoretic_count,fill="#1E90FF", alpha=0.5))+
      theme_classic()+
      theme(legend.position="none")
    return(g)
  })
  
  output$chose1=renderPrint({
    set.seed(1234) # permet de simuler toujours les mêmes comptages.
    theoretic_count <-rpois(248,mean(data$DOCVIS))
    
    # on incorpore ces comptages théoriques dans un data frame
    tc_df <-data.frame(theoretic_count)
    mean(data$DOCVIS)
  } 
  )
  output$chose3=renderPlot(plot(Chose3()))
  
#---------------------------------------------------------------------------------------------------------      
  
  
  model_performance_metric = reactive({
    
    if(input$includeClasses){
      data1=ndata()}
    else{data1=data}
    
    id = unlist(input$slider)
    train_id = sample(dim(data1)[1],id)
    data_train=data1[train_id,]
    data_train=SMOTE(DOCTOR~.,data_train)
    data_test=data1[-train_id,]
    form=as.formula(ModelLogReg())
    
    logit=step(glm(form,data = data_train,family = binomial),direction = 'both',trace = 0)
    prob_test=predict(logit,data_test,type = "response")
    pred_test=prediction(prob_test,data_test$DOCTOR)
    perf_roc_test=performance(pred_test,measure = "tpr",x.measure ="fpr")
    
    mycontrol = rpart.control(cp = 0, xval = 10)
    model_ar = rpart(form,method = "class" ,control = mycontrol, data=data_train)
    prob_test_ar=predict(model_ar,data_test,type = "prob")[,2]
    pred_test_ar=prediction(prob_test_ar,data_test$DOCTOR)
    perf_roc_test_ar=performance(pred_test_ar,measure = "tpr",x.measure ="fpr")
    
    Neural = nnet(form,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
    fitNeural = predict(Neural,newdata=data_test)
    prednn = prediction( fitNeural, data_test$DOCTOR)
    perfnn = performance(prednn, "tpr", "fpr")
    ######################################################
    
    
    m1_AUROC = round(performance(pred_test, measure = "auc")@y.values[[1]]*100, 2)
    m1_KS = round(max(attr(perf_roc_test,'y.values')[[1]]-attr(perf_roc_test,'x.values')[[1]])*100, 2)
    m1_Gini = (2*m1_AUROC - 100)
    
    m2_AUROC <- round(performance(pred_test_ar, measure = "auc")@y.values[[1]]*100, 2)
    m2_KS <- round(max(attr(perf_roc_test_ar,'y.values')[[1]]-attr(perf_roc_test_ar,'x.values')[[1]])*100, 2)
    m2_Gini <- (2*m2_AUROC - 100)
    
    m3_AUROC <- round(performance(prednn, measure = "auc")@y.values[[1]]*100, 2)
    m3_KS <- round(max(attr(perfnn,'y.values')[[1]] - attr(perfnn,'x.values')[[1]])*100, 2)
    m3_Gini <- (2*m3_AUROC - 100)
    
    models = c('Logistic Regression', 'Decision Trees','Neural Network')
    models_AUC <- c(m1_AUROC, m2_AUROC, m3_AUROC)
    models_KS = c(m1_KS, m2_KS, m3_KS)
    models_Gini <- c(m1_Gini, m2_Gini, m3_Gini)
    model_performance_metric <- as.data.frame(cbind(models, models_AUC, models_KS, models_Gini))
    colnames(model_performance_metric) = c("Modele", "AUC", "KS", "Gini")
    model_performance_metric
  })
  
  
  output$tabResrComparaison = renderTable({
    #if(input$includeClasses){return(model_performance_metric_ndata())}
    model_performance_metric()
  })
  
  #
  output$cumRocPlotTest = renderPlot({
    if(input$includeClasses){
      data1=ndata()}
    else{data1=data}
    
      id = unlist(input$slider)
      train_id = sample(dim(data1)[1],id)
      #train_id=sample(dim(data)[1],round(dim(data)[1]*0.75))
      data_train=data1[train_id,]
      data_train=SMOTE(DOCTOR~.,data_train)
      data_test=data1[-train_id,]
      form=as.formula(ModelLogReg())
      
      logit=step(glm(form,data = data_train,family = binomial),direction = 'both',trace = 0)
      prob_test=predict(logit,data_test,type = "response")
      pred_test=prediction(prob_test,data_test$DOCTOR)
      perf_roc_test=performance(pred_test,measure = "tpr",x.measure ="fpr")
      
      mycontrol = rpart.control(cp = 0, xval = 10)
      model_ar = rpart(form,method = "class" ,control = mycontrol, data=data_train)
      prob_test_ar=predict(model_ar,data_test,type = "prob")[,2]
      pred_test_ar=prediction(prob_test_ar,data_test$DOCTOR)
      perf_roc_test_ar=performance(pred_test_ar,measure = "tpr",x.measure ="fpr")
      
      Neural = nnet(form,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
      fitNeural = predict(Neural,newdata=data_test)
      prednn = prediction( fitNeural, data_test$DOCTOR)
      perfnn = performance(prednn, "tpr", "fpr")
      ######################################################
      
    plot(perf_roc_test,col='purple', lty=1,lwd=2, main='ROCs: Performance du Modele: Comparaison') 
    plot(perf_roc_test_ar, col='yellow',lty=2,lwd=2, add=TRUE); 
    plot(perfnn, col='maroon',lty=3,lwd=2, add=TRUE); 
    legend(0.6,0.5,
           c('Losgistic Regression','Decision Trees', 
             'Neural Network'),
           col=c('purple','yellow', 'maroon'),
           lwd=3)
  })
  
  nVoClient<-reactive({
    d=data.frame(
      FEMALE=as.factor(input$female),
      AGE=input$age,
      HHNINC=input$hhninc,
      HHKIDS=as.factor(input$hhkids),
      EDUC=input$educ,
      WORKING=as.factor(input$working),
      #DOCVIS=input$docvis,
      HOSPVIS=input$hospvis,
      Health_Satisfaction_Index=input$hsi
      
    )
    return(d)
  })
  
  output$tableNvoClient<-renderDataTable({
    nVoClient()
  })
  
  output$tableNvoClientDOCVIS<-renderDataTable({
    nVoClient()
  })
  
  
  output$textNvoClient = renderTable({
    Predire()
  })
  
  output$textNvoClientDOCVIS = renderTable({
    PredireDOCVIS()
  })
  
  Performance=reactive({
    model_performance_metric()
  })
  
  #'Logistic Regression','Decision Trees','Neural Network'
  #'"AUC", "KS", "Gini"
  output$LR <- renderValueBox({
    valueBox(
      paste("AUC: ",Performance()[1,2],'%'), paste("Logistic Regression"), icon = icon("cc-discover"),
      color = "purple"
    )
  })
  
  #red yellow aqua blue light-blue green navy teal olive lime orange fuchsia purple maroon black
  
  output$DT <- renderValueBox({
    valueBox(
      paste("AUC: ",Performance()[2,2],'%') #,paste0('model_performance_metric')
      ,paste("Decision Trees"), icon = icon("cc-discover"),
      color = "yellow"
    )
  })
  
  output$NN <- renderValueBox({
    valueBox(
      paste0("AUC: ",Performance()[3,2],'%')
      ,"Neural Network", icon = icon("cc-discover"),
      
      color = "maroon"
    )
  })
  
  
  
  output$corr1=renderPlot({
    pairs.panels(dataQuanti, 
                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE  # show density plots
                 ,ellipses = F # show correlation ellipses
    )})
#--------------   Creation de Classes   --------------------------------------------------
  ndata=reactive({
    data1=data
    clasAGE = as.numeric(unlist(strsplit(input$clasAGE,",")))
    if(sum(clasAGE)!=0){
      data1=formClasseEtSupColonne(data1, 'AGE',as.vector(clasAGE))
    }
    clasHHNINC = as.numeric(unlist(strsplit(input$clasHHNINC,",")))
    if(sum(clasHHNINC)!=0){
      data1=formClasseEtSupColonne(data1, 'HHNINC',as.vector(clasHHNINC))
    }
    clasEDUC = as.numeric(unlist(strsplit(input$clasEDUC,",")))
    if(sum(clasEDUC)!=0){
      data1=formClasseEtSupColonne(data1, 'EDUC',as.vector(clasEDUC))
    }
    clasHOSPVIS = as.numeric(unlist(strsplit(input$clasHOSPVIS,",")))
    if(sum(clasHOSPVIS)!=0){
      data1=formClasseEtSupColonne(data1, 'HOSPVIS',as.vector(clasHOSPVIS))
    }
    clasHSI = as.numeric(unlist(strsplit(input$clasHSI,",")))
    if(sum(clasHSI)!=0){
      data1=formClasseEtSupColonne(data1, 'Health_Satisfaction_Index',as.vector(clasHSI))
    }
    
    data1=data.frame(data1)
    return(data1)
  })
  
  output$t1=renderPrint({
    str(ndata())
    print('           ')
    summary(ndata())
  })
  
  output$Q1=renderDataTable({
    if(input$includeClasses){return(NouveauClientD())}
    ndata()
  })
  
  
  output$PredireNvo.LR <- renderValueBox({
    valueBox(
      paste(round(Predire()$ALR)), paste("By Logistic Regression"), icon = icon("cc-discover"),
      color = "purple"
    )
  })
  
  output$PredireNvo.DT <- renderValueBox({
    valueBox(
      paste(round(Predire()$ADT)), paste("By Decision Trees"), icon = icon("cc-discover"),
      color = "yellow"
    )
  })
  
  output$PredireNvo.NN <- renderValueBox({
    valueBox(
      paste(round(Predire()$ANN)), paste("By Neural Network"), icon = icon("cc-discover"),
      color = "maroon"
    )
  })
  
  
  NouveauClientD=reactive({
    data1=nVoClient()
    clasAGE = as.numeric(unlist(strsplit(input$clasAGE,",")))
    if(sum(clasAGE)!=0){
      data1=formClasseEtSupColonne(data1, 'AGE',as.vector(clasAGE))
    }
    clasHHNINC = as.numeric(unlist(strsplit(input$clasHHNINC,",")))
    if(sum(clasHHNINC)!=0){
      data1=formClasseEtSupColonne(data1, 'HHNINC',as.vector(clasHHNINC))
    }
    clasEDUC = as.numeric(unlist(strsplit(input$clasEDUC,",")))
    if(sum(clasEDUC)!=0){
      data1=formClasseEtSupColonne(data1, 'EDUC',as.vector(clasEDUC))
    }
    clasHOSPVIS = as.numeric(unlist(strsplit(input$clasHOSPVIS,",")))
    if(sum(clasHOSPVIS)!=0){
      data1=formClasseEtSupColonne(data1, 'HOSPVIS',as.vector(clasHOSPVIS))
    }
    clasHSI = as.numeric(unlist(strsplit(input$clasHSI,",")))
    if(sum(clasHSI)!=0){
      data1=formClasseEtSupColonne(data1, 'Health_Satisfaction_Index',as.vector(clasHSI))
    }
    
    data1=data.frame(data1)
    return(data1)
  })
  
  
  
  
  
  
  Predire=reactive({
    
    if(input$includeClasses){
      data1=ndata()
      NouvelD=NouveauClientD()
      }
    else{
      data1=data
      NouvelD=nVoClient()}
    
    id = unlist(input$slider)
    train_id = sample(dim(data1)[1],id)
    form=as.formula(ModelLogReg())
    data_train=data1[train_id,]
    data_train=SMOTE(DOCTOR~.,data_train)
    data_test=data1[-train_id,]
    
    logit=step(glm(form,data = data_train,family = binomial),direction = 'both',trace = 0)
    prob_test=predict(logit,NouvelD,type = "response")
    
    mycontrol = rpart.control(cp = 0, xval = 10)
    model_ar = rpart(form,method = "class" ,control = mycontrol, data=data_train)
    prob_test_ar=predict(model_ar,NouvelD,type = "prob")[,2]
    
    Neural = nnet(form,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
    fitNeural = predict(Neural,newdata=NouvelD)
    
    d=data.frame(ALR=as.vector(prob_test),ADT=as.vector(prob_test_ar),ANN=as.vector(fitNeural))
    return(d)
  })
  
  PredireDOCVIS=reactive({
    
    if(input$includeClassesDOCVIS){
      data1=ndata()
      NouvelD=NouveauClientD()
    }
    else{
      data1=data
      NouvelD=nVoClient()}
    
    id = unlist(input$sliderDOCVIS)
    train_id = sample(dim(data1)[1],id)
    form=as.formula(ModelLogRegDOCVIS())
    data_train=data1[train_id,]
    data_test=data1[-train_id,]
    
    nb.mod=step(glm.nb(form,data = data_train),direction = 'both',trace = 0)
    p_test=predict(nb.mod,NouvelD,type = "response")
    
    rf.mod = rpart(form,method = "poisson" ,data=data_train)
    test_ar=predict(rf.mod,NouvelD)
    
    d=data.frame(PM=as.vector(p_test),DT=as.vector(test_ar))
    d
    return(d)
  })
  
  output$PredireNvoDOCVIS.PM <- renderValueBox({
    valueBox(
      paste(round(PredireDOCVIS()$PM)), paste("By Neg Binomial Regression"), icon = icon("cc-discover"),
      color = "purple"
    )
  })
  
  output$PredireNvoDOCVIS.DT <- renderValueBox({
    valueBox(
      paste(round(PredireDOCVIS()$DT)), paste("By Decision Trees"), icon = icon("cc-discover"),
      color = "maroon"
    )
  })
  
  
  }