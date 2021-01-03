library(dplyr)
library(plotly)

#


df<-function(data,classe,colonne)
{
  df=NULL
  variab=data[,colonne]
  if(length(classe)<=1){
    df=cbind(df,ifelse(variab<classe,1,0))
    df=cbind(df,ifelse(variab>=classe,1,0))
    df=data.frame(df)
    colnames(df)=paste(c('inf','sup'),colonne, classe,sep = '')
  }
  
  else{
    
    for (i in 1:(length(classe)-1)) {
      d=ifelse((variab<classe[i+1])&(variab>=classe[i]),1,0)
      df=cbind(df,d)
    }
    
    df=cbind(ifelse(variab<classe[1],1,0),df)
    df=cbind(df,if_else(variab>=classe[length(classe)],1,0))
    df=data.frame(df)
    
    noms=paste('inf',colonne,classe[1],sep = '')
    for (i in 2:(length(classe))) {
      noms=c(noms,paste(colonne,classe[i-1],classe[i],sep = '.'))
    }
    noms=c(noms,paste('sup',colonne, classe[length(classe)],sep = ''))
    colnames(df)=noms
  }
  return(df)
}


factor.tranform=function(d){
  for(i in 1:ncol(d)){
    d[,i]=as.factor(d[,i])
  }
  return(d)
}

formClasseEtSupColonne=function(data,colonne,classe){
  attach(data)
  variab=data[,colonne]
  d=df(data,classe,colonne)
  d=factor.tranform(d)
  data=data[,!colnames(data)==colonne]
  data=cbind(data,d)
  return(data)
}



PieGraph=function(data,colonne,modalite,titreGraph){
  
  variable=data[,colonne]
  #modalite=c("OUI","NON")
  d=data.frame(table(variable))
  if(nlevels(variable)==length(modalite)){
    d$Quali=ifelse(d$variable==1,modalite[1],modalite[2])
  }
  labels = d$Quali
  values = d$Freq
  fig <- plot_ly(type='pie', labels=labels, values=values, 
                 textinfo='label+percent',
                 insidetextorientation='radial')
  fig=fig %>% layout(title = titreGraph,showlegend = T)
  #fig <- fig %>% layout(title = titreGraph,xaxis = list(title = ""),yaxis = list(title = ""))
  return(fig)
}

BarGraph=function(data,colonne,modalite,titreGraph){
  variable=data[,colonne]
  d=data.frame(table(variable))
  if(nlevels(variable)==length(modalite)){
    d$Quali=ifelse(d$variable==1,modalite[1],modalite[2])
  }
  labels = d$Quali
  values = d$Freq
  fig <- plot_ly(data, x = ~labels, y = ~values, type = 'bar',
                 text = values, textposition = 'auto'
                 #,marker = list(color = 'rgb(158,202,225)',
                 #               line = list(color = 'rgb(8,48,107)', width = 1.5))
  )
  fig <- fig %>% layout(title = titreGraph,xaxis = list(title = ""),yaxis = list(title = ""))
  return(fig)
}


Scatter.Line.Graph=function(data,x,y,titreGraph){
  #assign(x,data[,x])
  #assign(y,data[,y])
  #fig <- plot_ly(data = data, x =~ x, y = ~y)
  
  x = formula(paste('~',x,sep = ''))
  y = formula(paste('~',y,sep = ''))
  fig <- plot_ly(data = data, x = x, y = y,type = 'scatter')
  
  fig <- fig %>% layout(title = titreGraph)
  return(fig)
}


Density.Graph=function(data,x,titreGraph){
  dens=density(data[,x])
  fig <- plot_ly(x =~dens$x,y =~dens$y, type = 'scatter', mode = 'lines',fill = 'tozeroy')
  fig <- fig %>% layout(title = titreGraph,
                        xaxis = list(title = x),yaxis = list(title = 'Density'))
  
  return(fig)
}



Density.Quanti.Quali.Graph=function(data,quali,quanti,modalite,titreGraph){
  
  d=data
  d$ajout=ifelse(data[,quali]==1,modalite[1],modalite[2])
  
  d1 <- d[which(d$ajout == modalite[1]),]
  density1 <- density(d1[,quanti])
  
  d2 <- d[which(d$ajout == modalite[2]),]
  density2 <- density(d2[,quanti])
  name1=paste(quanti,modalite[1])
  name2=paste(quanti,modalite[2])
  
  fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name =name1, fill = 'tozeroy')
  fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = name2, fill = 'tozeroy')
  fig <- fig %>% layout(title=titreGraph,xaxis = list(title = quali),
                        yaxis = list(title = 'Density'))
  return(fig)
}




Boxplot.Graph=function(data,y,titreGraph){
  
  y=formula(paste0('~',y))
  fig <- plot_ly(y =y, type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
  fig <- fig %>% layout(title=titreGraph)
  return(fig)
}



Boxplot.Quali.Quanti.Graph=function(data,quali,quanti,modalite,titreGraph){
  d=data
  d$ajout=ifelse(data[,quali]==1,modalite[1],modalite[2])
  #assign('ajout',d$ajout)
  attach(d)
  y=formula(paste0('~',quanti))
  clr=formula(paste0('~',substitute(ajout)))
  fig <- plot_ly(data, y = y, color = clr, type = "box")
  fig <- fig %>% layout(title=titreGraph)
  return(fig)
}



Histogramme.Graph=function(data,quanti,titreGraph){
  x=formula(paste0('~',quanti))
  fig <- plot_ly(x =x, type = "histogram")
  fig <- fig %>% layout(title=titreGraph)
  return(fig)
}


Histogramme.Quali.Quanti.Graph=function(data,quali,quanti,modalite,titreGraph){
  d=data
  d$ajout=ifelse(data[,quali]==1,modalite[1],modalite[2])
  attach(d)
  y=formula(paste0('~',quanti))
  clr=formula(paste0('~',substitute(ajout)))
  fig <- plot_ly(data, x = y, color = clr, type = "histogram")
  
  
  
  return(fig)
}



##---------------------------------------------------------------

# substitute : recupere le nom de la variable
# assign( "nom", valeur) : donne ........
# get("AGE"): retourne la variable AGE
# quantile inverse: ecdf(x)(quantile(x,.75)) tq les percentiles inverses

#-------------------------------------------------------------



