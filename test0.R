options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library(reshape)
library(dplyr)
library(pracma)

mydir = "C:/Users/danie/OneDrive/Ambiente de Trabalho/mei-assignment/mei-assignment/code/"

lm_eqn <- function(x,y,deg,alg){
  # y ~ poly(x,deg)
  # transformar
  
  m <- lm( nthroot(y,deg) ~ x)
  print(m)
  eq <- substitute(a ,
                   list(a = format(unname(coef(m)[1]), digits = 2)))
  eq1 <- substitute(b ,
                    list(b = format(unname(coef(m)[2]), digits = 2)))
  
  
  eq3 <- substitute(r2 ,
                    list(r2 = format(summary(m)$r.squared, digits = 3)))
  if(deg == 2){
    eq2 <- substitute(b ,
                        list(b = format(unname(coef(m)[3]), digits = 2)))
    paste(alg,' y=',eq,"+ ",eq1,'x + ',eq2,'x^2 '," r²=",eq3)
    }
  else if(deg == 3)  {
      eq2 <- substitute(b ,
                      list(b = format(unname(coef(m)[3]), digits = 2)))
      eq4 <- substitute(b ,
                        list(b = format(unname(coef(m)[4]), digits = 2)))
      paste(alg,' y=',eq,"+ ",eq1,'x + ',eq2,'x^2 +',eq4,'x^3 '," r²=",eq3)
  }
  else{
    paste(alg,' y=',eq,"+ ",eq1,'x ' , "r²= ",eq3)
  }
  
  paste(alg,'y=',eq,"+ ",eq1,"√x", " r²=", eq3)
  
  
}

cPlot <- function(test,p){  
  
  tlim = 60
  mydir = paste(mydir,test,"/results.txt",sep = "")
  data <- read.table(mydir,header = TRUE)
  df <- subset(data, probability == p) 
  v = max(df$capacity)
  xpos = v - v/6
  df <- df %>%  
    mutate(ve = capacity)
  df <- df %>%  
    mutate(vm = capacity)
  
  df$EK[df$EK > 60 ] <- NA
  df$ve[df$EK > 60 ] <- NA
  df$Dinic[df$Dinic > 60 ] <- NA
  df$capacity[df$Dinic > 60 ] <- NA
  df$MPM[df$MPM > 60 ] <- NA
  df$vm[df$MPM > 60 ] <- NA
  print(df)
  tt = paste("Probability ",p*100,"%",sep="")
  ggplot(df) + 
    geom_jitter(aes(df$capacity,df$Dinic),colour="#56B4E9")+ geom_smooth(aes(df$capacity,df$Dinic),colour="#56B4E9",method='lm', formula=  nthroot(y,2)~x,se = FALSE) +
    geom_jitter(aes(df$ve,df$EK),colour = "#FF33CC")+ geom_smooth(aes(df$ve,df$EK),colour="#FF33CC",method='lm', formula= y~poly(x,2,raw = TRUE),se = FALSE) +
    geom_jitter(aes(df$vm,df$MPM),colour="#0033FF")+ geom_smooth(aes(df$vm,df$MPM),colour="#0033FF",method='lm', formula= y~poly(x,1,raw = TRUE),se = FALSE) +
    geom_text(x = xpos,y = 30,colour = "#56B4E9" ,label = lm_eqn(df$capacity,df$Dinic,1,"Dinic:"))+ 
    geom_text(x = xpos,y = 40,colour = "#FF33CC" ,label = lm_eqn(df$ve,df$EK,2,"EK:")) +  
    geom_text(x = xpos,y = 50,colour = "#0033FF" ,label = lm_eqn(df$vm,df$MPM,1,"MPM:")) +
    geom_text(aes(xpos, tlim-5, label = "Max CPU Time = 60", vjust = - 1),col = "black") +
    geom_hline(aes(yintercept=tlim)) +
    theme_bw()+
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 20, face = "bold", color = "darkgreen")) +
    labs(title = tt,x = "capacity", y = "Time(seconds)")
  
}

cPlot("test0",0.9)


scPlot <- function(test,p){  
 
  tlim = 60
  mydir = paste(mydir,test,"/results.txt",sep = "")
  data <- read.table(mydir,header = TRUE)
  df <- subset(data, probability == p) 
  v = max(df$vertexes)
  xpos = v - v/6
  df <- df %>%  
  mutate(ve = vertexes)
  df <- df %>%  
  mutate(vm = vertexes)

  df$EK[df$EK > 60 ] <- NA
  df$ve[df$EK > 60 ] <- NA
  df$Dinic[df$Dinic > 60 ] <- NA
  df$vertexes[df$Dinic > 60 ] <- NA
  df$MPM[df$MPM > 60 ] <- NA
  df$vm[df$MPM > 60 ] <- NA

  tt = paste("Probability ",p*100,"%",sep="")
  ggplot(df) + 
  geom_jitter(aes(df$vertexes,df$Dinic),colour="#56B4E9")+ geom_smooth(aes(df$vertexes,df$Dinic),colour="#56B4E9",method='lm', formula= nthroot(y,2) ~ x,se = FALSE) +
  geom_jitter(aes(df$ve,df$EK),colour = "#FF33CC")+ geom_smooth(aes(df$ve,df$EK),colour="#FF33CC",method='lm', formula= nthroot(y,2) ~ x,se = FALSE) +
  geom_jitter(aes(df$vm,df$MPM),colour="#0033FF")+ geom_smooth(aes(df$vm,df$MPM),colour="#0033FF",method='lm', formula= nthroot(y,3) ~ x,se = FALSE) +
  geom_text(x = xpos,y = 30,colour = "#56B4E9" ,label = lm_eqn(df$vertexes,df$Dinic,2,"Dinic:"))+ 
  geom_text(x = xpos,y = 40,colour = "#FF33CC" ,label = lm_eqn(df$ve,df$EK,2,"EK:")) +  
  geom_text(x = xpos,y = 50,colour = "#0033FF" ,label = lm_eqn(df$vm,df$MPM,3,"MPM:")) +
  geom_text(aes(xpos, tlim-5, label = "Max CPU Time = 60", vjust = - 1),col = "black") +
  geom_hline(aes(yintercept=tlim)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen")) +
  labs(title = tt,x = "Vertexes", y = "Time(seconds)")
  
}

scPlot("test0",0.9)












# Outliers sao mantidos e explicados (a nivel de desempenho computacional)


#Still not ready
bxPlot <- function (test,p){
  mydir = paste(mydir,test,"/results.txt",sep = "")
  data <- read.table(mydir,header = TRUE)
  newdata <- subset(data, probability == p )
  
  newdata <- melt(newdata,id=c("vertexes","probability","capacity","seed"))
  c1data <- subset(newdata,variable=="Dinic")
  c2data <- subset(newdata,variable=="MPM")
  c3data <- subset(newdata,variable=="EK")
  myplot <- ggplot(data = newdata, aes(x=vertexes, y= value,shape = variable,color = variable)) 
  
  
  tt = paste("Probability ",p*100,"%",sep="")
  myplot + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)  + labs(title = tt,x = "Vertexes", y = "Time(s)") + geom_text(aes(5, 50, label = "Max CPU Time = 60", vjust = - 1),col = "black") +
    scale_color_manual(labels = c("Dinic", "MPM","EK"),  values = c("blue", "red","green"))+
    scale_shape_manual(values = c(16, 17, 18)) + scale_y_continuous(trans = 'log') +  theme_bw()}
bxPlot("test0",0.5)


 
