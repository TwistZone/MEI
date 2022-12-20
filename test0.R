options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library(reshape)
library(dplyr)
library(pracma)
library(plotly)

mydir = "C:/Users/danie/OneDrive/Ambiente de Trabalho/mei-assignment/mei-assignment/code/"

lm_eqn <- function(x,y,deg,alg){
  # y ~ poly(x,deg)
  m <- lm(y ~ poly(x,deg))
  print(summary(m))
  eq <- substitute(a ,
                   list(a = format(unname(coef(m)[1]), digits = 2)))
  eq1 <- substitute(b ,
                    list(b = format(unname(coef(m)[2]), digits = 2)))
  
  
  eq3 <- substitute(r2 ,
                    list(r2 = format(summary(m)$r.squared, digits = 3)))
  if(deg == 2){
    eq2 <- substitute(b ,
                        list(b = format(unname(coef(m)[3]), digits = 2)))
    paste(alg,' y=',eq,"+ ",eq1,'x + ',eq2,'x² '," r²=",eq3)
    }
  else if(deg == 3)  {
      eq2 <- substitute(b ,
                      list(b = format(unname(coef(m)[3]), digits = 2)))
      eq4 <- substitute(b ,
                        list(b = format(unname(coef(m)[4]), digits = 2)))
      paste(alg,' y=',eq,"+ ",eq1,'x + ',eq2,'x² +',eq4,'x³ '," r²=",eq3)
  }
  else{
    eq2 <- substitute(b ,
                      list(b = format(unname(coef(m)[3]), digits = 2)))
    eq4 <- substitute(b ,
                      list(b = format(unname(coef(m)[4]), digits = 2)))
    eq5 <- substitute(b ,
                      list(b = format(unname(coef(m)[5]), digits = 2)))
    eq6 <- substitute(b ,
                      list(b = format(unname(coef(m)[6]), digits = 2)))
    paste(alg,' y=',eq,"+ ",eq1,'x + ',eq2,'x² +',eq4,'x³ +',eq5,'x⁴ + ',eq6,'x⁵'," r²=",eq3)
  }

}




scPlot <- function(test,p){  
 
  tlim = 20
  mydir = paste(mydir,test,"/results.txt",sep = "")
  data <- read.table(mydir,header = TRUE)
  df <- subset(data, probability == p) 
  v = max(df$vertexes)
  xpos = v - v/4
  df <- df %>%  
  mutate(ve = vertexes)
  df <- df %>%  
  mutate(vm = vertexes)

  df <- df %>%  
    mutate(ee = Arcs)
  df <- df %>%  
    mutate(em = Arcs)

  tt = paste("Probability ",p*100,"%",sep="")
  ggplot(df) + 
  geom_jitter(aes(df$vertexes,df$Dinic),colour="#56B4E9")+ geom_smooth(aes(df$vertexes,df$Dinic),colour="#56B4E9",method='lm', formula= y ~ poly(x, 2, raw=TRUE),se = FALSE) +
  geom_jitter(aes(df$ve,df$EK),colour = "#FF33CC")+ geom_smooth(aes(df$ve,df$EK),colour="#FF33CC",method='lm', formula= y ~ poly(x, 5, raw=TRUE),se = FALSE) +
  geom_jitter(aes(df$vm,df$MPM),colour="#0033FF")+ geom_smooth(aes(df$vm,df$MPM),colour="#0033FF",method='lm', formula= y ~ poly(x, 3, raw=TRUE),se = FALSE) +
  geom_text(x = xpos,y = 17.5,colour = "#56B4E9" ,label = lm_eqn(df$vertexes,df$Dinic,2,"Dinic:"))+ 
  geom_text(x = xpos,y = 15,colour = "#FF33CC" ,label = lm_eqn(df$ve,df$EK,5,"EK:")) +  
  geom_text(x = xpos,y = 12.5,colour = "#0033FF" ,label = lm_eqn(df$vm,df$MPM,3,"MPM")) +
  geom_text(aes(xpos, tlim-2, label = "Max CPU Time = 10", vjust = - 1),col = "black") +
  geom_hline(aes(yintercept=tlim/2)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen")) +
  labs(title = tt,x = "Vertexes", y = "Time(seconds)")
  
}

scPlot("test0",0.9)









arestasverticesPlot <- function(test,p){  
  mydir = paste(mydir,test,"/results.txt",sep = "")
  data <- read.table(mydir,header = TRUE)
  df <- subset(data, probability == p) 
  v = max(df$vertexes)
  xpos = v - v/6
  
  df$EK[df$EK > 60 ] <- NA
  df$ve[df$EK > 60 ] <- NA
  df$Dinic[df$Dinic > 60 ] <- NA
  df$vertexes[df$Dinic > 60 ] <- NA
  df$MPM[df$MPM > 60 ] <- NA
  df$vm[df$MPM > 60 ] <- NA
  
  tt = paste("Probability ",p*100,"%",sep="")
  ggplot(df) + 
    geom_jitter(aes(vertexes,Arcs),colour="#56B4E9")+
    theme_bw()+
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 20, face = "bold", color = "darkgreen")) +
    labs(title = tt,x = "Vertexes", y = "Arcs")
  
}

arestasverticesPlot("test0",0.9)

tresDPlot <- function(test){  
  mydir = paste(mydir,test,"/results.txt",sep = "")
  data <- read.table(mydir,header = TRUE)
  df <- subset(data) 
  df<-melt(df, id = c("Arcs","vertexes","probability", "capacity", "seed"))
  x<-df$value
  y<-df$vertexes
  z<-df$Arcs
  
  axx <- list(
    title = "Temp"
  )
  
  axy <- list(
    title = "Vertexes"
  )
  
  axz <- list(
    title = "Arcs"
  )
  fig <- plot_ly(data=df,x=~x,y=~y,z=~z, type="scatter3d", mode="markers", color=df$variable)
  fig <- fig %>% layout(title = "Comparison of the 3 Algorithms",scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
  
  fig
}
tresDPlot("test0")
 


