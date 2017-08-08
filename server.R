#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


#source("/home/carlos/Downloads/vacine/vacine.r")
#source("vacine.r")

library(stringr)
library(reshape2)
library(ggplot2)
library(dplyr)
vacine<-read.csv("vacine.csv")

vacineClear<-vacine[,c(1:13)]

head(vacineClear)

vacineClear$Outbreak<- gsub(".*[Ff]lu*","Flu",vacineClear$Outbreak)
vacineClear$Outbreak<-as.factor(vacineClear$Outbreak)

tmp_date<-str_split(vacineClear$Date,"-")

vacineClear$Country<-str_trim(vacineClear$Location.1.1)
extractDate<-function(x){
  return(x[1])
}

tmp_date2<-sapply(tmp_date,extractDate)

tmp_date2[grep("\\.",tmp_date2)]<-as.numeric(tmp_date2[grep("\\.",tmp_date2)])
tmp_date2
# imputacao de data
tmp_date2[grep("/",tmp_date2,invert=TRUE)]<-paste0("1/",tmp_date2[grep("/",tmp_date2,invert=TRUE)])

vacineClear$Date<-as.Date(paste0("1/",tmp_date2), format="%d/%m/%Y")

vacineClear$Year<-as.numeric(format(vacineClear$Date,"%Y"))

head(vacineClear)

vacine_melt<-melt(vacineClear,id.vars = c("Outbreak",
                                          "Year",
                                          "Date",
                                          "Country",
                                          "Continent")
                  , measure.vars = c("Cases","Fatalities"))

vacine_tbl<-tbl_df(vacineClear) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$Cases<-renderTable({
    vacine_tbl %>%
      group_by(Outbreak) %>%
      summarise(Cases=sum(Cases))
  })
  
  output$Fatal<-renderTable({
    vacine_tbl %>%
      group_by(Outbreak) %>%
      summarise(Fatalities=sum(Fatalities)) 
  })
  output$gt<-renderText({getwd()})
  
})
