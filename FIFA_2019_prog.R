install.packages('ggplot2')
library(dplyr)
library(lubridate)
library(stats)
library(ggplot2)

setwd('D:/01_Projetos/03_FIFA_2019/FIFA_2019_db')

FIFA_2019 <- read.csv(file = 'data.csv',header = TRUE,sep = ',',dec = '.', encoding = 'UTF-8',stringsAsFactors=FALSE)


#checking Data
str(FIFA_2019)
summary(FIFA_2019)
dim(FIFA_2019)



  
# Creating Value Columns as Nmeric  
FIFA_2019 <- FIFA_2019 %>% 
             mutate(Release.Clause.Num = gsub("€", "", Release.Clause)) %>% 
             mutate(Release.Clause.Num = gsub("M", "", Release.Clause.Num)) %>%
             mutate(Release.Clause.Num = as.numeric(gsub("K", "", Release.Clause.Num))) %>% 
             mutate(Release.Clause.Num = case_when(grepl("M", FIFA_2019$Release.Clause) == TRUE ~ Release.Clause.Num,
                                                   grepl("K", FIFA_2019$Release.Clause) == TRUE ~ Release.Clause.Num/1000)) %>% 
             mutate(Value.Num = gsub("€", "", Value)) %>% 
             mutate(Value.Num = gsub("M", "", Value.Num)) %>%
             mutate(Value.Num = as.numeric(gsub("K", "", Value.Num))) %>% 
             mutate(Value.Num = case_when(grepl("M", FIFA_2019$Value) == TRUE ~ Value.Num,
                                          grepl("K", FIFA_2019$Value) == TRUE ~ Value.Num/1000)) %>% 
             mutate(Wage.Num = gsub("€", "", Wage)) %>% 
             mutate(Wage.Num = gsub("M", "", Wage.Num)) %>%
             mutate(Wage.Num = as.numeric(gsub("K", "", Wage.Num))) %>% 
             mutate(Wage.Num = case_when(grepl("M", FIFA_2019$Wage) == TRUE ~ Wage.Num,
                                         grepl("K", FIFA_2019$Wage) == TRUE ~ Wage.Num/1000)) 


#Drop Columns
FIFA_2019 <- select (FIFA_2019,-c(Photo, Flag, Club.Logo, LS,ST,RS,LW,LF,CF,RF,RW,LAM,CAM,RAM,LM,LCM,CM,RCM,RM,LWB,LDM,CDM,RDM,RWB,LB,LCB,CB,RCB,RB))


