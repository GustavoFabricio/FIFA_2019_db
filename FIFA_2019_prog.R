
library(dplyr)
library(lubridate)
library(stats)
library(ggplot2)
library(lattice)

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
                                                   grepl("K", FIFA_2019$Release.Clause) == TRUE ~ Release.Clause.Num/1000,
                                                   TRUE ~ 0)) %>% 
             mutate(Value.Num = gsub("€", "", Value)) %>% 
             mutate(Value.Num = gsub("M", "", Value.Num)) %>%
             mutate(Value.Num = as.numeric(gsub("K", "", Value.Num))) %>% 
             mutate(Value.Num = case_when(grepl("M", FIFA_2019$Value) == TRUE ~ Value.Num,
                                          grepl("K", FIFA_2019$Value) == TRUE ~ Value.Num/1000,
                                          TRUE ~ 0)) %>%  
             mutate(Wage.Num = gsub("€", "", Wage)) %>% 
             mutate(Wage.Num = gsub("M", "", Wage.Num)) %>%
             mutate(Wage.Num = as.numeric(gsub("K", "", Wage.Num))) %>% 
             mutate(Wage.Num = case_when(grepl("M", FIFA_2019$Wage) == TRUE ~ Wage.Num,
                                         grepl("K", FIFA_2019$Wage) == TRUE ~ Wage.Num/1000,
                                         TRUE ~ 0)) 


#Drop Columns
FIFA_2019 <- select (FIFA_2019,-c(Photo, Flag, Club.Logo, LS,ST,RS,LW,LF,CF,RF,RW,LAM,CAM,RAM,LM,LCM,CM,RCM,RM,LWB,LDM,CDM,RDM,RWB,LB,LCB,CB,RCB,RB))

names(FIFA_2019)


unique(FIFA_2019$Position)

x <- FIFA_2019 %>% group_by(Position, Field.Pos) %>% summarise(n = n())

x<- FIFA_2019 %>% filter(is.na(Value.Num))


FIFA_2019$Field.Pos <- case_when(FIFA_2019$Position %in% c("GK") ~ "Golkeper",
                                 FIFA_2019$Position %in% c("CB","RCB","LCB") ~ "Center Back",
                                 FIFA_2019$Position %in% c("RB", "RWB", "LB", "LWB") ~ "Side Back",
                                 FIFA_2019$Position %in% c("CDM","RDM","LDM") ~ "Defensive Midfielder",
                                 FIFA_2019$Position %in% c("CM", "CAM", "LM", "RM", "LCM", "RCM") ~ "Midfieder",
                                 FIFA_2019$Position %in% c("RW","LW","LAM","RAM") ~ "Winger",
                                 FIFA_2019$Position %in% c("CF", "RF", "LF", "ST","LS", "RS") ~ "Striker",
                                 TRUE ~ "")
                                 

summary(FIFA_2019)

positions <- c(4,6,26:59, 61:63)
x<- FIFA_2019 %>% select(positions)
x<- x %>%  filter(is.na(SlidingTackle)==FALSE)

pairs(FIFA_2019[4,6,61:63], main = "test",
      pch = 21, bg = c("red", "green3", "blue")[FIFA_2019$Field.Pos])


splom(FIFA_2019[c(6,62)], data=FIFA_2019)

