#main<-read.csv("\\\\ai-fas3\\lcid_es\\CERNER data\\Commonwealth Cerner Placeholders\\placeholder_main.csv", header=TRUE)
####load libraries####
library(sas7bdat)
library(sqldf)
library(dplyr)
library(tidyverse)
library(tidyr)
library(Hmisc)
library(cluster)
library(taxize)
library(myTAI)

####read-in suscep data####
suscep<-read.sas7bdat('\\\\ai-fas3\\lcid_es\\CERNER data\\Commonwealth Cerner Placeholders\\placeholder_sus.sas7bdat')
#str(suscep)
#sofa<-read.csv("\\\\ai-fas3\\lcid_es\\CERNER data\\Commonwealth Cerner Placeholders\\placeholder_daily_sofa.csv")

####tidy up interpretation variable####
suscep$interp_char<-as.character(suscep$interp_result_desc)
suscep$new_interp<-ifelse(suscep$interp_char =='Susceptible'
       | suscep$interp_char =='Intermediate'
       | suscep$interp_char =='Resistant'
       ,suscep$interp_char
       ,NA)
suscep$new_interp=as.factor(suscep$new_interp)
suscep$new_interp<-factor(suscep$new_interp, levels=c("Susceptible", "Intermediate", "Resistant"))
levels(suscep$new_interp)
suscep$new_interp_num<-as.numeric(suscep$new_interp)

####sort by most resistant####
suscep.sort <- suscep[order(suscep$encounter_id, suscep$susc_day_offset, suscep$sepsis_antimicrobial_name, -suscep$new_interp_num),]

####delete NA interps, get most resistant, turn things wide####
suscep_nonnas<-filter(suscep.sort, !is.na(new_interp_num))%>% 
                group_by(encounter_id, 
                         susc_day_offset, 
                         sepsis_antimicrobial_name) %>% 
                summarise(most_res=max(new_interp_num))%>%
                spread(sepsis_antimicrobial_name, most_res)
describe(suscep_nonnas)
