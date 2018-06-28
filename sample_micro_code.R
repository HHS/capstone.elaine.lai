####set work directory####
setwd('C:\\Users\\laiy5\\Desktop\\capstone.elaine.lai')
####libraries####
library(tidyverse)
library(chron)


####read in data####
sample_micro<-read.csv("sample_10000_micro_data.csv", header=TRUE)

####clean up column name####
colnames(sample_micro)[1]<-"ENCOUNTER_ID"
names(sample_micro)

####adjust format for variables####
sample_micro$ISOLATE_NAME_CHAR<-as.character(sample_micro$ISOLATE_NAME)

#separate date and time#
new<-separate(sample_micro, FIRST_REPORT_ENTERED_DT_TM, 
              into=c("FIRST_REPORT_ENTERED_DT", "FIRST_REPORT_ENTERED_TM"),
              sep=" ",
              convert=TRUE)%>%
     separate(LAST_REPORT_UPDATED_DT_TM,
              into=c("LAST_REPORT_UPDATED_DT", "LAST_REPORT_UPDATED_TM"),
              sep=" ",
              convert=TRUE)%>%
     separate(MICRO_LAB_COMPLETED_DT_TM,
              into=c("MICRO_LAB_COMPLETED_DT", "MICRO_LAB_COMPLETED_TM"),
              sep=" ",
              convert=TRUE)%>%
     separate(MICRO_LAB_CANCELLED_DT_TM,
              into=c("MICRO_LAB_CANCELLED_DT", "MICRO_LAB_CANCELLED_TM"),
              sep=" ",
              convert=TRUE)%>%
     separate(MICRO_LAB_DRAWN_DT_TM,
              into=c("MICRO_LAB_DRAWN_DT", "MICRO_LAB_DRAWN_TM"),
              sep=" ",
              convert=TRUE)%>%
     separate(MICRO_LAB_ORDERED_DT_TM,
              into=c("MICRO_LAB_ORDERED_DT", "MICRO_LAB_ORDERED_TM"),
              sep=" ",
              convert=TRUE)%>%
     separate(MICRO_LAB_RECEIVED_DT_TM,
              into=c("MICRO_LAB_RECEIVED_DT", "MICRO_LAB_RECEIVED_TM"),
              sep=" ",
              convert=TRUE)


new$FIRST_REPORT_ENTERED_DT_TM<-chron(dates=new$FIRST_REPORT_ENTERED_DT, times=new$FIRST_REPORT_ENTERED_TM,
                                  format=c('y-m-d','h:m:s'))
new$LAST_REPORT_UPDATED_DT_TM<-chron(dates=new$LAST_REPORT_UPDATED_DT, times=new$LAST_REPORT_UPDATED_TM,
                                      format=c('y-m-d','h:m:s'))
new$MICRO_LAB_COMPLETED_DT_TM<-chron(dates=new$MICRO_LAB_COMPLETED_DT, times=new$MICRO_LAB_COMPLETED_TM,
                                      format=c('y-m-d','h:m:s'))
new$MICRO_LAB_CANCELLED_DT_TM<-chron(dates=new$MICRO_LAB_CANCELLED_DT, times=new$MICRO_LAB_CANCELLED_TM,
                                      format=c('y-m-d','h:m:s'))
new$MICRO_LAB_DRAWN_DT_TM<-chron(dates=new$MICRO_LAB_DRAWN_DT, times=new$MICRO_LAB_DRAWN_TM,
                                      format=c('y-m-d','h:m:s'))
new$MICRO_LAB_ORDERED_DT_TM<-chron(dates=new$MICRO_LAB_ORDERED_DT, times=new$MICRO_LAB_ORDERED_TM,
                                      format=c('y-m-d','h:m:s'))
new$MICRO_LAB_RECEIVED_DT_TM<-chron(dates=new$MICRO_LAB_RECEIVED_DT, times=new$MICRO_LAB_RECEIVED_TM,
                                      format=c('y-m-d','h:m:s'))


#staph<-filter(sample_micro, grepl("Staph", ISOLATE_NAME_CHAR, ignore.case = TRUE) 
#             & grepl("aureus", ISOLATE_NAME_CHAR, ignore.case = TRUE))
#staph$ISOLATE_NAME_FAC<-as.factor(staph$ISOLATE_NAME_CHAR)
#levels(staph$ISOLATE_NAME_FAC)

#sorted<-select(new,ENCOUNTER_ID, ACCESSION, MICRO_RESULT_TYPE_CODE, MICRO_RESULT_TYPE_DESC, ISOLATE_NAME_CHAR)%>%
#    distinct(ENCOUNTER_ID, ACCESSION, MICRO_RESULT_TYPE_CODE, ISOLATE_NAME_CHAR)%>%
#    group_by(ENCOUNTER_ID, ACCESSION, ISOLATE_NAME_CHAR)%>%
#    summarise(most_final=max(MICRO_RESULT_TYPE_CODE))%>%
#    mutate(rank=dense_rank(ISOLATE_NAME_CHAR))%>%
#    unite("most_final_new",c(most_final,rank), sep='-', remove=TRUE)%>%
#    mutate(rank=dense_rank(ISOLATE_NAME_CHAR))%>%
#    spread(key=most_final_new, value=ISOLATE_NAME_CHAR)

sorted1<-arrange(new,ENCOUNTER_ID, ACCESSION, FIRST_REPORT_ENTERED_DT_TM, LAST_REPORT_UPDATED_DT_TM)%>%
  select(ENCOUNTER_ID, ACCESSION, ISOLATE_NAME_CHAR)%>%
  distinct(ENCOUNTER_ID, ACCESSION, ISOLATE_NAME_CHAR)%>%
  group_by(ENCOUNTER_ID, ACCESSION)%>%
  mutate(rank=dense_rank(ISOLATE_NAME_CHAR))%>%
  spread(key=rank, value=ISOLATE_NAME_CHAR)
