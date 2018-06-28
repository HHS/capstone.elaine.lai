setwd('C:\\Users\\laiy5\\Desktop\\capstone.elaine.lai')
library(tidyverse)
library(Hmisc)
library(chron)


####read in CSV####
steno<-read.csv("steno.csv", header=TRUE)

colnames(steno)[1]<-"ENCOUNTER_ID"
names(steno)

####adjust format for variables####
steno$ISOLATE_NAME_CHAR<-as.character(steno$ISOLATE_NAME)


#separate date and time#
steno1<-separate(steno, FIRST_REPORT_ENTERED_DT_TM, 
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


steno1$FIRST_REPORT_ENTERED_DT_TM<-chron(dates=steno1$FIRST_REPORT_ENTERED_DT, times=steno1$FIRST_REPORT_ENTERED_TM,
                                      format=c('y-m-d','h:m:s'))
steno1$LAST_REPORT_UPDATED_DT_TM<-chron(dates=steno1$LAST_REPORT_UPDATED_DT, times=steno1$LAST_REPORT_UPDATED_TM,
                                     format=c('y-m-d','h:m:s'))
steno1$MICRO_LAB_COMPLETED_DT_TM<-chron(dates=steno1$MICRO_LAB_COMPLETED_DT, times=steno1$MICRO_LAB_COMPLETED_TM,
                                     format=c('y-m-d','h:m:s'))
steno1$MICRO_LAB_CANCELLED_DT_TM<-chron(dates=steno1$MICRO_LAB_CANCELLED_DT, times=steno1$MICRO_LAB_CANCELLED_TM,
                                     format=c('y-m-d','h:m:s'))
steno1$MICRO_LAB_DRAWN_DT_TM<-chron(dates=steno1$MICRO_LAB_DRAWN_DT, times=steno1$MICRO_LAB_DRAWN_TM,
                                 format=c('y-m-d','h:m:s'))
steno1$MICRO_LAB_ORDERED_DT_TM<-chron(dates=steno1$MICRO_LAB_ORDERED_DT, times=steno1$MICRO_LAB_ORDERED_TM,
                                   format=c('y-m-d','h:m:s'))
steno1$MICRO_LAB_RECEIVED_DT_TM<-chron(dates=steno1$MICRO_LAB_RECEIVED_DT, times=steno1$MICRO_LAB_RECEIVED_TM,
                                    format=c('y-m-d','h:m:s'))

####Check lab_procedures####
list_of_lab_procedure<-as.data.frame(summary(steno1$LAB_PROCEDURE_NAME))
list_of_lab_procedure$lab_procedure_name<-rownames(list_of_lab_procedure)
colnames(list_of_lab_procedure)[1]<-"Count_Rows"
rownames(list_of_lab_procedure)<-NULL

combocounts<-group_by(steno1, LAB_PROCEDURE_NAME, COLLECTION_SOURCE_DESC, COLLECTION_SITE_DESC)%>%summarise(NUM_ENCOUNTERS=n_distinct(ENCOUNTER_ID))%>%arrange(desc(NUM_ENCOUNTERS))
print(tail(combocounts))


?write.csv
write.csv(list_of_lab_procedure,file="list_of_lab_procedure.csv", row.names = FALSE)