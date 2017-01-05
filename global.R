
library(data.table)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)

# Importing Database ------------------------------------------------------


alerts=fread("6monthsalerts.csv",stringsAsFactors = TRUE)


# Converting Date and Time Variables --------------------------------------


alerts$attack_start_time=ymd_hms(alerts$attack_start_time)
alerts$xps_alert_create=ymd_hms(alerts$xps_alert_create)

alerts$log_date= ymd_hms(alerts$log_date)


# Data Cleaning -----------------------------------------------------------

#1. soc alert status
#checking the levels of alerts status


#will keep only the observations falling in specific levels and delete all others
keeplevels=c("ASSOCIATED","AUTO_ASSOCIATED","AUTO_ESCALATED","AUTO_ESCALATION_PENDING",
             "CLOSED","COMMENTED","ESCALATED")

#remove all observations not with these levels

alerts=alerts[alerts$ai_alert_soc_status %in% keeplevels,]

alerts$ai_alert_soc_status=factor(alerts$ai_alert_soc_status)

rm(keeplevels)

#2. Industry - remove blank levels

alerts$industry=factor(alerts$industry)


#3. Rule Name- remove blank levels

alerts$rule_name=factor(alerts$rule_name)

#4. Attack start time - clean

# take a summary

#remove all alerts that are not within the start time, end time window
starttime=ymd("2016-03-01")
endtime=ymd("2016-08-31")

alerts=alerts[alerts$attack_start_time>= starttime & alerts$attack_start_time<=endtime,]

#5. XPS alert create time- clean


#no changes

#6. time to create- create fresh one
alerts$time_to_create=as.numeric(alerts$xps_alert_create-alerts$attack_start_time)

#remove all rows with alert creation time in negative

alerts=alerts[alerts$time_to_create>0,]


#7. Src Geo - clean up



#first convert to character

alerts$src_geo=as.character(alerts$src_geo)

#remove all \\N from the data
pattern="\\\\N"

#replace all not alpha with null

src_geo=sub(pattern = pattern,replacement = "NULL",x = alerts$src_geo)


#check if the count_src_geo file exists, if yes then update src_geo with the file else run the function

if (file.exists("count_src_geo.csv")) {
  count_src_geo=read.csv("count_src_geo.csv")
  #ifelse loop to fill "multiple" in src_geo
  src_geo=ifelse(count_src_geo$x>4,"multiple",src_geo)
} else {
  count_src_geo=unlist(lapply(alerts$src_geo,function(x)str_count(x)))
  write.csv(count_src_geo,"count_src_geo.csv",row.names = FALSE)
  #ifelse loop to fill "multiple" in src_geo
  src_geo=ifelse(count_src_geo>4,"multiple",src_geo)
}

#update alerts with the src_geo object
alerts$src_geo=factor(src_geo)

#remove unwanted datasets
rm(src_geo,count_src_geo)

#8. dst Geo - clean up


#first convert to character

alerts$dst_geo=as.character(alerts$dst_geo)

#remove all \\N from the data
pattern="\\\\N"

#replace all not alpha with null

dst_geo=sub(pattern = pattern,replacement = "NULL",x = alerts$dst_geo)


#check if the count_dst_geo file exists, if yes then update dst_geo with the file else run the function

if (file.exists("count_dst_geo.csv")) {
  count_dst_geo=read.csv("count_dst_geo.csv")
  #ifelse loop to fill "multiple" in dst_geo
  dst_geo=ifelse(count_dst_geo$x>4,"multiple",dst_geo)
} else {
  count_dst_geo=unlist(lapply(alerts$dst_geo,function(x)str_count(x)))
  write.csv(count_dst_geo,"count_dst_geo.csv",row.names = FALSE)
  #ifelse loop to fill "multiple" in dst_geo
  dst_geo=ifelse(count_dst_geo>4,"multiple",dst_geo)
}

#update alerts with the dst_geo object
alerts$dst_geo=factor(dst_geo)

#remove unwanted datasets
rm(dst_geo,count_dst_geo)

#9. Event Vendors - clean up


#first remove all zero levels

alerts$event_vendors=factor(alerts$event_vendors)



#convert all \\N to NULL

#first convert to character

alerts$event_vendors=as.character(alerts$event_vendors)

#remove all \\N from the data
pattern="\\\\N"

#replace all not alpha with null

event_vendors=sub(pattern = pattern,replacement = "NULL",x = alerts$event_vendors)



pattern2="\\|"

#find pattern2 in event_vendors

#check if file exists

if (file.exists("multiple_ev_vendors.csv")){
  patternfound=read.csv("multiple_ev_vendors.csv")
  event_vendors[patternfound$x=="TRUE"]="Multiple"
} else {
  patternfound=unlist(lapply(event_vendors, FUN = function(x) str_detect(x,pattern2)))
  write.csv(patternfound,"multiple_ev_vendors.csv",row.names = FALSE)
  #replace pattern found by Multiple
  event_vendors[patternfound]="Multiple"
}

alerts$event_vendors=event_vendors
alerts$event_vendors=factor(alerts$event_vendors)

#remove all unwanted objects
rm(patternfound,event_vendors)

#10. Event Groups - Clean using the same logic as above

#first remove all zero levels

alerts$event_groups=factor(alerts$event_groups)



#convert all \\N to NULL

#first convert to character

alerts$event_groups=as.character(alerts$event_groups)

#remove all \\N from the data
pattern="\\\\N"

#replace all not alpha with null

event_groups=sub(pattern = pattern,replacement = "NULL",x = alerts$event_groups)


pattern2="\\|"

#find pattern2 in event_groups

#check if file exists

if (file.exists("multiple_ev_groups.csv")){
  patternfound=read.csv("multiple_ev_groups.csv")
  event_groups[patternfound$x=="TRUE"]="Multiple"
} else {
  patternfound=unlist(lapply(event_groups, FUN = function(x) str_detect(x,pattern2)))
  write.csv(patternfound,"multiple_ev_groups.csv",row.names = FALSE)
  #replace pattern found by Multiple
  event_groups[patternfound]="Multiple"
}

alerts$event_groups=event_groups
alerts$event_groups=factor(alerts$event_groups)

#remove all unwanted objects
rm(patternfound,event_groups)


# Create Unique Alerts Dataframe ------------------------------------------
names(alerts)
alerts=tbl_df(alerts)
unique.alerts=alerts[,c(1:12)]
unique.alerts=distinct(unique.alerts)

unique.alerts=tbl_df(unique.alerts)
rm(starttime,endtime,pattern,pattern2)
save.image("datasource.RData")
#chart to plot SOC status by customer and period
period1=ymd("2016-03-01")
period2=ymd("2016-08-31")
interval1=interval(period1,period2)
selection="day"
# levels(unique.alerts$ai_alert_soc_status)
# 
# escl=c("ASSOCIATED","AUTO_ESCALATED","AUTO_ESCALATION_PENDING","ESCALATED","AUTO_ASSOCIATED")
# table(unique.alerts$ai_alert_soc_status)
# # unique.alerts%>%
#   filter(attack_start_time %within% interval1)%>%
#     mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
#     group_by(att_Month)%>%
#     summarise(Status=round(n()/numcustomers,0))
data("mpg")
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  scale_y_continuous(
    "mpg (US)", 
    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
  ) 
  
# #chart to plot SOC status by customer and period
# df=unique.alerts%>%
#   filter(remedy_customer_id=="CIDS704355")%>%
#   filter(attack_start_time %within% interval1)%>%
#   mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
#   group_by(att_Month,ai_alert_soc_status)%>%
#   summarise(Status=n())

# df1=unique.alerts%>%
#   filter(remedy_customer_id=="CIDS704355")%>%
#   filter(attack_start_time %within% interval1)%>%
#   mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
#   group_by(att_Month,ai_alert_soc_status)%>%
#   summarise(Status=n())%>%
#   filter(ai_alert_soc_status %in% fp)%>%
#   group_by(att_Month)%>%
#   summarise(Avg_Alerts=round(mean(Status),0))

# unique.alerts%>%
#   filter(industry=="Automotive Manufacture")%>%
#   distinct(remedy_customer_id)

# 
# df2=unique.alerts%>%
#   filter(remedy_customer_id=="CIDS704355")%>%
#   filter(attack_start_time %within% interval1)%>%
#   mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
#   group_by(att_Month,ai_alert_soc_status)%>%
#   filter(ai_alert_soc_status %in% c("CLOSED","ESCALATED"))%>%
#   summarise(Status=n())
# df2cast=reshape2::dcast(df2,att_Month+Status~ai_alert_soc_status)
#   
#  p= ggplot(df,aes(att_Month,Status,fill=att_Month))+geom_bar(stat='identity')+facet_wrap(~ai_alert_soc_status,nrow = 3)+
#   theme(legend.position = "none")+geom_text(aes(att_Month,Status,label=Status),size=3)
# ggplotly(p)
# 
# plot_ly(df,x=~att_Month,y=~Status,color=~ai_alert_soc_status,type = "bar")
# #chart to plot src geo by customer, period
# 
# df=unique.alerts%>%
#   filter(remedy_customer_id=="CIDS704379")%>%
#   filter(attack_start_time %within% interval1)%>%
#   mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
#   group_by(att_Month,src_geo)%>%
#   summarise(status=n())
# 
# plot_ly(df,x=~att_Month,y=~status,color=~src_geo)
# p=ggplot(df,aes(att_Month,status,fill=att_Month))+geom_bar(stat='identity')+facet_wrap(~src_geo)+
#   theme(legend.position = "none")
# ggplotly(p)
# 
# 
# plot_ly(unique.alerts[unique.alerts$remedy_customer_id=="CIDS704379",],x=~(lubridate::month(attack_start_time,label=TRUE)),color = ~src_geo)
# 
# #rules fired
# 
# dfr=unique.alerts%>%
#   filter(remedy_customer_id=="CIDS704379")%>%
#   filter(attack_start_time %within% interval1)%>%
#   mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
#   group_by(att_Month,rule_name)%>%
#   summarise(status=n())
# 
# prule=ggplot(dfr,aes(att_Month,status,fill=att_Month))+geom_bar
  