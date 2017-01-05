
library(data.table)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)

#load the data source in the global environment

load("datasource.RData")

CustomerID=unique(as.character(unique.alerts$remedy_customer_id))
startdate=unique.alerts$attack_start_time[1]
last=nrow(unique.alerts)
enddate=unique.alerts$attack_start_time[last]
numcustomers=length(CustomerID)
escl=c("ASSOCIATED","AUTO_ESCALATED","AUTO_ESCALATION_PENDING","ESCALATED","AUTO_ASSOCIATED")


escalated_alerts=alerts%>%
  filter(ai_alert_soc_status %in% escl)%>%
  filter(sla_metric_type=="TA_TAKE_ACTION_CLOSE")
