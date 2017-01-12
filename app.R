
# Load required packages --------------------------------------------------
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(plotly)
library(gridBase)
library(grid)
library(gridExtra)
library(stringr)
# Link the source file ----------------------------------------------------

source("datasource.R")


# UI goes here ------------------------------------------------------------
#browser()
ui=dashboardPage(
                 dashboardHeader(title="MSS Analytics and Reporting Tool",titleWidth=500),
                 dashboardSidebar(width = '200px',
                  sidebarMenu(
                     menuItem("By Customer",tabName = "customer",icon=icon('users'),
                      menuSubItem(
                        selectInput("selectcustomer",label="Select Client ID",
                                   choices = c("All",CustomerID),selected = CustomerID[1]
                       
                     )),
                     menuSubItem(
                       dateRangeInput("alertdate",label = "Select Analysis Range",start = startdate,end = enddate)
                     ),
                     menuSubItem(radioButtons("radioalert",label = "Select Visualization",
                                              choices = list("Total Alerts"=1,"% False Positives"=2,
                                                             "Total Escalations"=3,"Misclassification Rate"=4,
                                                             "All Status"=5))),
                     menuSubItem("Show Visualization",tabName = "visual1",icon = icon('users'))),
                     menuItem("By Rule Name",tabName = "rulename",icon = icon('wrench')),
                     menuItem("By Attacker Geography",tabName = "srcgeo",icon = icon('globe')),
                     menuItem("By Event Vendor",tabName = "srcgeo",icon = icon('shield')),
                     menuItem("By Event Group",tabName = "srcgeo",icon = icon('bug')),
                     menuItem("By Handle Time",tabName = "hantime",icon=icon('eye'))
                     
                  )),
                 dashboardBody(
                   tabItems(
                     tabItem(tabName = "visual1",
                             fluidRow(
                               
                               infoBoxOutput("custdesc",width = 12)),
                             
                             
                             fluidRow(
                               
                               valueBoxOutput("averagealerts",width = 3),
                               valueBoxOutput("averageFP",width = 3),
                               valueBoxOutput("averageesc",width = 3),
                               valueBoxOutput("averageerr",width = 3)
                             ),
                             fluidRow(
                               box(
                              
                                 width = 7,status = "info",solidHeader = TRUE,
                                 title="Alert Visualization",collapsible = TRUE,
                                 plotOutput("alert1",width = "100%", height =400)
  
                               ),
   
                               box(
                                 status="info",width = 5,collapsible = TRUE,solidHeader = TRUE,
                                 title='List of all alerts for selected client',
                                 dataTableOutput("alerttable")
                               )
                               )
                                

                               
                             )
                   )
                 )
  
)


# Server Code goes here ---------------------------------------------------


server=function(input,output,session){
  
  totalalert=reactive({
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    unique.alerts%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
      group_by(att_Month,ai_alert_soc_status)%>%
      summarise(Status=n())
  })
  
  customeralert=reactive({
    #browser()
    cust=input$selectcustomer
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    unique.alerts%>%
      filter(remedy_customer_id==cust)%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
      group_by(att_Month,ai_alert_soc_status)%>%
      summarise(Status=n())
    
  })
  
  averagealerts=reactive({
    #browser()
    cust=input$selectcustomer
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    unique.alerts%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
      group_by(att_Month,ai_alert_soc_status)%>%
      summarise(Status=n())
  })
  
  averagealertsind=reactive({
    #browser()
    cust=input$selectcustomer
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
    unique.alerts%>%
      filter(industry==clientInd)%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
      group_by(att_Month,ai_alert_soc_status)%>%
      summarise(Status=n())
    
  })
  
  alertcusttable=reactive({
    cust=input$selectcustomer
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    unique.alerts%>%
      filter(remedy_customer_id==cust)%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))
    
  })
  
  alertalltable=reactive({
    
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    unique.alerts%>%
      
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))
    
  })
  errorrate=reactive({
    cust=input$selectcustomer
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    escalated_alerts%>%
      filter(remedy_customer_id==cust)%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
      group_by(att_Month)%>%
      summarise(misclass=n())
    
  })
  
  errorall=reactive({
    
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    escalated_alerts%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
      group_by(att_Month)%>%
      summarise(misclass=n())
    
  })
  
  errorrateind=reactive({
    
    cust=input$selectcustomer
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
    escalated_alerts%>%
      filter(industry==clientInd)%>%
      filter(attack_start_time %within% interval1)%>%
      mutate(att_Month=lubridate::month(attack_start_time,label=TRUE))%>%
      group_by(att_Month)%>%
      summarise(misclass=n())
  })
  
  totalind=reactive({
    
    cust=input$selectcustomer
    clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
    period1=input$alertdate[1]
    period2=input$alertdate[2]
    interval1=interval(period1,period2)
    indnum=unique.alerts %>% 
      filter(industry==clientInd)%>%
      filter(attack_start_time %within% interval1)%>%
      group_by(remedy_customer_id)%>%
      summarise(count=n())
  })
  
 output$custdesc=renderInfoBox({
   cust=input$selectcustomer
   clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
   period1=input$alertdate[1]
   period2=input$alertdate[2]
   infoBox(
    title = paste("Client ID :",cust),
    icon = icon('binoculars'),color = "light-blue",width = 12,fill = TRUE,subtitle = paste0("Client Industry : ",clientInd,",                        Analysis Period : ",period1," to ",period2)
    
   )
 })
  
   output$averagealerts=renderValueBox({
     #browser()
     if (input$selectcustomer=="All"){
       
       averagemonthly=totalalert()
       allaverage=averagemonthly%>%
         group_by(att_Month) %>% 
         summarise(Total_Alerts=sum(Status))
         allaverage=round(mean(allaverage$Total_Alerts),0)
       
       valueBox(
         value = formatC(allaverage,big.mark = ",",format = "d"),
         subtitle = paste("Average Monthly \n Alerts- MSS"),
         color="yellow")
       
     } else if (input$selectcustomer!="All") {
       
       
       cust=input$selectcustomer
       clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
       avgind=averagealertsind()
       avgind=avgind %>% 
         group_by(att_Month) %>% 
         summarise(Total_Alerts=mean(Status))
       
       averageind=round(mean(avgind$Total_Alerts),0)
       
       valueBox(
         value = averageind,
         subtitle = paste(clientInd,"Industry Averaged Monthly Alerts"),
         color="yellow")
       
     }
    
  })
   
  output$averageFP=renderValueBox({
    #browser()
    
    if (input$selectcustomer=="All"){
      allalerts=totalalert()
      fp=c("CLOSED","COMMENTED")
      custchart1=allalerts%>%
        group_by(att_Month)%>%
        summarise(Total_Alerts=sum(Status))
      custchart=allalerts%>%
        filter(ai_alert_soc_status %in% fp)%>%
        group_by(att_Month)%>%
        summarise(Total_Alerts=sum(Status))
      
      custchart$total=custchart1$Total_Alerts
      custchart$percent=round(custchart$Total_Alerts/custchart$total*100,1)
      averagefalsepositive=round(mean(custchart$percent),1)
      
      valueBox(
        value = paste0(averagefalsepositive,"%"),
        subtitle = paste("Averaged monthly \n False Positives- MSS"),
        color="purple")
      
      
    } else if (input$selectcustomer!="All"){
      
      cust=input$selectcustomer
      clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
      fp=c("CLOSED","COMMENTED")
      avgind=averagealertsind()
      avgind1=avgind%>%
        summarise(Avg_Alerts=sum(Status))
      
      avgind=avgind%>%
        filter(ai_alert_soc_status %in% fp)%>%
        summarise(Avg_Alerts=sum(Status))
      
      avgind$total=avgind1$Avg_Alerts
      avgind$avgpercent=round(avgind$Avg_Alerts/avgind$total*100,1)
      averagefalsepositive=round(mean(avgind$avgpercent),1)
      
      valueBox(
        value = paste0(averagefalsepositive,"%"),
        subtitle = paste(clientInd,"Industry Averaged monthly False Positives"),
        color="purple")
      
      
    }
    
    
    })
  
  output$averageesc=renderValueBox({
    #browser()
    
    if (input$selectcustomer=="All"){
      
      allalerts=totalalert()
      escl=c("ASSOCIATED","AUTO_ESCALATED","AUTO_ESCALATION_PENDING","ESCALATED","AUTO_ASSOCIATED")
      
      custchart=allalerts%>%
        filter(ai_alert_soc_status %in% escl)%>%
        group_by(att_Month)%>%
        summarise(Total_Alerts=sum(Status))
      Avg_Alerts=round(mean(custchart$Total_Alerts),0)
      
      valueBox(
        value = formatC(Avg_Alerts,big.mark = ",",format = "d"),
        subtitle = paste("Averaged monthly \n escalations- MSS"),
        color="olive")
      
    } else if (input$selectcustomer!="All") {
      
      cust=input$selectcustomer
      clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
      avgcust=averagealertsind()
      avgcust=avgcust%>%
        filter(ai_alert_soc_status %in% escl)%>%
        summarise(Avg_Alerts=round(mean(Status),0))
      Avg_Alerts=round(mean(avgcust$Avg_Alerts),0)
      valueBox(
        value = Avg_Alerts,
        subtitle = paste(clientInd,"Industry averaged monthly escalations"),
        color="olive")
      
    }
    
    
  })
  
  output$averageerr=renderValueBox({
    #browser()
    if (input$selectcustomer=="All"){
      errormss=errorall()
      meanerror=round(mean(errormss$misclass),0)
      valueBox(
        value = formatC(meanerror,big.mark = ",",format = "d"),
        subtitle = paste("Averaged monthly \n error rates- MSS"),
        color="aqua")
      
    }else if (input$selectcustomer!="All") {
      
      cust=input$selectcustomer
      clientInd=unique.alerts$industry[unique.alerts$remedy_customer_id==cust]
      
      
      errorind=errorrateind()
      errorind=errorind%>%
        summarise(meanerror=round(mean(misclass),0))
      meanerror=round(mean(errorind$meanerror),0)
      
     
      
      avgind=averagealertsind()
      avgind=avgind %>% 
        group_by(att_Month) %>% 
        summarise(Total_Alerts=mean(Status))
      
      averageind=round(mean(avgind$Total_Alerts),0)
      
      errorpercent=round(meanerror/averageind*100,1)
      valueBox(
        value = paste0(meanerror,"(",errorpercent,"%)"),
        subtitle = paste(clientInd,"Industry averaged monthly error rates"),
        color="aqua")
      
    }

  })
  
  output$alert1=renderPlot({
    
    if (input$selectcustomer=="All"){
      
      allalerts=totalalert()
      if(input$radioalert==1){
      allalerts=allalerts%>%
        group_by(att_Month)%>%
        summarise(Total_Alerts=sum(Status))
      
      ggplot(data=allalerts,aes(x=att_Month,y=Total_Alerts,fill=att_Month))+geom_bar(stat="identity")+
        geom_text(aes(att_Month,Total_Alerts,label=Total_Alerts),size=4)+theme(legend.position = "none")+
        labs(x="Months",y="Total Alerts Generated")+ggtitle(label = paste("Total alerts generated by month for all MSS Customers"))
      
      
      } else if(input$radioalert==2){
        fp=c("CLOSED","COMMENTED")
        custchart1=allalerts%>%
          group_by(att_Month)%>%
          summarise(Total_Alerts=sum(Status))
        custchart=allalerts%>%
          filter(ai_alert_soc_status %in% fp)%>%
          group_by(att_Month)%>%
          summarise(Total_Alerts=sum(Status))
        
        custchart$total=custchart1$Total_Alerts
        custchart$percent=round(custchart$Total_Alerts/custchart$total*100,1)
        
        ggplot(data=custchart,aes(x=att_Month,y=percent,group=1))+geom_line(stat="identity",position = "identity",linetype="solid",color="blue")+
          geom_text(aes(att_Month,percent,label=paste0(percent,"%")),size=4)+theme(legend.position ='none')+
          #geom_point(data=avgcust,aes(x=att_Month,y=Avg_Alerts,group=1),size=1)+theme_light()+
          labs(x="Months",y="False Positives Percentage")+ggtitle(label = paste("% False Positives by month for all customers"))
        
        
        
    } else if (input$radioalert==3){
      
      escl=c("ASSOCIATED","AUTO_ESCALATED","AUTO_ESCALATION_PENDING","ESCALATED","AUTO_ASSOCIATED")

        custchart=allalerts%>%
        filter(ai_alert_soc_status %in% escl)%>%
        group_by(att_Month)%>%
        summarise(Total_Alerts=sum(Status))
      

      custchart$att_Month=factor(custchart$att_Month)
      
      ggplot(data=custchart,aes(x=att_Month,y=Total_Alerts,fill=att_Month))+geom_bar(stat="identity")+
        geom_text(aes(att_Month,Total_Alerts,label=Total_Alerts),size=4)+theme(legend.position ='none')+
        labs(x="Months",y="Escalations")+ggtitle(label = paste("Escalations by month for all customers"))
      
      
    } else if (input$radioalert==4){
      errorcust=errorall()
      
      
      ggplot(data=errorcust,aes(x=att_Month,y=misclass,fill=att_Month))+geom_bar(stat="identity")+
        geom_text(aes(att_Month,misclass,label=misclass),size=4)+theme(legend.position ='none')+
        
        labs(x="Months",y="Misclassification")+ggtitle(label = paste("Misclassifications by month for all customers"))

          } else if (input$radioalert==5){
      
      ggplot(allalerts,aes(att_Month,Status,fill=att_Month))+geom_bar(stat='identity')+facet_wrap(~ai_alert_soc_status,nrow = 3)+
        theme(legend.position = "none")+geom_text(aes(att_Month,Status,label=Status),size=3)+
        labs(x="Months",y="Alerts generated")+ggtitle(label = paste("Status of all alerts generated for client for all clients"),subtitle = "Alerts faceted by SOC Status type")
      
      
    }
    
    } else if(input$selectcustomer!="All") {
      #browser()
      custchart=customeralert()
      avgcust=averagealertsind()
      ncust=totalind()
      
      indcust=nrow(ncust)
      
      if(input$radioalert==1){
        
        
        avgcust=avgcust%>%
          group_by(att_Month)%>%
          summarise(Avg_Alerts=round(mean(Status),0))
        
        
        
        custchart=custchart%>%
          group_by(att_Month)%>%
          summarise(Total_Alerts=sum(Status))
        
        
        
        ggplot(data=custchart,aes(x=att_Month,y=Total_Alerts,fill=att_Month))+geom_bar(stat="identity")+
          geom_text(aes(att_Month,Total_Alerts,label=Total_Alerts),size=4)+
          #geom_point(data=avgcust,aes(x=att_Month,y=Avg_Alerts,group=1),size=1)+theme_light()+
          geom_line(data=avgcust,aes(x=att_Month,y=Avg_Alerts,group=1),linetype="dashed",color="blue",size=1)+theme(legend.position ='none')+
          geom_text(data=avgcust,aes(att_Month,Avg_Alerts+5,label=Avg_Alerts))+
          labs(x="Months",y="Total Alerts Generated")+ggtitle(label = paste("Total alerts generated by month for",input$selectcustomer),subtitle = paste("Dotted line shows average alerts by Industry, n:",indcust))
        
      }else if(input$radioalert==2) {
        #avgcust gives the average by industry
        #browser()
        fp=c("CLOSED","COMMENTED")
        
        
        avgcust1=avgcust%>%
          group_by(att_Month)%>%
          summarise(Avg_Alerts=sum(Status))
        
        custchart1=custchart%>%
          group_by(att_Month)%>%
          summarise(Total_Alerts=sum(Status))
        
        avgcust=avgcust%>%
          filter(ai_alert_soc_status %in% fp)%>%
          group_by(att_Month)%>%
          summarise(Avg_Alerts=sum(Status))
        
        avgcust$total=avgcust1$Avg_Alerts
        avgcust$avgpercent=round(avgcust$Avg_Alerts/avgcust$total*100,1)
        
        custchart=custchart%>%
          filter(ai_alert_soc_status %in% fp)%>%
          group_by(att_Month)%>%
          summarise(Total_Alerts=sum(Status))
        
        custchart$total=custchart1$Total_Alerts
        custchart$percent=round(custchart$Total_Alerts/custchart$total*100,1)
        
        
        ggplot(data=custchart,aes(x=att_Month,y=percent,group=1))+geom_line(stat="identity",position = "identity",linetype="solid",color="blue")+
          geom_text(aes(att_Month,percent,label=paste0(percent,"%")),size=4)+
          #geom_point(data=avgcust,aes(x=att_Month,y=Avg_Alerts,group=1),size=1)+theme_light()+
          geom_line(data=avgcust,aes(x=att_Month,y=avgpercent,group=1),linetype="dashed",color="green",size=1)+theme(legend.position ='none')+
          geom_text(data=avgcust,aes(att_Month,avgpercent,label=paste0(avgpercent,"%")))+
          labs(x="Months",y="False Positives Percentage")+ggtitle(label = paste("% False Positives by month for",input$selectcustomer),subtitle = paste("Dotted line shows average % false positives by Industry, n:",indcust))
        
      } else if (input$radioalert==3){
        
        #avgcust gives the average by industry
        #browser()
        escl=c("ASSOCIATED","AUTO_ESCALATED","AUTO_ESCALATION_PENDING","ESCALATED","AUTO_ASSOCIATED")
        
        avgcust=avgcust%>%
          filter(ai_alert_soc_status %in% escl)%>%
          group_by(att_Month)%>%
          summarise(Avg_Alerts=round(mean(Status),0))
        
        custchart=custchart%>%
          filter(ai_alert_soc_status %in% escl)%>%
          group_by(att_Month)%>%
          summarise(Total_Alerts=sum(Status))
        
        #custchart$ai_alert_soc_status=sub("AUTO_ESCALATION_PENDING","AUTO_ESCALATED",x =custchart$ai_alert_soc_status )
        #custchart$ai_alert_soc_status=sub("AUTO_ASSOCIATED","AUTO_ESCALATED",x =custchart$ai_alert_soc_status )
        
        #custchart$ai_alert_soc_status=sub("ASSOCIATED","ESCALATED",x =custchart$ai_alert_soc_status )
        #custchart$ai_alert_soc_status=factor(custchart$ai_alert_soc_status)
        custchart$att_Month=factor(custchart$att_Month)
        
        ggplot(data=custchart,aes(x=att_Month,y=Total_Alerts,fill=att_Month))+geom_bar(stat="identity")+
          geom_text(aes(att_Month,Total_Alerts,label=Total_Alerts),size=4)+
          #geom_point(data=avgcust,aes(x=att_Month,y=Avg_Alerts,group=1),size=1)+theme_light()+
          geom_line(data=avgcust,aes(x=att_Month,y=Avg_Alerts,group=1),linetype="dashed",color="green",size=1)+theme(legend.position ='none')+
          geom_text(data=avgcust,aes(att_Month,Avg_Alerts,label=Avg_Alerts))+
          labs(x="Months",y="Escalations")+ggtitle(label = paste("Escalations by month for",input$selectcustomer),subtitle = paste("Dotted line shows average escalations by Industry, n:",indcust))
        
        
      } else if (input$radioalert==4){
        #browser()
        errorcust=errorrate()
        errorind=errorrateind()
        
        ggplot(data=errorcust,aes(x=att_Month,y=misclass,fill=att_Month))+geom_bar(stat="identity")+
          geom_text(aes(att_Month,misclass,label=misclass),size=4)+
          #geom_point(data=avgcust,aes(x=att_Month,y=Avg_Alerts,group=1),size=1)+theme_light()+
          geom_line(data=errorind,aes(x=att_Month,y=misclass,group=1),linetype="dashed",color="green",size=1)+theme(legend.position ='none')+
          geom_text(data=errorind,aes(att_Month,misclass,label=misclass))+
          labs(x="Months",y="Misclassification")+ggtitle(label = paste("Misclassifications by month for",input$selectcustomer),subtitle = paste("Dotted line shows total misclassifications by Industry in the analysis period, n=",indcust))
        
      }else {
        
        ggplot(custchart,aes(att_Month,Status,fill=att_Month))+geom_bar(stat='identity')+facet_wrap(~ai_alert_soc_status,nrow = 3)+
          theme(legend.position = "none")+geom_text(aes(att_Month,Status,label=Status),size=3)+
          labs(x="Months",y="Alerts generated")+ggtitle(label = paste("Status of all alerts generated for client",input$selectcustomer),subtitle = "Alerts faceted by SOC Status type")
        
        
      }
}
    })
  
  output$alerttable=renderDataTable(options = list(pageLength=10),{
    
    if (input$selectcustomer=="All"){
      alertcusttable=alertalltable()
      alertcusttable%>%
        select(Alert.ID=ai_alert_id,SOC.Status= ai_alert_soc_status,Date.Time=(attack_start_time))%>%
        arrange(Date.Time)
      
    } else if (input$selectcustomer!="All") {
      
      alertcusttable=alertcusttable()
      alertcusttable%>%
        select(Alert.ID=ai_alert_id,SOC.Status= ai_alert_soc_status,Date.Time=(attack_start_time))%>%
        arrange(Date.Time)
      
    }
    
  })
  
}

shinyApp(ui,server)

