setwd("C:/Documents and Settings/Administrator/Desktop/proj")
data1<-read.csv2('data1.csv',header=TRUE)
data2<-read.csv2('data2.csv',header=TRUE)
data3<-read.csv2('data3.csv',header=TRUE)
library(sqldf)
##ipologismos_statistikon
opens<-sqldf("select campaign_id,email_address,count(*) 
from data1
where action='open'
group by campaign_id,email_address")
unique_opens_per_campaign<-sqldf("select campaign_id,count(*) as opens
from opens
group by campaign_id")
clicks<-sqldf("select campaign_id,email_address,count(*) 
from data1
where action='click'
group by campaign_id,email_address")
unique_clicks_per_campaign<-sqldf("select campaign_id,count(*) as clicks
from clicks
group by campaign_id")
bounces<-sqldf("select campaign_id,email_address,count(*) 
from data1
              where action='bounce'
              group by campaign_id,email_address")
unique_bounces_per_campaign<-sqldf("select campaign_id,count(*) as bounces
from bounces
group by campaign_id")

new_table<-sqldf("select unique_opens_per_campaign.campaign_id,unique_opens_per_campaign.opens,
unique_clicks_per_campaign.clicks,unique_bounces_per_campaign.bounces
from unique_opens_per_campaign
left join unique_clicks_per_campaign
on  unique_opens_per_campaign.campaign_id=unique_clicks_per_campaign.campaign_id
left join unique_bounces_per_campaign  
on  unique_opens_per_campaign.campaign_id=unique_bounces_per_campaign.campaign_id ")

data4<- sqldf("select new_table.campaign_id,new_table.opens,new_table.clicks,new_table.bounces,data2.emails_sent,
data2.rec_list_id      from new_table
join data2
on new_table.campaign_id=data2.campaign_id ")
data4$bounces[is.na(data4$bounces)] <- 0
data4$open_rate<-data4$opens/(data4$emails_sent-data4$bounces)
data4$click_rate<-data4$clicks/(data4$emails_sent-data4$bounces)
data4$bounce_rate<-data4$bounces/data4$emails_sent
data4$click_to_open_rate<-data4$clicks/data4$opens
data4<-subset(data4,data4$emails_sent>100)
data5<-data4[,c(1,2,3,4,5,7)]
data5<-data5[order(data5$open_rate,decreasing = T),]
fig1<-barplot(data5[1:5,6],col=4,names.arg=data5[1:5,6],cex.names=0.5,ylim = c(0,0.5),ylab = 'open rate',xlab = 'campaign',main = 'top 5 campaigns based on open rate')
fig2<-barplot(data5[32:36,6],col=4,names.arg=data5[32:36,6],cex.names=0.5,ylim = c(0,0.2),ylab = 'open rate',xlab = 'campaign',main = 'worst 5 campaigns based on open rate')
data6<-data4[,c(1,2,3,4,5,8)]
data6<-data6[order(data6$click_rate,decreasing = T),]
fig3<-barplot(data6[1:5,6],col=4,names.arg=data5[1:5,6],cex.names=0.5,ylim = c(0,0.2),ylab = 'click rate',xlab = 'campaign',main = 'top 5 campaigns based on click rate')
fig4<-barplot(data6[32:36,6],col=4,names.arg=data5[32:36,6],cex.names=0.5,ylim = c(0,0.05),ylab = 'click rate',xlab = 'campaign',main = 'worst 5 campaigns based on click rate')
data7<-data4[,c(1,2,3,4,5,10)]
data7<-data7[order(data7$click_to_open_rate,decreasing = T),]
fig5<-barplot(data7[1:5,6],col=4,names.arg=data5[1:5,6],cex.names=0.5,ylim = c(0,0.5),ylab = 'click to open rate',xlab = 'campaign',main = 'top 5 campaigns based on click to open rate')
fig6<-barplot(data7[32:36,6],col=4,names.arg=data5[32:36,6],cex.names=0.5,ylim = c(0,0.1),ylab = 'click to open rate',xlab = 'campaign',main = 'worst 5 campaigns based on click to open rate')
summary(aov(open_rate~rec_list_id,data=data4))
summary(aov(click_rate~rec_list_id,data=data4))
summary(aov(click_to_open_rate~rec_list_id,data=data4))##iparxei diafora analoga me ti lista
cor.test(data4$open_rate,data4$click_rate)##thetiki sisxetisi anamesa sto click_rate kai sto open_rate
##sigkrisi click_rate,open_rate  ton eksrateion me auto ton email_lists
data8<-sqldf("select email_address,count(*)
from data1
             
             group by email_address")
data9<-sqldf("select data8.email_address,data3.list_id,data3.stats_avg_click_rate,data3.stats_avg_open_rate
from data8
join data3
on data8.email_address=data3.email_address    ")##sindeo tis idies email_address apo toys dio pinakes gia sigkriseis(open rate,click rate)


avg_click_rate_from_campaigns<-mean(as.numeric(as.character(data4$click_rate)))
avg_click_rate_from_email_lists<-mean(as.numeric(as.character(data9$stats_avg_click_rate)))
avg_open_rate_from_campaigns<-mean(as.numeric(as.character(data4$open_rate)))
avg_open_rate_from_email_lists<-mean(as.numeric(as.character(data9$stats_avg_open_rate)))
open_rate<-c(avg_open_rate_from_email_lists,avg_open_rate_from_campaigns)
barplot(open_rate,names.arg=c('overall','campaign'),col=3,ylab = 'open_rate')
click_rate<-c(avg_click_rate_from_email_lists,avg_click_rate_from_campaigns)
barplot(click_rate,names.arg=c('overall','campaign'),col=3,ylab = 'click_rate')
##open_rate ekstrateion less than open_rate email_lists
##sigkrisi click_rate,open_rate per list_id me ti list_d ton email_lists
avg_click_rate_per_list_id_from_campaigns<-aggregate(data4$click_rate,list(data4$rec_list_id),mean)
avg_click_rate_per_list_id_from_email_lists<-aggregate(as.numeric(as.character(data9$stats_avg_click_rate)),list(data9$list_id),mean)
avg_open_rate_per_list_id_from_campaigns<-aggregate(data4$open_rate,list(data4$rec_list_id),mean)
avg_open_rate_per_list_id_from_email_lists<-aggregate(as.numeric(as.character(data9$stats_avg_open_rate)),list(data9$list_id),mean)
category<-c('campaigns','campaigns','overall','overall')
##clickrate
list<-c('180b7eeb41','cd055c6fe3','180b7eeb41','cd055c6fe3')
clickrate<-c(0.03159928,0.03435853,0.04412319,0.06828130)
data10<-data.frame(category,list,clickrate)
library(ggplot2)
ggplot(data10, aes(list, clickrate, fill = category)) + 
  geom_bar(stat="identity", position = "dodge") 
##openrate
openrate<-c(0.3021558,0.1995284,0.4346874,0.3392220)
data11<-data.frame(category,list,openrate)
ggplot(data11, aes(list, openrate, fill = category)) + 
  geom_bar(stat="identity", position = "dodge") 
##clickrate,open_rate campaings based on list_id
barplot(avg_click_rate_per_list_id_from_campaigns$x,col=2,names.arg=avg_click_rate_per_list_id_from_campaigns$Group.1,cex.names=0.5,ylim = c(0,0.5),ylab = 'avg_click_rate',xlab = 'list_id')
barplot(avg_open_rate_per_list_id_from_campaigns$x,col=2,names.arg=avg_open_rate_per_list_id_from_campaigns$Group.1,cex.names=0.5,ylim = c(0,0.7),ylab = 'avg_open_rate',xlab = 'list_id')
##diereunisi statistikon stoixoion from table email_lists
data12<-sqldf("select email_address,list_id,stats_avg_click_rate,stats_avg_open_rate,email_client
from data3 
         where email_client!='' 
         order by email_client")
aggregate(data12[,2],list(data12[,5]),length)##tha afaireso tous tipous ton email me liga dedomena
data13<-sqldf("select email_address,list_id,stats_avg_click_rate,stats_avg_open_rate,email_client
from data12 
         where  email_client!='Outlook 2007'and email_client!='Outlook 2010' and email_client!='Windows Live Mail'
         and email_client!='Windows Phone' and email_client!='Thunderbird' and email_client!='Yahoo'  and email_client!='Hotmail'
         order by email_client")
##statistiki analisi gia click_rate,open_rate kai email_client
data13$stats_avg_click_rate <- as.numeric(as.character(data13$stats_avg_click_rate))
cur.formula1 <-as.formula('stats_avg_click_rate~email_client')
dataset<-data13
anova1<-aov(cur.formula1,data=dataset)
summary(anova1)##p value>0.05 we cannot reject null hypothesis ,therefore all means click_rate are equal
data13$stats_avg_open_rate <- as.numeric(as.character(data13$stats_avg_open_rate))
cur.formula2 <-as.formula('stats_avg_open_rate~email_client')
dataset<-data13
anova2<-aov(cur.formula2,data=dataset)
summary(anova2)#p value <0.05 we reject null hypothesis, all means open_rate are not equal
mean_open_rate_per_email_client<-aggregate(data13[,4],list(data13[,5]),mean)
mean_click_rate_per_email_client<-aggregate(data13[,3],list(data13[,5]),mean)
figure1<-barplot(mean_open_rate_per_email_client$x,col=2,names.arg=mean_open_rate_per_email_client$Group.1,cex.names=0.5,ylim = c(0,0.7),ylab = 'mean_open_rate')
figure1<-barplot(mean_click_rate_per_email_client$x,col=2,names.arg=mean_open_rate_per_email_client$Group.1,cex.names=0.5,ylab = 'mean_click_rate')
##statistiki analisi gia click_rate,open_rate kai location_country
data14<-sqldf("select stats_avg_click_rate,stats_avg_open_rate,location_country
         from data3 
         where location_country!=''  ")
data14$stats_avg_click_rate <- as.numeric(as.character(data14$stats_avg_click_rate))
cur.formula3<-as.formula('stats_avg_click_rate~location_country')
dataset<-data14
anova3<-aov(cur.formula3,data=dataset)
summary(anova3)##p value>0.05 we cannot reject null hypothesis 
data14$stats_avg_open_rate<- as.numeric(as.character(data14$stats_avg_open_rate))
cur.formula4 <-as.formula('stats_avg_open_rate~location_country')
dataset<-data14
anova4<-aov(cur.formula4,data=dataset)
summary(anova4)##p value>0.05 we cannot reject null hypothesis 

##statistiki analisi gia click_rate,open_rate kai location_timezone(per region)
data15<-sqldf("select email_address,list_id,stats_avg_click_rate,stats_avg_open_rate,location_timezone
from data3 
         where location_timezone!=''  ")

data15[,5]<-as.character(data15[,5])
data15[,5] <- sapply(strsplit(data15[,5], split='/', fixed=TRUE), function(x) (x[1]))
data15$stats_avg_click_rate <- as.numeric(as.character(data15$stats_avg_click_rate))
cur.formula5 <-as.formula('stats_avg_click_rate~location_timezone')
dataset<-data15
anova5<-aov(cur.formula5,data=dataset)
summary(anova5)##p value>0.05 we cannot reject null hypothesis 
data15$stats_avg_open_rate<- as.numeric(as.character(data15$stats_avg_open_rate))
cur.formula6 <-as.formula('stats_avg_open_rate~location_timezone')
dataset<-data15
anova6<-aov(cur.formula6,data=dataset)
summary(anova6)##p value<0.05 we  reject null hypothesis 
mean_open_rate_per_region<-aggregate(data15[,4],list(data15[,5]),mean)
mean_click_rate_per_region<-aggregate(data15[,3],list(data15[,5]),mean)
figure4<-barplot(mean_open_rate_per_region$x,col=2,names.arg=mean_open_rate_per_region$Group.1,cex.names=0.5,ylab = 'mean_open_rate',xlab = 'region')
figure5<-barplot(mean_click_rate_per_region$x,col=2,names.arg=mean_click_rate_per_region$Group.1,cex.names=0.5,ylab = 'mean_click_rate',xlab='region')
##eggrafes vasi imeras
data16<-sqldf("select timestamp_signup as date,email_address
         from data3
         WHERE status='subscribed'
         and timestamp_signup not like '%N'     ")
library(lubridate)
data16[,1]<-ymd_hms(data16[,1])
data16[,1]<-weekdays(data16[,1], abbreviate = FALSE)
subscribes_per_day<-aggregate(data16[,2],list(data16[,1]),length)
pie(subscribes_per_day$x, labels = subscribes_per_day$Group.1, col=2:8)
barplot(subscribes_per_day$x, names.arg = subscribes_per_day$Group.1, col=2:8,ylim = c(0,60))
##kataskeui montelou
data17<-sqldf("select data13.list_id,data13.email_address,data13.email_client,data13.stats_avg_open_rate,data13.stats_avg_click_rate,data15.location_timezone
from data13
join data15
on data13.email_address=data15.email_address")
model<-lm(stats_avg_open_rate ~ list_id+location_timezone+email_client, data=data17)
summary(model)##poli kako montelo theloume perissotera dedomena kai alles metavlites gia na provlepsoume to click rate kai to open rate
