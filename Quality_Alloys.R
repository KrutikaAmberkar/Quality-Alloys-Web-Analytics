#Please install this packages
#install.packages("plotly")
#install.packages("psych")
#install.packages("car")
#install.packages("corrgram")

library('readxl')
data<-read_xls('Web Analytics Case Student Spreadsheet.xls',sheet=2,skip=4)
data1<-read_xls('Web Analytics Case Student Spreadsheet.xls',sheet=3,skip=4)
data2<-read_xls('Web Analytics Case Student Spreadsheet.xls',sheet=4,skip=4)
data3<-read_xls('Web Analytics Case Student Spreadsheet.xls',sheet=5,skip=4)
merged_df <- merge(data,data1,by="Week (2008-2009)",sort = FALSE)
merged_df
library(tibble)
merged_df=add_column(merged_df, Id = seq(1:66), .before = 1)
for (i in 1:nrow(merged_df)){
  if (merged_df$Id[i]>=1 & merged_df$Id[i]<=14){
    merged_df$Period[i]<-'Initial'
  }
  if (merged_df$Id[i]>=15 & merged_df$Id[i]<=35){
    merged_df$Period[i]<-'Pre-Promotion'
  }
  if (merged_df$Id[i]>=36 & merged_df$Id[i]<=52){
    merged_df$Period[i]<-'Promotion'
  }
  if (merged_df$Id[i]>=53 & merged_df$Id[i]<=66){
    merged_df$Period[i]<-'Post-Promotion'
  }
  
}
colnames(merged_df)[which(names(merged_df) == "Week (2008-2009)")] <- "Week_(2008-2009)"
colnames(merged_df)[which(names(merged_df) == "Unique Visits")] <- "Unique_Visits"
colnames(merged_df)[which(names(merged_df) == "Pages/Visit")] <- "Pages_Visit"
colnames(merged_df)[which(names(merged_df) == "Avg. Time on Site (secs.)")] <- "Avg_Time_on_Site"
colnames(merged_df)[which(names(merged_df) == "Bounce Rate")] <- "Bounce_Rate"
colnames(merged_df)[which(names(merged_df) == "% New Visits")] <- "Percentage_New_Visits"
colnames(merged_df)[which(names(merged_df) == "Lbs. Sold")] <- "Lbs_Sold"
merged_df[1,'Visits']


func_cal<-function(colnumber,statfuncname){
  for(j in colnumber){
    stat_ini<-statfuncname(merged_df[which(merged_df[,'Period']=='Initial'),j])
    stat_pre<-statfuncname(merged_df[which(merged_df[,'Period']=='Pre-Promotion'),j])
    stat_pro<-statfuncname(merged_df[which(merged_df[,'Period']=='Promotion'),j])
    stat_post<-statfuncname(merged_df[which(merged_df[,'Period']=='Post-Promotion'),j])
    stat1<-c(stat_ini,stat_pre,stat_pro,stat_post)
  }
  return(stat1)
}
#-------------------------mean-----------------------------------
col_num<-which(colnames(merged_df)=="Visits" )
descrip_cal=func_cal(col_num,mean) #column number for visits
visits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Unique_Visits" )
descrip_cal=func_cal(col_num,mean) #column number for unique visits
uniquevisits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Revenue" )
descrip_cal=func_cal(col_num,mean) #column number for Revenue
revenue<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Profit" )
descrip_cal=func_cal(col_num,mean) #column number for Profit
profit<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Lbs_Sold" )
descrip_cal=func_cal(col_num,mean) #column number for Lbs.Sold
lbs_sold<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Bounce_Rate" )
descrip_cal=func_cal(col_num,mean) #column number for Bounce rate
bounce_rate<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Inquiries" )
descrip_cal=func_cal(col_num,mean) #column number for Inquiries
inquiries<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

period <-factor(c("Initial","Pre-Promotion","Promotion","Post-Promotion"),levels = c("Initial","Pre-Promotion","Promotion","Post-Promotion"))
df_mean<-data.frame("Period"=period,"Visits"=visits,"Unique_visits"=uniquevisits, "Revenue"=revenue,"Profit"=profit,"Lbs_sold"=lbs_sold,"BounceRate"=bounce_rate,"Inquiry"=inquiries)
df_mean

#-------------------------median-----------------------------------
col_num<-which(colnames(merged_df)=="Visits" )
descrip_cal=func_cal(col_num,median) #column number for visits
visits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Unique_Visits" )
descrip_cal=func_cal(col_num,median) #column number for unique visits
uniquevisits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Revenue" )
descrip_cal=func_cal(col_num,median) #column number for Revenue
revenue<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Profit" )
descrip_cal=func_cal(col_num,median) #column number for Profit
profit<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Lbs_Sold" )
descrip_cal=func_cal(col_num,median) #column number for Lbs.Sold
lbs_sold<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

period <-factor(c("Initial","Pre-Promotion","Promotion","Post-Promotion"),levels = c("Initial","Pre-Promotion","Promotion","Post-Promotion"))
df_median<-data.frame("Period"=period,"Visits"=visits,"Unique_visits"=uniquevisits, "Revenue"=revenue,"Profit"=profit,"Lbs_sold"=lbs_sold)
df_median

#-------------------------min-----------------------------------
col_num<-which(colnames(merged_df)=="Visits" )
descrip_cal=func_cal(col_num,min) #column number for visits
visits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Unique_Visits" )
descrip_cal=func_cal(col_num,min) #column number for unique visits
uniquevisits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Revenue" )
descrip_cal=func_cal(col_num,min) #column number for Revenue
revenue<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Profit" )
descrip_cal=func_cal(col_num,min) #column number for Profit
profit<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Lbs_Sold" )
descrip_cal=func_cal(col_num,min) #column number for Lbs.Sold
lbs_sold<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

period <-factor(c("Initial","Pre-Promotion","Promotion","Post-Promotion"),levels = c("Initial","Pre-Promotion","Promotion","Post-Promotion"))
df_min<-data.frame("Period"=period,"Visits"=visits,"Unique_visits"=uniquevisits, "Revenue"=revenue,"Profit"=profit,"Lbs_sold"=lbs_sold)
df_min

#-------------------------max-----------------------------------

col_num<-which(colnames(merged_df)=="Visits" )
descrip_cal=func_cal(col_num,max) #column number for visits
visits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Unique_Visits" )
descrip_cal=func_cal(col_num,max) #column number for unique visits
uniquevisits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Revenue" )
descrip_cal=func_cal(col_num,max) #column number for Revenue
revenue<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Profit" )
descrip_cal=func_cal(col_num,max) #column number for Profit
profit<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Lbs_Sold" )
descrip_cal=func_cal(col_num,max) #column number for Lbs.Sold
lbs_sold<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

period <-factor(c("Initial","Pre-Promotion","Promotion","Post-Promotion"),levels = c("Initial","Pre-Promotion","Promotion","Post-Promotion"))
df_max<-data.frame("Period"=period,"Visits"=visits,"Unique_visits"=uniquevisits, "Revenue"=revenue,"Profit"=profit,"Lbs_sold"=lbs_sold)
df_max
#-------------------------sd-----------------------------------
col_num<-which(colnames(merged_df)=="Visits" )
descrip_cal=func_cal(col_num,sd) #column number for visits
visits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Unique_Visits" )
descrip_cal=func_cal(col_num,sd) #column number for unique visits
uniquevisits<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Revenue" )
descrip_cal=func_cal(col_num,sd) #column number for Revenue
revenue<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Profit" )
descrip_cal=func_cal(col_num,sd) #column number for Profit
profit<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

col_num<-which(colnames(merged_df)=="Lbs_Sold" )
descrip_cal=func_cal(col_num,sd) #column number for Lbs.Sold
lbs_sold<-c(descrip_cal[1],descrip_cal[2],descrip_cal[3],descrip_cal[4])

period <-factor(c("Initial","Pre-Promotion","Promotion","Post-Promotion"),levels = c("Initial","Pre-Promotion","Promotion","Post-Promotion"))
df_sd<-data.frame("Period"=period,"Visits"=visits,"Unique_visits"=uniquevisits, "Revenue"=revenue,"Profit"=profit,"Lbs_sold"=lbs_sold)
df_sd

#---------------------------------------------------visitors from continent---------------------------
regions<-c("South America","Northern America","Central America","Western Europe","Eastern Asia","Northern Europe","Southern Asia"
           ,"South-Eastern Asia","Southern Europe","Eastern Europe")
visitsperregion<-c(22616,17509,6776,5214,3228,2721,2589,1968,1538,1427)
df_continent<-data.frame("Regions"=regions,"Visits"=visitsperregion)
df_continent
america <-vector()
europe <-vector()
asia <-vector()
for (i in 1:nrow(df_continent)){
  if (df_continent$Regions[i]=="South America" | df_continent$Regions[i]=="Northern America" | df_continent$Regions[i]=="Central America") {
    america[i]<-df_continent$Visits[i]
  }
  if (df_continent$Regions[i]=="Western Europe" | df_continent$Regions[i]=="Northern Europe" | df_continent$Regions[i]=="Southern Europe" | df_continent$Regions[i] =="Eastern Europe") {
    europe[i]<-df_continent$Visits[i]
  }
  if (df_continent$Regions[i]=="Eastern Asia" | df_continent$Regions[i]=="Southern Asia" | df_continent$Regions[i]=="South-Eastern Asia") {
    asia[i]<-df_continent$Visits[i]
  }
}

sum(america,na.rm = FALSE)
sum(europe,na.rm = TRUE)
sum(asia,na.rm = TRUE)
x<-c("America","Europe","Asia")
y<-c(sum(america,na.rm = FALSE),sum(europe,na.rm = TRUE),sum(asia,na.rm = TRUE))
conti<-data.frame("Geographic_Sources"=x,"Visits"=y)
colors <- c('rgb(172, 162, 190)', 'rgb(125, 162, 190)', 'rgb(117, 68, 114)')
conti_visits <- plot_ly(conti, labels = ~Geographic_Sources, values = ~Visits, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Visits from Geographic Sources',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

conti_visits

#---------------------------------------------------Bouncerate, Visitors,Profit---------------------------
library(ggplot2)

myplot<-ggplot(df_mean, aes(Period, Visits, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Period", y = "Visits", 
       title = "Visits Per Period")
require(scales)
myplot + expand_limits(y = 0)+scale_y_continuous(labels = comma) 


myplot<-ggplot(df_mean, aes(Period, BounceRate, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Period", y = "Bounce Rate", 
       title = "Bounce Rate Per Period")
myplot


myplot<-ggplot(df_mean, aes(Period, Profit, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Period", y = "Profit", 
       title = "Profit Per Period")
require(scales)
myplot + expand_limits(y = 0)+scale_y_continuous(labels = comma) 

myplot<-ggplot(df_mean, aes(Period, Revenue, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Period", y = "Revenue", 
       title = "Revenue Per Period")
require(scales)
myplot + expand_limits(y = 0)+scale_y_continuous(labels = comma) 


#---------------------------------------------------Traffic source------------------------------------------

traffic_source<-c("Referring Sites","Search Engines","Direct Traffic","Other")
visitsperts<-c(38754,20964,9709,4)
df_traffic_source<-data.frame("Traffic_source"=traffic_source,"Visits"=visitsperts)
df_traffic_source
#install.packages("plotly")
library(plotly)

bargraph_ts <- plot_ly(df_traffic_source, x = ~Traffic_source, y = ~Visits,type = 'bar',text = ~Visits ,textposition = 'auto') %>%
  layout(title = 'Traffic Source',
         xaxis = list(showgrid = TRUE,title="Traffic Source" ),
         yaxis = list(showgrid = TRUE,title="Visits" ))

bargraph_ts

#------------------------------------------top 10 referring sites-----------------------------------------------------
ref_sites<-c("googleads.g.doubleclick.net","pagead2.googlesyndication.com","sedoparking.com","globalspec.com","searchportal.information.com","freepatentsonline.com","thomasnet.com","mu.com","mail.google.com","psicofxp.com")
visitsperrefsites<-c(15626,8044,3138,693,582,389,379,344,337,310)
df_ref_sites<-data.frame("Referring_Sites"=ref_sites,"Visits"=visitsperrefsites)
df_ref_sites
#Below code is to make sure that x axis doesn't get sorted
df_ref_sites$Referring_Sites <- factor(df_ref_sites$Referring_Sites, levels = df_ref_sites[["Referring_Sites"]])

bargraph_refsites <- plot_ly(df_ref_sites, x = ~Referring_Sites, y = ~Visits, type = 'bar',text = ~round(Visits/sum(Visits)*100,2), textposition = 'auto') %>%
  layout(title = 'Top 10 Referring sites in %',
         xaxis = list(showgrid = TRUE,title="Referring sites" ),
         yaxis = list(showgrid = TRUE ,zeroline=FALSE,showticklabels = FALSE))

bargraph_refsites

#----------------------------------------------------------------------------------------------------------------------
data2=add_column(data2, Id = seq(1:290), .before = 1)

for (i in 1:nrow(data2)){
  if (data2$Id[i]<=13){data2$quarter[i]<-'2005_Q1'}
  else if (data2$Id[i]<=26){data2$quarter[i]<-'2005_Q2'}
  else if (data2$Id[i]<=39){data2$quarter[i]<-'2005_Q3'}
  else if (data2$Id[i]<=52){data2$quarter[i]<-'2005_Q4'}
  else if (data2$Id[i]<=65){data2$quarter[i]<-'2006_Q1'}
  else if (data2$Id[i]<=78){data2$quarter[i]<-'2006_Q2'}
  else if (data2$Id[i]<=91){data2$quarter[i]<-'2006_Q3'}
  else if (data2$Id[i]<=104){data2$quarter[i]<-'2006_Q4'} 
  else if (data2$Id[i]<=117){data2$quarter[i]<-'2007_Q1'}
  else if (data2$Id[i]<=130){data2$quarter[i]<-'2007_Q2'}
  else if (data2$Id[i]<=143){data2$quarter[i]<-'2007_Q3'}
  else if (data2$Id[i]<=156){data2$quarter[i]<-'2007_Q4'}
  else if (data2$Id[i]<=169){data2$quarter[i]<-'2008_Q1'}
  else if (data2$Id[i]<=182){data2$quarter[i]<-'2008_Q2'}
  else if (data2$Id[i]<=195){data2$quarter[i]<-'2008_Q3'}
  else if (data2$Id[i]<=208){data2$quarter[i]<-'2008_Q4'}
  else if (data2$Id[i]<=221){data2$quarter[i]<-'2009_Q1'}
  else if (data2$Id[i]<=234){data2$quarter[i]<-'2009_Q2'}
  else if (data2$Id[i]<=247){data2$quarter[i]<-'2009_Q3'}
  else if (data2$Id[i]<=260){data2$quarter[i]<-'2009_Q4'}
  else if (data2$Id[i]<=273){data2$quarter[i]<-'2010_Q1'}
  else if (data2$Id[i]<=286){data2$quarter[i]<-'2010_Q2'}
  else if (data2$Id[i]<=299){data2$quarter[i]<-'2010_Q3'}
}

colnames(data2)[which(names(data2) == "Lbs. Sold")] <- "Lbs_sold"


by_quarter <- data2 %>% group_by(quarter)
by_quarter

df_quat <-data.frame("Quarter"=by_quarter %>% summarise(
  Lbs_sold = sum(Lbs_sold)
))
df_quat
colnames(df_quat)


myp <- plot_ly(df_quat, x = ~Quarter.quarter, y = ~Quarter.Lbs_sold, type = 'scatter', mode = 'lines')%>%
  layout(title = 'Sales per Quarter (in lbs)',
         xaxis = list(showgrid = TRUE,title="Quarter" ),
         yaxis = list(showgrid = TRUE ,title="Lbs sold"))
myp
#-------------------------------Correlation----------------------------------------------
library(psych)
pairs.panels(merged_df[,-1:-2], method = "pearson", hist.col = "#00AFBB", density = TRUE, ellipse = TRUE)

#-----------------------------------multiple correlation-----------------------------------------------------
ln_df1 <- as.data.frame(matrix(nrow=nrow(merged_df),ncol=0))
ln_df1$Visits <- log(merged_df$Visits)
ln_df1$Unique_Visits <- log(merged_df$Unique_Visits)
ln_df1$Pageviews <- log(merged_df$Pageviews)
ln_df1$Pages_Visit <- log(merged_df$Pages_Visit)
ln_df1$Avg_Time_on_Site <- log(merged_df$Avg_Time_on_Site)
ln_df1$Bounce_Rate <- log(merged_df$Bounce_Rate)
ln_df1$Percentage_New_Visits <- log(merged_df$Percentage_New_Visits)
ln_df1$Revenue <- log(merged_df$Revenue)
ln_df1$Profit <- log(merged_df$Profit)
ln_df1$Lbs_Sold <- log(merged_df$Lbs_Sold)
ln_df1$Inquiries <- log(merged_df$Inquiries)

library(car)
model <- lm(Profit ~ Visits + Unique_Visits + Pageviews + Pages_Visit + Avg_Time_on_Site + Bounce_Rate + Percentage_New_Visits + Revenue + Lbs_Sold + Inquiries, data=ln_df1)
summary(model)
vif(model)

model_2 <- lm(Profit ~ Visits + Unique_Visits + Pageviews + Pages_Visit + Avg_Time_on_Site + Bounce_Rate + Percentage_New_Visits + Inquiries, data=ln_df1)
summary(model_2)
vif(model_2)

model_rev <- lm(Revenue ~ Visits + Unique_Visits + Pageviews + Pages_Visit + Avg_Time_on_Site + Bounce_Rate + Percentage_New_Visits + Inquiries, data=ln_df1)
summary(model_rev)
vif(model_rev)

model_rev_1 <- lm(Revenue ~ Visits, data = ln_df1)
summary(model_rev_1)

model_sold <- lm(Lbs_Sold ~ Visits + Unique_Visits + Pageviews + Pages_Visit + Avg_Time_on_Site + Bounce_Rate + Percentage_New_Visits + Inquiries, data = merged_df)
summary(model_sold)
vif(model_sold)

model_sold1 <- lm(Lbs_Sold ~ Avg_Time_on_Site + Percentage_New_Visits + Inquiries, data = merged_df)
summary(model_sold1)
vif(model_sold1)


scatterplot(x=merged_df$Visits, y=merged_df$Revenue, main="Linear Regression", smooth=FALSE, ellipse=TRUE)


#----------------------------------------------correlation------------------------------------------

library(corrgram)
corrgram(merged_df[,-1:-2], order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="Correlations")
