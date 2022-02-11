
#This script is for exploring the predict table created by Rachel Buxton for the WCPE. 
#script by Carlos Linares July 2020, modify and corrected with the help of Rachel Buxton.

#How does moon affect predictions?

rm(list = ls())

#setup
library(tidyverse)
library(lubridate)#work wih dates
library(lunar)#for lunar illumination
library(suncalc)#moon position and moon times
library(plyr)
library(remotes)
library(lme4)
library(schoolmath) #to determine if numbers are neg or pos
library(sjPlot)
library(dplyr)
library(ggpubr)# plot reg line equation


#load data

pred<-read.csv("data_code/WCPEPredictions_updated.csv")

#######################load wind and temp from weather data #################


#path2<-"C:/Users/Carlos/Google Drive/WCPE/RB_Merge_tables/Weather_data/tempBI_2017_2019.csv"

path2<-"C:/Users/Carlos/Google Drive/WCPE/RB_Merge_tables/Data/WindandTemp.csv"
# path2<-"C:\\Users\\rbuxton\\Documents\\Phillips\\Linares data\\WindandTemp.csv"

temp<-read.csv(path2)
temp<-read.csv('data_code/WindandTemp.csv')
temp$Date<-parse_date_time(temp$Date, "ymd")
temp$Date<-as.character(temp$Date)


#############################################################################

# Date as a date 

pred$Date<-parse_date_time(pred$Date, "ymd")
pred$Date<-as.Date(pred$Date)

#include lunar illumination and moon.phase this last one is in radians 0=new moon, pi=full

pred<-pred %>% 
  mutate(moon.ill= lunar.illumination(Date),#proportion of lunar illumination 
         moon.pha= lunar.phase(Date, name = T)#lunar phase
         )

#get coordinates in the data 

cobbler<- c(-54.010833, -38.042222)
top<- c(-54.008333, -38.065000)

# here we include Lat and Lon in the the data. To calculate portion of moon out. 

pred$lat<-ifelse(pred$Site=="COBBLERS", -54.010833, 54.008333)

pred$lon<-ifelse(pred$Site=="COBBLERS", -38.042222, 38.065000)


#before proceeding I want to check for missing values

na.pred<- pred %>% filter_all(any_vars(is.na(.))) #there are 25 missing values



#function getMoonTimes() to get moon set, rise, etc. 
tempCobb<-getMoonTimes(date = pred$Date, lat = -54.010833, lon = -38.042222,keep = , tz="UTC")
tempTop<-getMoonTimes(date = pred$Date, lat=-54.008333, lon = -38.065000, keep = , tz="UTC")


#Remove duplicates
tempCobb<-tempCobb[!duplicated(tempCobb$date),]
tempTop<-tempTop[!duplicated(tempTop$date),]


#Put in site names
tempCobb$Site<-unique(pred$Site)[1]
tempTop$Site<-unique(pred$Site)[2]

MoonRiseSet<-rbind(tempCobb, tempTop) 
MoonRiseSet$RiseBeforeSet<-MoonRiseSet$rise-MoonRiseSet$set

MoonRiseSet<-MoonRiseSet[complete.cases(MoonRiseSet$rise),]
MoonRiseSet<-MoonRiseSet[complete.cases(MoonRiseSet$set),]

MoonRiseSet<-MoonRiseSet[order(MoonRiseSet$Site, MoonRiseSet$date),]

ListofMoonTimes<-lapply(seq(nrow(MoonRiseSet)), function(i){
  # print(i)
  FirstRow<-MoonRiseSet[i,]
  if(is.positive(FirstRow$RiseBeforeSet)){
        data<-seq(MoonRiseSet$set[i], MoonRiseSet$rise[i+1], by="hour")  
    }else{
      data<-seq(MoonRiseSet$rise[i], MoonRiseSet$set[i], by="hour")
  }
  data.frame(Date=data, Site=FirstRow$Site)
})
ListofMoonTimes<-do.call("rbind", ListofMoonTimes)
ListofMoonTimes$Moon<-1
  
ListofMoonTimes$Yr<-as.numeric(format(ListofMoonTimes$Date, "%Y"))
ListofMoonTimes$Mo<-as.numeric(format(ListofMoonTimes$Date, "%M"))
ListofMoonTimes$Day<-as.numeric(format(ListofMoonTimes$Date, "%d"))
ListofMoonTimes$Hr<-as.numeric(format(ListofMoonTimes$Date, "%H"))


ListofMoonTimes$Date<-as.Date(ListofMoonTimes$Date)
ListofMoonTimes$Date<-as.character(ListofMoonTimes$Date)

#Moon rise/set and Sun rise/set

#Join up with predictions
pred$Date<-as.character(pred$Date)
  
# this variable tell us when the moon is above horizon (1) or not (0)
pred.moon<-left_join(pred, ListofMoonTimes, by=c("Site","Date","Yr", "Mo", "Day", "Hr"))
pred.moon$Moon<-ifelse(is.na(pred.moon$Moon),0,1) 


# wind and temp merg with pred.moon
temp$Yr<-as.numeric(temp$Yr)
temp$Mo<-as.numeric(temp$Mo)
temp$Day<- as.numeric(temp$Day)
temp$Hr<- as.numeric(temp$Hr)

str(temp)
summary(temp)
str(pred.moon)
summary(pred.moon)

pred.moon.nas<-merge(pred.moon, temp, by=c("Date", "Yr", "Mo", "Day", "Hr"), all=T) 
pred.moon.nas<-pred.moon.nas[complete.cases(pred.moon.nas$moon.ill),]

NAZ<-pred.moon.nas[which(is.na(pred.moon.nas$WindSpeed)),]

#REPLACE NAs with data from an hour previous
NAweather<-lapply(seq(unique(paste0(NAZ$Date, NAZ$Hr))), function(i){
   print(i)
  DateTime<-unique(paste0(NAZ$Date, NAZ$Hr))[i]
  NAZ_sub<-NAZ[which(paste0(NAZ$Date, NAZ$Hr)==DateTime),]
  date<-unique(NAZ_sub$Date)
  Hour<-unique(NAZ_sub$Hr)
  
  Subtemp<-subset(temp, Date==date)
  HoursTemp<-Subtemp$Hr
  
  if(nrow(Subtemp)==0){
    Subtemp<-subset(temp, Date==as.Date(date)+1)
    HoursTemp<-Subtemp$Hr
    NT<-Subtemp[which(abs(HoursTemp-Hour)==min(abs(HoursTemp-Hour))),]
  
    }else{
  #Nearest time
  NT<-Subtemp[which(abs(HoursTemp-Hour)==min(abs(HoursTemp-Hour))),]
  }
  
  NAZ_sub$WindSpeed<-NT[,6]
  NAZ_sub$Temperature<-NT[,7]
  NAZ_sub[,-18]
 
})
NAweather<-do.call("rbind", NAweather)

#Take out NAs
pred.moon.nas<-pred.moon.nas[-which(is.na(pred.moon.nas$WindSpeed)),]

#Put in nearest weather
pred.moon.nas<-rbind(pred.moon.nas, NAweather)


summary(pred.moon)
#should we make date as a date for feeding into the model? I think so
pred.moon$Date<-as.Date(pred.moon$Date)

#-----------------------------------Exploring Predictions patterns-----------------------####

# predictions by hr. 


ggplot(data = pred.moon, aes(x=Day, y=predictions_PA, fill= Site))+
  geom_bar(stat = "identity")



###------Modeling  --------------


m1 <- glm(predictions_PA ~
            moon.ill + moon.pha + Hr + as.factor(Mo) + WindSpeed + Temperature,
          data = pred.moon.nas) # not sure this is working

summary(m1)
plot_model(m1)

m2 <-
  lmer(
    predictions_PA ~ moon.ill + moon.pha + WindSpeed + Temperature + Hr + Mo +
      (1 | Site) + (1 | Date),
    data = pred.moon.nas,
    REML = F
  )

summary(m2)
# confint(m2)
AIC(m2)


a<-lmer(predictions_PA ~ 1+(1|Site)+(1|Date), data = pred.moon.nas) # this tell us how much variance is explained by the random effects. 
b<-lmer(predictions_PA ~ 1+(1|Site/Date), data = pred.moon.nas)
summary(a)
summary(b)

AIC(a,b)

m3<- lmer(
  predictions_PA ~ moon.ill + Moon + WindSpeed + Temperature + Hr + Mo +
    (1 | Site/Date),
  data = pred.moon.nas,
  REML = F
)

summary(m3)

m4<- lmer(
  predictions_PA ~ moon.pha + Moon + WindSpeed + Temperature + Hr + Mo +
    (1 | Site/Date),
  data = pred.moon.nas,
  REML = F
)

m5<- lmer(
  predictions_PA ~ moon.ill + Moon + WindSpeed + Temperature + Hr + Mo +
    (1 | Site) +(1|Date),
  data = pred.moon.nas,
  REML = F
)

m6<- lmer(
  predictions_PA ~ moon.pha + Moon + WindSpeed + Temperature + Hr + Mo +
    (1 | Site) +(1|Date),
  data = pred.moon.nas,
  REML = F
)

pred.moon.nas<-pred.moon.nas[complete.cases(pred.moon.nas$predictions_PA),]
ddply(pred.moon.nas, .(Hr), summarize, MeanAct=mean(predictions_PA))

##Try categories of hours instead of hour as continuous
pred.moon.nas$HourPer<-ifelse(pred.moon.nas$Hr>=17&pred.moon.nas$Hr<=23, "Arrival", ifelse(pred.moon.nas$Hr>=0&pred.moon.nas$Hr<=4,"Peak", "Leaving"))
pred.moon.nas$moon.ill_scale<-scale(pred.moon.nas$moon.ill)[,1]
pred.moon.nas$WindSpeed_scale<-scale(pred.moon.nas$WindSpeed)[,1]
pred.moon.nas$Temperature_scale<-scale(pred.moon.nas$Temperature)[,1]

##THIS MODEL SEEMS TO HAVE THE LOWEST AIC
m7<- lmer(
  predictions_PA ~ moon.ill_scale + Moon + WindSpeed_scale + Temperature_scale + HourPer + as.factor(Mo) +
    (1 | Site/Date),
  data = pred.moon.nas,
  REML = F
)

summary(m7)
# confint(m7) #ignore the warning, also -this takes a while to run

AIC(m3, m4, m5, m6, m7)

m8<- lmer(
  predictions_Count ~ moon.ill_scale + Moon + WindSpeed_scale + Temperature_scale + HourPer + as.factor(Mo)  +
    (1 | Site/Date),
  data = pred.moon.nas,
  REML = F
)

summary(m8)


set_theme(
  base = theme_classic())


p1<-plot_model(m8,
               title="",
               sort.est = T,
               axis.labels = c("Sep", "Oct", "Moon abv horiz", "Nov", "Wind spd", "Moon Illum", "Dec", "Temp", "Leaving", "Peak" ),
               colors = "gs", 
               vline.color = "grey",

                axis.lim = c(-4,4)
                
                
               
               
)
p1

p1<-p1+theme_538()+set_theme( base = theme_538(), plot.col = "white", panel.gridcol = "white",  )
p1


p2<-plot_model(m8, type = "re")

#--------------------Hourly Pattern August2021-------------------

b<- pred.moon.nas %>% group_by(Site, Date, Hr, HourPer) %>% summarise_all(mean)
summary(b)
b$Site<-as.character(b$Site)
unique(b$Site)

#change T0P-MEAD to TOP-MEAD spelling error
class(b$Site)
b$Site = ifelse(b$Site == "T0P-MEAD", "TOP-MEAD", b$Site)
b$Site = ifelse(b$Site == "CBBLERS", "COBBLERS", b$Site)
unique(b$Site)

b$Site<- as.factor(b$Site)

ggplot(b, aes(factor(Hr, levels = c(17,18,19,20,21,22,23,0,1,2,3,4,5,6,7,8)), predictions_PA, colour=Site))+
  geom_boxplot()+
  geom_jitter(width =.1 ,colour="grey",alpha=.123)+
  theme_classic()+
  xlab("Hours")+
  ylab("WCPE call prediction")+
  facet_grid(.~Site)


legendtitle<-"Period"
summary(b)


# stat for showing difference between groups. 
pred_h<-aov(b$predictions_PA ~ b$HourPer)
summary(pred_h)
TukeyHSD(pred_h)

pred_hper_site<- aov(b$predictions_PA ~ b$Site + b$HourPer)
summary(pred_hper_site)
TukeyHSD(pred_hper_site)

#--------figure 4 November 2021-------------- 
b$HourPer<- as.factor(b$HourPer)
b$HourPer<- factor(b$HourPer, levels = c("Arrival","Peak", "Leaving" ) )



# same figure but with violine plots. 

ggplot(b, aes(factor(Hr, levels = c(17,18,19,20,21,22,23,0,1,2,3,4,5,6,7,8)), predictions_PA))+
  geom_violin(outlier.shape = NA, aes(colour= HourPer))+
  geom_jitter(width =.1 ,alpha=.1,size= .9, fill="grey", aes(colour= HourPer))+
  # geom_pointrange(mapping = aes(x = factor(Hr, levels = c(17,18,19,20,21,22,23,0,1,2,3,4,5,6,7,8)), y = predictions_PA),
  #                 stat = "summary",
  #                 fun.min = function(z) {quantile(z,0.25)},
  #                 fun.max = function(z) {quantile(z,0.75)},
  #                 fun = median, show.legend = F,
  #                 )+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="grey3", fill="grey", show.legend = F)+
  color_palette(palette = c("grey80" ,"grey46", "black")) +
  # scale_shape_manual(values=c(3,18,20))+
  xlab("Time of day (24 hour clock, UTC)")+
  ylab("WCPE call prediction")+
  theme_blank()+
  theme(legend.position = "right")+
  labs(colour= "Period")
  



#-----------------Month_activity---------------------

c<-pred.moon.nas %>% group_by(Site, Date, Mo, HourPer) %>% summarise_all(mean)

labsx<-c("Sep","Oct", "Nov", "Dec", "Jan")

c$Site<-as.character(c$Site)
unique(c$Site)

#change T0P-MEAD to TOP-MEAD spelling error
class(c$Site)
c$Site = ifelse(c$Site == "T0P-MEAD", "TOP-MEAD", c$Site)
c$Site = ifelse(c$Site == "CBBLERS", "COBBLERS", c$Site)
unique(c$Site)

#stat for difference between sites. 

pred_month<- aov(c$predictions_PA ~ c$Mo+ c$Site)
summary(pred_month)
TukeyHSD(pred_month)# showing an error. 

#--------------figure  3 (August 2021)-------------------
ggplot(c, aes(factor(Mo, levels = c(9,10,11,12,1,2)), predictions_PA, colour=Site))+
  geom_boxplot(outlier.shape = NA)+
  scale_color_grey(start = .2, end = .3)+
  geom_jitter(width =.1 ,colour="grey",alpha=.4)+
  xlab("Months")+
  ylab("WCPE call prediction")+
  facet_grid(.~Site)+
  scale_x_discrete(labels=labsx)+
  theme_blank()+
  # theme(panel.background ="")+
   theme(
     legend.position="none",
     plot.background = element_blank(),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank()
   )
  
ggplot(c, aes(factor(Mo, levels = c(9,10,11,12,1,2)), predictions_PA))+
  geom_violin(outlier.shape = NA)+
  scale_color_grey(start = .2, end = .3)+
  geom_jitter(width =.1 ,colour="grey",alpha=.4)+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="grey3", fill="grey", show.legend = F)+
  xlab("Months")+
  ylab("WCPE call prediction")+
  scale_x_discrete(labels=labsx)+
  theme_blank()+
  # theme(panel.background ="")+
  theme(
    legend.position="none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

