
library(dplyr)
library(forcats)
library(ggplot2)
library(wesanderson)

dta <- read.csv("/Users/sarwaridas/Desktop/IDS 702/Final Project/ids720_CoffeeAndCrimes/02_processed_data/coffee_crime_controls.csv", header = TRUE,sep = ",",stringsAsFactors = FALSE)
dta <- subset(dta, select = -c(X,Year,Community.Area)) #removing index and yr column
str(dta)

f_cols= c('timeperiod','GEOG') #changing to factor dtype
dta[f_cols] = lapply(dta[f_cols], as.factor) #changing to factor dtype

levels(dta$timeperiod)
#dta=subset(dta, mgstr>1)

library(pastecs)
stat.desc(dta$coffeeshops)
#summarise(dta$coffeeshops)
dta$logcrime=log(dta$crimes)


nulls = sapply(dta, function(col) sum(length(which(is.na(col))))); nulls #checking for nulls
summary(dta$crime)#crime has outliers - not removing them though

# 
# #########CONSIDER REMOVING OUTLIERS!
# install.packages("viridis")  # Install
# library("viridis") 
# 
# ##INVESTIGATING OUTCOME OF INTEREST
# ggplot(dta,aes(crimes)) +
#   geom_histogram(aes(y=..density..),color="black",linetype="dashed",
#                  fill=inferno(15),bins=15) + theme(legend.position="none") +
#   geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
#   labs(title="Distribution of Crime",x="Crimes per neighborhood") + theme_classic()+theme(plot.title = element_text(hjust = 0.5)) #Taking transformation to fit normal assumption model better
# 
# 
# ggplot(dta,aes(logcrime)) +
#   geom_histogram(aes(y=..density..),color="black",linetype="dashed",
#                  fill=inferno(15),bins=15) + theme(legend.position="none") +
#   geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
#   labs(title="Distribution of log (Crime)",x="Crimes per neighborhood") + theme_classic()+theme(plot.title = element_text(hjust = 0.5)) 
# 
# ###relatively better
# 
# #Are there any variations in crime by CCA?
# 
# #Let's see for a random sample
# set.seed(100)
# sample_state <- sample(unique(dta$GEOG),8,replace=F)
# ggplot(dta[is.element(dta$GEOG,sample_state),],
#        aes(x=GEOG, y=logcrime, fill=GEOG)) +geom_boxplot() + scale_fill_brewer(palette="Greens")+
#   labs(title="Crime levels by neighborhoods in Chicago",
#        x="Nieghborhoods",y="Log crime") + 
#   theme(legend.position="none",axis.text.x = element_text(angle = 0)) #Differences are seen across cca
# 
# #Are there any variations in crime by time?
# str(dta)
# set.seed(1000)
# #sample_state <- sample(unique(dta$timeperiod),20,replace=F)
# ggplot(dta,aes(x=timeperiod, y=logcrime, fill=timeperiod)) +geom_boxplot() + scale_fill_brewer(palette="Greens")
#   labs(title="Log crime levels by CCA",
#        x="CCA",y="Log crime") + theme_classic() +
#   theme(legend.position="none",axis.text.x = element_text(angle = 0)) #Differences are seen across time
# 
# ###########PREDICTOR V RESPONSES
# 
# dta$logcoffee=log(dta$coffeeshops+1)
# str(dta)
# 
# #coffee shops -- taking log
# ggplot(dta,aes(x=log(coffeeshops), y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="x vs y") #linear, can be used
# 
# 
# ggplot(dta,aes(x=logcoffee, y=logcrime)) +
#   geom_point(alpha = .5,colour="indianred") +
#   xlim(0.01, 6.47)+
#   geom_smooth(method="lm",col="palegreen4") +
#   labs(title="Log crime vs Log Coffee Shops",x="Log Coffee Shops",y="Log crime") #
# 
# describe(dta$logcoffee)
# 
# set.seed(24)
# sample_state <- sample(unique(dta$GEOG),4,replace=F)
# ggplot(dta[is.element(dta$GEOG,sample_state),],aes(x=coffeeshops, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") + 
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="Crime vs Coffeeshops by neighborhoods") +
#   facet_wrap( ~ GEOG,ncol=4,scales="free")
# 
# 
# #unemployment -- taking log
# ggplot(dta,aes(x=UnemployedPerc, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="x vs y") #linear, can be used
# 
# 
# #100k -- taking log
# ggplot(dta,aes(x=Inc100k, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="x vs y") #linear, can be used
# 
# #WHITE -- taking log
# ggplot(dta,aes(x=WHITE, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="x vs y") #linear, can be used
# 
# 
# ###coffee for random sample of CCA
# 
# 
# 

###white for random sample of CCA
# 
# set.seed(1312)
# sample_state <- sample(unique(dta$GEOG),12,replace=F)
# ggplot(dta[is.element(dta$GEOG,sample_state),],aes(x=WHITE, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") + 
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="x vs y") +
#   facet_wrap( ~ GEOG,ncol=4,scales="free")
# 
# 
# ###unemployment for random sample of CCA
# 
# sample_state <- which(dta$WHITE> 10000)
# sample_state <- sample(unique(dta$GEOG),12,replace=F)
# ggplot(dta[is.element(dta$GEOG,sample_state),],aes(x=UnemployedPerc, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") + 
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="x vs y") +
#   facet_wrap( ~ GEOG,ncol=4,scales="free")
# 
# 
# ################ RANDOM INTERCEPTS BY CCA #####################
# 
# dta$logcoffee=log(dta$coffeeshops+1)
# 
# ggplot(dta,aes(logcoffee)) +
#   geom_histogram(aes(y=..density..),color="black",linetype="dashed",
#                  fill=rainbow(15),bins=15) + theme(legend.position="none") +
#   geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
#   labs(title="Distribution of Crime",y="n") + theme_classic() #Taking transformation to fit normal assumption model better
# 
# #numeric:logcoffee
# 
# ggplot(dta,aes(x=logcrime, y=coffeeshops)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="ppm vs mgstr") #linear, can be used
# 
# ggplot(dta,aes(x=logcoffee, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="ppm vs mgstr") #linear, can be used
# 
# summary(lm(logcrime~coffeeshops+GEOG+timeperiod,  data=dta))
# 
# #####looking at interactions
# 
# ggplot(dta,aes(x=logcrime, y=UnemployedPerc)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_minimal() +
#   labs(title="ppm vs mgstr") #linear, can be used
# 
# 
# library("dplyr")
# library("Hmisc")
# 
# dta$quartile <- with(dta, cut(UnemployedPerc, 
#                                 breaks=quantile(UnemployedPerc, probs=seq(0,1, by=0.25)
#                                 include.lowest=FALSE)))
# 
# dta$quartile <- with(dta, cut(UnemployedPerc, 
#                                 breaks=quantile(UnemployedPerc, probs=seq(0,1, by=0.25), na.rm=TRUE),labels=c("Quartile 1","Quartile 2","Quartile 3","Quartile 4"), 
#                                 include.lowest=TRUE))
# 
# sample_data2 <- filter(dta, quartile != "NA")
# ggplot(sample_data2,aes(x=logcoffee, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_classic() +
#   labs(title="Crime levels vs Coffee shops by Unemployment rate quartiles") +
#   #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
#   facet_wrap( ~ quartile) #looks similar, maybe some change in slope
# 
# 
# 
# 
# # ggplot(dta,aes(x=log(coffeeshops), y=logcrime)) +
# #   geom_point(alpha = .5,colour="blue4") +
# #   geom_smooth(method="lm",col="red3") + theme_minimal() +
# #   labs(title="x vs y") #
# 
# 
# 
# dta$non_white <- (dta$TOT_POP-dta$WHITE)/(dta$TOT_POP)
# 
# dta$quartile_black <- with(dta, cut(non_white, 
#                               breaks=quantile(BlackPerc, probs=seq(0,1, by=0.25), na.rm=TRUE),labels=c("Quartile 1","Quartile 2","Quartile 3","Quartile 4"), 
#                               include.lowest=TRUE))
# 
# 
# sample_data2 <- filter(dta, quartile_black != "NA")
# 
# ggplot(sample_data2,aes(x=logcrime, y=logcoffee, fill=quartile)) +
#   geom_boxplot(outlier.shape=NA) +
#   scale_fill_brewer(palette="Blues") + coord_flip()
# #scale_y_continuous(limits = c(0, 0.25))+
# labs(title="Interaction: Bulk Purchase and Source",x="",y="") + 
#   theme_classic() + theme(legend.position="left") #see difference in medians as well as change in trend
# 
# 
# 
# 
# sample_data2 <- filter(dta, quartile_black != "NA")
# ggplot(sample_data2,aes(x=logcoffee, y=logcrime)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") + theme_classic() +
#   labs(title="Crime levels vs Coffee shops by Proportion of Non-whites in quartiles") +
#   #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
#   facet_wrap( ~ quartile_black) #looks 




dtareq= dta[, c('timeperiod', 'GEOG','coffeeshops',"AsianPerc","BlackPerc",
                "HispPerc","WhitePerc","UnemployedPerc","EmployedPerc", "Inc25k",
                "Inc50k","Inc75k","Inc100k",'POP_HH','TOT_POP','logcrime')]
dtareq <- na.omit(dtareq)

dtareq$AsianPerc=dtareq$AsianPerc-mean(dtareq$AsianPerc)
dtareq$BlackPerc=dtareq$BlackPerc-mean(dtareq$BlackPerc)
dtareq$HispPerc=dtareq$HispPerc-mean(dtareq$HispPerc)
dtareq$WhitePerc=dtareq$WhitePerc-mean(dtareq$WhitePerc)

dtareq$UnemployedPerc=dtareq$UnemployedPerc-mean(dtareq$UnemployedPerc)
dtareq$EmployedPerc=dtareq$EmployedPerc-mean(dtareq$EmployedPerc)

dtareq$Inc25k=dtareq$Inc25k-mean(dtareq$Inc25k)
dtareq$Inc50k=dtareq$Inc50k-mean(dtareq$Inc50k)
dtareq$Inc75k=dtareq$Inc75k-mean(dtareq$Inc75k)
dtareq$Inc100k=dtareq$Inc100k-mean(dtareq$Inc100k)



#all_interactions=labels(terms(~.^2, data = dtareq[, 3:16]))

data = dtareq[, 3:16]
Model1<- lm(logcrime~coffeeshops,  data=data )
Model1_inter <- lm( logcrime ~ .^2, data=data)
Model_stepwise <- step(Model1, scope = formula(Model1_inter),direction="forward",trace=0)
Model_stepwise$call
summary(Model_stepwise)
plot(Model_stepwise)


library(stargazer)
stargazer(Model_stepwise)


stargazer(Model_stepwise,title= "Results", header=FALSE,font.size = "tiny", type='html',digits = 2,no.space = TRUE,column.sep.width = "3pt",single.row=TRUE)

anova(Model1,Model_stepwise) #keep interaction


##### do we need varying slopes?
set.seed(1000)
sample_state <- sample(unique(dta$GEOG),8,replace=F)
ggplot(dta[is.element(dta$GEOG,sample_state),],
       aes(x=logcrime, y=coffeeshops, fill=UnemployedPerc)) +
  geom_point() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Log price vs source by state",
       x="source",y="Log Price") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ GEOG,ncol=4)

#############################

dim(dta)
x <- na.omit(dta)
dim(x)



center(x$WhitePerc)
dta$WhitePerc-mean(x$WhitePerc)



library(lattice)

library(lme4)
Model2 <- lmer(logcrime ~ coffeeshops +  WhitePerc + TOT_POP + BlackPerc + 
                 UnemployedPerc + Inc25k + Inc50k + POP_HH + HispPerc + Inc100k + 
                 WhitePerc:BlackPerc + coffeeshops:Inc25k + TOT_POP:Inc25k + 
                 coffeeshops:TOT_POP + WhitePerc:TOT_POP + coffeeshops:WhitePerc + 
                 Inc25k:Inc50k + TOT_POP:POP_HH + TOT_POP:BlackPerc + WhitePerc:POP_HH + 
                 coffeeshops:UnemployedPerc + POP_HH:HispPerc + Inc50k:Inc100k + 
                 POP_HH:Inc100k + Inc25k:HispPerc + BlackPerc:HispPerc + (1 | GEOG) + (1 | timeperiod), data = dtareq)

Model2 <- lmer(logcrime ~ coffeeshops +  WhitePerc + TOT_POP + BlackPerc + 
                 UnemployedPerc + Inc25k + Inc50k + POP_HH + HispPerc + Inc100k + 
                 WhitePerc:BlackPerc + coffeeshops:Inc25k + TOT_POP:Inc25k + 
                 coffeeshops:TOT_POP + WhitePerc:TOT_POP + coffeeshops:WhitePerc + 
                 Inc25k:Inc50k + TOT_POP:POP_HH + TOT_POP:BlackPerc + WhitePerc:POP_HH + 
                 coffeeshops:UnemployedPerc + POP_HH:HispPerc + Inc50k:Inc100k + 
                 POP_HH:Inc100k + Inc25k:HispPerc + BlackPerc:HispPerc + (1 | GEOG) , data = dtareq)
anova(Model2a,Model2)




dotplot(ranef(Model2a,condVar=TRUE))$GEOG

summary(Model2)
plot(residuals(Model2))
qqnorm(residuals(Model2))



###############EDA


set.seed(1000)
sample_state <- sample(unique(dta$state),8,replace=F)




str(dta)
ggplot(dta,
       aes(x=logcoffee, y=logcrime, fill=timeperiod)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Crime levels vs coffee shops across time",
       x="Log Coffee",y="Log Crime") + theme(legend.position="none") +
  facet_wrap( ~ timeperiod,ncol=4)

set.seed(100)
sample_state <- sample(unique(dta$GEOG),8,replace=F)
ggplot(dta[is.element(dta$GEOG,sample_state),],
       aes(x=UnemployedPerc, y=logcrime, fill=GEOG)) +geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Crime levels vs Coffee shops by neighborhood",
       x="Community Area",y="Log Crime")  +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) +
  facet_wrap( ~ GEOG,ncol=4,scales="free")



set.seed(10000)
sample_state <- sample(unique(dta$GEOG),8,replace=F)



x=dta %>% 
  filter(GEOG =="North Center"|GEOG=="Norwood Park"|GEOG=="Lincoln Square"|GEOG=="West Lawn"
         |GEOG=="Montclare"|GEOG=="Garfield Ridge"|GEOG=="West Ridge"|GEOG=="Near West Side")
           
ggplot(x,aes(x=logcoffee, y=logcrime, fill=GEOG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  scale_fill_brewer(palette="Greens") +
  labs(title="Crime levels vs Coffee shops by neighborhood",
       x="Community Area",y="Log Crime")  +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) +
  facet_wrap( ~ GEOG,ncol=4,scales="free")


set.seed(7880)
sample_state <- sample(unique(dta$GEOG),8,replace=F)


ggplot(dta,aes(x=logppm, y=mgstr)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="ppm vs mgstr") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ source_new) #looks similar, maybe some change in slope



set.seed(1123)
sample_state <- sample(unique(dta$GEOG),8,replace=F)
ggplot(dta[is.element(dta$GEOG,sample_state),],aes(x=UnemployedPerc, y=logcrime, fill=GEOG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  scale_fill_brewer(palette="Greens") +
  labs(title="Crime levels vs Unemployment rates by neighborhood",
       x="Unemployment Rate",y="Log Crime")  +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) +
  facet_wrap( ~ GEOG,ncol=4,scales="free")




ggplot(dta[is.element(dta$GEOG,sample_state),],aes(x=UnemployedPerc, y=logcrime, fill=GEOG)) +geom_boxplot() +
  scale_fill_brewer(palette="Greens") +
  labs(title="Crime levels vs Coffee shops by neighborhood",
       x="Community Area",y="Log Crime")  +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) +
  facet_wrap( ~ GEOG,ncol=4,scales="free")









dta$non_white <- (dta$TOT_POP-dta$WHITE)/(dta$TOT_POP)
sample_data3 <- filter(dta, non_white != "NA")
sample_data3$non_white_quartile <- with(sample_data3, cut(non_white, 
                                        breaks=quantile(non_white, probs=seq(0,1, by=0.25), na.rm=TRUE),labels=c("Quartile 1","Quartile 2","Quartile 3","Quartile 4"), 
                                        include.lowest=TRUE))

set.seed(1123)
sample_state <- sample(unique(sample_data3$GEOG),8,replace=F)
ggplot(sample_data3[is.element(sample_data3$GEOG,sample_state),],
       aes(x=non_white, y=logcrime, fill=GEOG)) + geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  scale_fill_brewer(palette="Greens") +
  labs(title="Crime levels vs Non-white residents by neighborhood",
       x="Proportion of non white residents",y="Log Crime")  +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) +
  facet_wrap( ~ GEOG,ncol=4,scales="free")














ggplot(sample_data2,aes(x=logcrime, y=logcoffee, fill=quartile)) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_brewer(palette="Blues") + coord_flip()
#scale_y_continuous(limits = c(0, 0.25))+
labs(title="Interaction: Bulk Purchase and Source",x="",y="") + 
  theme_classic() + theme(legend.position="left") #see difference in medians as well as change in trend




sample_data2 <- filter(dta, quartile_black != "NA")
ggplot(sample_data2,aes(x=logcoffee, y=logcrime)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Crime levels vs Coffee shops by Proportion of Non-whites in quartiles") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ quartile_black) #looks 



