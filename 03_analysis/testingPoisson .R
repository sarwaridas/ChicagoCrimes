dta <- read.csv("/Users/sarwaridas/Desktop/IDS 702/Final Project/ids720_CoffeeAndCrimes/02_processed_data/coffee_crime_controls_revised.csv", header = TRUE,sep = ",",stringsAsFactors = FALSE)
colnames(dta)
dta= subset(dta, select = c(TOT_POP,timeperiod,GEOG,crimes,coffeeshops,AsianPerc,BlackPerc,HispPerc,WhitePerc,MED_AGE,UnemployedPerc,Inc25k,Inc50k,Inc75k,Inc100k,Inc150k,Inc_gt150k,HSperc,BACHperc)) 
f_cols= c('timeperiod','GEOG') #changing to factor dtype
dta[f_cols] = lapply(dta[f_cols], as.factor) #changing to factor dtype


#Pre-processing
dta$crimes= as.integer(dta$crimes)

dta$crimerate=(dta$crimes/dta$TOT_POP)
dta$logcrimerate=log(dta$crimerate)
dta$logcrime=log(dta$crimes)
dta$AsianPerc=dta$AsianPerc-mean(dta$AsianPerc, na.rm=TRUE)
dta$BlackPerc=dta$BlackPerc-mean(dta$BlackPerc,na.rm=TRUE)
dta$HispPerc=dta$HispPerc-mean(dta$HispPerc,na.rm=TRUE)
dta$WhitePerc1=dta$WhitePerc
dta$WhitePerc=dta$WhitePerc-mean(dta$WhitePerc,na.rm=TRUE)
dta$UnemployedPerc=dta$UnemployedPerc-mean(dta$UnemployedPerc,na.rm=TRUE)
dta$coffeeshops=log(dta$coffeeshops)

dta$Inc25k=dta$Inc25k-mean(dta$Inc25k,na.rm=TRUE)
dta$Inc50k=dta$Inc50k-mean(dta$Inc50k,na.rm=TRUE)
dta$Inc75k=dta$Inc75k-mean(dta$Inc75k,na.rm=TRUE)
dta$Inc100k=dta$Inc100k-mean(dta$Inc100k,na.rm=TRUE)
dta$Inc150k=dta$Inc150k-mean(dta$Inc150k,na.rm=TRUE)
dta$Inc_gt150k=dta$Inc_gt150k-mean(dta$Inc_gt150k,na.rm=TRUE)

dta$HSperc=dta$HSperc-mean(dta$HSperc,na.rm=TRUE)
dta$BACHperc=dta$BACHperc-mean(dta$BACHperc,na.rm=TRUE)

nulls = sapply(dta, function(col) sum(length(which(is.na(col)))))#; nulls #checking for nulls
dta_full= dta[!is.na(dta$TOT_POP),]

#distribution

# ggplot(dta,aes(logcrimerate)) +
#   geom_histogram(aes(y=..density..),color="black",linetype="dashed",bins=15,fill=mako(15)) +  theme(legend.position="none") +
#   geom_density(alpha=.25, fill="lightblue") +
#   labs(title="Figure X: Distribution of log (crimerate)",x="Crime rate per neighborhood") +theme(plot.title = element_text(hjust = 0.5))+ theme_bw() + 
#   theme(
#     plot.title = element_text(face = "bold", size = 12),
#     legend.background = element_rect(fill = "white", size = 4, colour = "green"),
#     legend.justification = c(0, 1),
#     legend.position = c(0, 1),
#     axis.ticks = element_line(colour = "grey70", size = 0.2),
#     panel.grid.major = element_line(colour = "grey70", size = 0.2),
#     panel.grid.minor = element_blank()
#   )
# 
# 
# ggplot(dta,aes(crimes)) +
#   geom_histogram(aes(y=..density..),color="black",linetype="dashed",bins=15,fill=mako(15)) +  theme(legend.position="none") +
#   geom_density(alpha=.25, fill="lightblue") +
#   labs(title="Figure X: Distribution of log (crimerate)",x="Crime rate per neighborhood") +theme(plot.title = element_text(hjust = 0.5))+ theme_bw() + 
#   theme(
#     plot.title = element_text(face = "bold", size = 12),
#     legend.background = element_rect(fill = "white", size = 4, colour = "green"),
#     legend.justification = c(0, 1),
#     legend.position = c(0, 1),
#     axis.ticks = element_line(colour = "grey70", size = 0.2),
#     panel.grid.major = element_line(colour = "grey70", size = 0.2),
#     panel.grid.minor = element_blank()
#   )

#SOME EDA
# 
# ggplot(dta, aes(x = coffeeshops, y = crimes)) +
#   geom_point(alpha = .5,colour="blue4") + theme_classic() +
#   geom_smooth(method="lm",col="red3") 
# #we have an obvious outlier
# ggplot(dta[(dta$crimes!=max(dta$crimes)),],
#        aes(x = coffeeshops, y = crimes)) +
#   geom_point(alpha = .5,colour="blue4") +
#   geom_smooth(method="lm",col="red3") 
# 
# 
# ggplot(dta, aes(x = GEOG, y = crimes,fill=GEOG)) +
#   geom_boxplot() + theme_classic() +
#   #scale_fill_brewer(palette="Blues") +
#   theme(legend.position="none") 
# 

#MODELS


library(lme4)
library(Metrics)
library(merTools)
library(nlme)
library(performance)



baseline_rate= lmer(logcrimerate ~ coffeeshops + WhitePerc + BlackPerc +
                 timeperiod  + Inc25k + Inc75k + Inc50k + 
                 WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k 
               +coffeeshops:Inc50k + (WhitePerc | GEOG), data = dta_full)

baseline_count= lmer(log(crimes) ~ coffeeshops + WhitePerc + BlackPerc +
                      timeperiod  + Inc25k + Inc75k + Inc50k + 
                      WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k 
                    +coffeeshops:Inc50k + (WhitePerc | GEOG), data = dta_full)

poisson_non_hierarchal=  glm(crimes ~ coffeeshops + WhitePerc + BlackPerc +
                               timeperiod  + Inc25k + Inc75k + Inc50k  +
                               WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k +coffeeshops:Inc50k,  
                             data=dta_full, family=poisson)

poisson_hierarchal_randIntercept=  glmer(crimes ~ coffeeshops + WhitePerc + BlackPerc +
                             timeperiod  + Inc25k + Inc75k + Inc50k  +
                             WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k +coffeeshops:Inc50k+ offset(log(TOT_POP))+ (1 | GEOG),  
                           data=dta_full, family="poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

poisson_hierarchal_randInterceptSlope=  glmer(crimes ~ coffeeshops + WhitePerc + BlackPerc +
                                         timeperiod  + Inc25k + Inc75k + Inc50k  +
                                         WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k +coffeeshops:Inc50k+ offset(log(TOT_POP))+ (WhitePerc | GEOG),  
                                       data=dta_full, family="poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

poisson_hierarchal_randInterceptSlope_noOffset=  glmer(crimes ~ coffeeshops + WhitePerc + BlackPerc +
                                                   timeperiod  + Inc25k + Inc75k + Inc50k  +
                                                   WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k +coffeeshops:Inc50k+ (WhitePerc | GEOG),  
                                                 data=dta_full, family="poisson",control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))


coef(poisson_hierarchal_randInterceptSlope)

summary(poisson_hierarchal_randInterceptSlope)$coefficients

dotplot(ranef(poisson_hierarchal_randInterceptSlope,condVar=TRUE))


AIC(baseline_rate)
AIC(baseline_count)
AIC(poisson_non_hierarchal)
AIC(poisson_hierarchal_randIntercept)
AIC(poisson_hierarchal_randInterceptSlope)
AIC(poisson_hierarchal_randInterceptSlope_noOffset)


#normalized = logical, should the result be returned on the scale of response variable standard deviations?
performance_rmse(baseline_rate,normalized=FALSE)
performance_rmse(baseline_count,normalized=FALSE) 
performance_rmse(poisson_non_hierarchal,normalized=FALSE)
performance_rmse(poisson_hierarchal_randIntercept,normalized=FALSE) 
performance_rmse(poisson_hierarchal_randInterceptSlope,normalized=FALSE) 
performance_rmse(poisson_hierarchal_randInterceptSlope_noOffset,normalized=FALSE) 


regresid1 <- resid(poisson_hierarchal_randIntercept, type = "pearson")
sqrt(mean(regresid1^2))
performance_rmse(poisson_hierarchal_randIntercept,normalized=FALSE) 


check_overdispersion(poisson_non_hierarchal) #Overdispersion detected.
check_overdispersion(poisson_hierarchal_randInterceptSlope) #Overdispersion detected.
check_overdispersion(poisson_hierarchal_randInterceptSlope_noOffset) #Overdispersion detected.



#negative binomial example

poisson_hierarchal_randInterceptSlope_noOffset2=  glmer(crimes ~ coffeeshops + WhitePerc + BlackPerc +
                                                         timeperiod  + Inc25k + Inc75k + Inc50k  +
                                                         WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k +coffeeshops:Inc50k+ (WhitePerc | GEOG),  
                                                       data=dta_full, family=MASS::negative.binomial(theta=1.75),control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))










#residuals and predictions
regresid1 <- resid(poisson_hierarchal_randInterceptSlope_noOffset, type = "pearson")
regpred1 <- predict(poisson_hierarchal_randInterceptSlope_noOffset,type="response")

#residuals vs fitted
library(ggplot2)
qplot(y=regresid1, x=regpred1,data=dta_full,col= timeperiod,geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals")




















dta_full$whitequartile <- with(dta_full, cut(WhitePerc,
                                        breaks=quantile(WhitePerc, probs=seq(0,1, by=0.25),
                                                        na.rm=TRUE),labels=c("Quartile 1","Quartile 2","Quartile 3","Quartile 4"), include.lowest=TRUE))



m2= lmer(logcrimerate ~ coffeeshops + WhitePerc + BlackPerc +
                      timeperiod  + Inc25k + Inc75k + Inc50k + coffeeshops*whitequartile+
                      WhitePerc:BlackPerc + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k 
                    +coffeeshops:Inc50k + (1 | GEOG), data = dta_full)

m1= lmer(logcrimerate ~ coffeeshops + whitequartile+coffeeshops*whitequartile+
                      timeperiod  + Inc25k + Inc75k + Inc50k + 
                      + coffeeshops:Inc25k +  coffeeshops:Inc75k+ timeperiod:Inc50k 
                    +coffeeshops:Inc50k + (1 | GEOG), data = dta_full)

summary(m2)

# library(tidyverse)
# #df %>% drop_na(non_white_quartile)
# 
# df= dta[!is.na(dta$non_white_quartile),]
# 



