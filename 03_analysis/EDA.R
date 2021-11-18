
library(dplyr)
library(forcats)
library(ggplot2)

dta <- read.csv("/Users/sarwaridas/Desktop/IDS 702/Final Project/ids720_CoffeeAndCrimes/02_processed_data/coffee_crime_controls.csv", header = TRUE,sep = ",",stringsAsFactors = FALSE)
dta <- subset(dta, select = -c(X,Year,Community.Area)) #removing index and yr column
str(dta)

f_cols= c('timeperiod','GEOG') #changing to factor dtype
dta[f_cols] = lapply(dta[f_cols], as.factor) #changing to factor dtype

levels(dta$timeperiod)
#dta=subset(dta, mgstr>1)


nulls = sapply(dta, function(col) sum(length(which(is.na(col))))); nulls #checking for nulls
summary(dta$crime)#crime has outliers - not removing them though


#########CONSIDER REMOVING OUTLIERS!


##INVESTIGATING OUTCOME OF INTEREST
ggplot(dta,aes(crimes)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Crime",y="n") + theme_classic() #Taking transformation to fit normal assumption model better
dta$logcrime=log(dta$crimes)

ggplot(dta,aes(logcrime)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Crimes",y="n") + theme_classic() 

###relatively better

#Are there any variations in crime by CCA?

#Let's see for a random sample
set.seed(1000)
sample_state <- sample(unique(dta$GEOG),20,replace=F)
ggplot(dta[is.element(dta$GEOG,sample_state),],
       aes(x=GEOG, y=logcrime, fill=GEOG)) +geom_boxplot() +
  labs(title="Log crime levels by CCA",
       x="CCA",y="Log crime") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) #Differences are seen across states

#Are there any variations in crime by time?

#Let's see for a random sample
set.seed(1000)
#sample_state <- sample(unique(dta$timeperiod),20,replace=F)
ggplot(dta,aes(x=timeperiod, y=logcrime, fill=timeperiod)) +geom_boxplot() +
  labs(title="Log crime levels by CCA",
       x="CCA",y="Log crime") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90)) #Differences are seen across time



################ RANDOM INTERCEPTS BY CCA #####################

dta$logcoffee=log(dta$coffeeshops)

ggplot(dta,aes(logcoffee)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Crime",y="n") + theme_classic() #Taking transformation to fit normal assumption model better

#numeric:logcoffee

ggplot(dta,aes(x=logcrime, y=coffeeshops)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_minimal() +
  labs(title="ppm vs mgstr") #linear, can be used

ggplot(dta,aes(x=logcoffee, y=logcrime)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_minimal() +
  labs(title="ppm vs mgstr") #linear, can be used

summary(lm(logcrime~coffeeshops+GEOG+timeperiod,  data=dta))

#####looking at interactions


dtareq= dta[, c('timeperiod', 'GEOG','coffeeshops',"AsianPerc","BlackPerc",
                "HispPerc","WhitePerc","UnemployedPerc","EmployedPerc", "Inc25k",
                "Inc50k","Inc75k","Inc100k",'POP_HH','TOT_POP','logcrime')]
str(dtareq)
#all_interactions=labels(terms(~.^2, data = dtareq[, 3:16]))
x <- na.omit(dtareq[, 3:16])
dim(x)
Model1<- lm(logcrime~coffeeshops,  data=x )
Model1_inter <- lm( logcrime ~ .^2, data=x)
Model_stepwise <- step(Model1, scope = formula(Model1_inter),direction="forward",trace=0)
Model_stepwise$call
summary(Model_stepwise)
anova(Model1,Model_stepwise) #keep interaction



