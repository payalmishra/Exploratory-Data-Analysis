################################################################
################################################################
################################################################
################ DATA EXPLORATION/ANALYSIS #####################
################################################################
################################################################
################################################################

require(Hmisc)
require(ggplot2)
require(useful)
require(BBmisc)
require(class)
require(dplyr)
require(corrplot)
require(PerformanceAnalytics)
require(psych)
require(car)
require(class)

getwd()
setwd("put working directory here")


########################################################################

# Q1 - Which city is the hottest??
# Q2 - Where are we seeing the maximum/minimum shortages??
# Q3 - In terms of average sales, which format is the most profitable??
# Q4 - For each format, calculate the Store performance??
# Q5 - In which format where we able to give the lowest average cost (= Sales/Units)
# Q6 - Is Sales correlated to any of the variables??
# Q7 - How do we figure out the competition’s strategy??

########################################################################

# Reading data
penta = read.csv("pentathalon.csv", header = T)
str(penta)
summary(penta)
View(penta)

# Looking at sales distribution overall
hist(penta$RS_SALES1)
median(penta$RS_SALES1, na.rm = T)
# 140,961.5
mean(penta$RS_SALES1, na.rm = T)
# 307,279.2

ggplot(data = penta, aes(x=WEEK, y = RS_SALES)) + geom_line(aes(colour = CITY))

# Subsetting data and checking individual-level distribution
mega = subset(penta, FORMAT == 'MEGABOX')
str(mega)
medium = subset(penta, FORMAT == 'MEDIUMBOX')
mini = subset(penta, FORMAT == 'MINIBOX')

hist(mega$RS_SALES1)
median(mega$RS_SALES1, na.rm = T) # 620880.1
mean(mega$RS_SALES1, na.rm = T) # 668812.5

hist(medium$RS_SALES1)
median(medium$RS_SALES1, na.rm = T) # 128457.8
mean(medium$RS_SALES1, na.rm = T) # 130135.6

hist(mini$RS_SALES1)
median(mini$RS_SALES1, na.rm = T) # 28354.49
mean(mini$RS_SALES1, na.rm = T) # 30694.7


## PERFORMING IMPUTATIONS INDEPENDENT OF FORMATS

penta$RS_SALES1[penta$FORMAT == 'MEGABOX'] = impute(penta$RS_SALES1[penta$FORMAT == 'MEGABOX'], median)
View(penta)
penta$RS_SALES1[penta$FORMAT == 'MEDIUMBOX'] = impute(penta$RS_SALES1[penta$FORMAT == 'MEDIUMBOX'], mean)
penta$RS_SALES1[penta$FORMAT == 'MINIBOX'] = impute(penta$RS_SALES1[penta$FORMAT == 'MINIBOX'], median)

# Starting EDA TO understand individual data elements
# SALES
ggplot(data = penta, aes(x=WEEK, y = RS_SALES)) + geom_line(aes(colour = CITY))
ggplot(data = mega, aes(x=WEEK, y = RS_SALES)) + geom_line(aes(colour = CITY))
ggplot(data = mini, aes(x=WEEK, y = RS_SALES)) + geom_line(aes(colour = CITY))
ggplot(data = medium, aes(x=WEEK, y = RS_SALES)) + geom_line(aes(colour = CITY))

# Temperature
ggplot(data = penta, aes(y = WEEK_MAX_TEMP1, x = CITY)) + 
  geom_jitter() + geom_violin()
ggplot(data = penta, aes(x= WEEK, y = WEEK_MAX_TEMP1)) + 
  geom_line(aes(colour = CITY))

# Rain
ggplot(data = penta, aes(x= WEEK, y = RAIN_MM)) + 
  geom_line(aes(colour = CITY))

# SHORTAGE
ggplot(data = penta, aes(x= WEEK, y = SHORTAGE1)) + 
  geom_line(aes(colour = CITY))
ggplot(data = penta, aes(y=SHORTAGE1, x=CITY)) + geom_boxplot()

# FORMAT
ggplot(data = penta, aes(y = RS_SALES, x = FORMAT)) + 
  geom_boxplot()


# Q3 - In terms of average sales, which format is the most profitable??
penta %>% group_by(FORMAT) %>% summarize(AvgSales = mean(RS_SALES))

# how do you order this by AvgSales?
penta %>% group_by(FORMAT) %>% summarize(AvgSales = mean(RS_SALES),
                                         MaxSales = max(RS_SALES),
                                         MinSales = min(RS_SALES))


# Q4 - For each CITY, calculate the Store performance??
penta %>% group_by(FORMAT, CITY) %>% summarize(AvgSales = mean(RS_SALES),
                                         MaxSales = max(RS_SALES),
                                         MinSales = min(RS_SALES))

SalesPerf = penta %>% group_by(FORMAT, CITY) %>% summarize(AvgSales = mean(RS_SALES),
                                                           MaxSales = max(RS_SALES),
                                                           MinSales = min(RS_SALES))
head(SalesPerf)


# HOMEWORK: Q5 - In which format where we able to give the lowest average cost (= Sales/Units)

# Calculate AvgCost = RS_Sales/Unit_Sales

penta_avgcost = penta %>% mutate(AvgCost= RS_SALES/UNIT_SALES)
head(penta_avgcost)
penta = penta %>% mutate(AvgCost= RS_SALES/UNIT_SALES)
head(penta)


# Q6 - Is Sales correlated to any of the variables - Sales, Temp, Rain, Shortage??

sales_corr_df = penta %>% select(RS_SALES, WEEK_MAX_TEMP1, RAIN_MM, SHORTAGE1)
head(sales_corr_df)
cor(sales_corr_df)

# look at individual formats and see if things are different
sales_corr_mega_df = penta %>% select(RS_SALES, WEEK_MAX_TEMP1, RAIN_MM, SHORTAGE1) %>% filter(FORMAT == 'MEGABOX')

sales_corr_mega_df = penta %>% filter(FORMAT == 'MEGABOX') %>% select(RS_SALES, WEEK_MAX_TEMP1, RAIN_MM, SHORTAGE1)

head(sales_corr_mega_df)
cor(sales_corr_mega_df)

# Visualization of correlation charts
res2 = rcorr(as.matrix(sales_corr_mega_df))
corrplot(res2$r, type = "upper", order = "hclust")

## To understand the impact of the individual variables on Sales as a dependent variable
# OLS

lm1 = lm(penta$RS_SALES ~ penta$NEWYEAR + penta$VALENTINES + penta$DUSSEHRA +
           penta$DIWALI + penta$CHRISTMAS + penta$WEEK_MAX_TEMP1 +
           penta$RAIN_MM + penta$SHORTAGE1 + penta$POPULATION)
summary(lm1)

# Mega
lm_mega = lm(mega$RS_SALES ~ mega$NEWYEAR + mega$VALENTINES + mega$DUSSEHRA +
               mega$DIWALI + mega$CHRISTMAS + mega$WEEK_MAX_TEMP1 +
               mega$RAIN_MM + mega$SHORTAGE1 + mega$POPULATION)
summary(lm_mega)

# Medium
lm_medium = lm(medium$RS_SALES ~ medium$NEWYEAR + medium$VALENTINES + medium$DUSSEHRA +
                 medium$DIWALI + medium$CHRISTMAS + medium$WEEK_MAX_TEMP1 +
                 medium$RAIN_MM + medium$SHORTAGE1 + medium$POPULATION)
summary(lm_medium)


## Q 7: Will we be able to figure out Hexathlon's strategy??!!

hexa = read.csv("hexathalon.csv", header = T)
View(hexa)

# KNN

set.seed(378600)
# Training dataset
penta_small1 = penta %>% select(WEEK, RS_SALES, UNIT_SALES, NEWYEAR, VALENTINES,
                                DUSSEHRA, DIWALI, CHRISTMAS, FORMAT)
summary(penta_small1[,c(1:8)])
# Normalize data
penta_small1_n = normalize(penta_small1[,c(1:8)], 
                           method = "range", range=c(0,1))
summary(penta_small1_n)

pentaTrain = penta_small1_n
summary(pentaTrain)

# creating training dataset's target
pentaTrain_target = penta_small1[,9]
summary(pentaTrain_target)

pentaTest = read.csv("hexathalon.csv", header = T)
pentaTest_target = pentaTest[,9]
summary(pentaTest_target)
pentaTest = pentaTest[,c(1:8)]
summary(pentaTest)

pentaTest_n = normalize(pentaTest, method = "range", range = c(0,1))
summary(pentaTest_n)

model1 = knn(train = pentaTrain, test = pentaTest_n, 
             cl = pentaTrain_target, k = 3)
model1

# Confusion matrix
table(pentaTest_target, model1)
