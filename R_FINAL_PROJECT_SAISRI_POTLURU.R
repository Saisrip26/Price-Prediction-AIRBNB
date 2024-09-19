#library's
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(tidyr)
library( gapminder)
library(MASS)
library(data.table)
library(onehot)
library(mltools)
library(EnvStats)
library(VIM)
library(Amelia)
library(utils)
library(graphics)
library(ggplot2)
library(naniar)
install.packages("ggcorrplot")
library(corrplot)
library(ggcorrplot)



# To read the train dataset without any blanks 
FULL_DATA = read.csv("/Users/saisripotluru/Desktop/Uni\ of\ ok/Stats/Project/advertising.csv",stringsAsFactors=TRUE)
View(FULL_DATA)

summary(FULL_DATA)


# to know the percentage of missingness in all the variables
FULL_DATA %>% mutate_all(is.na) %>% summarise_all(mean)

# the data doesnt have any missingness in it 

summary(FULL_DATA)

?summary


#missing value plot
gg_miss_var(FULL_DATA) + labs(y = "Look at all the missing ones")

#skewness of the data
skewness(FULL_DATA$TV) 
skewness(FULL_DATA$Radio)
skewness(FULL_DATA$Newspaper)
skewness(FULL_DATA$Sales)

#histograms of the features 
hist(FULL_DATA$TV ,breaks=100,main = "skewness of TV" ,xlab="TV")
hist(FULL_DATA$Radio ,breaks=100,main = "skewness of Radio" ,xlab="Radio")
hist(FULL_DATA$Newspaper ,breaks=100,main = "skewness of Newspaper" ,xlab="Newspaper")
hist(FULL_DATA$Sales ,breaks=100,main = "skewness of Sales" ,xlab="Sales")



#Correlation matrix and plot

corr<-cor(FULL_DATA)
ggcorrplot(corr,title="Correlation Plot")




#outliers plot
boxplot(FULL_DATA)
?boxplot

#dividing data to train and test
set.seed(123)
indf<-sample.int(2,nrow(FULL_DATA),replace=TRUE,prob=c(0.70,0.30))
# the training dataset which has the 70% of observations
training <- FULL_DATA[indf==1,]
# the testing dataset which has the 30% of the observations
testing <- FULL_DATA[indf==2,]

summary(training)
summary(testing)


#linear model
LinearModel1 <- lm(data = training,log(Sales) ~ TV+Radio+Newspaper)

# to predict the linear model with original one 
predictM1<-predict(LinearModel1,training)

# to calculate the rmse value for the test data for resampling 
rmse(log(training$Sales),predictM1)

# to predict the linear model with original one testing
predictM2<-predict(LinearModel1,testing)
head(predictM2)
# to calculate the rmse value for the test data for resampling 
rmse(log(testing$Sales),predictM2)
head(log(testing$Sales))
summary(LinearModel1)


plt.scatter(predictM2,testing$Sales)
plot(predictM2,testing$Sales)

plot(predictM2,testing$Sales,xlab="predicted",ylab="actual")
abline(a=0,b=1)


dev.off()


