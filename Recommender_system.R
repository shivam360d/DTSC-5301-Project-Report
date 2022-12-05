#Question of Interest: What features reflect some pattern that would be useful
# to target relevant audience, so that the customer engagement can be increased.

# Data:
# Source: Data is taken from Kaggle: https://www.kaggle.com/competitions/airbnb-recruiting-new-user-bookings/data

# Dataset is a collection of users along with their demographics, web session 
# records, and some summary statistics. All the users in this dataset are from 
# the USA.

# id: user id
# date_account_created: the date of account creation
# timestamp_first_active: timestamp of the first activity, note that it can be earlier than??date_account_created or date_first_booking because a user can search before signing up
# date_first_booking: date of first booking
# gender
# age
# signup_method - Platform used for singup
# signup_flow: the page a user came to signup up from
# language: international language preference
# affiliate_channel: what kind of paid marketing
# affiliate_provider: where the marketing is e.g. google, craigslist, other
# first_affiliate_tracked: whats the first marketing the user interacted with before the signing up
# signup_app
# first_device_type
# first_browser
# country_destination

# Importing data
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
data <-read.csv("A://Data Science as a Field//DTSC-5301-Project-Report//train_users_2.csv")
head(data, n = 15)

# Checking dimensions of data
dim(data)

# dropping id and timestamp related features
data <- data[,-(1:4), drop= FALSE]

# Checking Column wise null values
colSums(is.na(data))

#Inferences: Here colSums functions only detects values which are marked as NA values 
# hence showing only for column age. lets find out for other column missing values

# Checking structure of data
str(data)

# Checking summary of data
summary(data)

# Distribution of Gender
ggplot(data, aes(x=gender)) +
  geom_bar(aes(fill=gender))

# Checking % missing values in column age
sum(is.na(data$age))*100/dim(data)[1]

# Checking summary statistics of age
summary(data$age)

# From the above statistics, we can see that the maximum value of age is 2014.
# Hence, we shall remove outliers/errors while analyzing the same

# Removing outliers in age
age_filtered = data %>% filter(age <= IQR(age,na.rm = TRUE)*1.5 + 50)

# Distribution of age with respect to gender
plt1 = ggplot(age_filtered,aes(x=age,na.rm=TRUE))+
  stat_density(aes(fill=gender))+
  ggtitle("Distribution of Age with respect to Gender")+
  theme(plot.title = element_text(hjust=0.5))

# Possible values in Signup method
unique(data$signup_method)

# Distribution of Signup method
ggplot(data, aes(x=signup_method)) +
  geom_bar(aes(fill=signup_method))

# Distribution of age with respect to signup_method
plt2 = ggplot(age_filtered,aes(x=age,na.rm=TRUE))+
  stat_density(aes(fill=signup_method))+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("Distribution of Age with respect to Signup-method")+
  theme(plot.title = element_text(hjust=0.5))

grid.arrange(plt1,plt2,nrow=2,ncol=1)

# Column
unique(data$language)

# Distribution of Languages
ggplot(data, aes(x=language)) +
  geom_bar()

#From the above bar plot, we can infer that the most of the data is for a single
#language, that is English

# Checking column affiliate_channel
unique(data$affiliate_channel)

# Checking column affiliate_provider
unique(data$affiliate_provider)

#Distribution affiliate_channel
plt1= ggplot(data, aes(x=affiliate_channel)) +
  geom_bar(aes(fill=signup_method),color='black',position="dodge")+
  ggtitle("Distribution of Channel with respect to SignUp method")+
  ylab("Count")+
  xlab("Affiliate Channel")+
  theme(plot.title = element_text(hjust=0.5))

#Distribution affiliate_provider
plt2 = ggplot(data, aes(x=affiliate_provider)) +
  geom_bar(aes(fill=signup_method),color='black',position="dodge")+
  ggtitle("Distribution of Provider with respect to SignUp method")+
  ylab("Count")+
  xlab("Affiliate Provider")+
  theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle=30,vjust=0.8))

grid.arrange(plt1,plt2,nrow=2,ncol=1)

# Column - first_affiliate_tracked
# Checking column first_affiliate_tracked

unique(data$first_affiliate_tracked)

# Plotting barplot for different levels of column first_affiliate_tracked in data
plt1 = data %>% 
  filter(first_affiliate_tracked != "") %>% 
  ggplot(aes(x=first_affiliate_tracked)) +
  geom_bar(aes(fill=signup_method))+
  ggtitle("First marketing user interacted before SignUp")+
  ylab("Count")+
  xlab("First Affiliate")+
  theme(plot.title = element_text(hjust=0.5))

# Checking column signup_app
unique(data$signup_app)

# Plotting barplot for different levels of sign_up in data
plt2 = ggplot(data=data,aes(x=signup_app)) +
  geom_bar(aes(fill=signup_method))+
  ggtitle("Application used for signup")+
  ylab("Count")+
  xlab("Application type")+
  theme(plot.title = element_text(hjust=0.5))

grid.arrange

# CHI Squared Analysis to determine the dependency of categorical variables

# Hypothesis test
# Significance Level: 0.05
# Ho: There is no relation between method used to signup and the type of app
# Ha: There is some relation between method used to signup and the type of app

data$signup_method = factor(data$signup_method)
data$signup_app = factor(data$signup_app)

contigency_table = table(data$signup_method,data$signup_app)
chisq.test(contigency_table)

# Here, p-value is 2.2e-16, that is lower than the significance level. Hence we
# can reject the null hypothesis and infer that there is some relation between
# signup_app and signup_method

# ===============================================================================

# Column - first_device_type
# Checking column first_device_type

unique(data$first_device_type)

# Checking summary of first_device_type column

summary(data$first_device_type)

# Plotting barplot for different levels of column first_device_type in data

# barplot(summary(data$first_device_type))

ggplot(data, aes(x=first_device_type)) +
  geom_bar()

# Checking column first_browser

str(data$first_browser)

# Checking summary of first_browser column

summary(data$first_browser)

# Plotting barplot for different levels of column first_browser in data

# barplot(summary(data$first_browser))

ggplot(data, aes(x=first_browser)) +
  geom_bar()

# Checking column country_destination

str(data$country_destination)

# Checking summary of country_destination column

summary(data$country_destination)

# Plotting barplot for different levels of column country_destination in data

# barplot(summary(data$country_destination))

ggplot(data, aes(x=country_destination)) +
  geom_bar()



#Bi-variate analysis
# Checking summary of data
summary(data)

# Defining function for two categorical column visualization

bivariate <- function(arg_1, arg_2) {
  count <- table(arg_1, arg_2)
  barplot(count,beside = TRUE,legend.text = TRUE)
}

# Plotting two categorical columns
bivariate(data$gender, data$country_destination)

# Plotting two categorical columns
#plot(data$country_destination, data$age)

# Plotting two categorical columns
bivariate(data$signup_method, data$country_destination)

# Plotting two categorical columns
bivariate(data$signup_flow, data$country_destination)

# Plotting two categorical columns
bivariate(data$language, data$country_destination)

# Plotting two categorical columns
bivariate(data$affiliate_channel, data$country_destination)

# Plotting two categorical columns
bivariate(data$affiliate_provider, data$country_destination)

# Plotting two categorical columns
bivariate(data$first_affiliate_tracked, data$country_destination)

# Plotting two categorical columns
bivariate(data$signup_app, data$country_destination)

# Plotting two categorical columns
bivariate(data$first_device_type, data$country_destination)

# Plotting two categorical columns
bivariate(data$country_destination, data$first_browser)

