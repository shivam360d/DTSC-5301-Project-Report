# Importing data

data <-read.csv("~/Downloads/train_users_2.csv")

head(data, n = 15)

# Checking dimensions of data

dim(data)

# Check if data frame is NULL

is.null(data)

data <- data[,-(1:4), drop= FALSE]

head(data)

# Checking Column wise null values

colSums(is.na(data))

#Inferences: Here colSums functions only detects values which are marked as NA values 
# hence showing only for column age. lets find out for other column missing values

# Checking structure of data
str(data)

# Checking summary of data
summary(data)

# Univariate analysis

# Checking column gender

str(data$gender)

# Checking missing values in column gender

summary(data$gender)

# Plotting barplot for different levels of column gender in data

# barplot((data$gender))

ggplot(data, aes(x=gender)) +
  geom_bar()

# Column Age
# Checking column age

str(data$age)

# Checking missing values in column age

summary(data$age)

# Checking value distribution of column age

hist(data$age)

# Inferences: In column age we have some bad values which shows age greater than 1000 years. 
# Hence, replacing null values and bad data with median values.

# Imputing missing values with median values

data$age[is.na(data$age)] <- median(data$age,na.rm = TRUE)

# Checking summary of age column
summary(data$age)

# Replacing values above 150 years to median value of column age

data$age[data$age > 150] <- median(data$age,na.rm = TRUE)

# Checking value distribution of column age
hist(data$age)

# Checking column signup_method
str(data$signup_method)

# Checking missing values in column signup_method

summary(data$signup_method)

# Plotting barplot for different levels of column signup_method in data

# barplot(summary(data$signup_method))

ggplot(data, aes(x=signup_method)) +
  geom_bar()

# Column - language
# Checking column language

str(data$language)

# Checking summary of language column
summary(data$language)

# Plotting barplot for different levels of column language in data

# barplot(summary(data$language))
ggplot(data, aes(x=language)) +
  geom_bar()

# Checking column affiliate_channel

str(data$affiliate_channel)

# Checking summary of affiliate_channel column
summary(data$affiliate_channel)

# Plotting barplot for different levels of column affiliate_channel in data

# barplot(summary(data$affiliate_channel))

ggplot(data, aes(x=affiliate_channel)) +
  geom_bar()

# Checking column affiliate_provider

str(data$affiliate_provider)

# Checking summary of affiliate_provider column
summary(data$affiliate_provider)

# Plotting barplot for different levels of column affiliate_provider in data

# barplot(summary(data$affiliate_provider))

ggplot(data, aes(x=affiliate_provider)) +
  geom_bar()


# Column - first_affiliate_tracked
# Checking column first_affiliate_tracked

str(data$first_affiliate_tracked)

# Checking summary of first_affiliate_tracked column
summary(data$first_affiliate_tracked)

# Plotting barplot for different levels of column first_affiliate_tracked in data
# barplot(summary(data$first_affiliate_tracked))

ggplot(data, aes(x=first_affiliate_tracked)) +
  geom_bar()

# Column - signup_app
# Checking column signup_app

str(data$signup_app)

# Checking summary of signup_app column
summary(data$signup_app)

# Plotting barplot for different levels of column signup_app in data

# barplot(summary(data$signup_app))

ggplot(data, aes(x=signup_app)) +
  geom_bar()

# Column - first_device_type
# Checking column first_device_type

str(data$first_device_type)

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

