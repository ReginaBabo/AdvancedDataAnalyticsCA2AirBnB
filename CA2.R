## set working directory
setwd("~/DBS/AdvancedDataAnalytics/Assignments/CA2-AirBnB")

## file downloaded from internet
## header removed
## read in file
airbnbtrain <- read.csv("train_users_2.csv", header = FALSE)

## check header
head(airbnbtrain)

## asssign column names
colnames(airbnbtrain) <- c('id', 'date_account_created', 'timestamp_first_active', 'date_first_booking', 'gender', 'age', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser', 'country_destination')
head(airbnbtrain)

## check structure of dataset
str(airbnbtrain)

## view the data
View(airbnbtrain)

## check summary of data
summary(airbnbtrain)

## create boxplot
boxplot(airbnbtrain)
## re-creart boxplot without id, 2xdate and timestamp columns
subset1 <- airbnbtrain[ , -which(names(airbnbtrain) %in% c("id", "date_account_created", "timestamp_first_active", "date_first_booking")) ]
boxplot(subset1)

## country v age
plot(airbnbtrain$country_destination, airbnbtrain$age)
## set age range and re-plot
airbnbtrain$age[airbnbtrain$age > 65] <- NA
airbnbtrain$age[airbnbtrain$age < 20] <- NA
plot(airbnbtrain$country_destination, airbnbtrain$age)

## re-create boxplot without age column
subset2 <- subset1[ , -which(names(subset1) %in% c("age")) ]
boxplot(subset2)
## boxplots for signup_flow and language are consistent with expectations.

## change NDF & other to NA
airbnbtrain$country_destination[airbnbtrain$country_destination == "NDF"] <- NA
airbnbtrain$country_destination[airbnbtrain$country_destination == "other"] <- NA
plot(airbnbtrain$country_destination, airbnbtrain$age)
## remove rows with NA values in country destination column
airbnbtrain <- airbnbtrain[-which(is.na(airbnbtrain$country_destination)), ]
## remove rows with NA values in age column
airbnbtrain <- airbnbtrain[-which(is.na(airbnbtrain$age)), ]

## remove rows with "other" values in gender column
airbnbtrain$gender[airbnbtrain$gender == "OTHER"] <- NA
airbnbtrain <- airbnbtrain[-which(is.na(airbnbtrain$gender)), ]

## check
View(airbnbtrain)

## histogram of age
## cast age to numerical type
airbnbtrain$age_int <- as.numeric(factor(airbnbtrain$age))
hist(airbnbtrain$age_int)
## histogram is normal distribution, positvely skewed, as expected

## plots
plot(airbnbtrain$country_destination)
## US is by far the most common country destination
## create a new data frame with the US destination removed
airbnbUSremoved <- airbnbtrain
airbnbUSremoved$country_destination[airbnbUSremoved$country_destination == "US"] <- NA
airbnbUSremoved <- airbnbUSremoved[-which(is.na(airbnbUSremoved$country_destination)), ]
## check & plot
View(airbnbtrain)
View(airbnbUSremoved)
plot(airbnbUSremoved$country_destination)
## France is the most popular non-US destination

levels(airbnbtrain$country_destination)
## need numerical values for analysis of variance calculations
## some heavy-lifitng required here!
airbnbtrain$country_int <- as.numeric(factor(airbnbtrain$country_destination))
airbnbtrain$country_int
## AU=1, CA=2, DE=3, ES=4, FR=5, GB=6, IT=7, NL=8, PT=9, US=10

## create subset & sample (n=100) for each country
AU <- subset(airbnbtrain, airbnbtrain$country_destination == "AU")
sampleAU <- AU[sample(nrow(AU),100), ]
CA <- subset(airbnbtrain, airbnbtrain$country_destination == "CA")
sampleCA <- CA[sample(nrow(CA),100), ]
DE <- subset(airbnbtrain, airbnbtrain$country_destination == "DE")
sampleDE <- DE[sample(nrow(DE),100), ]
ES <- subset(airbnbtrain, airbnbtrain$country_destination == "ES")
sampleES <- ES[sample(nrow(ES),100), ]
FR <- subset(airbnbtrain, airbnbtrain$country_destination == "FR")
sampleFR <- FR[sample(nrow(FR),100), ]
GB <- subset(airbnbtrain, airbnbtrain$country_destination == "GB")
sampleGB <- GB[sample(nrow(GB),100), ]
IT <- subset(airbnbtrain, airbnbtrain$country_destination == "IT")
sampleIT <- IT[sample(nrow(IT),100), ]
NL <- subset(airbnbtrain, airbnbtrain$country_destination == "NL")
sampleNL <- NL[sample(nrow(NL),100), ]
PT <- subset(airbnbtrain, airbnbtrain$country_destination == "PT")
samplePT <- PT[sample(nrow(PT),100), ]
US <- subset(airbnbtrain, airbnbtrain$country_destination=="US")
sampleUS <- US[sample(nrow(US),100), ]

## bind psuedo-random generated country sample sets into a new dataframe for ANOVA
DF <- rbind(sampleAU, sampleCA, sampleDE, sampleES, sampleFR, sampleGB, sampleIT, sampleNL, samplePT, sampleUS)

## convert age variable
DF$age_char <- as.character(factor(DF$age))

## view cleaned-up sample dataset
View(DF)
#summary(DF)
#nrow(DF)
#ncol(DF)

##  build model
## ignore language and signup_flow columns (refer to boxplot above)
modelaov <- aov(DF$country_int ~ DF$gender + DF$age_char + DF$signup_method + DF$affiliate_channel + DF$affiliate_provider + DF$first_affiliate_tracked + DF$signup_app + DF$first_device_type  + DF$first_browser, data = DF)
modelaov
summary(modelaov)

## One-way analysis of means
## H0 = no difference in country destination between genders
## Welchs correction built into the oneway test function
## Default is equal variances (i.e. homogeneity of variance) not assumed
## i.e. Welchs correction applied
## (and this explains why the df (which is normally k*(n-1)) is not a whole number in the output)
## not assuming equal variances
oneway.test(airbnbtrain$country_int ~ airbnbtrain$gender)
## p-value = 1.531e-05
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## gender is an influence on the choice for country of destination

## build linear model
modeltrain = lm(country_int ~ gender, data = airbnbtrain)
modeltrain
summary(modeltrain)

## for simplicity
## convert gender to numeric value, bisexual = 1, female = 2, male = 3
airbnbtrain$gender_int = as.numeric(factor(airbnbtrain$gender))


## One-way analysis of means
## H0 = no difference in country destination across ages
oneway.test(airbnbtrain$country_int ~ airbnbtrain$age)
## p-value < 2.2e-16
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## age is an influence on the choice for country of destination

## continue building model
modeltrain = lm(country_int ~ gender + age, data = airbnbtrain)
modeltrain
summary(modeltrain)

levels(airbnbtrain$signup_method)
## yippee! ... only 3 levels this time.
## need numerical values fof analysis of variance calculations
airbnbtrain$signupmethod_int <- as.numeric(factor(airbnbtrain$signup_method))
airbnbtrain$signupmethod_int
## basic=1, facebook=2, google=3
## One-way analysis of means
## H0 = no difference in country destination for signup method
oneway.test(airbnbtrain$country_int ~ airbnbtrain$signupmethod_int)
## p-value = 0.1125
## 0.05 < p therefore do NOT reject H0 (null hypothesis)
## signup method is NOT an influence on the choice for country of destination

airbnbtrain$signupflow_int <- as.numeric(factor(airbnbtrain$signup_flow))
airbnbtrain$signupflow_int
## One-way analysis of means
## H0 = no difference in country destination per signup flow
oneway.test(airbnbtrain$country_int ~ airbnbtrain$signupflow_int)
## Error = not enough observations
## this is consistent with our findings as outline above

levels(airbnbtrain$affiliate_channel)
## 8 levels this time.
## need numerical values fof analysis of variance calculations
airbnbtrain$affiliatechannel_int <- as.numeric(factor(airbnbtrain$affiliate_channel))
airbnbtrain$affiliatechannel_int
## One-way analysis of means
## H0 = no difference in country destination per affiliate channel
oneway.test(airbnbtrain$country_int ~ airbnbtrain$affiliatechannel_int)
## p-value < 2.2e-16
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## affiliate channel is an influence on the choice for country of destination

## continue building model
modeltrain = lm(country_int ~ gender + age + affiliate_channel, data = airbnbtrain)
modeltrain
summary(modeltrain)

## use in-built R function to confirm our model ... or otherwise
## start with all the numerical variables,
## then remove variables that are NOT significantly contributing to the model.
## must first convert age variable to be of type factor
airbnbtrain$age <- factor(airbnbtrain$age)
modeltrain.fitall <- lm(airbnbtrain$country_int ~ gender + age + signup_method + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = airbnbtrain)
summary(modeltrain.fitall)
final.modeltrain <- step(modeltrain.fitall)
summary(final.modeltrain)
levels(airbnbtrain$age)

## now, assume equal variances
## this function provides more information:
aov.out = aov(airbnbtrain$country_int ~ gender + age + affiliate_channel + 
                      first_affiliate_tracked + signup_app + first_device_type, data = airbnbtrain)
summary(aov.out)
## Df=(2,1,7), F-values=10.80, 53.99 & 27.26
## ... there is a lot of information under the hood too.
## structure of anova
str(aov.out)

## histogram of residuals (check normal distribution)
hist(aov.out$residuals)
## woah! ... defintely wasn't expecting that!
## plot q-q norm & line
airbnbtrain.fit <- lm(airbnbtrain$country_int ~ gender + age + affiliate_channel + 
                              first_affiliate_tracked + signup_app + first_device_type, data = airbnbtrain)

qqnorm(airbnbtrain.fit$residuals, ylab = "Residuals")
qqline(airbnbtrain.fit$residuals, col = "red")
## clearly, not normally distributed

## Lets try the Shapiro-Wilks normality test
## for the columns containing numeric values
## and remember we should want our non-normal data to fail the normality test
## cast relevant variables to numerical type
DF$gender_int <- as.numeric(factor(DF$gender))
DF$age_int <- as.numeric(factor(DF$age))
DF$affiliate_channel_int <- as.numeric(factor(DF$affiliate_channel))
DF$first_affiliate_tracked_int <- as.numeric(factor(DF$first_affiliate_tracked))
DF$signup_app_int <- as.numeric(factor(DF$signup_app))
DF$first_device_type_int <- as.numeric(factor(DF$first_device_type))
## check dataset to confirm column numbers
View(DF)
grep("country_int", colnames(DF))
DFshapiro <- DF[, c(17, 19, 20, 21, 22, 23, 24)]
str(DFshapiro)
shapiro.test(DF$country_int)
## p-value < 2.2e-16
## p < 0.05 indicates distribution is NOT normal

## therefore, consider non-parametric testing
## Kruskal Wallis Test One Way Anova by Ranks
## where dependent variable is numeric and independent variable is a factor 
kruskal.test(airbnbtrain$country_int ~ airbnbtrain$gender)
## chi-squared = 31.2494
## df = 2
## p-value = 1.638e-07
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## gender is an influence on the choice for country of destination
 
kruskal.test(airbnbtrain$country_int ~ airbnbtrain$age) 
## chi-squared = 312.9931
## df = 45
## p-value < 2.2e-16
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## age is an influence on the choice for country of destination

kruskal.test(airbnbtrain$country_int ~ airbnbtrain$signup_method) 
## chi-squared = 2.8368
## df = 2
## p-value = 0.2421
## 0.05 < p therefore do NOT reject H0 (null hypothesis)
## signup methoed does NOT significantly contribute to the choice of country of destination
 
kruskal.test(airbnbtrain$country_int ~ airbnbtrain$affiliate_channel) 
## chi-squared = 236.7392
## df = 7
## p-value < 2.2e-16
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## affiliate channel is an influence on the choice for country of destination

kruskal.test(airbnbtrain$country_int ~ airbnbtrain$affiliate_provider) 
## chi-squared = 211.6026
## df = 15
## p-value < 2.2e-16
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## affiliate provider is an influence on the choice for country of destination

kruskal.test(airbnbtrain$country_int ~ airbnbtrain$first_affiliate_tracked) 
## chi-squared = 56.4446
## df = 7
## p-value = 7.707e-10
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## first_affiliate_tracked is an influence on the choice for country of destination

kruskal.test(airbnbtrain$country_int ~ airbnbtrain$signup_app) 
## chi-squared = 162.762
## df = 3
## p-value < 2.2e-16
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## signup_app is an influence on the choice for country of destination

kruskal.test(airbnbtrain$country_int ~ airbnbtrain$first_device_type)
## chi-squared = 154.0747
## df = 8
## p-value < 2.2e-16
## p < 0.05 therefore reject H0 (null hypothesis) in favour of H1 (alternative hypothesis)
## first_device_type is an influence on the choice for country of destination