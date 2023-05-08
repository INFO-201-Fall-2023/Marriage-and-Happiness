library(dplyr)
library(stringr)

df_1 <- read.csv("Happiness.csv")
df_2 <- read.csv("Marriage.csv")

#Data Cleaning
country <- c(df_2$Entity)
df_2$country <- country

#Data Joining
df <- merge(x=df_2, y=df_1, by= "country", all.x = TRUE)

#Categorical Variable
min(df$Crude.marriage.rate..per.1.000.inhabitants.)
max(df$Crude.marriage.rate..per.1.000.inhabitants.)

df$categorical_variable <- ifelse(df$Crude.marriage.rate..per.1.000.inhabitants. <=0.5, "smaller or equal to 0.5",
                                  ifelse(df$Crude.marriage.rate..per.1.000.inhabitants. <=29.5, "smaller or equal to 29.5",
                                         ifelse(df$Crude.marriage.rate..per.1.000.inhabitants. <=59, "smaller or equal to 59")))  

#Continuous/Numerical Variable
range <- df$Whisker.high - df$Whisker.low
df$range <- range

#Summarization Data Frame
summary(df)
