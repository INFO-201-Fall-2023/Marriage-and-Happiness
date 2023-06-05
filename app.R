library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(tidyverse)

# Data Cleaning -------------------------------------------------------------

df_1 <- read.csv("Happiness.csv")
df_2 <- read.csv("Marriage.csv")

#Data Cleaning
country <- c(df_2$Entity)
df_2$country <- country

#Data Joining
# df <- merge(x=df_2, y=df_1, by= "country", all.x = TRUE)

#Categorical Variable
min(df_2$Crude.marriage.rate..per.1.000.inhabitants.)
max(df_2$Crude.marriage.rate..per.1.000.inhabitants.)

df_2$categorical_variable <- ifelse(df_2$Crude.marriage.rate..per.1.000.inhabitants. <=0.5, "smaller or equal to 0.5",
                                    ifelse(df_2$Crude.marriage.rate..per.1.000.inhabitants. <=29.5, "smaller or equal to 29.5",
                                           ifelse(df_2$Crude.marriage.rate..per.1.000.inhabitants. <=59, "smaller or equal to 59")))  

#Continuous/Numerical Variable
range <- df_1$Whisker.high - df_1$Whisker.low
df_1$range <- range

#Summarization Data Frame
#summary(df)

df_2 <- df_2[df_2$Year> 2006 & df_2$Year< 2015,]
grouped_df <- group_by(df_2, country)
mar <- summarize(
  grouped_df,
  marriage = mean(Crude.marriage.rate..per.1.000.inhabitants.)
)

df_1 <- df_1[-c(5:6)]
df_2 <- df_2[-c(1:4)]
df_2 <- unique(df_2)
newdf <- left_join(df_2, mar)
df <- merge(newdf, df_1, by = "country")

avg_hap <- mean(df$Changes.in.happiness.scores)
avg_mar <- mean(df$marriage)

ordered_hap <- arrange(df, -Changes.in.happiness.scores)
most_hap_countries <- ordered_hap[1:5, "country"]
df.1 <- df[df$country==most_hap_countries[1] | df$country==most_hap_countries[2] | df$country==most_hap_countries[3] | df$country==most_hap_countries[4] | df$country==most_hap_countries[5],]

de <- data.frame("Average", 0, avg_mar, avg_hap, mean(df$Whisker.high), mean(df$Whisker.low), mean(df$range))
names(de) <- c("country", "categorical_variable", "marriage", "Changes.in.happiness.scores" , "Whisker.high", "Whisker.low", "range")

df.1 <- rbind(df.1, de)

plot1.1 <- ggplot(data=df.1, aes(x=country, y=Changes.in.happiness.scores)) +
  geom_bar(stat="identity")+
  ggtitle("Happiest Countries")+
  ylab("Changes in happiness scores")+
  xlab("Country")+
  labs(caption = " ")

plot1.2 <-ggplot(data=df.1, aes(x=country, y=marriage)) +
  geom_bar(stat="identity")+
  ggtitle("Happiest Countries' Marriage Rates")+
  ylab("Marriage Rate (per 1,000 inhabitants)")+
  xlab("Country")+
  labs(caption = " ")


ordered_mar <- arrange(df, -marriage)
most_mar_countries <- ordered_hap[1:5, "country"]
df.2 <- df[df$country==most_mar_countries[1] | df$country==most_mar_countries[2] | df$country==most_mar_countries[3] | df$country==most_mar_countries[4] | df$country==most_mar_countries[5],]
df.2 <- rbind(df.2, de)

plot2.1 <- ggplot(data=df.2, aes(x=country, y=marriage)) +
  geom_bar(stat="identity")+
  ggtitle("Countries with Highest Marriage Rates")+
  ylab("Marriage Rates")+
  xlab("Country")+
  labs(caption ="")

plot2.2 <- ggplot(data=df.2, aes(x=country, y=marriage)) +
  geom_bar(stat="identity")+
  ggtitle("Happiness of High-marriage Rate Countries")+
  ylab("Changes in Happiness Scores")+
  xlab("Country")+
  labs(caption =" ")

# UI ------------------------------------------------------------------


Tab_intro <- tabPanel("Introduction",
                      includeMarkdown("README.md"))

subTab1_hap <- tabPanel(
  "Top 5 Happiest Countries' Happiness",
  plot(plot1.1),
  p("Latvia had an exceptionally high increase from 2006-2015 with a 116% increase.
  The following four experienced roughly about the same increase, around 86%, which is still 
  much higher than the average of only 6%.")
)

subTab1_mar <- tabPanel(
  "Top 5 Happiest Countries' Marriage Rates",
  plot(plot1.2),
  p("However, the marriage rates are all very random, with ranges from 3.3 to 8.7.
  The average is 5.7, so this distribution is reflective of the average of other countries
  regardless of happiness rates.")
)

Tab1 <- tabPanel(
  "Happiest Country",
  radioButtons(
    inputId = "Guess1",
    label = "What country do you think is happiest?",
    choices = list("Latvia"=1,"Canada"=2,"Thailand"=3)
  ),
  textOutput(outputID = "hapAccurate"),
  subTab1_hap,
  subTab1_mar
)

subTab2_mar <- tabPanel(
  "Top 5 Countries with Highest Marriage Rates",
  plot(plot2.1),
  p("We can see that Mongolia has an exceptionally high marriage rate at 15.7 per 1000 inhabitants.
  For a population of of 3 million, 47,000 marriages occur in a year. The following countries are
  at around the same rate, 9. This is still much higher than the average of 5.7 marriages per
  1,000 inhbaitants. Russia is In the top 5 for both most married and happy, which is interesting.")
)

subTab2_hap <- tabPanel(
  "Happiness of the Top 5 Most Married Countries",
  plot(plot2.2),
  p("Their happiness score changes are quite high compared to the average of 0.06, a 6% increase.
  The lowest happiness score change of the 5 most-married countries is a 17.5% increase, still very high.
  Judging from just this perspective we could assume a weak correlation, but the previous conclusion
  proved otherwise. It is still unclear.")
)

Tab2 <- tabPanel(
  "Happiest Country",
  radioButtons(
    inputId = "Guess2",
    label = "What country do you think has the highest marriage rate?",
    choices = list("Russia"=1,"Mongolia"=2,"Brazil"=3)
  ),
  textOutput(outputID = "marAccurate"),
  subTab2_mar,
  subTab2_hap
)

# Tab3 <- LEAH HERE -----------------------
Tab3 <- tabPanel(
  "Marriage rates and Happiness rates", 
  inputId= "Your thought",
  label= "Were you suprised by the results?",
  choices = list ("yes"=1, "No"=2)
    )

ui <- navbarPage(
  title = "Marriage and Happiness", intro_tab, Tab1, Tab2
)

# SERVER -------------------------------------------------------------------------


# plot3 <- LEAH HERE -----------------------
plot <- ggplot (data=df.2, aes (x=country, y=marriage))
ggtitle("Happiness of High-marriage Rate Countries")+
  ylab("Changes in Happiness Scores")+
  xlab("Country")+
  labs(caption =" ")

server <- function(input, output){
  
  output$hapAccurate <- renderText({
    if(input$Guess1 == 1){
      return("You're right!")
    } else {
      return("Not quite")
    }
  })
  
  output$marAccurate <- renderText({
    if(input$Guess2 == 2){
      return("You're right!")
    } else {
      return("Not quite")
    }
  })
  
#not all the countries are included as data limited
 
}


shinyApp(ui = ui, server = server)