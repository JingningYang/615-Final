library(leaflet)
library(RColorBrewer)
#library(lattice)
library(dplyr)
library(scales)
library(shiny)
library(tidyverse)
library(leaflet.extras)
library(DT)
library(wordcloud)
library(wordcloud2)
library(shiny)
library(psych)
library(plotly)
library(reshape2)
business <- read.csv("business.csv")
att <- read.csv("attribute.csv")
att1 <- select(att, -state)
review <- read.csv("review.csv")
shinyUI(fluidPage(
    
    navbarPage("Exploring restaurants in state OH & PA",
               tabsetPanel(tabPanel("Interactive map", 
                                    fluidRow(
                                        column(3,
                                               selectInput("Res", "Select the Restaurant name", business$name)),
                                        column(2,
                                               selectInput("state1", "Select the State", business$state)),
                                        column(3,
                                               selectInput("star", "Select the rating stars", business$stars)),
                                        leafletOutput("map", width = "1000", height = "1000")
                                    )),
                           tabPanel("Data Exploration", 
                                    fluidRow(
                                        column(3,
                                               selectInput("Re", "Select the Restaurant", c("All",unique(as.character(business$name))))),
                                        column(2,
                                               conditionalPanel("input.Re"),
                                               selectInput("stat", "Select the state", c("All",unique(as.character(business$state))))),
                                        column(3,
                                               conditionalPanel("input.stat"),
                                               selectInput("sta", "Select the rating stars", c("All", unique(as.character(business$stars))))),
                                        hr(),
                                        DT::dataTableOutput("inf")
                                    )),
                           # tabPanel("EDA",
                           #          h3("Explore the relationship between review count with stars in different states:"),
                           #          plotlyOutput("eda")),
                           tabPanel("PCA",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h5("Because of there are correlation between each variables, we try PCA here to reduce the dimension so that we can explore potential relationship between variables."),
                                        sliderInput("factor", "Number of Factors", value = 0, min=0, max=length(att1))),
                                    mainPanel(plotOutput("result", width = "100%", height = "700px")))
                                    ),
                           tabPanel("Explore any preference", 
                                    h4("Try to solve this question, we need to use review.csv based on cleaned review.json. I merge review.csv and business.csv together and use it to do text mining for exploring any preference.
Before we do text mining, We try to figure out the top number of restaurants in every states: "),
                                    plotlyOutput("explore"),
                                    h3("we can see there is 1 top number of restaurants: Pizza restaurants."),
                                    h4("Therefore, we can make a guess:Pizza restaurants are popular in both state OH and PA") ),
                           tabPanel("Text mining",
                                  
                                    tabsetPanel( type = "tabs",
                                    tabPanel("Word Cloud",
                                             h3("For checking our guess, I do wordcloud to show the top highest frequency review word"),
                                             br(),
                                             h3("Featured reviews of restaurants with high stars(4~5) in different states:"),
                                             sliderInput("fre", "Top Word Frequency:", min=1, max=200, value=50),
                                             h4("Really slow to show the wordcloud, please wait for a while"),
                                             column(3,checkboxInput("pa", "State PA")),
                                             column(3,checkboxInput("oh", "State OH")),
                                             wordcloud2Output("wordcloud")),   
                                    tabPanel("Sentiment Analysis",
                                             h3("Performing sentiment analysis on the bigram review data of 2 states: "),
                                             h4("Based on plots, in general, review words in both states are more positive than negative, and that might because I only choose rating scores over 4.0."),
                                             h5("Also, we observe that the weight of positive review words for good restaurants in PA is more than in OH. That might because people in OH have higher demanding on food than people in PA."),
                                             selectInput("state", "Choose the state", c("PA", "OH")),
                                             plotOutput("sentiment", width = "100%", height = "600px")),   
                                    tabPanel("Sentiment Analysis comparison",
                                             h3("Do the sentiment analysis to tag positive and negative words using
                                                an inner join, then find the most common positive and negative words."),
                                             selectInput("state2", "Choose the state", c("PA", "OH")),
                                             plotOutput("compare")),
                                    tabPanel("Summary", 
                                             h3("Through previously analysis, we know that the top number of restaurants not represent the taste preference of people. And it shows the importance of the frequency of review word that illustrate what customers thought, feeling and focusing on. From previously text mining for 2 states, we know customers love ice cream, focus on customer service no matter which states they are coming from. "),
                                             br(),
                                             h2("Thus, maybe restaurants business can pay more attention on these general points, and improving more based on specific preference of different states.    ")))  )
                           
                           ))
))
