library(ggplot2)
#install.packages("maps")
library(maps)
#install.packages("GGally")
library(GGally)
#install.packages("rtweet")
library(rtweet)
#install.packages("syuzhet")
library(syuzhet)
#install.packages("tm")
library(tm)
#install.packages("wordcloud")
library(wordcloud)
library(plyr)
library(dplyr)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(shiny)
Score <- function()
{
happiness_2015 <- read.csv("C:/Users/DEEPESH/Desktop/2017.csv")
colnames(happiness_2015)<-c("Country","Region","Rank","Score","GDPPerCapita","Family",
                            "LifeExpectance","Freedom","Trust","Generosity","Residual")
all_countries <- map_data("world")
all_countries$Country <- all_countries$region
all_countries$Country <- tolower(all_countries$Country)
all_countries$Country <- as.factor(all_countries$Country)
happiness_2015$Country <- tolower(happiness_2015$Country)
happiness_2015$Country <- as.factor(happiness_2015$Country)
happiness_2015$Country <- revalue(happiness_2015$Country, c("united states"="usa", "united kingdom"="uk" , 
                                                            "congo (kinshasa)"= "democratic republic of the congo" ,
                                                            "congo (brazzaville)" = "republic of congo",
                                                            "palestinian territories"= "palestine",
                                                            "north cyprus"= "cyprus"))
all_countries$Country <- revalue(all_countries$Country , c("tobago" = "trinidad and tobago", 
                                                           "trinidad" = "trinidad and tobago"))
Total <- left_join(all_countries, happiness_2015, by = c("Country"))
g<-ggplot(data = Total, aes(x = long, y = lat, fill = Score, group = group)) +
  geom_polygon()+ labs(title = " World Map", x="", y="") + scale_fill_gradient(na.value = "grey") + labs(caption="*missing coutries in grey")
return(g)
}
GDP <- function()
{
  happiness_2015 <- read.csv("C:/Users/DEEPESH/Desktop/2017.csv")
  colnames(happiness_2015)<-c("Country","Region","Rank","Score","GDPPerCapita","Family",
                              "LifeExpectance","Freedom","Trust","Generosity","Residual")
  all_countries <- map_data("world")
  all_countries$Country <- all_countries$region
  all_countries$Country <- tolower(all_countries$Country)
  all_countries$Country <- as.factor(all_countries$Country)
  happiness_2015$Country <- tolower(happiness_2015$Country)
  happiness_2015$Country <- as.factor(happiness_2015$Country)
  happiness_2015$Country <- revalue(happiness_2015$Country, c("united states"="usa", "united kingdom"="uk" , 
                                                              "congo (kinshasa)"= "democratic republic of the congo" ,
                                                              "congo (brazzaville)" = "republic of congo",
                                                              "palestinian territories"= "palestine",
                                                              "north cyprus"= "cyprus"))
  all_countries$Country <- revalue(all_countries$Country , c("tobago" = "trinidad and tobago", 
                                                             "trinidad" = "trinidad and tobago"))
  Total <- left_join(all_countries, happiness_2015, by = c("Country"))
  g<-ggplot(data = Total, aes(x = long, y = lat, fill = GDPPerCapita, group = group)) +
    geom_polygon()+ labs(title = " World Map", x="", y="") + scale_fill_gradient(na.value = "grey", guide = "colourbar", low = 'blue', high = 'red') +labs(caption="*missing coutries in grey")
  return(g)
}
Family <- function()
{
  happiness_2015 <- read.csv("C:/Users/DEEPESH/Desktop/2017.csv")
  colnames(happiness_2015)<-c("Country","Region","Rank","Score","GDPPerCapita","Family",
                              "LifeExpectance","Freedom","Trust","Generosity","Residual")
  all_countries <- map_data("world")
  all_countries$Country <- all_countries$region
  all_countries$Country <- tolower(all_countries$Country)
  all_countries$Country <- as.factor(all_countries$Country)
  happiness_2015$Country <- tolower(happiness_2015$Country)
  happiness_2015$Country <- as.factor(happiness_2015$Country)
  happiness_2015$Country <- revalue(happiness_2015$Country, c("united states"="usa", "united kingdom"="uk" , 
                                                              "congo (kinshasa)"= "democratic republic of the congo" ,
                                                              "congo (brazzaville)" = "republic of congo",
                                                              "palestinian territories"= "palestine",
                                                              "north cyprus"= "cyprus"))
  all_countries$Country <- revalue(all_countries$Country , c("tobago" = "trinidad and tobago", 
                                                             "trinidad" = "trinidad and tobago"))
  Total <- left_join(all_countries, happiness_2015, by = c("Country"))
  g<-ggplot(data = Total, aes(x = long, y = lat, fill = Family, group = group)) +
    geom_polygon()+ labs(title = " World Map", x="", y="") + scale_fill_gradient(na.value = "grey", guide = "colourbar", low = 'yellow', high = 'brown') +labs(caption="*missing coutries in grey")
  return(g)
}
LifeExpectance <- function()
{
  happiness_2015 <- read.csv("C:/Users/DEEPESH/Desktop/2017.csv")
  colnames(happiness_2015)<-c("Country","Region","Rank","Score","GDPPerCapita","Family",
                              "LifeExpectance","Freedom","Trust","Generosity","Residual")
  all_countries <- map_data("world")
  all_countries$Country <- all_countries$region
  all_countries$Country <- tolower(all_countries$Country)
  all_countries$Country <- as.factor(all_countries$Country)
  happiness_2015$Country <- tolower(happiness_2015$Country)
  happiness_2015$Country <- as.factor(happiness_2015$Country)
  happiness_2015$Country <- revalue(happiness_2015$Country, c("united states"="usa", "united kingdom"="uk" , 
                                                              "congo (kinshasa)"= "democratic republic of the congo" ,
                                                              "congo (brazzaville)" = "republic of congo",
                                                              "palestinian territories"= "palestine",
                                                              "north cyprus"= "cyprus"))
  all_countries$Country <- revalue(all_countries$Country , c("tobago" = "trinidad and tobago", 
                                                             "trinidad" = "trinidad and tobago"))
  Total <- left_join(all_countries, happiness_2015, by = c("Country"))
  g<-ggplot(data = Total, aes(x = long, y = lat, fill = LifeExpectance, group = group)) +
    geom_polygon()+ labs(title = " World Map", x="", y="") + scale_fill_gradient(na.value = "grey", guide = "colourbar", low = 'yellow', high = 'orange') +labs(caption="*missing coutries in grey")
  return(g)
}
Freedom <- function()
{
  happiness_2015 <- read.csv("C:/Users/DEEPESH/Desktop/2017.csv")
  colnames(happiness_2015)<-c("Country","Region","Rank","Score","GDPPerCapita","Family",
                              "LifeExpectance","Freedom","Trust","Generosity","Residual")
  all_countries <- map_data("world")
  all_countries$Country <- all_countries$region
  all_countries$Country <- tolower(all_countries$Country)
  all_countries$Country <- as.factor(all_countries$Country)
  happiness_2015$Country <- tolower(happiness_2015$Country)
  happiness_2015$Country <- as.factor(happiness_2015$Country)
  happiness_2015$Country <- revalue(happiness_2015$Country, c("united states"="usa", "united kingdom"="uk" , 
                                                              "congo (kinshasa)"= "democratic republic of the congo" ,
                                                              "congo (brazzaville)" = "republic of congo",
                                                              "palestinian territories"= "palestine",
                                                              "north cyprus"= "cyprus"))
  all_countries$Country <- revalue(all_countries$Country , c("tobago" = "trinidad and tobago", 
                                                             "trinidad" = "trinidad and tobago"))
  Total <- left_join(all_countries, happiness_2015, by = c("Country"))
  g<-ggplot(data = Total, aes(x = long, y = lat, fill = Freedom, group = group)) +
    geom_polygon()+ labs(title = " World Map", x="", y="") + scale_fill_gradient(na.value = "grey", guide = "colourbar", low = 'light green', high = 'dark green') +labs(caption="*missing coutries in grey")
  return(g)
}
Trust <- function()
{
  happiness_2015 <- read.csv("C:/Users/DEEPESH/Desktop/2017.csv")
  colnames(happiness_2015)<-c("Country","Region","Rank","Score","GDPPerCapita","Family",
                              "LifeExpectance","Freedom","Trust","Generosity","Residual")
  all_countries <- map_data("world")
  all_countries$Country <- all_countries$region
  all_countries$Country <- tolower(all_countries$Country)
  all_countries$Country <- as.factor(all_countries$Country)
  happiness_2015$Country <- tolower(happiness_2015$Country)
  happiness_2015$Country <- as.factor(happiness_2015$Country)
  happiness_2015$Country <- revalue(happiness_2015$Country, c("united states"="usa", "united kingdom"="uk" , 
                                                              "congo (kinshasa)"= "democratic republic of the congo" ,
                                                              "congo (brazzaville)" = "republic of congo",
                                                              "palestinian territories"= "palestine",
                                                              "north cyprus"= "cyprus"))
  all_countries$Country <- revalue(all_countries$Country , c("tobago" = "trinidad and tobago", 
                                                             "trinidad" = "trinidad and tobago"))
  Total <- left_join(all_countries, happiness_2015, by = c("Country"))
  g<-ggplot(data = Total, aes(x = long, y = lat, fill = Trust, group = group)) +
    geom_polygon()+ labs(title = " World Map", x="", y="") + scale_fill_gradient(na.value = "grey", guide = "colourbar", low = 'pink', high = 'red') +labs(caption="*missing coutries in grey")
  return(g)
}

ui <- fluidPage(
  titlePanel("Happiness Factor Map"),
      selectInput(inputId = "Factor",
                  label = "Select Factor to plot",
                  choices = c("Family","Freedom","GDP","LifeExpectance","Score","Trust")),
      plotOutput("graph")
)

server <- function(input,output)
{
  output$graph <- renderPlot({
    if(input$Factor == "Family")
      Family()
    else if(input$Factor == "Freedom")
      Freedom()
    else if(input$Factor == "GDP")
      GDP()
    else if(input$Factor == "LifeExpectance")
      LifeExpectance()
    else if(input$Factor == "Score")
      Score()
    else if(input$Factor == "Trust")
      Trust()
  })
}

shinyApp(ui = ui, server = server)
