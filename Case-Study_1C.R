#     Case Study 1

# INTRODUCTION
# From a couple datasets ie Beers and Breweries, we will endeavor to explore 
# this craft Beers data set and thereof we attempt to answer two questions:
# =>  Is there a direct correlation between IBU and ABV in beer?
# =>  Which states produce the most beer/have the most breweries?

# We R Coding in the embeded analysis hereof
library(rvest)
library(XML)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(maps)
library(mapdata)
library(tidyverse)

# Importing data files for study
Beers <- read.csv("C:/Users/Kadyebo/Desktop/Beers.csv")
 View(Beers)
Breweries <- read.csv("C:/Users/Kadyebo/Desktop/Breweries.csv")
 View(Breweries)
 Commonbeerstyles <- read.csv("C:/Users/Kadyebo/Desktop/Commonbeerstyles.csv")
 names(Commonbeerstyles)[1]<-"Style"
 View(Commonbeerstyles)
 
 # median
 
 MASTER_beer <- merge(Breweries,Beers,by="Brew_ID")
 str(MASTER_beer)
 
 # Make data frame of: Style, Average ABV, Avaerage IBU
 BeerStyleABVIBUtb_df <- data.frame(Beers)
 str(BeerStyleABVIBUtb_df)
 
 # Make clean table of Cast with two colomns and without non data
 BeerStyleABVIBUtb_df <- BeerStyleABVIBUtb_df[!apply(is.na(BeerStyleABVIBUtb_df[,]) | BeerStyleABVIBUtb_df == "", 1, all),]
 
 #Finding number of rows before and after cleaning
 nrow(BeerStyleABVIBUtb_df)
 NROW(na.omit(BeerStyleABVIBUtb_df))
 
 
 names(BeerStyleABVIBUtb_df)[6]<-paste("Style")
 names(BeerStyleABVIBUtb_df)[3]<-paste("Average ABV")
 names(BeerStyleABVIBUtb_df)[4]<-paste("Average IBU")
 View(BeerStyleABVIBUtb_df[1:1405,c(6,3,4),drop=FALSE])
 
 # Is There a Correlation?
 # The first thing we did to check for a correlation between IBU and ABV 
 # is create a scatterplot comparing the two. We plotted all the beers in 
 # our data set that had IBU values and added a "line of best fit" using 
 # local polynomial regression fitting. There appears to be a slight overall 
 # trend of increasing IBU ratings leading to an increase in ABV, but as you 
 # can see, there is way too much variance to call that a direct correlation 
 # between the two.
 
 # Here is "Scatter plot" with the confidence interval on correlation line.
 ggplot(BeerStyleABVIBUtb_df, aes(BeerStyleABVIBUtb_df$'Average IBU', BeerStyleABVIBUtb_df$'Average ABV')) + 
   geom_point(shape=18, color="blue")+
   geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="green")+
 xlab("IBU(Bitterness Rating)") + ylab("ABV(Alcohol Content)") +
   ggtitle("Correlation Between IBU and ABV in Beer") +
   theme(plot.title = element_text(hjust = 0.5))
 
 # We now want to breakdown our analysis and thereof explore IBU and ABV trends based on specific
 #styles of beer. We want also to see if styles of beer that had higher IBU ratings on average also 
 # had higher ABV ratings on average. However, since our data set had over 200 different styles of 
 # beer (many of which only appeared once or twice), we decided to focus our analysis on only the 
 # most st common styles of beer.
 
 # Locating info production data for common beer styles
 Commonbeerstyles_df <- data.frame(Commonbeerstyles)
 #setnames(Commonbeerstyles_df, "?..Style", "Style")
 names(Commonbeerstyles)[1]<-"Style"
 
 ggplot(Commonbeerstyles_df, aes(Commonbeerstyles_df$'Style', Commonbeerstyles_df$'Number_produced', colors=Style, fill=Style)) +
   geom_bar(alpha = 0.2, stat="identity") +
   xlab("Styles of Beer") + ylab("Number Produced") +
   ggtitle("Most commonly Produced Craft Beers") +
   theme(plot.title = element_text(hjust = 0.5)) +
   theme(axis.text.x = element_text(size=8,angle=90,hjust=.5,vjust=.5,face="plain"))
 
 #Now we take a look at the distribution of the Breweries accross USA
 
 BreweriesUSA_df <- data.frame(Breweries)
 ggplot(BreweriesUSA_df, aes(State, colors=State, fill=State)) +
   geom_dotplot(aes(fill = State), binwidth = 1.5) +
   ggtitle("Breweries Count Per State") +
   theme(plot.title = element_text(hjust = 0.5)) +
   theme(axis.text.x = element_text(size=6,angle=90,hjust=.5,vjust=.5,face="plain"))
 
 # USA map demostration of breweries distribution accross mainland USA
 
statesUSA <- map_data("state")
dim(statesUSA)
ggplot(data = statesUSA) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "Red") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) 

 # Computing the median alcohol content and international bitterness unit 
 # for each state, and then Plot a bar chart to compare.
Beers <- read.csv("C:/Users/Kadyebo/Desktop/Beers.csv")
names(Beers)[5]<-"Brew_ID"
MASTER_beer <- merge(Breweries,Beers,by="Brew_ID")

# median of median alcohol content Plot a bar chart
med.ABV<-tapply(MASTER_beer$ABV,MASTER_beer$State, median)
barplot(med.ABV)

 # median of international bitterness unit for each state Plot a bar chart
med.IBU<-tapply(MASTER_beer$IBU, MASTER_beer$State, median)
barplot(med.IBU)

 
 