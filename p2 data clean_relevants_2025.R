setwd("C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/quantitative analysis")
getwd()
library(tidyverse)
library(haven)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(questionr)
library(pollster)
library(gmodels)
library(pivottabler)
library(viridis)
library(hrbrthemes)
library(gridExtra)
library(dplyr)
library(readxl)
library(extrafont)
library(scales)  # for date formatting
library(wordcloud)  
library(tm)
library(plotly)


#sampled posts_cwrowdtangle samples
mdata<-read.xlsx("C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection/cleaned_data_quantiative analysis.xlsx")
names(mdata)

##generate unique id codes for merged data
mdata$id<-c(1:111557)

#open revised file to remove extra pages
minus_data<-read.xlsx("page frequencies2.xlsx")
table(minus_data$acc)

#merge both files and drop unrelevant cases:
data<-left_join(mdata,minus_data, by= "Page.Name")

data<-filter(data, acc==1)  # leave only relevant cases
names(data)
table(data$acc)
data<-select(data, -acc, -Facebook.Id.y,  -link, -post.numbers) # remove duplicated vars

write.xlsx(data, "Cleaned_dataset_for_Analysis.xlsx")
