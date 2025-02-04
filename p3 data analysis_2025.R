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
library(psych)
library(Hmisc)
library(corrplot)
library(scales)
library(patchwork)
library(ggExtra)
library(sjPlot)


#sampled posts_cwrowdtangle samples
data<-read.xlsx("Cleaned_dataset_for_Analysis.xlsx")
names(data)


#_________________________________________________________________________manipulation________________
# Convert Post.Created.Date to Date class if it's not already 
data <- data %>% mutate(Post.Created = str_extract(Post.Created, "\\d{4}-\\d{2}-\\d{2}"))

# remove extra vars
data <- data %>% select(-Image.Text, -Sponsor.Id, -Sponsor.Name, -Sponsor.Category, -Post.Created.Date)

#_____________________________________________________post publication by period - date__________________________________________
# Create the new post_date variable 
data <- data %>% mutate(post_date = case_when( Post.Created < as.Date("2020-02-26") ~ 1, 
                                               Post.Created >= as.Date("2020-02-26") & Post.Created <= as.Date("2022-02-24") ~ 2, 
                                               Post.Created > as.Date("2022-02-24") ~ 3 )) 
#post by periods
table(data$post_date)
ggplot(data, aes(x = factor(post_date))) + 
  geom_bar() + 
  labs(title = "პოსტების გავრცელების სიხშირე", x = "პერიოდი", y = "სიხშირე") +
  scale_x_discrete(labels = c("1" = "2015 წლის აგვისტო - 2020 წლის 25 თებერვალი", "2" = "2020 წლის 26 თებერვალი - 2022 წლის 23 თებერვალი", "3" = "2022 წლის 24 თებერვალი - 2024 წლის 1-ლი თებერვალი"))+ 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  theme_minimal()



##time series    -----  post frequencies 
df_counts <- data %>%  
  filter(!is.na(post_date)) %>%  # Remove rows with NA post_date
  group_by(Post.Created, post_date) %>%
  summarise(count = n_distinct(id))


ts<-df_counts %>% 
  ggplot( aes(x=Post.Created, y=count, group=post_date, color=post_date)) +
  geom_line(size=1.1) + 
  ggtitle("პოსტების გავრცელების სიხშირე") +
  theme_minimal() +
  labs(x = "თარიღი",
       y = "პოსტების გავრცელების სიხშირე",
       subtitle = "2015 წლის აგვისტო - 2024 წლის 1-ლი თებერვალი")+
  coord_cartesian(ylim = c(0, 40))+
  geom_smooth(method = "lm", se = FALSE) +
  theme(text = element_text(family= "BPG 2017 DejaVu Sans"),
        plot.title = element_text(size=24, face="bold", family="BPG 2017 DejaVu Sans Caps", hjust = 0),
        plot.subtitle = element_text(size=18, family="BPG 2017 DejaVu Sans", hjust=0),
        axis.text = element_blank(),
        #        axis.text = element_text(size=0.2, family="BPG 2017 DejaVu Sans", color = "black"),
        legend.position = "top",
        legend.box = "vertical",
        legend.key.width = unit(2, "cm"),
        legend.text = element_text(size=8, family="BPG 2017 DejaVu Sans")
  )
ts

ggplotly(ts)



#_____________________________________________________________________post created times
data <- data %>% mutate(Post.Created.Time = format(as.POSIXct(Post.Created.Time, format="%H:%M:%OS"), format="%H:%M"))

time_freq<-data %>% count(Post.Created.Time) %>% arrange(desc(n))
write.xlsx(time_freq, "post times.xlsx")



#_____________________________________________________________________feedback___INteraction
#descriptives:
# Select the columns
interaction<-data %>% select(Total.Interactions, Likes, Comments, Shares, Love, Wow, Haha, Sad, Angry, Care)

psych::describe(interaction)
#write.xlsx(psych::describe(interaction), "interaction descriptives.xlsx")
freq(data$Total.Interactions)
hist(data$Total.Interactions)

quantile(data$Total.Interactions, probs = 0.25)
quantile(data$Total.Interactions, probs = 0.50)
quantile(data$Total.Interactions, probs = 0.75)
quantile(data$Total.Interactions, probs = 0.95)


# Calculate the Pearson correlation matrix
correlation_matrix <- cor(interaction, method = "pearson")

# Visualize the correlation matrix with corrplot
corrplot(cor(interaction, method = "pearson"), method = "number")



##All Time series    -----frequencies 

# Convert Post.Created to Date type
data$Post.Created <- as.Date(data$Post.Created, format = "%Y-%m-%d")

# Select the necessary columns in the specified order
ts_data <- data %>% select(Post.Created, Total.Interactions, Likes, Comments, Shares, Love, Wow, Haha, Sad, Angry, Care)

# Reshape the data for ggplot
long_data <- ts_data %>%
  gather(key = "Variable", value = "Value", -Post.Created)

# Plot for Total.Interactions with its own y-axis limits and adjusted x-axis labels
plot_total_interactions <- ggplot(long_data %>% filter(Variable == "Total.Interactions"), aes(x = Post.Created, y = Value, color = Variable)) +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE, color = "black") + # Add black trend line
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + # Show only years on x-axis
  labs(title = "Total Interactions",
       x = "Date",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none") + # Remove legend to avoid redundancy
  ylim(0, max(long_data$Value[long_data$Variable == "Total.Interactions"]))

# Plot for other variables with shared y-axis limits and adjusted x-axis labels
plot_other_variables <- ggplot(long_data %>% filter(Variable != "Total.Interactions"), aes(x = Post.Created, y = Value, color = Variable)) +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE, color = "black") + # Add black trend line
  facet_wrap(~ factor(Variable, levels = c("Likes", "Comments", "Shares", "Love", "Wow", "Haha", "Sad", "Angry", "Care")), scales = "free_y", ncol = 3) + # Order and distribute as 5x2 grid
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + # Show only years on x-axis
  labs(title = "Interactions",
       x = "Date",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine the plots using patchwork
combined_plot <- plot_total_interactions / plot_other_variables
combined_plot





#____________________________________________________Regresion____________________________________________
## total interaction model

#data recoding:
# in which country is this page-group based?
data$adm_country <-data$Page.Admin.Top.Country
data$adm_country [data$adm_country!="GE"]<-"სხვა ქვეყანა"
data$adm_country [data$adm_country=="GE"]<-"საქართველო"
data$adm_country [is.na(data$Page.Admin.Top.Country)]<-NA

table(data$adm_country)
table(data$Page.Admin.Top.Country)

#post-date period
data$post_date_f<-data$post_date
data$post_date_f<-factor(data$post_date_f, levels = c(1,2,3), labels = c("2015 აგვისტო - 2020, 25 თებერვალი", "2020, 26 თებერვალი - 2022, 23 თებერვალი","2022, 24 თებერვალი - 2024, 1-ლი თებერვალი"))
table(data$post_date_f)



#post type
data$post_type_r<-""
data$post_type_r[data$Type=="Link"]<-1
data$post_type_r[data$Type=="Native Video"]<-2
data$post_type_r[data$Type=="Video"]<-3
data$post_type_r[data$Type=="YouTube"]<-3
data$post_type_r[data$Type=="Live Video Complete"]<-4
data$post_type_r[data$Type=="Photo"]<-5
data$post_type_r[data$Type=="Status"]<-6

data$post_type_r<-factor(data$post_type_r, levels = c(1,2,3,4,5,6), labels = c("გაზიარებული ბმული","საავტორო ვიდეო","გაზიარებული ვიდეო","ლაივ ვიდეო","ფოტო","სტატუსი"))
table(data$post_type_r)
table(data$Type)


#linear model - interactions
# Fit multiple linear regression model
model <- lm(Total.Interactions ~ post_date_f + adm_country + post_type_r, data = data)

# Summarize the model
summary(model)

p2<-sjPlot::plot_model(model,
                       title = "ჯამური ინტერაქცია - გამოქვეყნების პერიოდის, ფეისბუქგვერდის დარეგისტრირების ადგილისა და  პოსტის ტიპის მიხედვით (მრავლობითი წრფივი რეგრესიის შედეგები)",
                       colors = c("seagreen3", "deepskyblue2", "orchid"),
                       show.values = TRUE,
                       value.offset = .4,
                       value.size = 6,
                       dot.size = 4,
                       line.size = 2,
                       vline.color = "red",
                       width = 0.5,
                       axis.labels = c("სტატუსი","ფოტო","ლაივ ვიდეო", "გაზიარებული ვიდეო", "საავტორო ვიდეო", "დარეგისტრირებული - სხვა ქვეყანაში", "2022-2024 წელი","2020-2022 წელი"))+
  font_size(title = 18, labels.y = 14)+
  theme(text = element_text(family= "BPG 2017 DejaVu Sans"),
        plot.title = element_text(size=24, face="bold", family="BPG 2017 DejaVu Sans Caps", hjust = 0),
        plot.subtitle = element_text(size=18, family="BPG 2017 DejaVu Sans", hjust=0),
        axis.text = element_blank(),
        #        axis.text = element_text(size=0.2, family="BPG 2017 DejaVu Sans", color = "black"),
        legend.position = "top",
        legend.box = "vertical",
        legend.key.width = unit(2, "cm"),
        legend.text = element_text(size=8, family="BPG 2017 DejaVu Sans")
  )
p2





#______________________________________________share____________________
# Fit multiple linear regression model
model2 <- lm(Shares ~ post_date_f + adm_country + post_type_r, data = data)

# Summarize the model
summary(model2)


#______________________________________________share____________________
# Fit multiple linear regression model
model3 <- lm(Likes ~ post_date_f + adm_country + post_type_r, data = data)

# Summarize the model
summary(model3)

#______________________________________________share____________________
# Fit multiple linear regression model
model4 <- lm(Comments ~ post_date_f + adm_country + post_type_r, data = data)

# Summarize the model
summary(model4)

tab_model(model2, model3, model4)




#______________________________________________pages. most active.=_______

#write.xlsx(data %>% count(Page.Name) %>% arrange(desc(n)), "Post freq by Page_name.xlsx")
#write.xlsx(data %>% count(Page.Category) %>% arrange(desc(n)), "Post freq by Page_category.xlsx")
#write.xlsx(data %>% count(Page.Created) %>% arrange(desc(n)), "Post freq by Page_created.xlsx")

#write.xlsx(select(data, Page.Name, Followers.at.Posting),"page followers.xlsx")














