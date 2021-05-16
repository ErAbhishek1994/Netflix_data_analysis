#######################Importing Library & Data#######################
library(dplyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(readr)
library(tidytext)
library(wordcloud)

#importing Data 
df <- read.csv("netflix_title?.csv")
View(df)

#Loading The Date Into Single Format
#Use Lubridate Library to Format the Date Column

li=parse_date_time(x = df$date_added,
                   orders = c("d m y", "d B Y", "m/d/y"),
                   locale = "eng")
df_new = df
df_new$da?e_added = li
#Dates Loaded In the Same Format in the new Dataframe
str(df_new$date_added)

#######################Lets see what people like to watch more Movies or Tv Show#######################

#grouping data on bases of type of data
df_type <- df %>% gr?up_by(type) %>% summarise(Count = n())

#calculating Percentage
pct <- round(100*df_type$Count/sum(df_type$Count))

#making Pie Chart
pie(df_type$Count, labels = paste(df_type$type, sep = " ", pct,"%"),
    col = rainbow(length(df_type$Count)),
    main = ?What people like to watch More Movie or Tv Show ?")

#Insights: As we can see 69% of people likes to watch movies & the rest 31% people like to watch TV Shows.


#######################Years Difference between release year and added year!##################?####

# lets create a new variable "year_diff" the difference years between release year and added year!

df_diff <- df_new %>% mutate(year_diff = year(date_added)- release_year)

df_diff %>% count(year_diff, sort = F)


#Insights: The items added in the s?me year of release-year are the most with 2825
#          10 items added before release year, 1 year
#          1 item added before release year, 2 year
#          1 added before release year, 3 year
#          lets check them

#added before release year
b?fore_release <- df_diff %>% select(-cast, -description) %>% filter(year_diff < 0) %>% arrange(year_diff)
View(before_release)


#Ploting graph of year difference
diff = table(df_diff$year_diff)

barplot(diff, xlab = "Difference year", ylab = "Count",
     ?  main = "Year difference between Year added and Release Year.",
        col = "#006633")

#There are some items that have difference of more than 60 years
#Lets have a look at them.

#Filtering the items that have difference more than 60 years
Higher_diff?rence <- df_diff %>%  select(title, type, release_year, date_added, year_diff) %>% filter(year_diff > 60) %>% arrange(desc(year_diff))
View(Higher_difference)

#There are 25 items which have difference between Year added and release year greater than 60 ye?rs.


#######################Rating by Type#######################

#grouping items on basis of rating and type
Rating <- df_new %>% select(rating,type) %>% group_by(rating,type) %>% summarise(Count = n())
View(Rating)

#Ploting graph Using Plotly
plot_ly(?ating, x = ~type, y= ~Count, type = "bar",
        color = ~rating,
        text = ~Count,
        textposition = 'outside',
        textfont = list(color = '#000000', size = 12))


#######################Distribution by Countries Top 10###################?###

Top_Countries <- df_new %>% group_by(country) %>% summarise(Count =n())%>% arrange(desc(Count))
View(Top_Countries)

#removing all null as well as NA Values
Top_Countries <- na.omit(mutate_all(Top_Countries, ~ifelse(. %in% c("N/A", "null", ""),  NA, .?))

#adding only top 10 Countries.
Top_Countries = head(Top_Countries,10)

#plotting graph
ggplot(Top_Countries, aes(x = reorder(country, -Count), y = Count)) + geom_bar(stat = "Identity", width = 0.5, fill = "firebrick") + ggtitle("Items distribution by C?untry") + xlab("Top 10 Countries") + ylab("Count") + theme_light() +geom_text(aes(label = Count), alpha = 1)

#Insight: Obviously United States is the first , then India and united Kingdom . 
#         Although there are some items are joined Country such ?ike United States-United Kingdom,
#         United States-Canada, United States-France, are not shown as Top 10

#######################Dataset split to check the durations#######################

#parsing Number

movies <- df_new %>% select(country, type,?duration, rating, title) %>%
  filter(type == "Movie") %>% 
  mutate(duration_min = parse_number(duration))

tv_show <- df_new %>% select(country, type, duration, rating, title) %>%
  filter(type == "TV Show") %>% mutate(duration_season = parse_number(dura?ion))


#plotting
movies %>% plot_ly(
  x = ~duration_min,
  type = "histogram",
  nbinsx = 40,
  marker = list(
    color = "firebrick",
    line = list(color = "black",
                width = 1)
  ))%>%
  layout(
    title = "Duration distrbution",
    ?axis = list(title = "Count",
                 zeroline = FALSE),
    xaxis = list(title = "Duration (min)",
                 zeroline = FALSE)
  ) 

#Insights: Duration 90-99min are the most movies duration,
#          then 100-109min, then 80-89min, then ?10-119min

#Out of curiosity, lets see which movies have duration greater than 200.

Higher_duration <- movies %>% select(title,duration,country) %>% filter(movies$duration_min > 200) %>% arrange(desc(movies$duration_min))
View(Higher_duration)

####TV-Sho? Durations
 
tv_show <- tv_show %>% group_by(duration_season) %>% summarise(Count = n())
ggplot(tv_show, aes(x = reorder(duration_season, -Count), y=Count)) + geom_bar(stat = "Identity", width = 0.5, fill = "#008080") + geom_text(aes(label = Count)) + xlab?"Season Duration") + theme_classic()

#Insights: One Season are the most Among the season duration.
#          Just before leaving , lets check which tv-show has 16 seasons.

episode <- tv_show <- df_new %>% select(country, type, duration, rating, title) %?%
  filter(type == "TV Show") %>% mutate(duration_season = parse_number(duration))

Tv <- episode %>% select(title, duration_season) %>% filter(duration_season > 15)
View(Tv)

#Insights: Grey's Anatomy has season 16

#######################Time series#####?#################

netflix_date = df_new %>% select(date_added) %>%  mutate(year_added = year(date_added)) %>% group_by(year_added) %>% summarise(Count = n()) %>% arrange(desc(Count))

netflix_date <- na.omit(mutate_all(netflix_date, ~ifelse(. %in% c("N/A"? "null", ""),  NA, .)))

ggplot(netflix_date, aes(x = year_added)) +
  geom_col(aes(y = Count), color = "darkblue", fill = "steelblue", size = 1) +
  geom_line(aes(y = Count),size = 1.5, color="darkred", group = 1) + xlab("Years") + theme_light() +
  ggtit?e("Number of Items added per Year")

#Insights: As We can see that most of the titles were added from 2016-2019, it spiked in the year 2019



#######################Most frequent words in description variable For Movies (word cloud)#######################?
#WordCloud for Movie
desc_words_m <- df_new %>% select(type, show_id, description) %>%
  filter(type == "Movie") %>% 
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

count_word <- desc_words_m %>%
  count(word, sort = TRUE)

wordcloud(word? = count_word$word,  
          freq = count_word$n, 
          min.freq = 50,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2")) 

#Wordcloud for Tv shows

des?_words_tv <- NetFlix %>% select(type, show_id, description) %>%
  filter(type == "TV Show") %>% 
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

count_word <- desc_words_tv %>%
  count(word, sort = TRUE)


wordcloud(words = count_word$word,? 
          freq = count_word$n, 
          min.freq = 30,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2"))










