###### Package load ########

# Loads packages using the groundhog package. Groundhog enables reproducible     
# analysis by recording the date the packages were used. It then downloads those 
# exact package version upon later analysis, ensuring scripts run as intended.

library("groundhog")
pkgs <- c("tidyverse", "kableExtra", "lubridate", "gridExtra")
groundhog.library(pkgs, "2022-07-25")

# Disabling scientific notation to improve readability of certain variables.
# options(scipen=0, digits=7) # To return to default setting
options(scipen = 999)

# Setting seed for results replication
set.seed(12345)

###### Data Load ##########

# Load the three datasets of removed tweets acquired from Twitter and one baseline dataset. Feb 2021 removal
# of Russian IRA accounts, Feb 2021 removal of Iranian accounts, and Dec 2021
# removal of PRC Xinjiang-focused accounts.

russia_clean <- read_csv("russia_2021_cleaned.csv")
iran_clean <- read_csv("iran_2021_cleaned.csv")
china_clean <- read_csv("china_2021_cleaned.csv")

# Datasets comes from Arunava Kumar Chakraborty on Kaggle: https://www.kaggle.com/datasets/arunavakrchakraborty/covid19-twitter-dataset
# These datasets contains tweets about COVID-19 from August to September 2020 and April to June 2020, respectively
covid_2020_1 <- read_csv("Covid-19 Twitter Dataset (Apr-Jun 2020).csv")
covid_2020_2 <- read_csv("Covid-19 Twitter Dataset (Aug-Sep 2020).csv")

###### Descriptive Analysis ######

#### Russia ####

# How many unique accounts?
r1 <- length(unique(russia_clean[["userid"]]))

# How many unique tweets?
r2 <- length(unique(russia_clean[["tweetid"]]))

# Average tweets per account
r3 <- signif(r2 / r1, 4)

# Date of tweets
russia_clean$tweet_year <- year(russia_clean$tweet_time)

summary(russia_clean$tweet_year)

ggplot(russia_clean, aes(x = tweet_year)) +
  geom_bar(fill = "#0072CE") +
  scale_x_continuous(n.breaks = 11) +
  xlab("Year")

# Account creation date
account_year_ru <- russia_clean %>%
  select(userid, account_creation_date) %>%
  unique() 

account_year_ru$account_creation_date <- year(account_year_ru$account_creation_date)

summary(account_year_ru$account_creation_date)

ggplot(account_year_ru, aes(x = account_creation_date)) +
  geom_bar(fill = "#0072CE", width = 0.7) + 
  scale_x_continuous(n.breaks = 11) +
  xlab("Year")

# Follower count distribution - how many followers does each account have?
follower_ru <- russia_clean %>%
  select(user_screen_name, follower_count) %>%
  unique()

summary(follower_ru)

ggplot(follower_ru, aes(x = follower_count)) +
  geom_histogram(fill = "#0072CE") +
  xlab("Follower Count")

top_n(follower_ru, 10, follower_count) %>% # Top ten
  arrange(desc(follower_count)) %>% 
  select(user_screen_name, follower_count)

# Following count distribution - how many other accounts does each account follow?
following_ru <- russia_clean %>%
  select(userid, following_count) %>%
  unique()

summary(following_ru)

ggplot(following_ru, aes(x = userid, y = following_count)) +
  geom_col(fill = "#0072CE") +
  scale_x_discrete("Accounts", labels = seq(1:r1)) +
  scale_y_continuous(n.breaks = 10) +
  ylab("Following Count")

top_n(following_ru, 10, following_count) %>% # Top ten 
  arrange(desc(following_count)) %>% 
  select(user_screen_name, following_count)

# Proportion of retweets
russia_clean %>%
  summarize("Retweet" = mean(is_retweet) * 100,
            "Not Retweet" = mean(!is_retweet) * 100)

# Amplification metrics - Mean and median of quotes, replies, retweets, and likes for tweets in the dataset
  # Quotes
summary(russia_clean$quote_count)

ggplot(russia_clean, aes(x = quote_count)) +
  geom_area(stat = "bin", fill = "#0072CE") +
  ylab("Number of Quote Tweets")

top_n(russia_clean, 10, quote_count) %>% # Top ten 
  arrange(desc(quote_count)) %>% 
  select(user_screen_name, quote_count)

  # Replies
summary(russia_clean$reply_count)

ggplot(russia_clean, aes(x = reply_count)) +
  geom_histogram(fill = "#0072CE") +
  ylab("Number of Replies")

top_n(russia_clean, 10, reply_count) %>% # Top ten
  arrange(desc(reply_count)) %>% 
  select(user_screen_name, reply_count)

  # Retweets
summary(russia_clean$retweet_count)

ggplot(russia_clean, aes(y = retweet_count)) +
  geom_boxplot(fill = "#0072CE") +
  ylab("Number of Retweets")

top_n(russia_clean, 10, retweet_count) %>% # Top ten
  arrange(desc(retweet_count)) %>% 
  select(user_screen_name, retweet_count)

  # Likes
summary(russia_clean$like_count)

ggplot(russia_clean, aes(y = like_count)) +
  geom_boxplot(fill = "#0072CE") +
  ylab("Number of Likes")

top_n(russia_clean, 10, like_count) %>% # Top ten 
  arrange(desc(like_count)) %>% 
  select(user_screen_name, like_count)

# Would be great to have - impressions: how many people were these tweets served to?


#### Iran ####

# How many unique accounts?
i1 <- length(unique(iran_clean[["userid"]]))

# How many unique tweets?
i2 <- length(unique(iran_clean[["tweetid"]]))

# Average tweets per account
i3 <- signif(i2 / i1, 4)

# Date and time of tweets
iran_clean$tweet_year <- year(iran_clean$tweet_time)

summary(iran_clean$tweet_year)

ggplot(iran_clean, aes(x = tweet_year)) +
  geom_bar(fill = "#009639")+
  scale_x_continuous(n.breaks = 11) +
  xlab("Year")

# Account creation date
account_year_ir <- iran_clean %>%
  select(userid, account_creation_date) %>%
  unique() 

account_year_ir$account_creation_date <- year(account_year_ir$account_creation_date)

summary(account_year_ir$account_creation_date)

ggplot(account_year_ir, aes(x = account_creation_date)) +
  geom_bar(fill = "#009639", width = 0.7) + 
  scale_x_continuous(n.breaks = 11) +
  xlab("Year")

# Follower count distribution - how many followers does each account have?
follower_ir <- iran_clean %>%
  select(user_screen_name, follower_count) %>%
  unique()

summary(follower_ir)

ggplot(follower_ir, aes(x = follower_count)) +
  stat_density(fill = "#009639") +
  scale_x_continuous("Number of Followers") +
  scale_y_continuous(n.breaks = 12)

top_n(follower_ir, 10, follower_count) %>% # Top ten
  arrange(desc(follower_count)) %>% 
  select(user_screen_name, follower_count)

# Following count distribution - how many other accounts does each account follow?
following_ir <- iran_clean %>%
  select(userid, following_count) %>%
  unique()

summary(following_ir)

ggplot(following_ir, aes(x = following_count)) +
  stat_density(fill = "#009639") +
  scale_x_continuous("Accounts Followed") +
  scale_y_continuous(n.breaks = 10) 

top_n(following_ir, 10, following_count) %>% # Top ten 
  arrange(desc(following_count)) %>% 
  select(user_screen_name, following_count)

# Proportion of retweets
iran_clean %>%
  summarize("Retweet" = mean(is_retweet) * 100,
            "Not Retweet" = mean(!is_retweet) * 100)

# Amplification metrics - Mean and median of quotes, replies, retweets, and likes for tweets in the dataset
# Quotes
summary(iran_clean$quote_count)

ggplot(iran_clean, aes(y = quote_count)) +
  geom_boxplot(fill = "#009639") +
  ylab("Number of Quote Tweets")

top_n(iran_clean, 10, quote_count) %>% # Top ten 
  arrange(desc(quote_count)) %>% 
  select(user_screen_name, quote_count)

# Replies
summary(iran_clean$reply_count)

ggplot(iran_clean, aes(y = reply_count)) +
  geom_boxplot(fill = "#009639") +
  ylab("Number of Replies")

top_n(iran_clean, 10, reply_count) %>% # Top ten
  arrange(desc(reply_count)) %>% 
  select(user_screen_name, reply_count)

# Retweets
summary(iran_clean$retweet_count)

ggplot(iran_clean, aes(y = retweet_count)) +
  geom_boxplot(fill = "#009639") +
  ylab("Number of Retweets")

top_n(iran_clean, 10, retweet_count) %>% # Top ten
  arrange(desc(retweet_count)) %>% 
  select(user_screen_name, retweet_count)

# Likes
summary(iran_clean$like_count)

ggplot(iran_clean, aes(y = like_count)) +
  geom_boxplot(fill = "#009639") +
  ylab("Number of Likes")

top_n(iran_clean, 10, like_count) %>% # Top ten 
  arrange(desc(like_count)) %>% 
  select(user_screen_name, like_count)

# Would be great to have - impressions: how many people were these tweets served to?


#### China ####

# How many unique accounts?
c1 <- length(unique(china_clean[["userid"]]))

# How many unique tweets?
c2 <- length(unique(china_clean[["tweetid"]]))

# Average tweets per account
c3 <- signif(c2 / c1, 4)

# Date of tweets
china_clean$tweet_month <- month(china_clean$tweet_time) 
# Dataset contains 4 months of tweets, so time scale was changed to represent this

tt3 <- ggplot(china_clean, aes(x = tweet_month)) +
  geom_bar(fill = "#C8102E") +
  scale_x_binned(labels = cmonths) +
  xlab("Months in 2021")
tt3
# Account creation date
account_year_ch <- china_clean %>%
  select(userid, account_creation_date) %>%
  unique() 

account_year_ch$account_creation_date <- year(account_year_ch$account_creation_date)

summary(account_year_ch$account_creation_date)
table(account_year_ch$account_creation_date)

ggplot(account_year_ch, aes(x = account_creation_date)) +
  geom_bar(fill = "#C8102E") + 
  xlab("Year")

# Follower count distribution - how many followers does each account have?
follower_ch <- china_clean %>%
  select(userid, follower_count) %>%
  unique()

summary(follower_ch)

ggplot(follower_ch, aes(x = follower_count)) +
  stat_density(fill = "#C8102E") +
  scale_x_continuous("Number of Followers") +
  scale_y_continuous(n.breaks = 12)

top_n(follower_ch, 10, follower_count) %>% # Top ten
  arrange(desc(follower_count)) %>% 
  select(user_screen_name, follower_count)

# Following count distribution - how many other accounts does each account follow?
following_ch <- china_clean %>%
  select(userid, following_count) %>%
  unique()

summary(following_ch)

ggplot(following_ch, aes(x = following_count)) +
  stat_density(fill = "#C8102E") +
  scale_x_continuous("Accounts Followed") +
  scale_y_continuous(n.breaks = 10) 

top_n(following_ch, 10, following_count) %>% # Top ten 
  arrange(desc(following_count)) %>% 
  select(user_screen_name, following_count)

# Proportion of retweets
china_clean %>%
  summarize("Retweet" = mean(is_retweet) * 100,
            "Not Retweet" = mean(!is_retweet) * 100)

# Amplification metrics - Mean and median of quotes, replies, retweets, and likes for tweets in the dataset
# Quotes
summary(china_clean$quote_count)

ggplot(china_clean, aes(y = quote_count)) +
  geom_boxplot(fill = "#C8102E") +
  ylab("Number of Quote Tweets")

top_n(china_clean, 10, quote_count) %>% # Top ten 
  arrange(desc(quote_count)) %>% 
  select(user_screen_name, quote_count)

# Replies
summary(china_clean$reply_count)

ggplot(china_clean, aes(y = reply_count)) +
  geom_boxplot(fill = "#C8102E") +
  ylab("Number of Replies")

top_n(china_clean, 10, reply_count) %>% # Top ten
  arrange(desc(reply_count)) %>% 
  select(user_screen_name, reply_count)

# Retweets
summary(china_clean$retweet_count)

ggplot(china_clean, aes(y = retweet_count)) +
  geom_boxplot(fill = "#C8102E") +
  ylab("Number of Retweets")

top_n(china_clean, 10, retweet_count) %>% # Top ten
  arrange(desc(retweet_count)) %>% 
  select(user_screen_name, retweet_count)

# Likes
summary(china_clean$like_count)

ggplot(china_clean, aes(y = like_count)) +
  geom_boxplot(fill = "#C8102E") +
  ylab("Number of Likes")

top_n(china_clean, 10, like_count) %>% # Top ten 
  arrange(desc(like_count)) %>% 
  select(user_screen_name, like_count)


##### Presenting data #####

#grid.arrange(plot1, plot2, plot3, ncol=3)



