rm(list=ls())
path<-"F:/DMBI R/Mini Project"
setwd(path)
getwd()
df <- read.csv("USvideos.csv")
str(df)
head(df)
library(plyr)

library(tidyverse)

library(wordcloud)

library(tm)

library(SnowballC)

library(lubridate)

library(ggcorrplot)

library(DMwR)
library(caret)
library(rpart)
library(rpart.plot)

library(pROC)

library(randomForest)

library(ipred)

library(caretEnsemble)
# removing the thumbnail link column
df <- df[,-12]

# setting category_id as a factor variable
df$category_id <- as.factor(df$category_id)

# setting all 0 values for likes, dislikes and comments as NA
df[, 9:11][df[, 9:11] == 0] <- NA

# counting the missing values
sapply(df, function(x) sum(is.na(x)))

# removing the records where the likes and dislikes both are NA
df <- df %>% 
  filter(!is.na(likes) & !is.na(dislikes))

# imputing the missing comment_count values using knn
knnOut <- round(knnImputation(df[,8:11], k = 10))

# inserting the imputed values into the original dataframe
df <- cbind(df[,1:10], knnOut[,4], df[,12:15])

# renaming the column
colnames(df)[11] <- 'comment_count'

# counting the missing values
sapply(df, function(x) sum(is.na(x)))

# a new derived variable that gives the ratio of likes and dislikes for a video 
df$like_percentage <- df$likes/(df$dislikes+df$likes)

# converting the trending date into date format
df$trending_date <- ydm(df$trending_date)

# getting the publish date for the trending video
df$publish_date <- ymd(substr(df$publish_time, start = 1, stop = 10))

# calculating the difference between the video being published and became trending
df$diff_days <- df$trending_date-df$publish_date

# converting diff_days into a numerical feature
df$diff_days <- as.numeric(df$diff_days)

# function to normalize the numerical features
z_normalize <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# normalizing the numerical features
youtube_norm <- cbind(df[,1:7], lapply(df[,c(8:11, 18)], z_normalize), 
                      df[,12:17])

# summary of numerical features after normalizing
summary(youtube_norm[,8:12])

# clearly, the range is too large for one dataset, as we can see that the 3rd quartile
# for each of the numerical features is less than the mean. and, then there is a small
# porition of videos with extremely high number of views. so, moving forward, the data will
# be divided into two subsets, one with views less than one million and the other with
# views greater than one million and less than ten million
# videos with views more than 10 million will be removed

# videos with less than one million views
low_youtube <- df %>% 
  filter(views < 1000000)

# normalizing the numerical features
low_youtube_norm <- cbind(low_youtube[,1:7], lapply(low_youtube[,c(8:11, 18)], z_normalize), 
                          low_youtube[,12:17])

# removing the rows with sd greater than 3
low_youtube_norm <- low_youtube_norm %>% 
  filter(likes < 3 & dislikes < 3 & comment_count < 3 & diff_days < 3)

# summary of numerical features after normalizing
summary(low_youtube_norm[,8:12])

# videos with views between one and ten million

#high_youtube <- df %>% 
 #filter(views > 1000000) still the 3rd quartile values are less than mean

high_youtube <- df %>% 
 filter(views > 1000000 & views < 10000000)

# normalizing the numerical features
high_youtube_norm <- cbind(high_youtube[,1:7], lapply(high_youtube[,c(8:11, 18)], z_normalize), 
                           high_youtube[,12:17])

# removing the rows with sd greater than 3
high_youtube_norm <- high_youtube_norm %>% 
  filter(likes < 3 & dislikes < 3 & comment_count < 3 & views < 3 & diff_days < 3)

# summary of numerical features after normalizing
summary(high_youtube_norm[,8:12])

higher_youtube <- df %>% 
  filter(views > 10000000)


# normalizing the numerical features
higher_youtube_norm <- cbind(higher_youtube[,1:7], lapply(higher_youtube[,c(8:11, 18)], z_normalize), 
                           higher_youtube[,12:17])

# removing the rows with sd greater than 3
higher_youtube_norm <- higher_youtube_norm %>% 
  filter(likes < 3 & dislikes < 3 & comment_count < 3 & views < 3 & diff_days < 3)

# summary of numerical features after normalizing
summary(higher_youtube_norm[,8:12])

#EDA
# bar plot to see the most popular cateogry for trending videos
ggplot(low_youtube_norm) +
  geom_bar(aes(category_id, fill = category_id)) +
  labs(y = 'Count', title = 'Trending Videos Category (Low)') +
  theme(plot.title = element_text(hjust = 0.5))
# for videos with views < one million category 24 is the most popular,
# followed by 26 and 10
ggplot(high_youtube_norm) +
  geom_bar(aes(category_id, fill = category_id)) +
  labs(y = 'Count', title = 'Trending Videos Category (High)') +
  theme(plot.title = element_text(hjust = 0.5))
# for videos with views > one million, again category 24 is the most
# popular, with category 10 being the second most frequent
ggplot(higher_youtube_norm) +
  geom_bar(aes(category_id, fill = category_id)) +
  labs(y = 'Count', title = 'Trending Videos Category (Higher)') +
  theme(plot.title = element_text(hjust = 0.5))
# for videos with views > ten million category 10 is the most popular,
# followed by 24
# creating a frequency table to determine the number of trending videos by channel
low_channels <- as.data.frame(table(low_youtube_norm$channel_title))
low_channel_freq <- low_channels %>% 
  filter(rank(desc(Freq)) <= 10)
low_channel_freq
# bar plot to visualize the top channels by number of trending videos
ggplot(low_channel_freq) +
  geom_bar(aes(Var1, Freq, fill = Var1), stat = 'identity') +
  labs(title = 'Trending Videos by Channel (Low)') +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 15, hjust = 1),
        legend.position="none")

# ESPN has the highest number of trending videos with views less than one million
# creating a frequency table to determine the number of trending videos by channel
high_channels <- as.data.frame(table(high_youtube_norm$channel_title))
high_channel_freq <- high_channels %>% 
  filter(rank(desc(Freq)) <= 10)
high_channel_freq

# bar plot to visualize the top channels by number of trending videos
ggplot(high_channel_freq) +
  geom_bar(aes(Var1, Freq, fill = Var1), stat = 'identity') +
  labs(title = 'Trending Videos by Channel (High)') +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 15, hjust = 1),
        legend.position="none")

higher_channels <- as.data.frame(table(higher_youtube_norm$channel_title))
higher_channel_freq <- higher_channels %>% 
  filter(rank(desc(Freq)) <= 10)
higher_channel_freq
# bar plot to visualize the top channels by number of trending videos
ggplot(higher_channel_freq) +
  geom_bar(aes(Var1, Freq, fill = Var1), stat = 'identity') +
  labs(title = 'Trending Videos by Channel (Higher)') +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 15, hjust = 1),
        legend.position="none")

# Screen Junkies has the highest number of trending videos with views more than
# one million
#

#View(df)
# difference between publishing and trending days
ggplot(subset(low_youtube, diff_days<30)) +
  geom_bar(aes(as.factor(diff_days), fill = as.factor(diff_days))) +
  labs(x = 'Days', y = 'Count', title = 'Difference between publising and trending (Low)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title="Days"))

# low view count video will most likely start trending within 10 days 
# of being published
ggplot(subset(high_youtube, diff_days<30)) +
  geom_bar(aes(as.factor(diff_days), fill = as.factor(diff_days))) +
  labs(x = 'Days', y = 'Count', title = 'Difference between publising and trending (High)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title="Days"))

# high view count video will also most likely start trending within 
# 10 days of being published
ggplot(subset(higher_youtube, diff_days<30)) +
  geom_bar(aes(as.factor(diff_days), fill = as.factor(diff_days))) +
  labs(x = 'Days', y = 'Count', title = 'Difference between publising and trending (High)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title="Days"))
# calculating the category wise like percentage of videos
cat_like_mean <- ddply(df, .(category_id), summarize, 
                       mean = mean(like_percentage, na.rm = TRUE))
cat_like_mean
# plotting the category wise like percentage
ggplot(cat_like_mean, aes(x = category_id, y = mean)) + 
  geom_point(size = 4, color = "red", fill = alpha("orange", 0.3), 
             alpha = 0.7, shape = 21, stroke = 2) + 
  geom_segment(aes(x = category_id, xend = category_id, y = 0, yend = mean)) + 
  labs(title="Category Like Percentage", x = 'Category', y = 'Percentage') + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6), 
        plot.title = element_text(hjust = 0.5))

# although the category 24 has the highest number of trending videos, the 
# ratio for number of likes and dislikes is the lowest
# graph to see for how long does a video remain trending for low view count
low_days_trending <- as.data.frame(table(low_youtube$title))

ggplot(low_days_trending, aes(as.factor(Freq), fill = as.factor(Freq))) +
  geom_bar() +
  labs(x = 'Days', y = 'Count', title = 'Days Trending (Low)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title="Days"))

# a video with lower number of views can remain trending for more than a week
# graph to see for how long does a video remain trending for high view count
high_days_trending <- as.data.frame(table(high_youtube$title))

ggplot(high_days_trending, aes(as.factor(Freq), fill = as.factor(Freq))) +
  geom_bar() +
  labs(x = 'Days', y = 'Count', title = 'Days Trending (High)') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title="Days"))

# a video with higher view count is not likely to remain trending for more than
# a single day
# getting the numerical features of the data frame for low video count
low_num_youtube <- low_youtube[,c("category_id","views","likes","dislikes",
                                  "comment_count", 'diff_days')]

# setting categorical_id as numerical feature
low_num_youtube$category_id <- as.numeric(low_num_youtube$category_id)

# calculating the correlations among numerical features
low_cor_num_youtube <- cor(low_num_youtube, use = 'pairwise.complete.obs')


# plot for correlations among numerical features
ggcorrplot(low_cor_num_youtube, hc.order = FALSE, type = "lower", lab = TRUE, 
           lab_size = 4, colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlation of Numerical features (Low)", 
           ggtheme = theme_bw) +
  theme(plot.title = element_text(hjust = 0.5))

# we can see that number of views, likes, dislikes and the comment count 
# for a video are highly correlated
# repeating the procedure for videos with high view count
high_num_youtube <- high_youtube[,c("category_id","views","likes","dislikes",
                                    "comment_count", 'diff_days')]

high_num_youtube$category_id <- as.numeric(high_num_youtube$category_id)

high_cor_num_youtube <- cor(high_num_youtube, use = 'pairwise.complete.obs')

ggcorrplot(high_cor_num_youtube, hc.order = FALSE, type = "lower", lab = TRUE, 
           lab_size = 4, colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlation of Numerical features (High)", 
           ggtheme = theme_bw) +
  theme(plot.title = element_text(hjust = 0.5))

# again number of views, likes, dislikes and the comment count for a 
# video are highly correlated

###############################################################################



utube_us <- read.csv("USvideos.csv", 
                     encoding = "UTF-8", stringsAsFactors=FALSE,
                     na.strings=c("", "NA"))

utube_us$category_id <- factor(utube_us$category_id)
utube_us$video_id <- factor(utube_us$video_id)
utube_us$channel_title <- factor(utube_us$channel_title)
utube_us$comments_disabled <- factor(utube_us$comments_disabled)
utube_us$ratings_disabled <- factor(utube_us$ratings_disabled)
utube_us$video_error_or_removed <- factor(utube_us$video_error_or_removed)
utube_us$trending_date <- as.Date(utube_us$trending_date, format = '%y.%d.%m')
utube_us$publish_time <- as.Date(utube_us$publish_time, format = '%Y-%m-%d')
utube_us$pub_to_trend <- as.numeric(utube_us$trending_date - utube_us$publish_time)
# description column has missing values
missing_per_col <- sapply(utube_us, function(x) sum(is.na(x)))
(total_missing <- sum(missing_per_col))
# clean discription column
# exclude emojis
utube_us_nodup <- utube_us[!duplicated(utube_us$video_id), ]

# PCA
# channels associated with each other

library(pcaPP)
nodup_numeric <-select_if(utube_us_nodup, is.numeric)
pr_out <- PCAproj(nodup_numeric, scale = sd, k = 5)
rownames(pr_out$scores) <- utube_us_nodup$channel_title
biplot(pr_out, scale = 0)
pr_out$loadings
prop_var <- (pr_out$sdev ^ 2) / (sum(pr_out$sdev ^ 2))
plot(prop_var, type='b')
#library(factoextra)
#fviz_screeplot(pr_out, ncp=10)

# hierarchical clustering
# so that the channel names show up in the plot instead of numbers
top_views <- nodup_numeric %>% 
  mutate(channel_title = utube_us_nodup$channel_title) %>%
  arrange(desc(views))
rownames(top_views) <- make.names(top_views$channel_title, unique = TRUE)
top_views <- select(top_views, -channel_title)

hc_complete <- hclust(dist(top_views[1:30, ]), method = "complete")
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "")

which(cutree(hc_complete, 6) == 5)

#sentiment analysis

library(wordcloud)
# Text manipulation
df1 <- read.csv("USvideos.csv")

library(data.table)
library(tidytext)
library(stringr)
library(tm)
library(sentimentr)
library(wordcloud)
library(RSentiment)
biga <- unnest_tokens(df1,bigram, title, token = "ngrams", n = 2)
biga <- as.data.table(biga)

ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(title="Top Description bigrams")+xlab(NULL)+ylab(NULL)

biga <- unnest_tokens(df1,bigram, tags, token = "ngrams", n = 3)
biga <- as.data.table(biga)

ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(title="Top Description bigrams")+xlab(NULL)+ylab(NULL)

biga <- unnest_tokens(df1,bigram, description, token = "ngrams", n = 3)
biga <- as.data.table(biga)

ggplot(biga[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram,-N),N,fill=bigram))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(title="Top Description bigrams")+xlab(NULL)+ylab(NULL)


library(RColorBrewer)

yu<-data.frame(df1)
yu.Corpus<-Corpus(VectorSource(yu$description))
#yu.Corpus<-Corpus(DataframeSource(yu$description))
yu.Clean<-tm_map(yu.Corpus, PlainTextDocument)
yu.Clean<-tm_map(yu.Corpus,tolower)
yu.Clean<-tm_map(yu.Clean,removeNumbers)
yu.Clean<-tm_map(yu.Clean,removeWords,stopwords("english"))
yu.Clean<-tm_map(yu.Clean,removePunctuation)
yu.Clean<-tm_map(yu.Clean,stripWhitespace)
yu.Clean<-tm_map(yu.Clean,stemDocument)
wordcloud(yu.Clean,max.words = 200,random.color = TRUE,random.order=FALSE)
