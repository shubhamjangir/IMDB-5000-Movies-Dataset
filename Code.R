## Loading packages
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)


###########################################################################
###########################################################################
###                          SECTION 1                                  ###
###    DATA IMPORT, CLEANING AND MERGIND TO PRODUCE FINAL DATAFRAME     ###
###                                                                     ###
###########################################################################
###########################################################################


##-------------##
## DATA IMPORT
##-------------##

# 1.1 Setting up the working directory and Reading the Movie dataset into IMDB

#   Getting the working directory
getwd()
#   Setting the working directory
setwd("~/Desktop/IMDB")
#   Reading the dataset into R
IMDB <- read_csv('movie_metadata.csv')

# 1.2 Exploring the initial structure and summary statistics on the dataset

#   Checking the head of the dataset
data.frame(head(IMDB))
#   Checking the structure of the dataset
str(IMDB)
#   Exploring the summary statistics on the dataset
summary(IMDB)
#   Checking the number of rows
nrow(IMDB)
#   Checking the number of columns
ncol(IMDB) 

#   We have 5043 obs. of 28 variables


##-------------##
## DATA CLEANING
##-------------##

# 1.3 Checking and removing duplicates in the dataset

#   Checking the duplicate rows
sum(duplicated(IMDB)) 
#   Delete duplicate rows
IMDB <- IMDB[-which(duplicated(IMDB)),] 

#   We have 4998 obs. of 28 variables

# 1.4 Checking and removing rows with NA values in the dataset

#   To find the missing value in each column and sum them
apply(is.na(IMDB),2,sum)

# 1.4.1 Gross and Budget have too many missing values and we can not impute anything in place of NA values
# 1.4.1 so we need to delete the NA values because we need to keep these two variables for our analysis.

#   To find the NA values in  the gross field
which(!complete.cases(IMDB$gross)) 
#   Remove NA values in the gross field
IMDB <- IMDB[-which(!complete.cases(IMDB$gross)),] 

#   We have 4124 obs. of 28 variables

#   To find the NA values in  the budget field
which(!complete.cases(IMDB$budget))
#   Remove NA values in the budget field
IMDB <- IMDB[-which(!complete.cases(IMDB$budget)),] 

#   We have 3857 obs. of 28 variables

# 1.4.2 Aspect ration contains the third highest NA values and the most common
# 1.4.2 aspect ratio is 2.35 and 1.85 for analysis, we group other aspect ratio together

#   Checking the most common aspect ratio and NA values 
table(IMDB$aspect_ratio,useNA = 'always')
#   Assigning missing ratios to 0 for analysis purpose
IMDB$aspect_ratio[is.na(IMDB$aspect_ratio)] <- 0
#   Mean of gross for aspect ratio 1.85
mean(IMDB$gross[IMDB$aspect_ratio == 1.85])
#   Mean of gross for aspect ratio 2.35
mean(IMDB$gross[IMDB$aspect_ratio == 2.35])
#   Mean of gross for aspect ratio other than 2.35 and 1.85
mean(IMDB$gross[IMDB$aspect_ratio != 1.85 & IMDB$aspect_ratio != 2.35])

#   As the mean is in the range of 44M and 58M it wont affect our analysis much so we can remove aspect ration
IMDB <- select(IMDB,-27) 

#   We have 3857 obs. of 27 variables

# 1.4.3 Analyzing the content rating

#   Checking the most common content rating and NA values
table(IMDB$content_rating,useNA = 'always')
#   To find the NA values in  the content rating field
which(!complete.cases(IMDB$content_rating))
#   Remove NA values in the content rating field
IMDB <- IMDB[-which(!complete.cases(IMDB$content_rating)),]
#   As per the motion picture M,GP=PG and we have given Unrated, Not Rated, Passed and Approved under R and PG-13
IMDB$content_rating[IMDB$content_rating == 'M'] <- 'PG'
IMDB$content_rating[IMDB$content_rating == 'GP']  <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'X']   <- 'NC-17'
IMDB$content_rating[IMDB$content_rating == 'Unrated']   <- 'R'
IMDB$content_rating[IMDB$content_rating == 'Not Rated']   <- 'R'
IMDB$content_rating[IMDB$content_rating == 'Passed']   <- 'PG-13'
IMDB$content_rating[IMDB$content_rating == 'Approved']   <- 'PG-13'
#   Changing the content rating to factor 
IMDB$content_rating <- factor(IMDB$content_rating)
#   Checking the levels of content rating 
levels(IMDB$content_rating) 

#   We have 3806 obs. of 27 variables

# 1.4.3 Analyzing the colour of the movies

#   Checking the most common color and NA values 
table(IMDB$color, useNA = 'always')
#   Removing color column as 95% of the movies are color
IMDB <- select(IMDB,-1)

#   We have 3806 obs. of 26 variables

# 1.4.4 Analyzing the language of the movies

#   Checking the most common language and NA values 
table(IMDB$language, useNA = 'always')
#   Removing language column as most of the movies are in English 
IMDB <- subset(IMDB,select=-c(language)) 

#   We have 3806 obs. of 25 variables

# 1.4.5 Analyzing the facenumber_in_poster, director_facebook_likes and movie_facebook_likes of the movies

#   As these 3 columns contains zeros so lets assign NA to all the zeros for analysis purpose
IMDB[,c(4,15,25)][IMDB[,c(4,15,25)] == 0] <- NA 
#   Filling NA's with round of mean of their subsequent column values 
IMDB$facenumber_in_poster[!complete.cases(IMDB$facenumber_in_poster)] <- round(mean(IMDB$facenumber_in_poster, na.rm = TRUE))
IMDB$director_facebook_likes[!complete.cases(IMDB$director_facebook_likes)] <- round(mean(IMDB$director_facebook_likes, na.rm = TRUE))
IMDB$movie_facebook_likes[!complete.cases(IMDB$movie_facebook_likes)] <- round(mean(IMDB$movie_facebook_likes, na.rm = TRUE))

# 1.4.6 Analyzing the num_critic_for_reviews, actor_3_facebook_likes,actor_1_facebook_likes and actor_2_facebook_likes of the movies

#   As these 4 columns contains very less NA values we will fill it with round of mean of their subsequent column values 

IMDB$num_critic_for_reviews[!complete.cases(IMDB$num_critic_for_reviews)] <- round(mean(IMDB$num_critic_for_reviews, na.rm = TRUE))
IMDB$actor_3_facebook_likes[!complete.cases(IMDB$actor_3_facebook_likes)] <- round(mean(IMDB$actor_3_facebook_likes, na.rm = TRUE))
IMDB$actor_2_facebook_likes[!complete.cases(IMDB$actor_2_facebook_likes)] <- round(mean(IMDB$actor_2_facebook_likes, na.rm = TRUE))
IMDB$actor_1_facebook_likes[!complete.cases(IMDB$actor_1_facebook_likes)] <- round(mean(IMDB$actor_1_facebook_likes, na.rm = TRUE))

# 1.4.7 Analyzing the actor_1_name,actor_2_name and actor_3_name

#   Checking the actor_1_name and NA values, as it contain 1 NA we can remove that
table(IMDB$actor_1_name, useNA = 'always')
#   Remove NA values in the actor_1_name field
IMDB <- IMDB[-which(!complete.cases(IMDB$actor_1_name)),] 

#   We have 3805 obs. of 25 variables

#   Checking the actor_2_name and NA values, as it contain 1 NA we can remove that
table(IMDB$actor_2_name, useNA = 'always')
#   Remove NA values in the actor_2_name field
IMDB <- IMDB[-which(!complete.cases(IMDB$actor_2_name)),]

#   We have 3804 obs. of 25 variables

#   Checking the actor_3_name and NA values, as it contain 4 NA we can remove that
table(IMDB$actor_3_name, useNA = 'always')
#   Remove NA values in the actor_3_name field
IMDB <- IMDB[-which(!complete.cases(IMDB$actor_3_name)),] 

#   We have 3800 obs. of 25 variables

# 1.4.8 Analyzing the plot_keyword

#   Checking the plot_keyword and NA values, as it contains 21 NA we can remove that
table(IMDB$plot_keywords, useNA = 'always')
#   Remove NA values in the plot_keyword field
IMDB <- IMDB[-which(!complete.cases(IMDB$plot_keywords)),] 

#   We have 3779 obs. of 25 variables

# 1.4.9 Removing | from the genres and plot keywords

#   Removing | from the Genre
IMDB$genres <- gsub(pattern="\\|",replacement=" ",IMDB$genres)
#   Removing | from the Plot keyword
IMDB$plot_keywords <- gsub(pattern="\\|",replacement=" ",IMDB$plot_keywords)

# 1.4.10 Adding new column to our dataset for analysis

#   Adding Revenue Generated and Return Percent field 
IMDB$revenue_generated <- IMDB$gross - IMDB$budget
IMDB <- IMDB %>% mutate(returns_percent = (revenue_generated/budget)/100)

#   We have 3779 obs. of 27 variables as our FINAL IMDB DATASET

write.csv(IMDB, '~/Desktop/IMDB_FINAL.csv')


###########################################################################
###########################################################################
###                          SECTION 2                                  ###
###          EXPLORATION OF THE DATA FOR OVERALL UNDERSTANDING          ###
###                                                                     ###
###########################################################################
###########################################################################

##----------------##
## DATA EXPLORATION
##----------------##

# 2.1 Exploring all the variables of the dataset for overall understanding.

#   2.1.1 Gives all the 27 column names
names(IMDB)

#   2.1.2 Gives the structure of the dataset
str(IMDB)

#   2.1.3 Gives the summary statistics of every variable in the dataset
summary(IMDB)

#   2.1.4 Exploring Content Rating visually- Most of the movies have R content rating and after that its PG-13
ggplot(data = IMDB) +
  geom_bar(mapping = aes(x = content_rating))+
  xlab("Content Rating") + ylab("Movie Count") + 
  ggtitle("Histogram of Content Rating")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=17,family='Courier'))


#  2.1.5 Exploring IMDB Score- Most movies fall in the range of 5-7.5 IMDB Rating.
ggplot(IMDB, aes(imdb_score)) +
    geom_bar() + xlab("IMDB Score") + ylab("Movie Count") + 
  ggtitle("Histogram of IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=17,family='Courier'))

#  2.1.6 Exploring Title year i.e. year in which the movie got released- Most movies released after 2000's
ggplot(IMDB, aes(title_year)) +
  geom_bar() +xlab("Year Movie Released") + ylab("Movie Count") + 
  ggtitle("Histogram of Movie Released")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=17,family='Courier'))

#   2.1.7 Exploring Country with highest number of movie release- US has produced most number of the movies and after that it's UK.
  ggplot(data=IMDB, aes(x=title_year,y=country),colour=country) +
  geom_point() +geom_smooth()+xlab("Year Movie Released") + ylab("Country") + 
  ggtitle("Country vs Movie release Year")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.8 Exploring Percent Return by IMDB Score - Movies with higher IMDB rating has higher percentage return if we leave few outliners
  ggplot(data=IMDB, aes(x=returns_percent,y=imdb_score,colour=imdb_score,size=imdb_score)) +
    geom_point(alpha = 0.5)+geom_smooth()+
    xlab("Percent Return") + ylab("IMDB Score") + 
    ggtitle("Percent Return by IMDB Score")+
    theme(axis.title.x = element_text(colour="Black",size=12),
          axis.title.y = element_text(colour="Black",size=12),
          plot.title=element_text(colour="Black",size=15,family='Courier'))


#   2.1.9 Exploring Budget by IMDB Score- Budget is quite high for movies with higher IMDB Rating
  ggplot(data=IMDB, aes(x=budget/100000000,y=imdb_score,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("Budget in Milltions") + ylab("IMDB Score") + 
  ggtitle("Budget by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.10 Exploring Gross by IMDB Score - Movies with higher IMDB Rating have higher gross 
ggplot(data=IMDB, aes(x=gross/100000000,y=imdb_score,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("Gross in Millions") + ylab("IMDB Score") + 
  ggtitle("Gross by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.11 Exploring Movie Facebook Likes by IMDB Score - Higher IMDB Rating movies have higher Facebook likes
ggplot(data=IMDB, aes(x=movie_facebook_likes,y=imdb_score,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("Movie Facebook Likes") + ylab("IMDB Score") + 
  ggtitle("Movie Facebook Likes by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.12 Exploring Critics Review by IMDB Score - If the critics reviews are higher there are more chances that the movie will have higher IMDB Rating.
ggplot(data=IMDB, aes(x=num_critic_for_reviews,y=imdb_score,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("Critics Review") + ylab("IMDB Score") + 
  ggtitle("Critics Review by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.13 Exploring User Review by IMDB Score -If the user reviews are higher there are more chances that the movie will have higher IMDB Rating.
ggplot(data=IMDB, aes(x=num_user_for_reviews,y=imdb_score,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("User Review") + ylab("IMDB Score") + 
  ggtitle("User Review by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.14 Exploring Content Rating by IMDB Score - Using the boxplot we can see that movies with R and PG-13 Content Rating have the higher IMDB Rating.
ggplot(data=IMDB, aes(x=content_rating,y=imdb_score,colour=imdb_score)) +
  geom_boxplot(alpha = 0.5)+geom_smooth()+
  xlab("Content Rating") + ylab("IMDB Score") + 
  ggtitle("Content Rating by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.15 Exploring Director facebook likes by IMDB Score - IMDB Rating is not much dependent on Director's facebook likes.
ggplot(data=IMDB, aes(x=director_facebook_likes,y=imdb_score,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("Director Facebook Likes") + ylab("IMDB Score") + 
  ggtitle("Director facebook likes by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

#   2.1.16 Exploring Revenue Generated by IMDB Score - Apart from few outliners most the movies with higher IMDB Rating have higher Revenue.
ggplot(data=IMDB, aes(x=revenue_generated/1000000000,y=imdb_score,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("Revenue in Millions") + ylab("IMDB Score") + 
  scale_x_continuous(labels = scales::comma)+
  ggtitle("Revenue Generated by IMDB Score")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))


#   2.1.17 Exploring Average Budget by Countries-South Korea has the highest budget but this can be the outliners as most of the movies are produced in USA.
ggplot(data=avg_budget, aes(x=budget/1000000000,y=country,colour=budget,size=budget)) +
  geom_point(alpha = 0.5)+geom_smooth()+
  xlab("Average Budget in Millions") + ylab("Countries") + 
  ggtitle("Average Budget by Countries")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))


###########################################################################
###########################################################################
###                          SECTION 3                                  ###
###         INSIGHT FROM THE DATASET ON A PARTICULAR SUBSET             ###
###                                                                     ###
###########################################################################
###########################################################################

##--------------------##
## INSIGHT FROM THE DATA
##--------------------##

# 3.1 Exploring all the variables of the dataset for overall understanding.

# 3.1.1 Insight-1

#   What is the trend for number of movies produced annually and their corresponding IMDB Rating?
IMDB %>% group_by(title_year) %>% summarise(count_year = n()) %>%
  ggplot(aes(x = title_year , y = count_year, fill = count_year)) + 
  geom_bar(stat = "identity") +
  xlab("Year of Movie Produced") + ylab("Count of Movies") + 
  ggtitle("Number of Movies Produced by Year")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

IMDB %>% group_by(title_year) %>% summarise(mean_IMDB = mean(imdb_score)) %>%
  ggplot(aes(x = title_year, y = mean_IMDB)) + 
  geom_point()+geom_line()+geom_smooth() +
  xlab("Year of Movie Produced") + ylab("Avg. IMDB Rating") + 
  ggtitle("IMDB Rating by Title Year")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

# 3.1.2 Insight-2

#   What is the trend of IMDB Score as per Budget and Gross?

IMDB %>%group_by(imdb_score) %>% summarise(mean_budget = mean(budget)) %>%
  ggplot(aes(x = imdb_score, y = mean_budget/10000000)) + 
  geom_point() +geom_line() +
  xlab("IMDB Rating") + ylab("Budget in Millions") + 
  ggtitle("IMDB Rating by Budget")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

IMDB %>%group_by(imdb_score) %>% summarise(mean_gross = mean(gross)) %>%
  ggplot(aes(x = imdb_score, y = mean_gross/10000000)) + 
  geom_point() +geom_line() +
  xlab("IMDB Rating") + ylab("Gross in Millions") + 
  ggtitle("IMDB Rating by Gross")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

# 3.1.3 Insight-3

#   Top Profilic Directors with highest IMDB Rating and minimum 10 movies directed?

IMDB %>%
  group_by(director_name) %>%
  summarise(director_movies_count = n(),mean_IMDB = mean(imdb_score)) %>%
  filter(director_movies_count >= 10) %>%
ggplot(aes(x = mean_IMDB, y = director_name,colour=mean_IMDB,size=mean_IMDB)) + 
  geom_boxplot() +
  xlab("Avg. IMDB Rating ") + ylab("Directors") + 
  ggtitle("Profilic Directors by IMDB Rating")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

# 3.1.4 Insight-4

#   What is the impact of Critics Review and Audience Review together on IMDB Rating? 

ggplot(data=IMDB, aes(x=num_critic_for_reviews,y=num_user_for_reviews,colour=imdb_score,size=imdb_score)) +
  geom_point(alpha = 0.5)+geom_smooth()+
xlab("Critics Review") + ylab("Audience Review") + 
  ggtitle("Critics Review and Audience Review on IMDB Rating")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

# 3.1.5 Insight-5

#   Top 10 movies with Highest Budget and Highest Revenue Generated?

IMDB %>%
  filter(title_year > 2000) %>%
  arrange(desc(revenue_generated)) %>%
  top_n(10, revenue_generated) %>%
  ggplot()+
  geom_point(aes(x=budget/10000000,y=revenue_generated/10000000)) + 
  geom_smooth(aes(x=budget/10000000,y=revenue_generated/10000000))+
  geom_text(aes(x=budget/10000000,y=revenue_generated/10000000,
                label=movie_title,hjust=1,vjust=1,check_overlap = T))+
  xlab("Budget in Million") + ylab("Revenue Generated") + 
  ggtitle("Top 10 movies with Highest Revenue and Highest Budget")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

# 3.1.6 Insight-6

#   Movies with Highest IMDB Rating and Highest Revenue generated after 2000's

IMDB %>%
  filter(title_year > 2000) %>%
  arrange(desc(imdb_score)) %>%
  top_n(7, imdb_score) %>%
  ggplot(aes(x=revenue_generated/10000000,y=imdb_score))+
  geom_point() + 
  geom_smooth()+
  geom_text(aes(label=movie_title),size=3,hjust=1,vjust=1,check_overlap = T)+
xlab("Revenue Generated in Millions") + ylab("IMDB Rating") + 
  ggtitle("Top 10 Movies with Highest IMDB Rating and Revenue")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

# 3.1.7 Insight-7

#   Top 10 prolific directors with highest IMDB Rating and Countries

IMDB %>%
  filter(title_year > 2000) %>%
  arrange(desc(imdb_score)) %>%
  top_n(10, imdb_score) %>%
  ggplot(aes(x=country,y=imdb_score))+
  geom_point(aes(colour=director_name)) + 
  geom_text(aes(label=director_name),size=3,hjust=0.4,vjust=1,check_overlap = T)+
  xlab("Countries") + ylab("IMDB Rating") + 
  ggtitle("Top 10 Director with highest IMDB Rating and Countries")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))

# 3.1.8 Insight-8

#   Popular content rating with highest IMDB Rating and Revenue Generated
IMDB %>%
  filter(title_year > 2000) %>%
  arrange(desc(imdb_score)) %>%
  top_n(50, imdb_score) %>%
  ggplot(aes(x=revenue_generated/10000000,y=imdb_score,colour=content_rating))+
  geom_point() +
  geom_text(aes(label=content_rating),size=3,hjust=1,vjust=1,check_overlap = T) +
  xlab("Revenue in Millions") + ylab("IMDB Rating") + 
  ggtitle("Top 10 Director with highest IMDB Rating and Countries")+
  theme(axis.title.x = element_text(colour="Black",size=12),
        axis.title.y = element_text(colour="Black",size=12),
        plot.title=element_text(colour="Black",size=15,family='Courier'))











































