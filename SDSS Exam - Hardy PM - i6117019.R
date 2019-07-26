### S D S S  E X A M I N A T I O N ####################################################################################
# Pierre Michel B. Hardy
# I6117019

### P A C K A G E S ###################################################################################################
library(tm)
library(SnowballC)
library(dplyr)
library(sentimentr)
library(reshape2)
library(fastDummies)
library(tidyr)
library(MASS)
library(imager)
library(colordistance)
library(caret)
library(e1071)
library(xgboost)
library(MLmetrics)
library(DMwR)

### D A T A ###########################################################################################################
load("Reviews_5.RData")
load("Business information.RData")
load("Labels_test_set_for_students.RData")
load("Labels_photos_5.RData")

### P A R T  O N E ####################################################################################################

### QUESTION A ###

# extract the first review

  reviews_sample$text[1]


### QUESTION B ###

# Preprocessing the review data before analysis. 

# Remove the punctiation and keeping the first review in each step so we can see changes. 

  reviews_sample$Clean_Text <- removePunctuation(reviews_sample$text)
  cleaning.steps.1 <- reviews_sample$Clean_Text[1]

# Remove the numbers
  
  reviews_sample$Clean_Text <- removeNumbers(reviews_sample$Clean_Text)
  cleaning.steps.2 <- reviews_sample$Clean_Text[1]

# Lowercase everyting
  
  reviews_sample$Clean_Text <- tolower(reviews_sample$Clean_Text)
  cleaning.steps.3 <- reviews_sample$Clean_Text[1]

# Remove whitespaces
  
  reviews_sample$Clean_Text <- stripWhitespace(reviews_sample$Clean_Text)
  cleaning.steps.4 <- reviews_sample$Clean_Text[1]

# Remove common words
  
  reviews_sample$Clean_Text <- removeWords(reviews_sample$Clean_Text, stopwords("English"))
  cleaning.steps.5 <- reviews_sample$Clean_Text[1]
  
# Remove special characters
  
  reviews_sample$Clean_Text <- gsub("[:punct:]", "", reviews_sample$Clean_Text)
  cleaning.steps.6 <- reviews_sample$Clean_Text[1]
  
# Stem the word
  
  reviews_sample$Clean_Text <- stemDocument(reviews_sample$Clean_Text)
  cleaning.steps.7 <- reviews_sample$Clean_Text[1]

  
### QUESTION C ###
 
# First, we transform it into a corpus and create a document term matrix
  
  review.corpus <- Corpus(VectorSource(reviews_sample$Clean_Text))
  review.corpus.tf <- DocumentTermMatrix(review.corpus)  
  inspect(review.corpus.tf[1:10,1:10])
  
# We create an TF-IDF matrix
  
  review.corpus.tfidf <- weightTfIdf(review.corpus.tf)
  inspect(review.corpus.tfidf[1:10,1:10])
  
# In order for me to interpret the output better, I decided to transform it into a matrix since I am more acquainted 
# with it. 
  
  tfidf.matrix <- as.matrix(review.corpus.tfidf)
  
# Take the average per column (per term) in order quantify which terms are most important
  
  ave.term <- colMeans(tfidf.matrix)
  
# Take the top ten and bottom ten terms
  
  ave.term.most <- order(ave.term, decreasing = TRUE)
  ave.term.least <- order(ave.term, decreasing = FALSE)
  ave.term[ave.term.most[1:10]]
  ave.term[ave.term.least[1:36]]
  
  
### QUESTION D ###
  
# Calculate the sentiment score of each review
  
  reviews_sample$Sentiment <- sentiment(reviews_sample$Clean_Text)

### QUESTION E ###
 
# Create a scatterplot between the star ratings and polarity scores 
  
  plot(reviews_sample$stars, reviews_sample$Sentiment$sentiment, main="Star Reviews vs Polarity Score", xlab="Stars",
       ylab="Polarity")
  
# Fit a regression line to clearly see the relationship 
  
  reg.line <- lm(reviews_sample$Sentiment$sentiment~reviews_sample$stars)
  abline(reg.line)
  abline(h=0, col="red")
  
# Let's see if the line is significant (it is)  
  summary(reg.line)
  
         
### QUESTION F ###
  
# Let's join the reviews and business datasets
  
  combined <- left_join(reviews_sample, business, by="business_id")

# I will select the columns I find relevant for the analysis and would give informative value
  
  combined <- dplyr::select(combined, c("stars.x", "date", "useful","funny", "cool", "Sentiment", "city", "stars.y", 
                                 "review_count", "is_open", "categories","monday","tuesday","wednesday","thursday",
                                 "friday","saturday","sunday", "checkins"))
  combined$polarity <- combined$Sentiment$sentiment
  combined$word.count <- combined$Sentiment$word_count
  combined <- combined[,-6]  

# Next, I transform the dates of the review into seasons. I later re-read the question and realized that only the
# firm level characteristics are asked. I will keep this part of the code anyways. 
  
  combined$date <- substr(combined$date, 6, 7)  
  combined$date <- as.numeric(combined$date)
  combined$date <- lapply(combined$date, function(x){
    if(x<3){
      x<-"Winter"
    } else if (x>2 && x < 6) {
      x <- "Spring"
    } else if (x>5 && x < 9){
      x <- "Summer"
    } else if (x > 8 && x < 12) {
      x <- "Autumn"
    } else {
      x <- "Winter"
    }
  })
  
# Next, I decided to separate the opening and closing times of the establishment per day
  
# monday
  combined$monday <- as.character(combined$monday)
  combined$monday.open <- substr(combined$monday, 1,2)
  combined$monday.close <- substr(combined$monday,nchar(combined$monday)-3,nchar(combined$monday)-2)
  combined$monday.open <- gsub("[:punct:]", "", combined$monday.open)
  combined$monday.open <- gsub("No","", combined$monday.open)
  combined$monday.close <- gsub("[:punct:]", "", combined$monday.close)
  combined$monday.close <- gsub("No","", combined$monday.close)
  combined$monday.close <- gsub("-","",combined$monday.close)
  combined$monday.open <- as.numeric(combined$monday.open)
  combined$monday.close <- as.numeric(combined$monday.close)
  combined$monday.hr.open <- combined$monday.close-combined$monday.open
  combined$monday.hr.open <- lapply(combined$monday.hr.open, function(x){
    if (is.na(x)==FALSE && x<0){
      x<- x+24 
    } else {
      x <- x
    }
  })
  
# tuesday
  combined$tuesday <- as.character(combined$tuesday)
  combined$tuesday.open <- substr(combined$tuesday, 1,2)
  combined$tuesday.close <- substr(combined$tuesday,nchar(combined$tuesday)-3,nchar(combined$tuesday)-2)
  combined$tuesday.open <- gsub("[:punct:]", "", combined$tuesday.open)
  combined$tuesday.open <- gsub("No","", combined$tuesday.open)
  combined$tuesday.close <- gsub("[:punct:]", "", combined$tuesday.close)
  combined$tuesday.close <- gsub("No","", combined$tuesday.close)
  combined$tuesday.close <- gsub("-","",combined$tuesday.close)
  combined$tuesday.open <- as.numeric(combined$tuesday.open)
  combined$tuesday.close <- as.numeric(combined$tuesday.close)
  combined$tuesday.hr.open <- combined$tuesday.close-combined$tuesday.open
  combined$tuesday.hr.open <- lapply(combined$tuesday.hr.open, function(x){
    if (is.na(x)==FALSE && x<0){
      x<- x+24 
    } else {
      x <- x
    }
  })
  
# wednesday
  combined$wednesday <- as.character(combined$wednesday)
  combined$wednesday.open <- substr(combined$wednesday, 1,2)
  combined$wednesday.close <- substr(combined$wednesday,nchar(combined$wednesday)-3,nchar(combined$wednesday)-2)
  combined$wednesday.open <- gsub("[:punct:]", "", combined$wednesday.open)
  combined$wednesday.open <- gsub("No","", combined$wednesday.open)
  combined$wednesday.close <- gsub("[:punct:]", "", combined$wednesday.close)
  combined$wednesday.close <- gsub("No","", combined$wednesday.close)
  combined$wednesday.close <- gsub("-","",combined$wednesday.close)
  combined$wednesday.open <- as.numeric(combined$wednesday.open)
  combined$wednesday.close <- as.numeric(combined$wednesday.close)
  combined$wednesday.hr.open <- combined$wednesday.close-combined$wednesday.open
  combined$wednesday.hr.open <- lapply(combined$wednesday.hr.open, function(x){
    if (is.na(x)==FALSE && x<0){
      x<- x+24 
    } else {
      x <- x
    }
  })
  
# thursday
  combined$thursday <- as.character(combined$thursday)
  combined$thursday.open <- substr(combined$thursday, 1,2)
  combined$thursday.close <- substr(combined$thursday,nchar(combined$thursday)-3,nchar(combined$thursday)-2)
  combined$thursday.open <- gsub("[:punct:]", "", combined$thursday.open)
  combined$thursday.open <- gsub("No","", combined$thursday.open)
  combined$thursday.close <- gsub("[:punct:]", "", combined$thursday.close)
  combined$thursday.close <- gsub("No","", combined$thursday.close)
  combined$thursday.close <- gsub("-","",combined$thursday.close)
  combined$thursday.open <- as.numeric(combined$thursday.open)
  combined$thursday.close <- as.numeric(combined$thursday.close)
  combined$thursday.hr.open <- combined$thursday.close-combined$thursday.open
  combined$thursday.hr.open <- lapply(combined$thursday.hr.open, function(x){
    if (is.na(x)==FALSE && x<0){
      x<- x+24 
    } else {
      x <- x
    }
  })
  
# friday 
  combined$friday <- as.character(combined$friday)
  combined$friday.open <- substr(combined$friday, 1,2)
  combined$friday.close <- substr(combined$friday,nchar(combined$friday)-3,nchar(combined$friday)-2)
  combined$friday.open <- gsub("[:punct:]", "", combined$friday.open)
  combined$friday.open <- gsub("No","", combined$friday.open)
  combined$friday.close <- gsub("[:punct:]", "", combined$friday.close)
  combined$friday.close <- gsub("No","", combined$friday.close)
  combined$friday.close <- gsub("-","",combined$friday.close)
  combined$friday.open <- as.numeric(combined$friday.open)
  combined$friday.close <- as.numeric(combined$friday.close)
  combined$friday.hr.open <- combined$friday.close-combined$friday.open
  combined$friday.hr.open <- lapply(combined$friday.hr.open, function(x){
    if (is.na(x)==FALSE && x<0){
      x<- x+24 
    } else {
      x <- x
    }
  })
  
# saturday
  combined$saturday <- as.character(combined$saturday)
  combined$saturday.open <- substr(combined$saturday, 1,2)
  combined$saturday.close <- substr(combined$saturday,nchar(combined$saturday)-3,nchar(combined$saturday)-2)
  combined$saturday.open <- gsub("[:punct:]", "", combined$saturday.open)
  combined$saturday.open <- gsub("No","", combined$saturday.open)
  combined$saturday.close <- gsub("[:punct:]", "", combined$saturday.close)
  combined$saturday.close <- gsub("No","", combined$saturday.close)
  combined$saturday.close <- gsub("-","",combined$saturday.close)
  combined$saturday.open <- as.numeric(combined$saturday.open)
  combined$saturday.close <- as.numeric(combined$saturday.close)
  combined$saturday.hr.open <- combined$saturday.close-combined$saturday.open
  combined$saturday.hr.open <- lapply(combined$saturday.hr.open, function(x){
    if (is.na(x)==FALSE && x<0){
      x<- x+24 
    } else {
      x <- x
    }
  })
  
# sunday
  combined$sunday <- as.character(combined$sunday)
  combined$sunday.open <- substr(combined$sunday, 1,2)
  combined$sunday.close <- substr(combined$sunday,nchar(combined$sunday)-3,nchar(combined$sunday)-2)
  combined$sunday.open <- gsub("[:punct:]", "", combined$sunday.open)
  combined$sunday.open <- gsub("No","", combined$sunday.open)
  combined$sunday.close <- gsub("[:punct:]", "", combined$sunday.close)
  combined$sunday.close <- gsub("No","", combined$sunday.close)
  combined$sunday.close <- gsub("-","",combined$sunday.close)
  combined$sunday.open <- as.numeric(combined$sunday.open)
  combined$sunday.close <- as.numeric(combined$sunday.close)
  combined$sunday.hr.open <- combined$sunday.close-combined$sunday.open
  combined$sunday.hr.open <- lapply(combined$sunday.hr.open, function(x){
    if (is.na(x)==FALSE && x<0){
      x<- x+24 
    } else {
      x <- x
    }
  })
  
# I will once again select only the columns I find useful
  
  combined <- dplyr::select(combined, c("stars.x", "date", "useful","funny", "cool", "city", "stars.y", 
                                 "review_count", "is_open", "categories","monday.hr.open","tuesday.hr.open",
                                 "wednesday.hr.open","thursday.hr.open","friday.hr.open","saturday.hr.open",
                                 "sunday.hr.open", "checkins", "polarity","word.count"))
  
# I decided to dummify the categories variable. I start by separating the multiple categories 
  
  combined <- separate_rows(combined, categories, sep=";")
  combined$categories.value <- 1
  
# After assigning 1 to all the corresponding categories, I will spread them out so that one category is one column
  
  combined <- spread(combined, categories, categories.value, fill=0)
  
# Since there are 334 categories, I choose only those that are relevant. I decided to investigate the categories 
# that have an above average number of shops to it. Then I subjectively chose the high level categories (e.g. 
# "restaurants" only and excluding other cuisines). I chose 20 relevant categories from the 334. 
  
  categs <- dplyr::select(combined, 20:ncol(combined))
  categs.sum <- colSums(categs)
  categs.ordered <- order(categs.sum, decreasing = TRUE)
  categs.sum[categs.ordered[1:67]]
  combined <- dplyr::select(combined, c("stars.x", "date", "useful","funny", "cool", "city", "stars.y", 
                         "review_count", "is_open","monday.hr.open","tuesday.hr.open", "wednesday.hr.open",
                         "thursday.hr.open","friday.hr.open","saturday.hr.open","sunday.hr.open", "checkins", 
                         "polarity","word.count","Restaurants", "Nightlife","Event Planning & Services", 
                         "Arts & Entertainment","Shopping","Coffee & Tea","Beauty & Spas","Desserts","Active Life",
                         "Bakeries","Local Services","Home Services","Fast Food","Casinos","Automotive",
                         "Wine & Spirits","Hotels & Travel","Fashion","Buffets","Health & Medical"))
  
  
### QUESTION G ###  
  
# turn review star rating into factor and hours open into numeric
  
  combined$stars.x <- as.factor(combined$stars.x)
  combined$monday.hr.open <- as.numeric(combined$monday.hr.open)
  combined$tuesday.hr.open <- as.numeric(combined$tuesday.hr.open)
  combined$wednesday.hr.open <- as.numeric(combined$wednesday.hr.open)
  combined$thursday.hr.open <- as.numeric(combined$thursday.hr.open)
  combined$friday.hr.open <- as.numeric(combined$friday.hr.open)
  combined$saturday.hr.open <- as.numeric(combined$saturday.hr.open)
  combined$sunday.hr.open <- as.numeric(combined$sunday.hr.open)
  
# in case I wish to try different combinations, I create another data frame with my chosen columns. 
  
  reg1 <- dplyr::select(combined, c("stars.x", "city", 
                             "review_count", "is_open","monday.hr.open","tuesday.hr.open", "wednesday.hr.open",
                             "thursday.hr.open","friday.hr.open","saturday.hr.open","sunday.hr.open", "checkins", 
                             "polarity", "Restaurants", "Nightlife","Event Planning & Services", "word.count",
                             "Arts & Entertainment","Shopping","Coffee & Tea","Beauty & Spas","Desserts","Active Life",
                             "Bakeries","Local Services","Home Services","Fast Food","Casinos","Automotive",
                             "Wine & Spirits","Hotels & Travel","Fashion","Buffets","Health & Medical"))
  
# scale checkins
  
  mean.checkin <- mean(reg1$checkins)
  sd.checkin <- sd(reg1$checkins)
  reg1$checkins <- (reg1$checkins-mean.checkin)/sd.checkin
  
# estimate the regression
  
  combined.reg <- polr(stars.x ~., data = reg1, method="logistic", Hess = TRUE)
  
# error: attempt to find suitable starting values failed
  
  summary(reg1$city)
  reg1 <- reg1[reg1$city %in% reg1$city[duplicated(reg1$city)],]
  
# retry
  
  combined.reg <- polr(stars.x ~., data = reg1, method="logistic", Hess = TRUE)
  summary(combined.reg)
  combined.reg$coefficients
  
# calculate for significance
  
  coef.table.star <- coef(summary(combined.reg))
  p.value <- pnorm(abs(coef.table.star[, "t value"]),lower.tail = FALSE)* 2
  coef.table.star <- cbind(coef.table.star, "p value" = round(p.value,2))
  coef.table.star <- as.data.frame(coef.table.star)
  coef.table.star$sig <- coef.table.star$`p value`
  coef.table.star$sig <- lapply(coef.table.star$sig, function(x){
    if(x<=0.05){
      coef.table.star$sig<-"*"
    } else {
      coef.table.star$sig <- ""
    }
  })
  
  
### QUESTION H ###

# I recreate another dataframe as with the previous question
  
  reg2 <- dplyr::select(combined, c("stars.x", "city", 
                                    "review_count", "is_open","monday.hr.open","tuesday.hr.open", "wednesday.hr.open",
                                    "thursday.hr.open","friday.hr.open","saturday.hr.open","sunday.hr.open", "checkins", 
                                    "polarity", "Restaurants", "Nightlife","Event Planning & Services", "word.count",
                                    "Arts & Entertainment","Shopping","Coffee & Tea","Beauty & Spas","Desserts",
                                    "Active Life", "Bakeries","Local Services","Home Services","Fast Food","Casinos",
                                    "Automotive", "Wine & Spirits","Hotels & Travel","Fashion","Buffets",
                                    "Health & Medical"))
  reg2$polarity <- as.factor(reg2$polarity)
  
# scale checkins
  
  mean.checkin2 <- mean(reg2$checkins)
  sd.checkin2 <- sd(reg2$checkins)
  reg2$checkins <- (reg2$checkins-mean.checkin2)/sd.checkin2
  
# remove variables with only one sample to avoid the error encountered earlier
  
  reg2 <- reg2[reg2$city %in% reg2$city[duplicated(reg2$city)],]
  
# estimate the model and extract the coefficients
  
  combined.reg2 <- polr(polarity ~., data = reg2, method="logistic", Hess = TRUE)
  summary(combined.reg2)
  combined.reg2$coefficients
  
# calculate for significance
  
  coef.table.polar <- coef(summary(combined.reg2))
  p.value <- pnorm(abs(coef.table.polar[, "t value"]),lower.tail = FALSE)* 2
  coef.table.polar <- cbind(coef.table.polar, "p value" = round(p.value,2))
  coef.table.polar <- as.data.frame(coef.table.polar)
  coef.table.polar$sig <- coef.table.polar$`p value`
  coef.table.polar$sig <- lapply(coef.table.polar$sig, function(x){
    if(x<=0.05){
      coef.table.polar$sig<-"*"
    } else {
      coef.table.polar$sig <- ""
    }
  })    


### P A R T  T W O ###################################################################################################
  
# Due to the the following questions' computationally intensive nature, the Global Environment will be cleared
  
  remove(list=ls())  
  
# Reload the required datasets
  
  load("Labels_test_set_for_students.RData")
  load("Labels_photos_5.RData")
  

### QUESTION A ###
  
# load the images
# first, we specify which destination folder
  
  test.images.folder <- "C:/Users/pierr/OneDrive/Documents/Maastricht University/Masters/SDSS/Exam/stud_5/stud_5"
  
# second, we list the names of each file/image

  test.images.name <- list.files(test.images.folder)
  
# these files are being removed since the algorithm identifies them as png files masked as jpegs
 
  test.images.name <- test.images.name[-23]
  test.images.name <- test.images.name[-67]

# third, we create a list of path names include the image name
  
  test.images.fnames <- paste(test.images.folder, "/", test.images.name[1:length(test.images.name)],sep="")
  
# To save on RAM space, we won't load all the images into RStudio
  

### QUESTION B ###
  
# In this analysis, we will extract features capturing the color model and lines of an image
# We start by capturing the number of important lines in an image
# This part is computationally intensive and is divided into four chunks. 
# This results to inefficient for-loops that, while not elegant, gets the job done. 
# RAM is cleared a bit in between chunks
  
# isolate the name of images as label
  
  image.names <- substr(test.images.name, 1, 22) 
  
# declare a dataframe to collect the lines 
    
  n.n.line <- as.data.frame(0)
  
# first chunk
  
  # each chunk takes the lines for 100 images
  
  for (i in 1:100){ 
    # load the image
    
      test.images <- load.image(test.images.fnames[i])
    
    # detect the edges  
      
      test.image.lines <- cannyEdges(test.images)
      
    # from the edges, get the lines  
      
      lines <- hough_line(test.images, ntheta = 800, data.frame=TRUE)
      
    # we want only the important lines based on the score  
      
      quant <- quantile(lines$score, probs = seq(0.995,1,0.05))
    
    # filter out the important lines
    
      lines <- filter(lines, lines$score>=quant)
    
    # count how many important lines there are
    
      n.lines <- nrow(lines)
      
    # place this number in a dataframe alongside the name of the image
      
      n.n.line [i,1] <- image.names[i]
      n.n.line [i,2] <- n.lines
  }
  
# We remove some heavy variables in the Global Environment between chunks to free up RAM  
  rm(test.images)

# second chunk. The process is the same as the first chunk. 
  
  for (i in 101:200){
    test.images <- load.image(test.images.fnames[i])
    test.image.lines <- cannyEdges(test.images)
    lines <- hough_line(test.images, ntheta = 800, data.frame=TRUE)
    quant <- quantile(lines$score, probs = seq(0.995,1,0.05))
    lines <- filter(lines, lines$score>=quant)
    n.lines <- nrow(lines)
    n.n.line [i,1] <- image.names[i]
    n.n.line [i,2] <- n.lines
  }
  
# clear some space
  
  rm(test.images)
  rm(lines)
  
# third chunk
  
  for (i in 201:300){
    test.images <- load.image(test.images.fnames[i])
    test.image.lines <- cannyEdges(test.images)
    lines <- hough_line(test.images, ntheta = 800, data.frame=TRUE)
    quant <- quantile(lines$score, probs = seq(0.995,1,0.05))
    lines <- filter(lines, lines$score>=quant)
    n.lines <- nrow(lines)
    n.n.line [i,1] <- image.names[i]
    n.n.line [i,2] <- n.lines
  }
  
# clear some space
  rm(test.images)
  rm(lines)
  
# we remove these three images. It causes the fourth chunk to crash RStudio for some reason
  
  test.images.fnames <- test.images.fnames[-304]
  test.images.fnames <- test.images.fnames[-337]
  test.images.fnames <- test.images.fnames[-336]

# fourth and final chunk  
  
  for (i in 301:length(test.images.fnames)){
    test.images <- load.image(test.images.fnames[i])
    test.image.lines <- cannyEdges(test.images)
    lines <- hough_line(test.images, ntheta = 800, data.frame=TRUE)
    quant <- quantile(lines$score, probs = seq(0.995,1,0.05))
    lines <- filter(lines, lines$score>=quant)
    n.lines <- nrow(lines)
    n.n.line [i,1] <- image.names[i]
    n.n.line [i,2] <- n.lines
  }
  
# in order to save time and not having to repeat this process, I will save it as a csv
  
  write.csv(n.n.line, file="lines.csv")
  
# Second feature I wish to extract is the color histogram containing the RGB

# extract the names of the image
  
  rgb.mat.names <- substr(test.images.name, 1, 22)  
  
# declare the matrix to put the values of the color histogram
  
  rgb.mat <- matrix(0, ncol = 81, nrow = length(rgb.mat.names))
  
# create a loop to collect the rgb info per image  
  
  for (i in 1:length(test.images.fnames)){
    
    # get the actual image histogram
    
      x <- getImageHist(test.images.fnames[i], bins = 3, hsv = FALSE, plotting=FALSE)
      
    # we get the columns containing the quantified info of R, G, & B.   
      
      r <- x$r
      g <- x$g
      b <- x$b
      
    # We append them all into one row   
      
      a <- append(r, g)
      a <- append(a, b)
      rgb.mat[i,] <- a
  }
  
# We combine the rgb information with the image name  
  
  rgb.mat <- cbind(rgb.mat, rgb.mat.names)

# Third features I wish to extract is the color histogram containing the HSV
# The process is exactly the same as the RGB above except that hsv = TRUE instead of FALSE

  hsv.mat.names <- substr(test.images.name, 1, 22)  
  length(hsv.mat.names)
  hsv.mat <- matrix(0, ncol = 81, nrow = length(hsv.mat.names))
  for (i in 1:length(test.images.fnames)){
    x <- getImageHist(test.images.fnames[i], bins = 3, hsv = TRUE, plotting=FALSE)
    r <- x$h
    g <- x$s
    b <- x$v
    a <- append(r, g)
    a <- append(a, b)
    hsv.mat[i,] <- a
  }
  hsv.mat <- cbind(hsv.mat, hsv.mat.names)
  
# Rename the columns of the rgb and hsv files
  
  r.names <- c(paste("r",1:27,sep=""))
  g.names <- c(paste("g",1:27,sep=""))
  b.names <- c(paste("b",1:27, sep=""))
  h.names <- c(paste("h",1:27,sep=""))
  s.names <- c(paste("s",1:27,sep=""))
  v.names <- c(paste("v",1:27,sep=""))
  colnames(rgb.mat) <- c(r.names,g.names,b.names,"photo_id")
  colnames(hsv.mat) <- c(h.names, s.names, v.names,"photo_id")
  colnames(n.n.line) <- c("photo_id","n.lines")
  
# turn them into data frames  
  
  rgb.mat <- as.data.frame(rgb.mat)
  hsv.mat <- as.data.frame(hsv.mat)

# build the main data frame. I join the three data.frames with the labels to create the dataset ready for training
  
  image.main <- left_join(n.n.line, photo_labels, by="photo_id")
  image.main <- left_join(image.main, rgb.mat, by="photo_id")
  image.main <- left_join(image.main, hsv.mat, by="photo_id")
  
# to save on time, I create a copy and save it as csv
    
  write.csv(image.main, file="image_main.csv")
  
  
### QUESTION D ###
  
# I set a seed so i can reproduce the results
  
  set.seed(14)

# declare the target attribute as a factor  
    
  image.main$label <- as.factor(image.main$label)

# i create a partition of 80-20
  
  part <- createDataPartition(y=image.main$label, p=0.8, list=FALSE)
  
# I remove some unnecessary columns  
  
  image.main2 <- image.main[-1]
  image.main2 <- image.main2[-3]
  image.main2 <- image.main2[-2]
  
# I create the partition: 80% training set and 20% testing set
  
  train.set2 <- image.main2[part,]
  test.set <- image.main2[-part,]
  

### QUESTION E ###

# I calculate the evaluation metrics of a baseline model
# The baseline model is is just predicting that everything "food," which is the majority attribute
  
# In-Sample 
# Create a dataframe with the true value and the "prediction"
  
  base.in <- train.set2[2]
  base.in$pred <- "food"

# compute the accuracy  

  base.in.acc <- Accuracy(y_pred = base.in$pred, y_true = base.in$label)
  base.in.acc

# comput the recall

  base.in.recall <-Recall(y_pred = base.in$pred, y_true = base.in$label)
  base.in.recall

# compute the f1 score
  base.in.f1 <- F1_Score(y_true = base.in$label, y_pred = base.in$pred) #  error

# out of sample
# the process is similar to above 
  
  base.out <- test.set[2]
  base.out$pred <- "food"
  base.out.acc <- Accuracy(y_pred = base.out$pred, y_true = base.out$label)
  base.out.acc
  base.out.recall <-Recall(y_pred = base.out$pred, y_true = base.out$label)
  base.out.recall
  base.out.f1 <- F1_Score(y_true = base.out$label, y_pred = base.out$pred)
  base.out.f1

# We train the first model: gradient boosted regression trees using the xgboost function
  
  train.set <- train.set2

# xgboost requires a separate dataframe for the labels  
  
  labels <- train.set$label
  
# xgboost also requires the labels to be numeric
    
  labels <- as.numeric(labels)

# remove the labels from the train.set
    
  train.set <- train.set[-grep('label', colnames(train.set))]
  
# actually train the xgboost model. nrounds and eta is put there to prevent overfitting. seed to make it reproducible
# subsample is retained as one to use what limited data is at hand. 
    
  xgb <- xgboost(data=data.matrix(train.set), label=labels, nrounds=25, eta=0.1, seed=14,subsample=1)

# a bit of insight to the resulting model. Highlights top 10 important variables
  
  model <- xgb.dump(xgb, with.state=T)
  model[1:10]
  names <- dimnames(data.matrix(train.set))[[2]]
  imp.mat <- xgb.importance(names, model = xgb)
  xgb.plot.importance(imp.mat[1:10,])

# evaluation of the xgboost model 
# in-sample
  
# predict the values
  
  xg.pred.in <- predict(xgb, data.matrix(train.set))
  xg.pred.in <- round(xg.pred.in)
  
# create a dataframe with the true values and predicted values
  
  xg.in <- train.set2
  xg.in <- xg.in[2]
  xg.in$pred <- xg.pred.in
  
# transform the predicted values from numeric into its original form  
  
  xg.in$pred <- lapply(xg.in$pred, function(x){
  if(x==1){
    x<-"drink"
  } else if (x==2) {
    x <- "food"
  } else if (x==3){
    x <- "inside"
  } else {
    x <- "outside"
  } 
  })

# calculate evaluation metrics: accuracy, recall, and f1-score
  
  xg.in$pred <- unlist(xg.in$pred)
  xg.in.acc <- Accuracy(y_pred = xg.in$pred, y_true = xg.in$label)
  xg.in.acc
  xg.in.recall <-Recall(y_pred = xg.in$pred, y_true = xg.in$label)
  xg.in.recall
  xg.in.f1 <- F1_Score(y_true = xg.in$label, y_pred = xg.in$pred)
  xg.in.f1

# out of sample
# the entire process is similar to the insample evaluation
  
  xg.pred.out <- predict (xgb, data.matrix(test.set[,-2]))
  xg.pred.out <- round(xg.pred.out)
  xg.out <- test.set
  xg.out <- xg.out[2]
  xg.out$pred <- xg.pred.out
  xg.out$pred <- lapply(xg.out$pred, function(x){
  if(x==1){
    x<-"drink"
  } else if (x==2) {
    x <- "food"
  } else if (x==3){
    x <- "inside"
  } else {
    x <- "outside"
  } 
  })
  xg.out$pred <- unlist(xg.out$pred)
  xg.out.acc <- Accuracy(y_pred = xg.out$pred, y_true = xg.out$label)
  xg.out.acc
  xg.out.recall <-Recall(y_pred = xg.out$pred, y_true = xg.out$label)
  xg.out.recall
  xg.out.f1 <- F1_Score(y_true = xg.out$label, y_pred = xg.out$pred)
  xg.out.f1

# Next, we train the second model: SVM

# Train the model  
  
  svm.model <- svm(label~., data=train.set2)

# evaluation of the svm model 
# the whole process is similar to the evaluation of the xgboost model from lines 763-826  
# in-sample
  
  svm.in.pred <- predict(svm.model, train.set2)
  svm.in <- train.set2
  svm.in <- svm.in[2]
  svm.in$pred <- svm.in.pred
  svm.in.acc <- Accuracy(y_pred = svm.in$pred, y_true = svm.in$label)
  svm.in.acc
  svm.in.recall <-Recall(y_pred = svm.in$pred, y_true = svm.in$label)
  svm.in.recall
  svm.in.f1 <- F1_Score(y_true = svm.in$label, y_pred = svm.in$pred)
  svm.in.f1
  
# out sample
  
  svm.out.pred <- predict(svm.model, test.set)
  svm.out <- test.set
  svm.out <- svm.out[2]
  svm.out$pred <- svm.out.pred
  svm.out.acc <- Accuracy(y_pred = svm.out$pred, y_true = svm.out$label)
  svm.out.acc
  svm.out.recall <-Recall(y_pred = svm.out$pred, y_true = svm.out$label)
  svm.out.recall
  svm.out.f1 <- F1_Score(y_true = svm.out$label, y_pred = svm.out$pred)
  svm.out.f1

# Due to disappointing results suspected to be caused by the unbalanced dataset, we will attempt oversampling  

# calculate proportion table of the original training data set
  
  prop.table(table(train.set2$label))

# create datasets that compensates for the underrepresented classes using SMOTE
# we also calculate the resulting dataset's proportion table
  
  smote.svm <- SMOTE(label~., data=train.set2, perc.over=300, seed=14)
  draw.one <- smote.svm
  prop.table(table(smote.svm$label))
  smote.svm <- SMOTE(label~., data=smote.svm, perc.over=200, seed=14)
  draw.two <- smote.svm
  prop.table(table(smote.svm$label))
  smote.svm <- SMOTE(label~., data=smote.svm,perc.over=100, seed=14)
  draw.three <- smote.svm
  prop.table(table(smote.svm$label))
  smote.svm <- SMOTE(label~., data=smote.svm,perc.over=50, seed=14)
  draw.four <- smote.svm
  prop.table(table(smote.svm$label))

# combine all the datasets made by SMOTE and the original dataset

  draw2 <- bind_rows(train.set2, draw.one)
  draw2 <- bind_rows(draw2, draw.two)
  draw2 <- bind_rows(draw2, draw.three)
  draw2 <- bind_rows(draw2, draw.four)
  train.set2 <- draw2
  
# the proportion table shows that the dataset is more balanced now
  
  prop.table(table(train.set2$label))

# for a less time consuming analysis, i have saved the balanced dataset into a csv   
  
draw2 <- read.csv("balanced data.csv")

# prepared the balanced dataset to be the new training set

  draw2 <- draw2[-1]
  train.set2 <- draw2

# train the xgboost on the balanced dataset. 
# This part is similar to the earlier training of the xgboost from line 750
  
  train.set <- train.set2
  labels <- train.set$label
  labels <- as.numeric(labels)
  train.set <- train.set[-grep('label', colnames(train.set))]
  xgb.bal <- xgboost(data=data.matrix(train.set), label=labels, nrounds=25, eta=0.1, seed=14,subsample=1)
  model <- xgb.dump(xgb.bal, with.state=T)
  model[1:10]
  names <- dimnames(data.matrix(train.set))[[2]]
  imp.mat <- xgb.importance(names, model = xgb)
  xgb.plot.importance(imp.mat[1:10,])

# evaluate the new xgboost 
# this part is similar to the earlier evaluation of a xgboost from line 763
  
# in sample
  
  xg.pred.in <- predict(xgb.bal, data.matrix(train.set))
  xg.pred.in <- round(xg.pred.in)
  xg.in <- train.set2
  xg.in <- xg.in[2]
  xg.in$pred <- xg.pred.in
  xg.in$pred <- lapply(xg.in$pred, function(x){
  if(x==1){
    x<-"drink"
  } else if (x==2) {
    x <- "food"
  } else if (x==3){
    x <- "inside"
  } else {
    x <- "outside"
  } 
  })
  xg.in$pred <- unlist(xg.in$pred)
  xg.in.acc <- Accuracy(y_pred = xg.in$pred, y_true = xg.in$label)
  xg.in.acc
  xg.in.recall <-Recall(y_pred = xg.in$pred, y_true = xg.in$label)
  xg.in.recall
  xg.in.f1 <- F1_Score(y_true = xg.in$label, y_pred = xg.in$pred)
  xg.in.f1

# out of sample
  
  xg.pred.out <- predict (xgb.bal, data.matrix(test.set[,-2]))
  xg.pred.out <- round(xg.pred.out)
  xg.out <- test.set
  xg.out <- xg.out[2]
  xg.out$pred <- xg.pred.out
  xg.out$pred <- lapply(xg.out$pred, function(x){
  if(x==1){
    x<-"drink"
  } else if (x==2) {
    x <- "food"
  } else if (x==3){
    x <- "inside"
  } else {
    x <- "outside"
  } 
  })
  xg.out$pred <- unlist(xg.out$pred)
  xg.out.acc <- Accuracy(y_pred = xg.out$pred, y_true = xg.out$label)
  xg.out.acc
  xg.out.recall <-Recall(y_pred = xg.out$pred, y_true = xg.out$label)
  xg.out.recall
  xg.out.f1 <- F1_Score(y_true = xg.out$label, y_pred = xg.out$pred)
  xg.out.f1


# re-train the svm model on the balanced dataset
# this process, even the evaluation, is similar to the earlier svm training from line 830  

  svm.model <- svm(label~., data=train.set2)

# in sample
  
  svm.in.pred <- predict(svm.model, train.set2)
  svm.in <- train.set2
  svm.in <- svm.in[2]
  svm.in$pred <- svm.in.pred
  svm.in.acc <- Accuracy(y_pred = svm.in$pred, y_true = svm.in$label)
  svm.in.acc
  svm.in.recall <-Recall(y_pred = svm.in$pred, y_true = svm.in$label)
  svm.in.recall
  svm.in.f1 <- F1_Score(y_true = svm.in$label, y_pred = svm.in$pred)
  svm.in.f1
  
# out of sample
  
  svm.out.pred <- predict(svm.model, test.set)
  svm.out <- test.set
  svm.out <- svm.out[2]
  svm.out$pred <- svm.out.pred
  svm.out.acc <- Accuracy(y_pred = svm.out$pred, y_true = svm.out$label)
  svm.out.acc
  svm.out.recall <-Recall(y_pred = svm.out$pred, y_true = svm.out$label)
  svm.out.recall
  svm.out.f1 <- F1_Score(y_true = svm.out$label, y_pred = svm.out$pred)
  svm.out.f1

  
### QUESTION G ###
  
# Based on the results, the chosen model to predict the 100 test images is the xgboost training on the unbalanced
# dataset

# The 100 teest images will now undergo the same feature extraction earlier. For a more in-depth detailing of the 
# process, please refer from line 459 
  
# extract file path
  
  test.images.folder2 <- "C:/Users/pierr/OneDrive/Documents/Maastricht University/Masters/SDSS/Exam/test/test"

# list names

  test.images.name2 <- list.files(test.images.folder2)

# two images removed. One is png and one causes RStudio to crash  
  
  test.images.name2 <- test.images.name2[-20]
  test.images.name2 <- test.images.name2[-93]

# create a list of path names include the image name

test.images.fnames2 <- paste(test.images.folder2, "/", test.images.name2[1:length(test.images.name2)],sep="")

# get lines

  image.names2 <- substr(test.images.name2, 1, 22) 
  n.n.line2 <- as.data.frame(0)
  for (i in 1:length(test.images.fnames2)){
    test.images2 <- load.image(test.images.fnames2[i])
    test.image.lines2 <- cannyEdges(test.images2)
    lines2 <- hough_line(test.images2, ntheta = 800, data.frame=TRUE)
    quant2 <- quantile(lines2$score, probs = seq(0.995,1,0.05))
    lines2 <- filter(lines2, lines2$score>=quant2)
    n.lines2 <- nrow(lines2)
    n.n.line2 [i,1] <- image.names2[i]
    n.n.line2 [i,2] <- n.lines2
  }

# get rgb
  
  rgb.mat.names2 <- substr(test.images.name2, 1, 22)  
  length(rgb.mat.names2)
  rgb.mat2 <- matrix(0, ncol = 81, nrow = length(rgb.mat.names2))
  for (i in 1:length(test.images.fnames2)){
    x <- getImageHist(test.images.fnames2[i], bins = 3, hsv = FALSE, plotting=FALSE)
    r <- x$r
    g <- x$g
    b <- x$b
    a <- append(r, g)
    a <- append(a, b)
    rgb.mat2[i,] <- a
  }
  rgb.mat2 <- cbind(rgb.mat2, rgb.mat.names2)

# get hsv

  hsv.mat.names2 <- substr(test.images.name2, 1, 22)  
  length(hsv.mat.names2)
  hsv.mat2 <- matrix(0, ncol = 81, nrow = length(hsv.mat.names2))
  for (i in 1:length(test.images.fnames2)){
    x <- getImageHist(test.images.fnames2[i], bins = 3, hsv = TRUE, plotting=FALSE)
    r <- x$h
    g <- x$s
    b <- x$v
    a <- append(r, g)
    a <- append(a, b)
    hsv.mat2[i,] <- a
  }
  hsv.mat2 <- cbind(hsv.mat2, hsv.mat.names2)

# rename the columns 
  
  colnames(rgb.mat2) <- c(r.names,g.names,b.names,"photo_id")
  colnames(hsv.mat2) <- c(h.names, s.names, v.names,"photo_id")
  colnames(n.n.line2) <- c("photo_id","n.lines")
  rgb.mat2 <- as.data.frame(rgb.mat2)
  hsv.mat2 <- as.data.frame(hsv.mat2)

# build the main data frame
  
  image.main2 <- left_join(n.n.line2, photos.test, by="photo_id")
  image.main2 <- left_join(image.main2, rgb.mat2, by="photo_id")
  image.main2 <- left_join(image.main2, hsv.mat2, by="photo_id")
  write.csv(image.main2, file="image_main_test.csv")

# remove unneccssary columns
  
  image.main3 <- image.main2
  image.main3 <- image.main3[-1]
  image.main3 <- image.main3[-2]
  image.main3 <- image.main3[-2]

# Chosen classifier: XGBoost trained on unbalanced dataset. Make predictions
  
  xg.pred.test <- predict(xgb, data.matrix(image.main3))

# transform predictions
  
  xg.pred.test <- round(xg.pred.test)
  xg.test <- image.main2
  xg.test <- xg.test[1]
  xg.test$pred <- xg.pred.test
  xg.test$pred <- lapply(xg.test$pred, function(x){
  if(x==1){
    x<-"drink"
  } else if (x==2) {
    x <- "food"
  } else if (x==3){
    x <- "inside"
  } else {
    x <- "outside"
  } 
  })
  
# save into RData file
  
  save(xg.test, file="Predictions_Hardy_PM_i6117019.RData")
  
# done!
  
#
#  \'__                         
#  \   '__                  
#  \      '__               
#  \  _    _ '__            
#  \ \ \  \ \   '__         
#  \ \ \  \ \      '__                                _  
#  \ \ \  \ \       __'                              | \ 
#  \ \ \__\ \    __'   __'\                          | | 
#  \  \____/  __'   __'   \                         | | 
#  \       __'   __'      \    |\                   | | 
#  \    __'   __' __  __  \   /, ~\                / / 
#  \,__'   __'   \  \/  \ \  X     `-.....-------./ /                                                                          
#       __'      \ \  | \ \   ~-. ~  ~              |                                                                            
#      '__       \ \\|\ \ \      \             /    | 
#         '__    \ \  \ \ \       \  /_     ___\   / 
#            '__ \_\  \_\ \       | /\ ~~~~~   \ | 
#   		        '__       \       | | \        || | 
#                  '__    \       | |\ \       || ) 
#                     '__,\      (_/ (_/      ((_/ 
# 
# plus points for cat?   
  