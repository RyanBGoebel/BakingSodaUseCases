start_time <- Sys.time()
#---------------------------------------------------------#

#   Trending Hashtags.R
#   VIA XN Project 
#
#   Roux Institute @ Northeastern University // Spring 2021                               
#   ALY6080.81288 
#   Ryan Goebel

#---------------------------------------------------------#
#   Description: 
#       This file has the code that creates the necessary objects for the "Trending Hashtags"
#       page on the dashboard including building the corpus from the Code Clean.R output, d. 
#
#---------------------------------------------------------#

#######################################################################
# Note:  Start with cleaned dataframe "d" from Code Clean
#######################################################################

# Load required packages
library(tm)
library(tidyverse)
library(forecast)


# Function for calculating Holt-Winters trend ("slope") variable
prediction_fn <- function(trendData){
  library(forecast)
  library(tm)
  
  # Remove the 'Month' (or 'Week') Column (index = 1)
  trendData <- trendData[-c(1)]
  predictionData_df <- data.frame(item = names(trendData),
                                  slope = NA)
  
  for (j in 1:ncol(trendData)){
    tryCatch({
      tempRow <- trendData[,j]
      temp.ts <- ts(tempRow)#,frequency=tsFreq,start=minDate)
      temp.HW <- HoltWinters(temp.ts, gamma=FALSE)
      temp.f <- forecast(temp.HW, h=5)
      
      predictionData_df$slope[j] <- 100*(temp.f$mean[2] - temp.f$mean[1])
    }, error=function(e){})
  }
  predictionData_df <- predictionData_df[order(-predictionData_df$slope),]
  return(predictionData_df)
}




############# BEGIN CORP BUILDING AND FREQ CALCS #############

# Keep only date and content variables of d
d.ht <- subset(d, select=c("date", "mentionComplete","mediaGroup"))

# Keep only rows with hashtags
d.ht <- d.ht %>% filter(grepl("(#+[a-zA-Z0-9(_)]{1,})", mentionComplete))

# New column for MonthYear
d.ht$week <- floor_date(d.ht$date,c("week"))
d.ht$month <- floor_date(d.ht$date,c("month"))

d.ht <- na.omit(d.ht)

############# CREATE CLEAN CORPORA #############
c <- as.vector(d.ht$mentionComplete)
corp <- Corpus(VectorSource(c))

corp <- tm_map(corp, function(x) gsub("[^\x01-\x7F]", " ", x))
corp <- tm_map(corp, function(x) gsub("http\\S+", " ", x))

for (j in seq(corp)) {
  temp <- unlist(strsplit(corp[[j]]$content, " "))                    # break string into individual terms
  hash <- grepl("#", temp)                                            # logical vector of terms with "#" in them
  corp[[j]]$content <- temp[hash]                                     # keep only terms with "#"
  corp[[j]]$content <- gsub("[\r\n]", " ", corp[[j]]$content)         # remove line breaks inserted in previous steps

}

corp <- tm_map(corp, function(y) gsub("&#x200b;", " ", y))
corp <- tm_map(corp, stripWhitespace)

# Create document term matrices
dtm <- DocumentTermMatrix(corp)   
dtm <- removeSparseTerms(dtm, 0.999) 

# Convert to matrix and then to dataframe
dtm_df <- as.data.frame(as.matrix(dtm))

# Join dtm with dates
dtm_df$doc_id <- seq.int(nrow(dtm_df))

d.ht$doc_id <- seq.int(nrow(d.ht))
dtm_df <- left_join(dtm_df,d.ht[c("doc_id","week","month","mediaGroup")],by=c("doc_id"))

# ==============================================================================
# Establishing Normalization Factors
# ==============================================================================

# Create a dataframe consisting of the number of documents per unit time
docsPerWeek <- data.frame(table(d.ht$week))
docsPerWeek<- docsPerWeek[as.Date(docsPerWeek$Var1)>=as.Date(weekStart),]
docsPerWeek<- docsPerWeek[as.Date(docsPerWeek$Var1)<=as.Date(weekEnd),]

docsPerMonth <- data.frame(table(d.ht$month))
docsPerMonth<- docsPerMonth[as.Date(docsPerMonth$Var1)>=as.Date(monthStart),]
docsPerMonth<- docsPerMonth[as.Date(docsPerMonth$Var1)<=as.Date(monthEnd),]

# Create our final table
trendMonths_hashtags <- month_df
trendWeeks_hashtags <- week_df

# Normalize by calculating relative frequency within each month
for (i in 1:(ncol(dtm_df)-3)){
  hashtag <- names(dtm_df)[i]
  temp_df <- dtm_df[, c("week","month", hashtag)]
  
  temp_df <- temp_df[temp_df[c(hashtag)]>0,]
  
  temp_month_df <- data.frame(table(temp_df$month))
  names(temp_month_df) <- c("month","Freq")
  temp_month_df <- left_join(month_df,temp_month_df,by=c("month"))
  temp_month_df[is.na(temp_month_df)] <- 0
  
  temp_month_df$Freq <- round(temp_month_df$Freq / docsPerMonth$Freq,4)
  names(temp_month_df) <- c("month",hashtag)
  
  trendMonths_hashtags <- left_join(trendMonths_hashtags,temp_month_df,by=c("month"))
  
  temp_week_df <- data.frame(table(temp_df$week))
  names(temp_week_df) <- c("week","Freq")
  temp_week_df <- left_join(week_df,temp_week_df,by=c("week"))
  temp_week_df[is.na(temp_week_df)] <- 0
  
  temp_week_df$Freq <- round(temp_week_df$Freq / docsPerWeek$Freq,4)
  names(temp_week_df) <- c("week",hashtag)
  
  trendWeeks_hashtags <- left_join(trendWeeks_hashtags,temp_week_df,by=c("week"))
}

#names(trendMonths_hashtags) <- gsub("#", "", names(trendMonths_hashtags))
#names(trendWeeks_hashtags) <- gsub("#", "", names(trendWeeks_hashtags))


end_time <- Sys.time()
print("Trending Hashtags Dataset Created!")
print(end_time - start_time)
