################################################################
######## CODE TO CREATE CUSTOM BLOCKLIST FOR PROLIFIC ##########
################################################################

# THIS CODE NEEDS TO BE SAVED IN THE SAME FOLDER WERE .CSV FROM PROLIFIC WITH DEMO DATA ARE
# INPUT: .CSV FROM PROLIFIC
# OUTPUT: .TXT FILE FOR CUSTOM BLOCK LIST WITH PARTICIPANTS ID

# NOTES:
# It is very important that all .csv imported have the same number of columns
# Prolific has lately changed the demographic data so that more columns are now created
# and also have different names
# CHECK THAT ALL .CSV FILES HAVE A COLUMNS CALLED "participant_id" and "status"
# THOSE ARE OUR COLUMNS OF INTEREST

# 1. Set up materials
## libraries

library(tidyverse) # to keep things tidy
library(stringr) # useful for cleaning variables
library(janitor) # for use of regular expressions (i.e., finding matches of text in variables)
library(data.table)

# SET WD

# set wd
# this sets the working directory to wherever this file is stored
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Modify R functions to make the code run smoother

options(scipen=999) # avoid e notation
`%!in%` <- Negate(`%in%`) #use %!in% to filter out

# 2. FUNCTIONS

## CREATE FUNCTION

# Important bits
# We may want to filter based on the completion status (i.e., returned, approved, rejected)
# We may not want to exclude all participants based from all studies

# create a function where we can input a dataframe and say based on what values we want to filter out the table

filter_experiments <- function(df, column_name, values_reject){
  # input: df = name of the data frame
  # column_name: where the status is stored
  # values_reject *needs* to be a list of the values (i.e., status, experiment) that we do not want to include
  # output: filtered data frate
  df <- df %>%
    dplyr::filter(column_name %!in% values_reject)
  return(df)
}

## load data
# this function lists the file names in this folder that end in .csv
# NB this means that only the participants' files are in the folder
# if there is another .csv in the folder, the code will crash

# 3. BRING DATA

temp <- list.files(patter = "*.csv")

# put all the dfs in one and do some word

AllExps <- data.frame() # create an empty data frame to then store our data in it

# we are now going to important all the data files 
# we do that with a for loop

for(i in temp){ # goes through each item in the vector temp, which is the file names of the files stored in the folder
  
  FILE <- read_csv(i) # read in each file
  FILE$prolific_name <- paste(tolower(i)) # save the name of the prolific study in the df by coyping via paste the name of the file (i). we save it in lower case (tolower())
  FILE$year_test <- str_sub(FILE$started_datetime, 1, 4) # get the year, this is specfic to our code and data
  FILE <- FILE %>%
    select(c("participant_id", "status", "prolific_name", "year_test")) # select only the columns of interest
  AllExps <- rbind(AllExps, FILE) # bind the imported file i to the larger dataframe, AllExps
}

# clean the environment

rm(FILE, i, temp)

# Edit to filter out by exps
# this is code-specific
# when new experiments with different names are fed
# we need to create a new column
#THIS IS THE DATA DICTIONARY#

AllExps <- AllExps %>% 
  mutate(
    key_word = case_when(
      str_detect(prolific_name, "questionnaires") == TRUE ~ "questionnaire",
      str_detect(prolific_name, "what is it called") == TRUE ~ "what_called",
      str_detect(prolific_name, "what's it called") == TRUE ~ "what_called",
      str_detect(prolific_name, "online games") == TRUE ~ "online_games", # in the same group we have online games with remove players, juego online and online game
      str_detect(prolific_name, "juego online") == TRUE ~ "online_games",
      str_detect(prolific_name, "online game") == TRUE ~ "online_games",
      str_detect(prolific_name, "how formal is") == TRUE ~ "word_formal",
      str_detect(prolific_name, "what are we talking about") == TRUE ~ "what_talk",
      str_detect(prolific_name, "where is this name") == TRUE ~ "where_word_from",
      str_detect(prolific_name, "who says this") == TRUE ~ "who_word",
      TRUE ~ NA_character_
    )
  )

# Test to check that there are no NA
# this code uses the function assert() from the package testit
# if checks whether the statement written inside the function is true
# in case, it checks whether the statement any(is.na(AllExps$key_word)) == FALSE is TRUE

testit::assert(any(is.na(AllExps$key_word)) == FALSE)

#############################################
# THIS IS THE BIT OF CODE YOU CAN COSTUMISE #
#############################################

# FILTER BY STATUS

# this the previously defined function
# the values to modify are "values_reject"
# this is for status of participants
# ! WRITE ONLY THE VALUES YOU WANT TO REJECT
# that is, we usualyl don't care about participants who were rejected
# but we want to keep the participants who were paid to use them in order block list

AllExps <- filter_experiments(AllExps, column_name = "status", values_reject = c("REJECTED"))

# FILTER BY EXPERIMENT
# the values to modify are "values_reject"
# this is for experiments of participants (need a list of the experiments in the lab)

AllExps <- filter_experiments(AllExps, column_name = "key_word", values_reject = c("what_called", "online_games", "what_talk", "where_word_from", "who_word"))

# This writes the df in a .txt that is then used to filter out ppts in Prolific
# remove duplicates

AllExps <- AllExps[!duplicated(AllExps$participant_id),]

# writes the txt file
# the text file will appear in the folder and it's called "filtered_participants"

AllExps %>%
  select("participant_id") %>%
  write.table(., "filtered_participants.txt", sep = "\t",
              row.names = FALSE)


