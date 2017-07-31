# Ben: Aaron wants the results (overall as well as precinct level) of County Measure A for the June 1990 (gubernatorial primary). 
library(dplyr)
library(DT)
library(readr)
library(readxl)
library(tidyr)

###########################################################################

data <- read_xlsx("E:/JordanBerninger/Prop_A_1990p (Aaron)/a1279_1990p (copy).xlsx")

# there were 4 responses to Prop A:
## counter number 1052: MonoRail
## counter number 1053: Metro Rail
## counter number 1054: Light Rail
## counter number 1055: No Rail

# find the columns that have these counter numbers in the column
# also grab the corresponding votes cast 
## this will be the column immediately following and will have the same _# appended on the column name


# this divides the dataframe into counter number and votes cast sets
# we do this to look up the specific counter numbers (some votes cast = 1052, so we need to divide it up)
# the counter number and its corresponding votes cast column will have the same _# appended onto it
# we use the function below to get all the counter number cells that match our query


#############################################################################
#############################################################################
# now, we will use this object to grab the votes cast data 
## votes cast will have the same indices as the counter_prop_A object
colnames(data)
data2 <- data %>% select(74:581)
# ^ columns 74:581 is the set of alternating counter number , votes cast columns
## I grabbed these values by manually looking at str(data) / colnames(data) 
# ^ make a new object so the other columns are saved for later
data3 <- data.frame(matrix(NA, nrow = nrow(data2), ncol = ncol(data2)))
colnames(data3) <- colnames(data2)
# ^ make a new object so the other columns are saved for later
#############################################################################
# this function will take the pertinent information from the original data frame to the new one

system.time(for(i in 1:nrow(data2)){
  for(j in 1:ncol(data2)){
    if(data2[i,j] %in% c(1052:1055)){    # these are the counter numbers were interested in
      data3[i,j] <- data2[i,j]           # this saves the counter number to the new object
      data3[i,j+1] <- data2[i,j+1]       # this saves the votes cast for that counter number (one cell to the right)
    }
    else {}                              # if the entry is not one of the counter numbers, leave it as NA
  }
})

data3 <- cbind(data[,c(1:16, 73)], data3)
# ^ this will just bring back in the relevant precinct demographic data
View(data3)
#############################################################################
#############################################################################
# remove the columns that are entirely NA, in a new object

data4 <- data3[, !apply(is.na(data3), 2, all)]

# this kept some extra columns because cells had votes cast values as one of (1052:1055)
# can remove the ones that start with votes cast, then have counter number, and NO votes cast following that (BASED OFF the appended _#)
## hard to do that programatically
colnames(data4)
## from a manual inspection, I want to drop data4 columns 17:28 , 45:50
data5 <- data4 %>% select(-c(18:29 , 46:51))

# now ^ has just the relevant demographic columns and the columns that include counter numbers 1052:1055 , and 
## their respective (following) votes cast columns
# all other enteries are NA 

# Now, we want to remove all the rows that are NA on the non demographic data

#data5[!apply(is.na(data5), 1, all),]

data6 <- data5[!apply(is.na(data5[,18:33]), 1, all),]

data7 <- data6 %>% unite("join", c(18:33)) %>% mutate(join = gsub("NA_", "", join)) %>% mutate(join = gsub("_", ":", join)) %>%
  mutate(join = gsub("NA", "", join)) %>%
  separate(join, sep = ":", into = c("monorail", "votes_monorail", "metrorail", "votes_metrorail", "lightrail", "votes_lightrail", "no_rail", "votes_no_rail", "test"))

## this might be it!!!
###########################################################################
#----- TO DO FROM CODE REVIEW WITH BEN ------#
# bring in the total votes cast column - done
# mutate sum of votes cast - done
# mutate all the percentages - done 
# mutate didnt vote and percent didnt vote - done
# change the final column names to the specific vote options - done
# remove the election precincts that begin with 999 - done
## confirm that sum of votes cast =< total votes cast - done
# return the plurality
### confirm the votes cast numbers are close to data from records: - done
### monorail = 111,673
### metro rail = 23,668
### light rail = 50,272
### no rail = 49,653


###########################################################################
data8 <- data7 %>% mutate(votes_monorail = as.numeric(votes_monorail), votes_metrorail = as.numeric(votes_metrorail),
                          votes_lightrail = as.numeric(votes_lightrail), votes_no_rail = as.numeric(votes_no_rail)) %>% 
          mutate(sum_votes_prop_a = (votes_monorail + votes_metrorail + votes_lightrail + votes_no_rail)) %>% 
          mutate(didnt_vote = data7$'TOTAL REGISTRATION' - sum_votes_prop_a) %>%
          mutate(didnt_vote_percent = didnt_vote/data7$'TOTAL REGISTRATION') %>% 
          mutate(monorail_percent = votes_monorail/sum_votes_prop_a , metrorail_percent = votes_metrorail/sum_votes_prop_a, 
                 lightrail_percent = votes_lightrail/sum_votes_prop_a, no_rail_percent = votes_no_rail/sum_votes_prop_a) %>% 
          mutate(vote_check = data7$'TOTAL BALLOTS CAST' - sum_votes_prop_a) %>% 
  mutate(majority = "no_majority") %>%
  mutate(majority = ifelse(monorail_percent > 0.5, "monorail", paste(majority, ""))) %>%
  mutate(majority = ifelse(metrorail_percent > 0.5, "metrorail", paste(majority, ""))) %>%
  mutate(majority = ifelse(lightrail_percent > 0.5, "light_rail", paste(majority, ""))) %>%
  mutate(majority = ifelse(no_rail_percent > 0.5, "no_rail", paste(majority, "")))

## Everything checks out. tidy the column order below

data9 <- data8 %>% select(1, 3:5, 13:16, 12, 6:11, 17, 19, 21, 23, 25, 28, 30:33, 29, 35) 

data9 <- data9 %>% mutate(plurality_on_A = colnames(data9[,17:20])[apply(data9[,17:20],1,which.max)]) %>%
  mutate(plurality_on_A = gsub("votes_", "", plurality_on_A)) %>%
  filter(!grepl("999", data9$'ELECTION PRECINCT'))


write_csv(data9, "Prop_A_1990p_cleaned_data.csv")
###########################################################################
# give some summary statistics for the email body (maybe a presentation)
# histograms, visualizations, etc

t2 <- setDT(data9)
colnames(t2)[9] <- "total_registration"
t3 <- t2[, list(registered_voters = sum(total_registration),
          nonpartisan = sum(NONPARTISAN), 
          democrat = sum(DEMOCRATIC),
          republican = sum(REPUBLICAN),
          independent = sum(`AMERICAN INDEPENDENT`),
          green = sum(GREEN),
          libertarian = sum(LIBERTARIAN),
          total_ballots = sum(`TOTAL BALLOTS CAST`),
          votes_monorail = sum(votes_monorail),
          votes_metrorail = sum(votes_metrorail),
          votes_lightrail = sum(votes_lightrail),
          votes_no_rail = sum(votes_no_rail)),
          by = 'COUNTY SUPERVISORIAL-DISTRICT']

t4 <- t3 %>% as.data.frame() %>%  mutate(sum_votes_prop_a = (votes_monorail + votes_metrorail + votes_lightrail + votes_no_rail)) %>% 
        mutate(monorail_percent = votes_monorail/sum_votes_prop_a , 
               metrorail_percent = votes_metrorail/sum_votes_prop_a, 
               lightrail_percent = votes_lightrail/sum_votes_prop_a, 
               no_rail_percent = votes_no_rail/sum_votes_prop_a) %>%
        mutate(didnt_vote = registered_voters - sum_votes_prop_a) %>%
        mutate(didnt_vote_percent = didnt_vote/registered_voters) %>% 
        mutate(plurality_on_A = colnames(t3[,10:13])[apply(t3[,10:13],1,which.max)]) %>%
        mutate(plurality_on_A = gsub("votes_", "", plurality_on_A)) %>% mutate(majority = "no_majority") %>%
  mutate(majority = ifelse(monorail_percent > 0.5, "monorail", paste(majority, ""))) %>%
  mutate(majority = ifelse(metrorail_percent > 0.5, "metrorail", paste(majority, ""))) %>%
  mutate(majority = ifelse(lightrail_percent > 0.5, "light_rail", paste(majority, ""))) %>%
  mutate(majority = ifelse(no_rail_percent > 0.5, "no_rail", paste(majority, "")))

t5 <- t4 %>% select(1:13, 19, 15:18, 20, 22, 21)

write_csv(t5, "Prop_A_1990p_cleaned_data_sv.csv")
#################################
