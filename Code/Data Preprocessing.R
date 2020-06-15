################################################################################
## Data Preprocessing Code and EDA
## Mary Yu
################################################################################

## Import packages
library(dplyr)
library(tidyr)
library(stringr)

## Import three data files

cancer <- read.csv("~/Desktop/Second Year Scholarship/Data/USA_CancerRates_All_ByCounty.csv",
                   skip = 11, header = T, stringsAsFactors = F)
superfund <- read.csv("~/Desktop/Second Year Scholarship/Data/Superfund National Priorities List (NPL) Sites with Status Information.csv",
                      header = T, stringsAsFactors = F)
powerplant <- read.csv("~/Desktop/Second Year Scholarship/Data/Powerplant.csv",
                       header =T, stringsAsFactors = F)

################################################################################
## Preprocess Cancer rate data set
################################################################################

## remove the first row (contains the data for whole US)
cancer <- cancer[-1,]

## Separate state and county
## Remove the word "county" for future joins
## remove "#" from age adjusted cancer cases
## for puerto rico and DC, use county as state

cancerSep <- cancer %>%
  separate(State, c("County", "State"), sep = "([\\,\\(])", extra = "drop")%>%
  mutate(County = str_remove_all(County, " County"))%>%
  mutate(Age.AdjustedCANCERcases_per100000 = str_remove_all(Age.AdjustedCANCERcases_per100000, " #"))%>%
  mutate(State = ifelse(County == "Puerto Rico", "Puerto Rico", ifelse(County == "District of Columbia", "District of Columbia", State)))

## look at variable types
sapply(cancerSep, class)

## adjust variable type
cancerf <- transform(cancerSep, Age.AdjustedCANCERcases_per100000 = as.numeric(Age.AdjustedCANCERcases_per100000),
                     Recent.5.Year.Tren.in.Incidence.Rates = as.numeric(Recent.5.Year.Tren.in.Incidence.Rates),
                     Lower.95.CI = as.numeric(Lower.95.CI), Upper.95..CI = as.numeric(Upper.95..CI))

## look for rows with NA, these counties does not have data about recent trends
na_data <- cancerf %>% filter_all(any_vars(is.na(.))) 

################################################################################
## Preprocess superfund data set
################################################################################

## Remove columns such as site ids, and columns with too many missing values
superfund <- superfund[, -c(3, 4, 5, 15, 18:32)]

################################################################################
## Preprocess powerplant data set
################################################################################

## remove columns such as site ids, and columns with too many missing values
powerplant <- powerplant[, -c(1, 2, 4, 12:14)]

power1 <- powerplant %>%
  mutate(County = str_remove_all(County, " County"))

################################################################################
## EDA tables
################################################################################

## extract counties with top age-adjusted cancer case
top_cancer_adjusted <- cancerf %>%
  select(County, State, Age.AdjustedCANCERcases_per100000, Recent.Trend)%>%
  arrange(desc(Age.AdjustedCANCERcases_per100000))

## extract counties with top annual cancer case count
top_cancer_count <- cancerf %>%
  select(County, State, Average.Annual.Count, Recent.Trend)%>%
  arrange(desc(Average.Annual.Count))

## extract states with top age-adjusted cancer case
top_cancer_adjusted_state <- cancerf %>%
  select(State, Age.AdjustedCANCERcases_per100000)%>%
  group_by(State)%>%
  summarise(Age.AdjustedCANCERcases_per100000 = sum(Age.AdjustedCANCERcases_per100000))%>%
  arrange(desc(Age.AdjustedCANCERcases_per100000))

## extract states with annual cancer case count
top_cancer_count_state <- cancerf %>%
  select(State, Average.Annual.Count)%>%
  group_by(State)%>%
  summarise(Average.Annual.Count = sum(Average.Annual.Count))%>%
  arrange(desc(Average.Annual.Count))

## look at count of active superfund locations for each state
top_superfund_state <- superfund %>%
  filter(Status == "NPL Site")%>%
  select(State)%>%
  group_by(State)%>%
  summarise(Count = n())%>%
  arrange(desc(Count))
    
## look at count of active superfund locations for each county
top_superfund_county <- superfund %>%
  filter(Status == "NPL Site")%>%
  select(County, State)%>%
  group_by(County, State)%>%
  summarise(Count = n())%>%
  arrange(desc(Count))

    
    
    

