################################################################################
## Calculations
## Mary Yu
################################################################################

library(dplyr)
library(lubridate)

cancer <- read.csv("~/Desktop/SecondYearScholarship/Data/cancer_clean.csv",
                   header = T, stringsAsFactors = F)
superfund <- read.csv("~/Desktop/SecondYearScholarship/Data/superfund_clean.csv",
                      header = T, stringsAsFactors = F)
powerplant <- read.csv("~/Desktop/SecondYearScholarship/Data/powerplant_clean.csv",
                       header =T, stringsAsFactors = F)


################################################################################
## Calculations
################################################################################

## Percentage of rising-trend counties that have superfund sites

cancer_sf <- merge(cancer, superfund, by=c("State", "County"), all.x = T)

cancer_sf_rising <- filter(cancer_sf, Recent.Trend == "rising")

cancer_sf_rising <- na.omit(cancer_sf_rising)

cancer_sf_rising <- cancer_sf_rising%>%
  select(County, State)%>%
  group_by(County, State)%>%
  summarise(n = n())

cancer_rising <- cancer%>%
  filter(Recent.Trend == "rising")

perc_rising_sf <- nrow(cancer_sf_rising)/nrow(cancer_rising)

## Percentage of rising-trend counties that have power plant sites

cancer_pp <- merge(cancer, powerplant, by=c("State", "County"), all.x = T)

cancer_pp_rising <- filter(cancer_pp, Recent.Trend == "rising")

cancer_pp_rising <- na.omit(cancer_pp_rising)

cancer_pp_rising <- cancer_pp_rising%>%
  select(County, State)%>%
  group_by(County, State)%>%
  summarise(n = n())

perc_rising_pp <- nrow(cancer_pp_rising)/nrow(cancer_rising)


## Percentage of counties that have one or more power plant yet displays decreasing/stable trend

pp_one_or_more <- powerplant%>%
  filter(primary_fuel=="Coal" | primary_fuel=="Nuclear")%>%
  select(County, State)%>%
  group_by(County, State)%>%
  summarise(n=n())

cancer_pp_nonrising <- merge(cancer, pp_one_or_more, by=c("State", "County"), all.x = T)

cancer_pp_nonrising <- na.omit(cancer_pp_nonrising)

cancer_pp_nonrising <- filter(cancer_pp_nonrising, Recent.Trend != "rising")

perc_nonrising_pp <- nrow(cancer_pp_nonrising)/nrow(pp_one_or_more)

## Percentage of counties that have one or more superfund site yet displays decreasing/stable trend

sf_one_or_more <- superfund%>%
  select(County, State)%>%
  group_by(County, State)%>%
  summarise(n=n())

cancer_sf_nonrising <- merge(cancer, sf_one_or_more, by=c("State", "County"), all.x = T)

cancer_sf_nonrising <- na.omit(cancer_sf_nonrising)

cancer_sf_nonrising <- filter(cancer_sf_nonrising, Recent.Trend != "rising")

perc_nonrising_sf <- nrow(cancer_sf_nonrising)/nrow(sf_one_or_more)


## percentage of top 50 average cancer rate counties with active NPL sites

top_cancer_adjusted <- cancer %>%
  select(County, State, Age.AdjustedCANCERcases_per100000, Recent.Trend)%>%
  arrange(desc(Age.AdjustedCANCERcases_per100000))

top_50_cancer<- top_cancer_adjusted[1:50,]

sf_50 <- merge(top_50_cancer, superfund, by=c("State", "County"), all.x = T)

sf_50 <- na.omit(sf_50)

top_50_with_sf <- nrow(sf_50)/50


## percentage of top 50 average cancer rate counties with coal/nuclear power plants

top_cancer_adjusted <- cancer %>%
  select(County, State, Age.AdjustedCANCERcases_per100000, Recent.Trend)%>%
  arrange(desc(Age.AdjustedCANCERcases_per100000))

top_50_cancer<- top_cancer_adjusted[1:50,]

pp_50 <- merge(top_50_cancer, powerplant, by=c("State", "County"), all.x = T)

pp_50 <- na.omit(pp_50)

top_50_with_pp <- nrow(pp_50)/50


################################################################################
## Hypothesis testing
## this tests whether counties with facilities have significant different
## cancer rates than counties without facilities.
################################################################################

## superfund sites
cancer_sf <- merge(cancer, superfund, by=c("State", "County"), all.x = T)

sf_yes <- na.omit(cancer_sf)

sf_yes <- sf_yes%>%
  group_by(County, State)%>%
  summarise(mean = mean(Age.AdjustedCANCERcases_per100000))

sf_no <- cancer_sf%>%
  filter(is.na(Site.Name))%>%
  select(Age.AdjustedCANCERcases_per100000)

t.test(sf_yes[,3]/100000, sf_no/100000)


## power plants

cancer_pp <- merge(cancer, powerplant, by=c("State", "County"), all.x = T)
cancer_pp1 <- filter(cancer_pp, primary_fuel=="Coal" | primary_fuel=="Nuclear")

pp_yes <- na.omit(cancer_pp1)

pp_yes <- pp_yes%>%
  group_by(County, State)%>%
  summarise(mean = mean(Age.AdjustedCANCERcases_per100000))

pp_no <- cancer_pp%>%
  filter(is.na(name))%>%
  select(Age.AdjustedCANCERcases_per100000)

t.test(pp_yes[,3]/100000, pp_no/100000)


