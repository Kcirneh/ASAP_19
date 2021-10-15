library(Hmisc)
library(tidyverse)
library(reshape)
library(writexl)
library(stargazer)
library(readxl)
library(ROCR)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(psych)
library(stargazer)
library(ROCR)
library(plyr)
library(reshape2)


#clean census dataset

dataset_census <- read.csv("cc-est2019-alldata.csv", header = TRUE, sep = ",")
dfCensus <- data.frame(dataset_census)
dfCensus <- subset(dfCensus, AGEGRP == 0 & YEAR == 12)
dfCensus$YEAR <- NULL
dfCensus$AGEGRP <- NULL
dfCensus <- subset(dfCensus, select = c(STNAME, CTYNAME, TOT_POP, TOT_MALE, TOT_FEMALE, WA_MALE, WA_FEMALE))
dataset_census <- NULL

dfCensus$CTYNAME <- gsub(" County","", (dfCensus$CTYNAME))
colnames(dfCensus)[1] <- "state"
colnames(dfCensus)[2] <- "county"


#clean age dataset

dataset_age <- read.csv("county_age.csv", header = TRUE, sep = ",")
dfAge <- data.frame(dataset_age)
dfAge$name <- gsub(" County","", (dfAge$name))
colnames(dfAge)[2] <- "state"
colnames(dfAge)[3] <- "county"
dataset_age <- NULL

#clean density dataset

dataset_density <- read.csv("census-population-landarea.csv", header = TRUE, sep = ",")
dfDensity <- data.frame(dataset_density)
dataset_density <- NULL

#clean econ dataset

dataset_econ <- read.csv("unemployment_data_csv.csv", header = TRUE, sep = ",")
dfEcon <- data.frame(dataset_econ)
dfEcon <- subset(dfEcon, select = c(county, state, Unemployment_rate_2020, Median_Household_Income_2019, Med_HH_Income_Percent_of_State_Total_2019))
dataset_econ <- NULL

dfEcon$county <- gsub(" County","", (dfEcon$county))


#clean election dataset

dataset_election <- read.csv("Election_data3_csv.csv", header=TRUE, sep = ",")
dfElection <- data.frame(dataset_election)
dataset_election <- NULL

dfElection$county <- gsub(" County","", (dfElection$county))


#clean covid dataset

dataset_covid <- read.csv("us-counties_complete.csv", header = TRUE, sep = ",")
dfCovid <- data.frame(dataset_covid)
dfCovid <- subset(dfCovid, date == "2020-10-10" | date == "2020-11-10" | date == "2020-12-10" | date == "2021-01-10"
                  | date == "2021-02-10" | date == "2021-03-10")
dfCovidLM <- data.frame(dataset_covid)
dfCovidLM <- subset(dfCovidLM, date == "2021-08-31")
dataset_covid <- NULL

#merge everything

dfMerge <- NULL
dfMerge <- merge(dfCovid, dfElection, by=c("state", "county"))
dfMerge <- merge(dfMerge, dfCensus, by=c("state", "county"))
dfMerge <- merge(dfMerge, dfEcon, by=c("state", "county"))
dfMerge <- merge(dfMerge, dfAge, by=c("state", "county"))

dfControls <- NULL
dfControls <- merge(dfElection, dfCensus, by=c("state", "county"))
dfControls <- merge(dfControls, dfAge, by=c("state", "county"))
dfControls <- merge(dfControls, dfEcon, by=c("state", "county"))
dfControls <- merge(dfControls, dfDensity, by=c("fips"))




dfMerge$fips <- NULL

dfMerge$Minorities <- dfMerge$TOT_POP - (dfMerge$WA_FEMALE + dfMerge$WA_MALE)
dfMerge$R_Minorities <- NULL
dfMerge$P_Minorities <- (dfMerge$Minorities / dfMerge$TOT_POP) * 100

#change column names dfControls

colnames(dfControls)[7] <- "Trump_votes_ratio"
colnames(dfControls)[8] <- "Biden_votes_ratio"
colnames(dfControls)[6] <- "Total_votes"
colnames(dfControls)[3] <- "county"
colnames(dfControls)[9] <- "population"
colnames(dfControls)[10] <- "male"
colnames(dfControls)[11] <- "female"
colnames(dfControls)[12] <- "white_male"
colnames(dfControls)[13] <- "white_female"
colnames(dfControls)[15] <- "unemployment_rate"
colnames(dfControls)[16] <- "median_HH_income"
colnames(dfControls)[17] <- "median_HH_income_percentage_of_state_avg"
colnames(dfControls)[14] <- "median_age"

dfControls$minorities <- dfControls$population - dfControls$white_male - dfControls$white_female
dfControls$minorities.ratio <- (dfControls$minorities / dfControls$population) * 100

colnames(dfControls)[19] <- "minorities"
colnames(dfControls)[20] <- "minorities.ratio"

dfMergeLM2 <- merge(dfCovidLM, dfControls, by = c("fips"))
dfMergeLM2$county.x <- NULL
dfMergeLM2$state.x <- NULL



#change column names

colnames(dfMerge)[6] <- "Trump_votes"
colnames(dfMerge)[7] <- "Biden_votes"
colnames(dfMerge)[8] <- "Total_votes"
colnames(dfMerge)[2] <- "county"
colnames(dfMerge)[11] <- "population"
colnames(dfMerge)[12] <- "male"
colnames(dfMerge)[13] <- "female"
colnames(dfMerge)[14] <- "white_male"
colnames(dfMerge)[15] <- "white_female"
colnames(dfMerge)[16] <- "unemployment_rate"
colnames(dfMerge)[17] <- "median_HH_income"
colnames(dfMerge)[18] <- "median_HH_income_percentage_of_state_avg"
colnames(dfMerge)[19] <- "minorities"
colnames(dfMerge)[20] <- "minorities.ratio"


#merge LM

dfMergeLM <- merge(dfCovidLM, dfElection, by=c("state", "county"))
dfMergeLM <- merge(dfMergeLM, dfCensus, by=c("state", "county"))
dfMergeLM <- merge(dfMergeLM, dfEcon, by=c("state", "county"))
dfMergeLM$fips <- NULL



colnames(dfMergeLM)[6] <- "Trump_votes"
colnames(dfMergeLM)[7] <- "Biden_votes"
colnames(dfMergeLM)[8] <- "Total_votes"
colnames(dfMergeLM)[2] <- "county"
colnames(dfMergeLM)[11] <- "population"
colnames(dfMergeLM)[12] <- "male"
colnames(dfMergeLM)[13] <- "female"
colnames(dfMergeLM)[14] <- "white_male"
colnames(dfMergeLM)[15] <- "white_female"
colnames(dfMergeLM)[16] <- "unemployment_rate"
colnames(dfMergeLM)[17] <- "median_HH_income"
colnames(dfMergeLM)[18] <- "median_HH_income_percentage_of_state_avg"


dfMergeLM$minorities <- dfMergeLM$population - dfMergeLM$white_male - dfMergeLM$white_female
dfMergeLM$minorities.ratio <- (dfMergeLM$minorities / dfMergeLM$population) * 100


#Export to excel

write_xlsx(dfMergeLM2,"C:\\Users\\Andrei\\Documents\\Master Erasmus Uni\\Advanced Stats and Programing\\Individual Assignment 3\\Group Assignment clean\\linear_model_dataset2.xlsx")

write_xlsx(dfMergeLM,"C:\\Users\\Andrei\\Documents\\Master Erasmus Uni\\Advanced Stats and Programing\\Individual Assignment 3\\Group Assignment clean\\linear_model_dataset.xlsx")

########################### RUNNING THE LINEAR MODEL ###################################

attach(dfMergeLM2)

dfMergeLM2$casescap <- dfMergeLM2$cases/dfMergeLM2$population

mdl1 <- casescap ~ Trump_votes_ratio + median_age + minorities.ratio + 

  
rsltOLS1 = lm(mdl1, data = dfMergeLM2)

stargazer(rsltOLS1, type  = "text")




############################DIFF in DIFF################################################

dfdid <- merge(dfdid, dfControls, by = c("state", "county"))
dfdid$daily_casesCapita <- dfdid$daily_cases/dfdid$population

datasetdid <- read_excel("Diff_in_diff.xlsx")
dfdid <- data.frame(datasetdid)
dfdid <- subset(dfdid, date == "2021-04-24" | date == "2021-06-24")

dfdid$delete1 <- NULL
dfdid$delete2 <- NULL

dfdid <- merge (dfdid, dfElection, by = c("state", "county"))
dfdid$dRepublican <- ifelse(dfdid$Trump.ratio > 0.5, 1, 0)
dfdid$dRepublicanAfterInteraction <- dfdid$dAfter * dfdid$dRepublican
dfdid$dTreatedAfter.interaction <- dfdid$dTreated * dfdid$dAfter
dfdid$dthreeway <- dfdid$dRepublican * dfdid$dTreated * dfdid$dAfter

reg.didR <- lm(daily_cases ~ dRepublican + dAfter + dTreated + dTreatedAfter.interaction + dRepublicanAfterInteraction + dfdid$dthreeway, data = dfdid)
reg.didR2 <- lm(daily_cases ~ dRepublican + dAfter + dTreated + dTreatedAfter.interaction + dRepublicanAfterInteraction + dfdid$dthreeway + as.factor(state), data = dfdid)
reg.didR3 <- lm(cases ~ dRepublican + dAfter + dTreated + dTreatedAfter.interaction + dRepublicanAfterInteraction + dfdid$dthreeway, data = dfdid)
reg.didR4 <- lm(daily_casesCapita ~ dRepublican + dAfter + dTreated + dTreatedAfter.interaction + dRepublicanAfterInteraction + dfdid$dthreeway, data = dfdid)



stargazer(reg.didR, reg.didR2, intercept.bottom = F, align = T, no.space = T,
          omit = "state", type = "text")


stargazer(reg.didR, reg.didR2,  intercept.bottom = F, align = T, no.space = T,
          omit = "state")




noMasks <- c("Montana", "North Dakota", "South Dakota", "Idaho", "Wyoming", "Nebraska", "Iowa", "Missouri", "Nevada",
             "Utah", "Arizona", "Texas", "Oklahoma", "Kansas", "Arkansas", "Tennessee", "Alabama", "Georgia", "Florida",
             "South Carolina", "Ohio", "New Hampshire", "Alaska")

dfdid$dTreated <- ifelse(is.element(dfdid$state, noMasks), 1, 0)

#######################################################################################################
########################### Linear ####################################### Model ######################

dfMergeLM2$Trump_minus_Biden <- dfMergeLM2$Trump_votes_ratio - dfMergeLM2$Biden_votes_ratio
dfMergeLM2$R_male <- dfMergeLM2$male/dfMergeLM2$population

mdlOLS <- casescap ~ Trump_votes_ratio + R_male + minorities.ratio + unemployment_rate +
  median_HH_income_percentage_of_state_avg + pop_dens + median_age + Trump_minus_Biden
mdlOLS2 <- casescap ~ Trump_minus_Biden + R_male + minorities.ratio + unemployment_rate +
  median_HH_income_percentage_of_state_avg + pop_dens + median_age
  

rsltOLS11 <- lm(mdlOLS, data = dfMergeLM2)
rsltOLS12 <- lm(mdlOLS2, data = dfMergeLM2)


stargazer(rsltOLS11, rsltOLS12, type = "text")



df1[1:5,]
attach(dfdid)
avgdc <- NULL
tmp <- NULL

avgdc <- ddply(dfdid, .(dTreated, dAfter, dRepublican), summarize,
               avgDailyCases = mean(daily_cases, na.rm=TRUE))

tmp <- dcast(avgdc, dAfter ~ dTreated + dRepublican, value.var="avgDailyCases")
tmp<- rbind(tmp, tmp[2,]-tmp[1,])

rownames(tmp) <- c("Before", "After", "Difference")
tmp[3, "dAfter"] <- NA

stargazer(tmp, summary = FALSE, align = T, type = "text")
stargazer(tmp, summary = FALSE, align = T)

