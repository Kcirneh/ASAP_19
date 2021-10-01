library(Hmisc)
library(tidyverse)
library(reshape)
library(writexl)
library(stargazer)
library(dplyr)


did1 <- subset(df1, date == "2021-04-24")
did2 <- subset(df1, date == "2021-06-24")

did <- subset(df1, date == "2021-06-24" | date == "2021-04-24")

did$dAfter <- ifelse(did$date == "2021-06-24", 1, 0)

noMasks <- c("Montana", "North Dakota", "South Dakota", "Idaho", "Wyoming", "Nebraska", "Iowa", "Missouri", "Nevada",
             "Utah", "Arizona", "Texas", "Oklahoma", "Kansas", "Arkansas", "Tennessee", "Alabama", "Georgia", "Florida",
             "South Carolina", "Ohio", "New Hampshire", "Alaska")

did$dTreated <- ifelse(is.element(did$state, noMasks), 1, 0)

did$dTreatedAfter.interaction <- did$dTreated * did$dAfter

did.text <- merge(did, df20, by=c("state", "county"))

did.text$CasesCap <- did.text$cases/did.text$POPESTIMATE2020
did.text$DeathsCap <- did.text$deaths/did.text$POPESTIMATE2020


reg.did <- lm(CasesCap ~ dTreated + dAfter + dTreatedAfter.interaction + as.factor(state), data = did.text)
stargazer(reg.did, intercept.bottom = F, align = T, no.space = T,
          omit = "state", type = "text")

electiond <- read.csv("Election_data3_csv.csv", header=TRUE, sep = ",")
electiond1 <- data.frame(election)

electiond1$county <- gsub(" County","", (election1$county))
electiond1

dfdid <- merge(did.text, electiond1, by=c("state", "county"))
dfdid$dRepublican <- ifelse(dfdid$Trump.ratio > 0.5, 1, 0)

dfdid$dRepublicanAfterInteraction <- dfdid$dAfter * dfdid$dRepublican
dfdid$dTreatedRepublican <- dfdid$dRepublican * dfdid$dTreated
dfdid$dthreeway <- dfdid$dRepublican * dfdid$dTreated * dfdid$dAfter

reg.didR <- lm(DeathsCap ~ dRepublican + dAfter + dTreated + dTreatedAfter.interaction + dRepublicanAfterInteraction + dfdid$dthreeway, data = dfdid)
stargazer(reg.didR, intercept.bottom = F, align = T, no.space = T,
          omit = "state", type = "text")


