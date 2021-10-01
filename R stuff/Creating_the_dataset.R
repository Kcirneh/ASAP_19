library(Hmisc)
library(tidyverse)
library(reshape)
library(writexl)
library(stargazer)

dataset <- read.csv("us-counties_complete.csv", header = TRUE, sep = ",")
df1 <- data.frame(dataset)

#delete a column
df1$fips <- NULL

#delete all rows that contain 2020
df2 <- df1[!grepl("2020", df$date),]

#write to Excel
write_xlsx(df2, "C:\\Users\\Andrei\\Documents\\Master Erasmus Uni\\Advanced Stats and Programing\\Group Assignment\\R stuff\\US_Counties_CLEANED.xlsx")

#delete all additional months
df3 <- df2[!grepl("2021-01", df2$date),]
df4 <- df3[!grepl("2021-02", df3$date),]
df5 <- df4[!grepl("2021-03", df4$date),]
df6 <- df5[!grepl("2021-04", df5$date),]
df7 <- df6[!grepl("2021-05", df6$date),]
df8 <- df7[!grepl("2021-06", df7$date),]
df9 <- df8[!grepl("2021-07", df8$date),]
df10 <- subset(df9, date=="2021-08-31")

write_xlsx(df10, "C:\\Users\\Andrei\\Documents\\Master Erasmus Uni\\Advanced Stats and Programing\\Group Assignment\\R stuff\\US_Counties_CLEANED_31st_August.xlsx")

#census dataset
dataset2 <- read.csv("co-est2020-alldata.csv", header = TRUE, sep = ",")
df20 <- data.frame(dataset2)
df20$CTYNAME <- gsub(" County","", (df20$CTYNAME))
df20

#rename columns
colnames(df20)[4] <- "STATE"
colnames(df20)[5] <- "COUNTY"
df20

colnames(df20)[6] <- "state"
colnames(df20)[7] <- "county"
df20

#merge dataframes covid and census
df30 <- merge(df10,df20,by=c("state","county"))
df30

write_xlsx(df30, "C:\\Users\\Andrei\\Documents\\Master Erasmus Uni\\Advanced Stats and Programing\\Group Assignment\\R stuff\\US_Counties_covid_and_census.xlsx")

#economic dataset
dataset3 <- read.csv("unemployment_data_csv.csv", header = TRUE, sep = ",")
df40 <- data.frame(dataset3)
df40$county <- gsub(" County","", (df40$county))
df40

#merge
df40 <- merge(df30, df40, by=c("state", "county"))
df40

write_xlsx(df40, "C:\\Users\\Andrei\\Documents\\Master Erasmus Uni\\Advanced Stats and Programing\\Group Assignment\\R stuff\\US_Counties_covid_and_census_and_economic.xlsx")

election <- read.csv("Election_data3_csv.csv", header=TRUE, sep = ",")
election1 <- data.frame(election)

election1$county <- gsub(" County","", (election1$county))
election1

df60 <- merge(df40, election1, by=c("state", "county"))
df60

write_xlsx(df60, "C:\\Users\\Andrei\\Documents\\Master Erasmus Uni\\Advanced Stats and Programing\\Group Assignment\\R stuff\\US_Counties_covid_and_census_and_economic_and_election.xlsx")

data <- df60

data$dRepubplican <- ifelse(data$Trump.ratio > 0.5, 1, 0)

reg1 = lm(cases ~ POPESTIMATE2020 + dRepubplican, data = data)
stargazer(reg1, type = "text")
reg2 = lm(deaths ~ POPESTIMATE2020 + dRepubplican, data = data)
stargazer(reg2, type = "text")