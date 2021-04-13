#Conducting data cleaning and Preprocessing to analyze voter behaviour in 18 states with respect to their
#Votes for and against Ballot Types :1 - Gambling , 2- Wagering
rm(list=ls()); gc()
setwd('C:\\Users\\navee\\Dropbox (CSU Fullerton)\\ISDS 415\\Project 1')

dat = read.csv('Gaming_Data_Set_Project.csv', head=T, stringsAsFactors=F, na.strings='')
matrix.na = is.na(dat) #create a matrix which shows TRUE if there's a missing value
dat1 = na.omit(dat) # remove rows with empty/'NA' values. 

 # check for outliers in each column
hist(dat1$Votes_For)
hist(dat1$Votes_Against)
hist(dat1$Total_Votes)
hist(dat1$Population)
hist(dat1$Per_Capita_Income)
hist(dat1$Medium_Family_Income)
hist(dat1$Size_of_County)
hist(dat1$Population_Density)
hist(dat1$No_of_Churches)
hist(dat1$No_of_Church_Members)
hist(dat1$Age_Less_than_18)
hist(dat1$Age_18_24)
hist(dat1$Age_24_44)
hist(dat1$Age_44_64)
hist(dat1$Age_Older_than_64)

#install.packages('ggplot2')
library(ggplot2)
#install.packages('data.table')
library(data.table)

#Write a function to replace outlier values above a certain number in each column with 'NA'
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

# replace outlier values in each column with NA
outlierReplace(dat1,'Votes_For',which(dat1$Votes_For >20000), newValue = NA)
outlierReplace(dat1,'Votes_Against',which(dat1$Votes_Against >20000), newValue = NA)
outlierReplace(dat1,'Total_Votes',which(dat1$Total_Votes >50000), newValue = NA)
outlierReplace(dat1,'Population',which(dat1$Population >300000), newValue = NA)
outlierReplace(dat1,'Per_Capita_Income',which(dat1$Per_Capita_Income >25000), newValue = NA)
outlierReplace(dat1,'Medium_Family_Income',which(dat1$Medium_Family_Income >45000), newValue = NA)
outlierReplace(dat1,'Size_of_County',which(dat1$Size_of_County >2500), newValue = NA)
outlierReplace(dat1,'Population_Density',which(dat1$Population_Density >400), newValue = NA)
outlierReplace(dat1,'No_of_Churches',which(dat1$No_of_Churches >200), newValue = NA)
outlierReplace(dat1,'No_of_Church_Members',which(dat1$No_of_Church_Members >75000), newValue = NA)
outlierReplace(dat1,'Age_Less_than_18',which(dat1$Age_Less_than_18 >75000), newValue = NA)
outlierReplace(dat1,'Age_18_24',which(dat1$Age_18_24 >30000), newValue = NA)
outlierReplace(dat1,'Age_24_44',which(dat1$Age_24_44 >100000), newValue = NA)
outlierReplace(dat1,'Age_44_64',which(dat1$Age_44_64 >50000), newValue = NA)
outlierReplace(dat1,'Age_Older_than_64',which(dat1$Age_Older_than_64 >30000), newValue = NA)

#create a new data frame with outlier rows removed and export to csv file
dat2=na.omit(dat1) # we are left with 1074 rows
write.csv(dat2, file = "Dataset1.csv")

#normalize all columns with continous variables and export to csv file
Normalized_dat = scale(dat2[,-c(1,2,6,26)])
NormalizedDataset=cbind(dat2[,c(1,2,6,26)],Normalized_dat)
write.csv(NormalizedDataset, file = "NormalizedDataset.csv")

#check for correlation 
correlation1=cor(dat2[,-c(1,2,6,26)])
write.csv(correlation1, file = "Correlation.csv")

bal1 = read.csv('Ballot 1.csv', head=T, stringsAsFactors=F, na.strings='')
bal2 =read.csv('Ballot2.csv', head=T, stringsAsFactors=F, na.strings='')

# plot multiple linear regression for third hypothesis
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit1 = lm(bal1[,1] ~ Size_of_County + Population, data = bal1)
summary(fit1)
fit2 = lm(bal2[,1] ~ Size_of_County + Population, data = bal2)

w1<- ggplotRegression(fit1)
w1 + labs(x = "Size of County, Population", y= "Votes_For")

w2<- ggplotRegression(fit2)
w2 + labs(x = "Size of County, Population", y= "Votes_For")

## check the frequency table of categorical variables
#table(dat1[,'Ballot_Type'])
#library(fastDummies)
#dat2 = dummy_columns(dat1, select_columns = 'Ballot_Type', remove_most_frequent_dummy = T)

#library(xlsx)
#write.xlsx(dataset,'C:\\Users\\navee\\Dropbox (CSU Fullerton)\\ISDS 415\\Project 1', sheetName = "Sheet1", 
        #   col.names = TRUE, row.names = TRUE, append = FALSE)

#PCA analysis with normalization
#obj2 = prcomp( na.omit(dat2[,c(3,4,5,7,8,9,10,11,21,22,23,24,25)]), scale=T)
#obj2$rotation # check for PCA coefficents
#datascore=obj2$x # check for PCA scores
#summary(obj2)
  
#dat2$Ballot_Type=NULL
#cor(dat2)
#dat3=dummy_columns(dat2, select_columns = 'MSA', remove_most_frequent_dummy = T)

