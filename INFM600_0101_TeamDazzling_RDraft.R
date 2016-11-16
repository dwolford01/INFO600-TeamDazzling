#Team Dazziling

#--Research Question: Some hospitals claim to have better service and charge more for high quality care, but are patients really getting their money's worth?--#

#--To answer this question we look at the data availabe in spendingsbyclaimtype.csv. 
#--The variable to consider here is average spending per episode which measures the money's worth.This is the dependent variable. The variable is of the ratio scale
#--Filter out the data for the column Period by using only values for Complete Episode and removing the values that are blank or not applicable.
#--In this dataset we only take the spending for the states 'VA' and 'DC'

spending=read.csv(file.choose())
#--This is the command to read a csv file in R. X is the variable that stores spendingbyclaim.csv dataset.

head(spending)
#--The command displays the first 6 rows of the dataset.

hist(spending$Avg)
#The distribution of the variable is skewed. Hence the central tendency we measure for the variable is median

medianofspending=aggregate(spending$Avg,by=list(state=spending$State),FUN = function(x) c(median=median(x)))
#--State wise data is aggregated based on median of the average spending per episode.

print(medianofspending)
#--This command displays the value of median per state.

summaryofspending=aggregate(spending$Avg,by=list(state=spending$State),FUN = function(x) c(summary=summary(x)))
#--this gets the descriptive stats for average spending per episode(min,max,1stand 3rd quartile,mean and median) grouped by the state.

print(summaryofspending)
#--This command displays the summary per state.

sdofspending=aggregate(spending$Avg,by=list(state=spending$State),FUN = function(x) c(sd=sd(x)))
#--State wise data of standard deviation of average spending per episode.

print(sdofspending)
#--This command displays the standard deviation of average spending per episode for state.


#--complicationsdata.csv contains Independent Variable, score, which use to measure for high quality care.
#--The variable is of the ratio scale.
#--Filter out the data by removing all the blank, not available and not applicable values from the score column. 
#--In this dataset we only take the score for the states 'VA' and 'DC'

complications=read.csv(file.choose())
#--This is the command to read a csv file in R. X is the variable that stores complicationsdata.csv dataset.

head(complications)
#--The command displays the first 6 rows of the dataset.

hist(complications$Score)
#The distribution of the variable is skewed. Hence the central tendency we measure for the variable is median

medianofcomplications=aggregate(complications$Score,by=list(state=complications$State,measure=complications$MeasureName),FUN = function(x) c(median=median(x)))
#--State and measure name wise data is aggregated based on median of the score.

print(medianofcomplications)
#--This command displays the value of median of score per state and measure name.

summaryofcomplications=aggregate(complications$Score,by=list(state=complications$State,measure=complications$MeasureName),FUN = function(x) c(summary=summary(x)))
#--this gets the descriptive stats for score(min,max,1stand 3rd quartile,mean and median) grouped by the state.

print(summaryofcomplications)
#--This command displays the summary per state.

sdofcomplication=aggregate(complications$Score,by=list(State=complications$State),FUN = function(x) c(sd=sd(x)))
#--State wise data of standard deviation of score.

print(sdofcomplication)
#--This command displays the standard deviation of score per state.


#--hcahps.csv contains Independent Variable, survery star rating, which use to measure for high quality care.
#--The variable is of the interval scale(likert scale which is considered as interval in this case).
#--Filter out the data by removing all the blank, not available and not applicable values from the score column. 
#--In this dataset we only take the survey rating for the states 'VA' and 'DC'

hcahps=read.csv(file.choose())
#--This is the command to read a csv file in R. X is the variable that stores hcahps.csv dataset.

head(hcahps)
#--The command displays the first 6 rows of the dataset.

hist(hcahps$Patient.Survey.Star.Rating)#-- Checking whether to take mean or median based on symmetry.
# The distribution for the variable looks normal, therefore the central tendncy we measure is mean

hcahpsmean=aggregate(hcahps$Patient.Survey.Star.Rating,by=list(state=hcahps$State,measure=hcahps$HCAHPS.Measure.ID),FUN = function(x) c(mean=mean(x)))
#--State and measure ID wise data is aggregated based on mean of the survey star rating.

print(hcahpsmean)
#--This command displays the value of mean of survey star rating per state.

hcahpssummary=aggregate(hcahps$Patient.Survey.Star.Rating,by=list(state=hcahps$State,measure=hcahps$HCAHPS.Measure.ID),FUN = function(x) c(summary=summary(x)))
#--this gets the descriptive stats for survey star rating(min,max,1stand 3rd quartile,mean and median) grouped by the state.

print(hcahpssummary)
#--This command displays the summary per state.

sdofhcahps=aggregate(hcahps$Patient.Survey.Star.Rating,by=list(State=hcahps$State),FUN = function(x) c(sd=sd(x)))
#--State wise data of standard deviation of survey star rating.

print(sdofhcahps)
#--This command displays the standard deviation of survey star rating.



