#' COMM 550-Lab Eight and Nine- Chi^2 and Correlation
#'=========================================================== 
#' #### [Back](http://joshaclark.com/?page_id=138)
#' Today we'll be looking at both the Chi^2 test and some forms of correlation and reliability analysis (if we have time)! Let's start by installing two packages we'll need for this demo.


#'  >install.packages('ppcor')   
#'  >install.packages('MASS')  
library(MASS)
library(psych)
library(ppcor)
library(ggplot2)

#' First up is a Chi Square test. This test looks at the observed distribution of cases between
#' two groups and compares it to a theoretical distribution if the two groups were totally independent.
#' This is useful in determining if a factor or categorical variable is influencing a dependent variable
#' you are interested in. 

#' Let's look at some data built into R, a survey of 237 statistics students at the University of Adelaide. This data is built into R so we can just call it with the data() command. 


#' We'll be using some randomly generated data so let's set the random number seed to start.
set.seed(11)

data(survey)
head(survey)
survey<-na.omit(survey)


#' Putting on our health comm hat for a moment, say we wanted to see if gender influenced the likelihood of
#' statistics students at the University of Adelaide to smoke. If there was no influence then we would expect the 
#' proportion of smokers to be evenly distributed between the two genders represented in this survey.

#' We can start by looking at the cross tab of the two variables.
table<-table(survey$Smoke, survey$Sex)
table

#' Clearly not exactly the same, but are the differences significant? We can just drop the table that we made into R and it will
#' take care of the rest.

chi1<-chisq.test(table) 
chi1

#' Well, that isn't significant at all. The variation in the probability of smoking does not differ significantly between men and women
#' 

#' Or we can pass variables like the other tests we've used before
chi2<-chisq.test(survey$Smoke, survey$Sex)
chi2

#' The ChiSquare syntax in R isn't very picky, you can even reverse your terms.
chi3<-chisq.test(survey$Sex, survey$Smoke)
chi3

#' ChiSquare has been addressing two nominal variables, (smoking y/n and gender m/f), but what if we wanted to examine
#' two ratio or interval variables then we can use PEARSON'S r!!!!   

#' It is called R so it **HAS** to be good. :D   

#' Pearson's R has some assumptions, the variables need to be interval or ratio with some form of linear relationship.
#' Let's change datasets and use another one built into R, this one looks at the weight of 144 cats.
data(cats)
head(cats)

#' Let's add some new variables for teaching purposes, a cuteness rating which is a random variable between 1 and 5. This should
#' NOT be correlated with the original variables
cats$cuteness<- sample(1:5, 144, replace=T)
cats$cuteness<-as.numeric(cats$cuteness)

#' And "meow score" an interval variable between one and one hundred
cats$meow.score<- runif(144, 1, 100)


#' Let's do a scatter plot to see the relationship between the body weight of cats (Bwt) and heart weight (Hwt).
#' Looking at the scatter plot there seems to be a strong linear relationship there, so let's do a Pearson correlation
qplot(Bwt, Hwt, data=cats)
#'Compare that relationship to the scatter showing the correlation between Body Weight and the random variable we just created.
qplot(Bwt, meow.score, data=cats)

#' The difference also shows up in the correlations
cor.test(cats$Bwt, cats$Hwt, method='pearson')
cor.test(cats$Bwt, cats$meow.score, method='pearson')

#' Looking at the p-value and the correlation coefficent we can see that the relationship between Bwt and Hwt is highly significant, while as expected the relationship between Bwt and the randomly generated "meow score" is not.

#' Looking at weight and cuteness the ordinal nature of cuteness comes into play creating some striated data, fortunately
#' we can use Spearman's Rho to look at the relationship.
qplot(Bwt, cuteness, data=cats)
cor.test(cats$Bwt, cats$cuteness, method='spearman')

#' Given that cuteness was randomly generated it should be no surprise that there is not a strong correlation here

#' We can also control for independent variables in our correlations
pcor.test(cats$Bwt, cats$Hwt, cats$meow.score)

#' Showing us that while controlling for the radnomly generated meow score variable the relationship between Bwt and Hwt remains strong


#' ### Bonus Section, Cronbach's Alpha   
#'Cronbach's alpha is a measure of reliability, in other words how variables vary together. This is a good thing
#' if you have a scale, as you want your questions to vary together as they are all measuring the same concept. 
#' Higher scores are better!
alpha(data.frame(cats$Bwt, cats$Hwt))

#' #### [Back](http://joshaclark.com/?page_id=138)
