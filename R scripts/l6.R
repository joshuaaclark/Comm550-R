#' COMM 550-Lab Six- ANOVA
#'=========================================================== 
#' #### [Back](http://joshaclark.com/?page_id=138)
#' This guide will walk you through how to perform the various types of ANOVA in R. We'll be 
#' using some real survey data which I collected for 550. The survey features players of the 
#' online game EVE online and has a number of demographic and game play variables. For the first 
#' example we'll be examining the difference in social capital levels between players at different 
#' levels of experience. There are competing theories regarding the relationship between gaming and 
#' social capital with some authors suggesting that games detract  from social capital stocks by 
#' taking people away from their friends. An alternative view is that games provide a new venue for 
#' interaction and may boost social capital stocks.
#' 
#' Let's start as always by loading the libraries.
library(car)
library(psych)

#' We'll be looking at one way ANOVA today. This is a statistical test which determines is there is
#' a significant difference between the means of two groups. Let's read the dataset into R and just omit
#' any value with NA for the sake of time. Normally we would clean and examine the data a bit more closely
#' but we have already covered these techniques in other labs.
a.data<-read.csv(url('http://joshaclark.com/wp-content/uploads/2014/05/evedemodata.csv'))


#' Our grouping variable will be time spent in game. The idea being that new players or casual gamers
#' are different from their more intensely nerdy counterparts. Instead of binning according to some present
#' values we are going to bin by percentile. So in this case I am making two bins, one which holds everyone
#' from zero to the 50th percentile called 'low' and another for those in the 50+ percentile called 'high'
a.data<-na.omit(a.data)
summary(a.data$EVETime)
#'Take a look at the quantile breakdown. This gives us the values for the 50th and 100th percentiles as well
#' as the lowest score.
splits<-quantile(a.data$EVETime, c(0, .50, 1)) 
splits
a.data$timebin<-cut(a.data$EVETime, breaks=splits, labels=c('low', 'high'))
a.data<-na.omit(a.data)
table(a.data$timebin)
table(a.data$EVETime)
summary(a.data$timebin)

#'We need to do a Levene test to make sure that the variance is equal between our bins
leveneTest(a.data$Bridgeindex, a.data$timebin)

#' The test is not significant so we have equal variance, meaning that we can go ahead 
#' with our ANOVA. 

#' The one way ANOVA simply compares means between groups. the dependent variable comes
#' first, separated by a tilde and then the independent variable(s). Note that data can be
#' called with a separate function or included within the formula

a1 <- aov(a.data$Bridgeindex ~ a.data$timebin)
#' OR
a1.1<-aov(Bridgeindex~timebin, data=a.data)
summary(a1)
summary(a1.1)
#'Both forms of syntax give us the same thing. The high f-value and significant results suggest
#' that there is greater variance between the two groups then we would expect by chance, so there
#' is a difference between less and more experienced players.

#' Let's try doing the ANOVA step by step to see if we can get the same results. This 
#' will let us review how ANOVA operates as well as reviewing the math which R just
#' did for us.

#' First things first let's obtain the grand mean of the entire dataset with regards to
#' our dependent variable, bridging social capital.
grand.mean<-mean(a.data$Bridgeindex)


#' Next let's grab the group means for the low and high bins. One way to do this would
#' be to use a variant of our old friend describe and look at the descriptive statistics 
#' by group
describeBy(a.data$Bridgeindex, a.data$timebin)

#' Group mean for low =3.89
low.mean<-3.89
#' High mean = 4
high.mean<- 4

#' Or we can make two new dataframes low and high and calculate the means separately. The subset
#' function is extremely helpful and you should read the documentation when you get a chance.
?subset
low<-subset(a.data, timebin=='low', select=Bridgeindex)
high<-subset(a.data, timebin=='high', select=Bridgeindex)
low.mean<-mean(low$Bridgeindex)
high.mean<-mean(high$Bridgeindex)

#' Now that we have our various means we calculate our Sum of Squares. Although we don't actually need
#' it for the analysis let's calculate the total sum of squares.
a.data$sstotal<-(a.data$Bridgeindex-grand.mean)^2
ss.total<-sum(a.data$sstotal)
#' SSbetween
low$ssbtwn<-(low.mean-grand.mean)^2
high$ssbtwn<-(high.mean-grand.mean)^2
ssbtwn.total<-(sum(low$ssbtwn)+sum(high$ssbtwn))


#'SSwithin is the summation of the score for each user on Bridging social capital minus the mean of their group, then squared.
low$sswithin<-(low$Bridgeindex-low.mean)^2
high$sswithin<-(high$Bridgeindex-high.mean)^2
#' Add up the two group scores to get the SSwithin total.
sswithin.total<-(sum(low$sswithin)+sum(high$sswithin))
#' DF for between is number of groups -1 (therefore 1) and DF within is n-#' of groups (511-2) 
f<-(ssbtwn.total/1)/(sswithin.total/509)
f

#' When we compare the by hand ANOVA to the one R calculated we can see the ssbtwn.total is close to the Sum Sq for timebin and the sswithin.total is close to the Sum Sq of the residual, additionally our F value is very close to the one R provides.
summary(a1)
ssbtwn.total
sswithin.total
f


#'ANOVA is helpful because we can also compare across more then two groups. Let's break out sample into three
#' bins and run the ANOVA again.

quantile(a.data$EVETime, c(0, .25, .50, .70, 1)) 
a.data$timebin4<-cut(a.data$EVETime, breaks=c(0,8,14,20,80), labels=c('bottom', 
                                                                      'middle/low', 
                                                                      'middle/high',
                                                                     'high'))

a2 <- aov(a.data$Bridgeindex ~ a.data$timebin4)
summary(a2)


#' We will continue our examination of ANOVA next class when we look at factorial ANOVA and different strategies for drilling down and comparing between groups.
#' 
#'#### [Back](http://joshaclark.com/?page_id=138)