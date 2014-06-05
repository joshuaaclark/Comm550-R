#' COMM 550-Lab 11- Factor Analysis
#'===========================================================   
#' #### [Back](http://joshaclark.com/?page_id=138)   

#>install.packages('GPArotation')
library(GPArotation)
library(psych)
library(ggplot2)



#' Let's look a a dataset which is built into R, the Motortrend Car Guide. This features
#' a number of variables about different cars including their miles per gallon (mpg), number of
#' cylinders (cyl), engine displacement (disp), horsepower (hp), rear axle ratio (drat), weight
#' (wt) and quarter mile time (qsec) as well as some other variables that we aren't interested in 
#' such as transmission type. Let's load the data, pull out the variables we need and examine it.
data(mtcars)
mtcars<-mtcars[,-8:-10]
mtcars<-mtcars[,-6]
head(mtcars)

#' The goal of factor analysis is to reduce the number of dimensions (variables) which are expressed
#' within our dataset. In this cars example we are dealing with a number of variables which may all 
#' reflect a smaller number of internal factors. As an example the preference for gas guzzlers may 
#' be reflected in high horsepower, high cylinder low MPG cars which go fast. Factor analysis looks
#' at the relationships between our variables and tries to find these common trends.

#' Factor analysis works off of a correlation matrix, let's take a look at ours
mtcor<-cor(mtcars)
mtcor

#' Not the diagonal of 1's running down the side. From this matrix we can extract the eigenvalues.
#' I am not going to get too deep into eigenvalues or vectors here because that leads down a linear
#' algebra rabbit hole that is well beyond this class. Suffice to say that eigenvectors are specific
#' combinations of numbers that when multiplied by a matrix lead to the same values as if they were multiplied
#' by a specific value, the eigenvalue. If you have more questions about how this ties into factor analysis
#' feel free to ask Professor Monge ;)
ev<-eigen(mtcor)
ev<-data.frame(ev)

#' Eigenvalues are useful for factor analysis because they provide hints at how many different underlying variables
#' exist within our dataset, and by extension how many we should extract from that big messy correlation matrix.
#' There are a couple of rules we can use to determine this. One is to look at the number of factors with an eigenvalue
#' >1
ev$values

#' In this case that equals two. This rule is simple but can be too strict. Another approach would be to look at what's called
#' a scree plot (like the sound a hawk makes when it is diving towards your head, "SCREEEEEEEEE!"). 

qplot(y=ev$values, main='SCREE Plot', xlab='Factor #', ylab='Eigenvalue')+geom_line()

#' On the scree plot (SCREEEE!) we are looking for the point where the line bends and begins to flatten out, so around
#' value 3. This is a subjective decision, some people might say 2 and others 4. But let's move forward extracting 3 factors
#' from the data.

#' Doing a factor analysis is dead simple, simply provide a dataframe or correlation matrix of the variables you are interested
#' in. If your data has other variables in it you will need to make a new dataframe or specify what you want to examine beforehand.

#' Running the factor analysis is easy, just input the dataframe or correlation matrix and the number of factors you
#' want to extract.
factan.1<-fa(mtcars, nfactors= 3)

#'The summary gives us some goodness of fit tests. You generally want the Tucker Lewis Index above 0.9 and the Chi Square
#' test of the hypothesis that 'n' factors and sufficient to be NOT significant. Like the Levene's test non significance 
#' IS GOOD in this case.
summary(factan.1)
#' We can then see how the various variables load onto the three factors. We are looking for high absolute values which 
#' can be used to craft a narrative about our data.
factan.1$loadings

#'These factors can be translated into scores for each row that can then be used as predictor variables in further analysis
factan.1$scores



#'The issue with factor analysis is that it tends to overload the first factors in the model, by rotating the model
#' we can have a more even distribution. Rotations can be orthogonal (all factors are independent) or oblique (factors
#' can correlate). Let's look at varimax a popular orthogonal rotation
r.factan.3<-fa(mtcars, nfactors = 3, rotate='varimax')
summary(r.factan.3)

r.factan.3$loadings
#' From this we can see that MR2 has a strong positive loading for Cylinders and Engine Displacement
#' as well as a negative loading for miles per gallon and rear axle ration. MR1 seems to be primarily
#' the amount of carburetors the car has while MR3 has a strong negative relationship with quarter mile time (ie
#' really fast cars). We can extract these scores into our original dataframe for use in further analysis
mtscores<-data.frame(r.factan.3$scores)
mtscores
mtcars$GasGuz<-mtscores$MR2
mtcars$Carb<-mtscores$MR1
mtcars$Speedy<-mtscores$MR3

#' By examining the various scores in descending order we can see the various narratives played out!
mtcars[order(-mtcars$GasGuz),] 
mtcars[order(-mtcars$Carb),]
mtcars[order(-mtcars$Speedy),]

#' #### [Back](http://joshaclark.com/?page_id=138)