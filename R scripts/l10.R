#' COMM 550-Lab Ten- Regression
#'=========================================================== 
#' #### [Back](http://joshaclark.com/?page_id=138)   
#' 
#' Regression is an extremely power family of statistics. Today we will only be looking at one
#' specific type, linear models (LM) aka Ordinary Least Squares (OLS) regression. Because 
#' regression is so powerful it is also important to do it right!

#' First let's load the libraries and install a new package, Zelig. Zelig will help us
#' interpret regression results later in this example
#' >install.packages('Zelig')

library(Zelig)
library(ggplot2)
library(car)
#' Let's start by loading the EVE data which we used earlier this year.

evedat<-read.csv(url('http://joshaclark.com/wp-content/uploads/2014/05/evedemodata.csv'))



#'This is from my COMM550 project
#' oh so long ago and is a sample of users who play the popular game EVE Online. Let's look at the relationship
#' between Bridging and Bonding Social Capital among the players. According to authors like Putnam social capital
#' forms a virtuous circle, so players who have more Bridging Capital (knowing a wide array of people) should also
#' have a lot of Bonding capital (feeling tight connections to people). We can specify a regression testing the relationship
#' between Bonding Capital (as the DV) and Bridging Capital as the IV. 

OLS.1<-lm(BondindexHD~BridgeindexHD, data=evedat)
summary(OLS.1)


#' R gives us a wide variety of outputs, including:

#' Descriptive statistics of the residuals (error) generated from the model
#' Coefficients, their STD Error, t-tests and P values
#' R2 (explained variance)
#' And omnibus ANOVA test to examine if the model as a whole is a good predictor.

#' We are most interested in the coefficients at this point. The intercept is the point
#' where the OLS line meets the y axis (so all predictors are pegged at zero). The coefficients
#' represent the increase in the DV per one unit increase of the IV holding any other variables constant. 
#' So a one unit increase in Bridging Capital leads to a 0.25426 increase in Bonding capital.

#' In other words we can calculate a predicted values of our DV using the classic linear slope formula
#' y=m*x+b where m is the coefficient, x is a given value of the dependent variable and b is the intercept,
#' so for Bridgeindex=3 we can say

3*0.35518+2.31450


#' We can plot what the fitted line looks like by adding a regression line to a scatter plot. 
#' NOTE: order is flipped for plotting, IV first, DV second.
m<-qplot(BridgeindexHD,BondindexHD,  data=evedat)
m
m+geom_smooth(method=lm, se=TRUE)
#' We can also wrap a confidence interval around a regression line (basically an estimate of where the "true"
#' line would run)
m+geom_smooth(method=lm, se=TRUE)


#' If we run plot on the linear model we get a number of useful graphics.
#' -Residuals vs. fitted shows the relationship between the predicted values and their error (zero being perfect prediction)
#' -The QQ plot tells us if the residuals are normally distributed (just like ANOVA)
#' -Don't worry about scale vs. location right now
#' Residuals vs. leverage shows us the error versus how much a given measure changes the slope of the line, we are looking
#' for outliers which have really high residuals as well as high leverage as they have a big influence on our line.
plot(OLS.1)

#' Variable 243 is way off the map, we can view the scores by typing
evedat[243,]
#' You can treat this variable like any outlier and leave it, replace it or ignore it, let's drop the case and rerun our model
evedat.tr<-evedat[-243,]

OLS.1.tr<-lm(BondindexHD~BridgeindexHD, data=evedat.tr)
summary(OLS.1.tr)
summary(OLS.1)
plot(OLS.1.tr)

#' So we have some change in the coefficents from dropping the outlier. 



#' We can also include nominal variables into a regression model through a process known as dummy coding. First we 
#' have to tell R to treat our variable as a nominal variable.
table(evedat$Educ)

#' Oh dear, that's a lot of ones and twos, let's assign values to each and tell R to treat the variable as a factor.

evedat$Educ.nom <- factor(evedat$Educ,
                          levels = c(1,2,3,4,5,6),
                          labels = c("No HS", "HS", "Some PS"
                                     , "Trade School", "PS degree"
                                     , "Post-Grad Deg"))

table(evedat$Educ.nom)

#' Having turned the variable into a factor we can now include it in a regression model.
#' Regression allows us to estimate the effects of multiple independent variables on a predictor
#' essentially "controlling" for them. In other words the coefficients for multiple regression are 
#' expressions of the relationship between any given independent and dependent variable while taking
#' into account the influence of other predictors.

#' Let's estimate a multiple regression with Bridging Social Capital, Education and Age as independent
#' variables.

OLS.2<-lm(BondindexHD~BridgeindexHD+Educ.nom+Age, data=evedat)
summary(OLS.2)

#'If multiple predictors are strongly correlated with each other then this can inflate standard errors
#' through a process called multi-colinearity. We can test for it with VIF, variable inflation factor. VIF>2
#' is troublesome.
vif(OLS.2)

#' If you have a high VIF you may want to consider dropping or modifying the variable in question.

#' The results show that Bridging Capital is still a significant predictor as well as demonstrating that
#' higher levels of education have higher rates of social capital. Each of the categories broken out of
#' the dummy variables represents a shift in the intercept of the regression line in comparison to the first
#' category (no highschool). So the intercept for those with a Post-Secondary Degree is 0.36 points higher than 
#' those without,
#' 
#' If variables are on different scales then we can do a z-transformation, this alters each variable to have a mean
#' of zero and 1 represents a one standard deviation increase in values. This has the advantage of transforming all
#' your variables to be on the same scale (increases in standard deviation) but at the cost of some interpretability
#' Since the nominal variable only influences the intercept of the OLS line it doesn't make sense to standardize it.




OLS.3<-lm(scale(BondindexHD)~scale(BridgeindexHD)+Educ.nom+scale(Age), data=evedat)
summary(OLS.3)

#' These results are read in the same way as OLS.2 but the coefficients represent a one standard deviation
#' increase instead of a one unit variance.      




#' Finally let's talk about generating predictions from your regression. With any given model you can plug in
#' chosen values from your data and then predict what that hypothetical person's score. Zelig is a package that
#' allows you to simulate predicted values by taking a bunch of sub-samples from your data and estimating a predicted
#' value. This gives you a prediction as well as a confidence interval

#' First we estimate the model just like OLS, the only changes being the Zelig call at the front of the line and the model term
#' where ls=least squares ie ordinary LEAST SQUARES.

#' Let's add another term just so we are aren't doing the same thing over and over again, this one is a nominal general trust
#' question asking "Can people be trusted?"
evedat$GenTrust.nom <- factor(evedat$GenTrust,
                              levels = c(1,2,3),
                              labels = c("Yes","Sometimes","No"))

OLS.Z<-zelig(BondindexHD~BridgeindexHD+Educ.nom+Age+GenTrust.nom, data=evedat, model='ls')

#' Zelig delivers the same coefficients as the earlier model (OLS.2)
summary(OLS.Z)

#' Now the cool part, we can set values for a hypothetical person and simulate what their values would be at any given score.
#' We can simulate a single point, such as someone who is 27 years old, everything else will be pegged at the mean or median
#' depending on the type of variable

stx.1<-setx(OLS.Z, BridgeindexHD=3)
sim.1<-sim(OLS.Z, x=stx.1)
plot(sim.1)
summary(sim.1)

#' This gives us a range of values which were generated by the simulation which converge around 3.78.

#' We can also estimate a range of values to see how predicted values alter as an independent variable changes.
#' So we can predict the scores of people with Bridging Scores going from one to five and even compare between 
#' these two predictions

stx.2<-setx(OLS.Z, BridgeindexHD=1:5, GenTrust.nom='No', Educ.nom='No HS')
stx.3<-setx(OLS.Z, BridgeindexHD=1:5, GenTrust.nom='Yes', Educ.nom='Post-Grad Deg')
sim.2<-sim(OLS.Z, x=stx.2, x1=stx.3)
plot(sim.2)

#'The two lines clearly diverge and we can see how the change in Bridging capital plays out at various points!

#' #### [Back](http://joshaclark.com/?page_id=138)