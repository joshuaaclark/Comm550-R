#' COMM 550-Lab Seven- ANOVA II
#'=========================================================== 
#' #### [Back](http://joshaclark.com/?page_id=138)

#' Let's begin by installing two packages we need for this example
#' heplots gives us access to a nice eta^2 function which we will
#' discuss in a few minutes, while gplots allows us to graph the
#' changes in the means across various conditions. 

#' >install.packages('heplots')   
#' >install.packages('gplots')   

library(car)
library(heplots)
library(gplots)
library(ggplot2)
#' For a complete change of pace from last week, we'll be looking at 
#' a dataset about pretty flowers! The iris dataset contains a 150 observations
#' of the size of flowers from three species of iris growing in two different soil conditions, this is a modification of an existing dataset in R with some added variables.

iris<-read.csv(url('http://joshaclark.com/wp-content/uploads/2014/06/irisdemo.csv'))
head(iris)


#' We can see that the flowers are evenly divided among species with roughly even growing 
#' conditions
table(iris$Soil)
table(iris$Species)
qplot(Sepal.Width, data=iris, fill=Soil)

#'We're interested in seeing if a particular species produces bigger sepals (the outermost part 
#' of a flower) then the other species, and what soil conditions promote the best growth. 

#' As always let's start by testing for the equality of variance, we can load multiple categories
#' into the Levene's Test with the comma. 

leveneTest(iris$Sepal.Width, iris$Species:iris$Soil)

#' Now onto some one way ANOVAs! This should be review from last week, but let's see
#' if a specific species does better than the others in the garden.
a.1<-aov(iris$Sepal.Width~iris$Species)

#' Eta squared is another way to conceptualize the size of a statistical effect. It is basically
#' the proportion of variance explained by your model, with a perfect model explaining 100% of the 
#' variance in the dependent variable.
etasq(a.1)

summary(a.1)

#' Clearly one species is doing better. But which one? The omnibus ANOVA which we just ran
#' does not differentiate between more than two groups, the omnibus test call tell us that there
#' are differences between species but not which species does better or worse. 

#' One way to tell is to plot the means of the various groups and wrap a 95% CI around each value. 
#' This will show us which group is higher or lower and the relative confidence of each estimate.
plotmeans(iris$Sepal.Width~iris$Species)

#' Another option is to do a pairwise t test to compare between the various groups. Once you 
#' start comparing between more than two groups the chances increase that there will be a 
#' difference due to random chance alone. As an example, if we compared our flowers based 
#' on species, soil, light, rainfall, number of surrounding plants, pesticide use etc etc
#' the odds increase that there will be one combination which may lead to a difference even
#' if only by random noise. To counter act that we use a number of p value adjustments
#' which take into account this problem and try to adjust the criteria for significance 
#' in response. The oldest and most conservative (i.e. most likely to give you a false negative
#' but least likely to give you a false positive) is the Bonferroni correction, which is also
#' the name of band which I played bass in during high school.
??pairwise.t.test
a1.t<-pairwise.t.test(iris$Sepal.Width, iris$Species, p.adj = "bonf")
a1.t


#' We can see that even with the correction there is a significant difference based on species.

#' We've been working various diagnostics processes into the class without my explicitly mentioning it, but
#' it is time to bring in another test to make sure our data fits the assumptions of ANOVA, namely the normality
#' of the residuals. Residuals are basically the difference between the observed result from the data and the 
#' estimation of any given statistical model. ANOVA demands that the residuals be normally distributed for a 
#' number of reasons which Hayes talks about at greater length. We can test the normality of the residuals with
#' a histogram or a qqplot. With the histogram we are just looking for a roughly normal distribution. The qqplot
#' compares the distribution of the residuals against the sample, if they are the same (i.e. the plot is a roughly
#' straight line) then our residuals are normally distributed.

qqnorm(residuals(a.1))
qplot(residuals(a.1))

#' Let's do the entire process again only looking at soil conditions. Because there are only two possible 
#' groups (sunny and shady) we don't need a t-test with adjustments.
a.2<-aov(Sepal.Width~Soil, dat=iris) #'alternative way of writing a function
summary(a.2)
qqnorm(residuals(a.2))
plotmeans(Sepal.Width~Soil, dat=iris) #'alternative way of writing, does the same thing
etasq(a.2)

#' Now the fun part! What about if the species is interacting with the soil type. As an example some species 
#' may grow bigger in shade or sun. Interaction can take all kinds of forms, but can basically be represented as the
#' effect which occurs when both of the interacting variables are present. To use a silly example, if we are testing
#' sweetness of the first sip of coffee with two binary variables sugar (y/n) and stirring (y/n) then we have the following 
#' research design
#' 
#' 
#' . | Yes Sugar | No Sugar
#' ---|-----------|---------
#' **Yes Stir** | Sweet | Not Sweet
#' **No Stir** | Not Sweet | Not Sweet
#' 
#' 
#' Sweetness is not a function or sugar or stirring but interaction of the two, similarly with our flowers it might
#' not be simply the species or the conditions but a mix of the two. We can express this within an ANOVA by 
#' multiplying the two variables together and calculating the results. Note, you need the components of the interaction
#' in the equation, so if you don't include them R will add them in automatically.
a.3<-aov(Sepal.Width~Soil+ Species + Soil*Species, dat=iris)
summary(a.3)
qqnorm(residuals(a.3))
etasq(a.3)

#' We can look at the differences between and among groups with an interaction plot, specifically we are 
#' looking for instances where lines cross or intersect in some way
interaction.plot(iris$Species, iris$Soil, iris$Sepal.Width, type='b', col=c("red", "dark green", "blue"),
                 xlab="Partner Status", ylab=" Mean Sepal.Width Score")

#' There appears to be some form of interaction surrounding growing conditions and virginica, we can
#' examine this more closely with the "Tukey Honest Significant Differences" test, basically a cousin
#' to the adjusted paired t-test that we did earlier. This will give us the difference between all the groups
#' and possible conditions as well as p-values, we can also pick out subsets which we are interested in. We 
#' are looking for significant results where both the soil type and the species change, so we go from sun to shade
#' and from one species to another
TukeyHSD(a.3)
TukeyHSD(a.3, "Soil")
TukeyHSD(a.3, "Species")
TukeyHSD(a.3, "Soil:Species")

#' Be VERY careful when interpreting "main effects" such as the influence of soil or species alone if there is
#' a significant interaction term because the influence of these terms will shift according to the other independent
#' variables in the equation.

#' #### [Back](http://joshaclark.com/?page_id=138)
