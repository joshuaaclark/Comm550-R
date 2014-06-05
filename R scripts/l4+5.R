#' COMM 550-Labs Four and Five: T and Z Tests
#' =========================================================== 
#'#### [Back](http://joshaclark.com/?page_id=138)
#'
#' Welcome to the lab four and five demo! Lab four will be primarily done with pen and paper
#' but it is useful to know how these concepts works in R as they will pop up again in later
#' labs.

#' Although you should learn how to do a Z test by hand let's do one in R to show you to code as well.
#' This is useful for double-checking answers!

#' T Z=test is not normally used in statistics because it is rare that we know the parameters of a population.
#' Therefore we have to install a new package, "TeachingDemos" and mount it.

#'>install.packages('TeachingDemos') 
library(TeachingDemos)
#' We'll also need the car packages (vrooom!)
library(car)

#'To run a z test we need a value which we are testing, the mean of the population, the standard deviation of
#' the population and what we are looking for (probabilities of values greater than? Less than?).
#' Let's try with an example from the lab, we know battery lifetime is normally distributed, mean=500, sd=50. We 
#' want to know how many of our 200 batteries will last longer than 600 hours.
z.test(600, mu=500, sd=50, alternative='greater')

#' P value of 0.02275, so that's the proportion of the 200 we can expect to have a life greater than 600 hours. 
#' To get the raw total we have to multiply that proportion by the number of batteries we have.
200*0.02275

#' So roughly 4 of the two hundred batteries will last longer than 200 hours (maybe).

#' Most of lab 4 requires calculations by hand. This is a useful exercise and isn't as hard as it might seem
#' you can always use R as a calculator or to use the Z-test function to get results. As an example to calculate 
#' a 95% confidence interval we can use the formula m+1.96(SE) and m-1.96(SE) where SE = SD/sqrt(n). Let's take 
#' the battery example where the mean is 500, n=200 (the number of batteries we bought) and SD=50
M.CI<-500
N.CI<-200
Std.Dev.CI<-50
Std.Err.CI<-Std.Dev.CI/sqrt(N.CI)
high.ci<-M.CI+1.96*Std.Err.CI
high.ci
low.ci<-M.CI-1.96*Std.Err.CI
low.ci

#' T-TESTS AND OTHER FUN
#' The data set contains part of the data for a study of oral condition of cancer patients conducted at the
#' Mid-Michigan Medical Center.  The oral conditions of the patients were measured and recorded at the initial stage,
#' at the end of the second week, at the end of the fourth week, and at the end of the sixth week.  The variables age, 
#' initial weight and initial cancer stage of the patients were recorded.  Patients were divided into two groups at
#' random:  One group received a placebo and the other group received aloe juice treatment.
#' Sample size, n = 25 patients with neck cancer. The treatment is Aloe Juice. The variables in the data set are:
#'   
#' * ID- Patient ID
#' * TRT- treatment group:  0 = placebo; 1 = aloe juice
#' * AGE- patient's age in years
#' * WEIGHTIN- patient's weight at the initial stage
#' * STAGE- initial cancer stage, coded 1 through 4
#' * TOTALCIN- oral condition at the initial stage
#' * TOTALCW2- oral condition at the end of week 2
#' * TOTALCW4- oral condition at the end of week 4
#' * TOTALCW6- oral condition at the end of week 6
#' 
#' NOTE:  The variables TOTALCIN, TOTALCW2, TOTALCW4, and TOTALCW6 are the dependent variables, 
#' constituting repeated measures over time.  
#' All of the study participants were male and of the same ethnicity so these variables aren't in our data.
#' This data is drawn from [Central Michighan University](http://calcnet.mth.cmich.edu/org/spss/Prj_cancer_data.htm) 

#' Let's read the data, notice the line saying that the file has a header row with variable names included.
can<-read.csv(url('http://joshaclark.com/wp-content/uploads/2014/05/cancer.csv'))

#' And take a look to make sure everything matches up
head(can)

#' In the lecture on parameter estimation we talked about the z transformation, normalizes variables so that they
#' have a mean of zero through the formula z=(X-m)/sd. To convert back from z-scores to raw data X=z*sd+m
#' Let's calculate the z-score for the first variable in AGE (52)
mu<-mean(can$AGE)
stn.d<-sd(can$AGE)
(52-mu)/stn.d

#' We get -0.59 or just about 2/3rds of a standard deviation lower than the mean of 60 which can also be expressed
#' as:
mu-stn.d*0.59

#' To transform an entire variable into a its z-transformed version use the scale function and create a new
#' variable
can$AGE.Z<-scale(can$AGE)
head(can)


#'Let's recode our treatment variable to state if a given patient was given the treatment or placebo
can$TRT.CODE<-factor(can$TRT, level=c(0:1), labels=c("Placebo", "Aloe Juice"))

#' Be don't have any missing data and because this is an experiment we have very good measurements
#' about the weight and treatment patterns of each person in the study. Extremely abnormal patients 
#' have been screened out in the pre-study testing so we don't have to worry about missing data or
#' outliers. So let's dive straight into analysis.

#' The first thing we are interested in is if our subject population is significantly different from the 
#' average weight of the popular (in this case people in Michigan or the US depending on how broadly you 
#' define things). Through the national health institute I've determined that the average weight for men in this
#' age range is roughly 170lbs. Given this population average is our sample significantly different?

#' To find out we can do a one sample t-test. This compares a sample's mean to a population mean in order
#' to see if there is a statistically meaningful difference between the two given the standard error of the sample.
mean(can$WEIGHIN)
t.onesamp<-t.test(can$WEIGHIN, mu=170)
t.onesamp



#' Our p-value is high, this means that there is not a difference between the sample and the population which
#' is big enough to attribute to something other than chance. This bodes well for the generalizability of our
#' study. 

#' Next let's make sure that the ages in our two treatment conditions as close enough. For this we'll use an 
#' independent sample t-test. This compares the means between two groups where membership is independent, i.e.
#' membership in group A precludes membership in group B. Because of the experimental controls people in the 
#' placebo group cannot be in the treatment group and vice versa, so the two are independent

#' Before we test this however we need to see if the variance between the two groups is equal, as this will
#' influence how the t-test is calculated. We can do this with Levene's test. The null hypothesis for this test
#' is that the variances are equal. So if p>0.05 then the two groups have roughly equal variance. If p<0.05 then
#' the variance is not equal and we have to modify our t-test. The formula in R for the levene test features the
#' variable whose variance you are measuring first followed by a comma then a factor which breaks the data up into
#' two groups, in this case the two treatment groups.
#' Just a note, R is actually doing a  Brown-Forsythe test here comparing the medians of two groups, but the syntax is called Levene Test so we'll stick with that name to avoid confusion.

leveneTest(can$AGE, can$TRT.CODE)

#'The value is significant so we need to tell our t-test to use the formula adapted for independent groups with
#' different variances Notice the tilde sign between age and the treatment code variable. This notation will appear more and more
#' often as we start to use R more to differentiate dependent variables from other parts of a model.

t.indie<-t.test(can$AGE~can$TRT.CODE, var.equal=FALSE)
t.indie
#' For giggles let's run it with the equal variance formula to see the difference.
t.indie.giggles<-t.test(can$AGE~can$TRT.CODE, var.equal=TRUE)
t.indie.giggles

#' As you can see they are close but there is some variance between the two results. That can be the difference between
#' a significant or non-significant result in many cases!


#' Next, let's look at if participation in the study had an impact on the various treatment scores recorded
#' during the study. For this we'll be looking at the same subjects with two different sets of observations.
#' Therefore the groups cannot be independent as the paired observations are from the same subjects. Therefore
#' we do a paired samples t-test which compares the mean at the start of the study to the values two weeks in.
t.paired<-t.test(can$TOTALCIN,can$TOTALCW2,paired=TRUE)
t.paired

#' There is a significant difference and it is going downwards, woohoo!

#' And done, don't forget to raise a beer to Student and his t-test when you get a chance.

#'#### [Back](http://joshaclark.com/?page_id=138)