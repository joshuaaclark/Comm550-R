#' COMM 550-Lab Two: Welcome to R
#' ===========================================================
#' #### [Back](http://joshaclark.com/?page_id=138)                                                  

#'Welcome to R! This lab will walk you through the basics of installing packages, loading data and
#'performing basic descriptive statistics on a dataset. For the purposes of this demo we'll be using
#'a dataset drawn from the TV show "Grey's Anatomy." This data is adapted from the [Bad Hessian](http://badhessian.org/2012/09/lessons-on-exponential-random-graph-modeling-from-greys-anatomy-hook-ups/) tutorial on ERGMs so all credit goes to them. 

#' Packages
#' -----------------------------------------
#'Packages are pieces of code which extend the functionality of R. They do everything from automating 
#'basic tasks to transforming R into an awesome tool for exotic forms of analysis. In order to take 
#'advantage of this power we need to learn to install packages using the simple syntax shown below.
#'To run one or two lines at a time select the code and hit ctrl (or command)-enter.

#'
#' >install.packages('ggplot2')  
#' >install.packages('psych')
#'

#'Install packages needed for this lab. Once done you can "comment out" these lines by placing
#' a "#'" sign in front of the line. This tells the computer not to read that script. If you don't
#'then R will re-download the package every time you run the entire script which is a pain. Notice
#'that the brackets have to be closed and the package name in quotes, if missing a bracket R will
#'prompt you in the console to complete the code, but missing quotes will break the code.
istall.packages('ggplot2') 
install.packages(ggplot2) 


#'Mount the packages so R can use them. R won't load packages unless you tell it to in order to save memory.
library(psych)
library(ggplot2)


#'Let's read our dataset into R. The data is in the common comma separated variable format which R is very good
#'at reading. As long as the file is in your working directory just use the read.csv command.
dat<-read.csv(url('http://joshaclark.com/wp-content/uploads/2014/05/greydata.csv'))

#'Let's take a look at the data by looking at the variable names.
names(dat)

#'Oh they aren't labeled, we need to change that

#'The head command shows you the first few lines of a dataset, useful to see what's in it without
#'printing out thousands and thousands of rows
head(dat)

#'To see the whole thing just type the name of the data frame
dat

#'Okay, time to name our variables something sensible. The c(") indicator marks a list of strings
#'which R will assign to variables in order. So name goes on the first col. gender on the second etc.
names(dat) <- c("name","gender", "race", "byear", "position", "season", "sign", "rank")
names(dat)
head(dat)


#'R will makes best guesses about the measurement levels of your variables but you may have to recode. In this case
#'we'll be treating position as a factor, or a nominal variable. So each job title is distinct and unordered.
dat$position<-factor(dat$position, level=c(0:6), labels=c('non-staff','attending','chief', 'resident', 'intern', "nurse", 'other'))

#'We can do the same with gender and race, making it a factor and assigning a variable name
dat$gender<-factor(dat$gender, labels=c('Male','Female'))
dat$race<-factor(dat$race, level=c(0:3), labels=c('White','Black',"Latino", "Asian"))
summary(dat$race)                

#'R will do its best to identify factors beforehand, as an example it has already labelled sign as a factor because
#'it doesn't contain any numbers.                
is.factor(dat$sign)                
                
                
#'Now we want to make an ordered variable representing how many seasons a back a character was introduced
#'Unfortunately the only data that we have is what season they appeared, so right now Yang, played by (Sandra Oh)
#'would have a score of 1 because that is the season she joined the show, but we want her character to have a score
#'of 8 representing her duration as a member of the cast. Therefore we need to recode. Reverse coding is easy, just subtract
#'the old variable from a value+1 of its max value.
maxseason<-max(dat$season)+1
maxseason
dat$intro=maxseason-dat$season

#'Finally let's look at the rank variable, this is a ranking of characters by fans from 1 (awful) to 5(awesome)
#'This makes it an ordinal variable but R is not treating it as such
is.ordered(dat$rank)
#'To convert it, use the following command
dat$rank<-ordered(dat$rank)
#'Run is.ordered again to check to make sure it worked
is.ordered(dat$rank)

#'Good stuff! Let's take a look at what our dataset looks like now.
head(dat)

#'Descriptive stats are easy to do with the 'psych' package we installed and mounted earlier. All we do is run the following:
descriptives<-describe(dat)
descriptives
#'Easy, eh? We can also pass single variables or lists of variables to get more focused stats
describe(dat$byear)

#'Similarly frequency tables can be found with the table() function. Passing the entire dataset makes a mess so let's just look
#'at position                 
table(dat$position)


#'Running the same command over and over again is a pain, so the "apply" family of functions automates this
#'lapply looks over a list and runs the command "table" on each of the variables listed, giving us multiple
#'frequency tables at one. Try adding or removing variable names to see what happens.                 
lapply(dat[c('gender', 'race', 'sign', 'position')],table)

#'Let's say we are interested in the breakdown between character gender and position on the show. The first step
#'is to make a cross-tabulation. This shows genders as columns and position as rows with the frequency of people who
#'fit both descriptors as a value at the intersection of the two.                 
genderjob<-table(dat$position, dat$gender)
genderjob

#'We can transpose the table to make gender the rows and position the columns by flipping the order of the two in the
#'syntax.                 
jobgender<-table(dat$gender,dat$position)                 
jobgender

#'To represent things as a clustered plot we can use the ggplot2 plackage. First we call the qplot function  (quick plot), provide the variable we want to examine (gender) as well as the dataframe (dat), then the "geom" or type of graph (in this case a histogram) and tell R to segement the graph by position so we can compare.            

m<-qplot(gender, data=dat, geom='histogram', fill=position)
m
#'ggplot2 is vert versatile, if we simply wanted to see the distribution of fan rankings we can simplify the synax above and get:              
n<-qplot(rank, data=dat, geom='histogram')                 
n

#'And done! Great work! R is extremely forgiving with regards to errors and messing things up. If things go wrong
#'you can always reload your dataset from the hard drive to roll back changes. Mess around with the help files by adding
#'?? before commands in the console or searching the help tab to the right. This demo should give you all the tools you need
#'to do the lab, or email me at joshuaac@usc.edu                

#'#### [Back](http://joshaclark.com/?page_id=138)