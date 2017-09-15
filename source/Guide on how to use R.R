# INTRODUCTION------------------------------------------------------------

# this is a basic guide on how to use R

# First you will notice that their are 4 windows top left, bottom left, top right, bottom right,
# you write your code in the top left. The code is actually executed in the bottom left
# the bottom right displays graphs. Also every function (ex sum)has a document that tells you how to use
# the function. That document displays in the bottom right
# in the top right it displays all of the objects you created. We will go over objects later

# now to go over executing commands
# you can execute commands line by line by clicking anywhere in a line and 
# pressing ctrl and enter.

# EXAMPLE. try putting the mouse in he beginning of the line,
# the middle of the line and the end of line. See what happends in the bottom
2+2+2

# you highliting what you want to execute and 
# pressing ctrl and enter. 
2+2
3-2
4*7-3

# you should note that ctrl is on both the right and left side of the keyboard
# Choose what is most comfortable. 

# ASSIGNING-----------------------------------------------------------------
# assigning simply just means that you give something a name
# it is better to show you what assigning is by example than to explain it

# for example try the following. If you like you can verify the results
# with a calculator 
x=2+2
y=2*5
x
y
x+y
x*y
x*y+2
x*y+x+y

# you can remove any object as follows
rm(x,y)
x
y
# notice how the object is not found

# assigning can be a single letter or a long word

# when assigning capitalization is very important
# abc is different from Abc is different from aBc or ABC
# try the following
abc=2
aBc=3
ABC=4
abc
aBc
ABC
abc+aBc

# also numbers, underscore, and periods can be used after a legitimate assignment name
# character
a1=1
# doesnt work
1a=2

a1b=2
ab1=3

a.=4
a.b=5

ab_=6

# here 1 follows _ and . follows _
ab_1.=8

# for naming objects with multiple words some popular styles are as follows
# word1Word2   word1_word2  word1.word2
# the important thing is to have consistency by using the same style for similar types of objects

# if you are interacting with people who use sql at a company you might want to avoid using
# word1.word2 style because that has a different meaning within sql.



# In general = and <- mean exactly the same thing. There are a few exeptions
# it has become common practice to use <- instead of = in the programming community
# therefore from now on I will try to use <- for assignment
# there is a shortcut for <- which is pressing "alt"+"-" 

x=2+2
x <- 2+2


# VECTORS-----------------------------------------------------------
# a vector is like a single column from a data table. for those with a math background
# a vector of numbers in R is very similar to a vector of numbers in linear algebra

# you create a vector as follows with function c

c(1,2,3,4,5)
c(1,4,2,7,7,5)

# the neat thing is that any operation you perform on it is done on each element
x=c(1,2,3,4,5)
y=c(6,7,8,9,10)
x+2
x*2
y-x
# multiplies each element
y*x
# creating an object
z=x+2
z

# if 2 vectors are not the same length you can't perform operations with both of them
x=c(1,3,5)
y=c(6,7,8,9)
x+y
x*y

# you can refer to 1,2,3,4,5 with 1:5
x <- c(1,2,3,4,5)

# this is an equivelent vector
x <- 1:5

# you can also go backwards
5:1

# you can also do a combination of range and not range
x <- c(1:5,10:15,17,18,21)

# you can also do multiplication on the range
x <- 2*1:5
# compare with
x <- (2*1):5

# you can access individual elements of a vector or a subset of a vector
x <- 10:1
x[3]
x[2]
x[1:5]

# using a vector of indices
x[c(1,3,5,7,9)]


# the above with assignment
z <- x[c(1,3,5,7,9)]
z

# while on the topic of subsetting I will breifly introduce logical vectors
# logical vectors take on only TRUE and FALSE depending if the condition is met

# some example
x <= 5
x >=5
# for exactly equals we use == instead of = 
x==5


# we can also use and/or
# | is used for 'or'
x <=2 | x >=9

# & is used for 'and'
x <=5 & x<=3

# GO OVER THIS NOT CORRECT
# they can also be used in combination but the parenthesis should be used 
# to determine if the middle argument belongs to the left or right hand side

(x <=5 & x<=3) | x > 9

# a logical vector can also be assigned 
i <- x <=5 & x<=3
# logical vectors can be used to subset vectors
x[x <=5 & x<=3]
x[i]
# FUNCTION AND PACKAGE BASICS-------------------------------------
# before going any further I would like to discuss the basics of packages and functions
# You can learn what any function does by highlighting a function and pressing f1
# try the following
mean
sum

# notice that when you pull up mean you see x, trim, na.rm, ...
# x is the vector, trim removes extreme values, na.rm decides whther missing values should be
# removed before calculating the mean. 

# by trim and na.rm some value is set. That means that unless otherwise specified, trim is 0
# and na.rm=False (You can also input F for False and T for True)

# for example
x=c(1:10,NA)
x
# compare the following
mean(x)
mean(x,na.rm=T)
# notice that if only one NA value is present R does not work.

# trim removes a certain percentage of the highest and lowest values
# in the example below I removed 1% from each end of x which is 1 value from each end
mean(x,trim=.1,na.rm=T)
mean(c(2,3,4,5,6,7,8,9),na.rm=T)

# you don't need to name the arguments
# compare the following
mean(x,trim=.1,na.rm=T)
mean(x,.1,T)

# if you want to use the arguments out of order than you do need to name the arguments
mean(x,na.rm=T)
mean(x,T)

# you also don't have to finish naming the arguments  
# I chose to use tr for trim and na for na.rm
# when working with others you might want to spell out the arguments for clarity
mean(x,tr=.1,na=T)

# PACKAGES
# packages simply contain functions that do very useful things
# you install a package with the following syntax

# install.packages('package')
# you load a package with the following syntax
# library(package)
# or library('package')
# or require(package)
# or require('package')

# for an example the following is the package developed for the most part by the cheif scientist
# at R studio

install.packages('tidyverse')
library(tidyverse)

# an example of a useful function to use right away is n_distinct
# which calculates the number of unique values
# pull up the documentation for n_distinct using f1
n_distinct

# example
x=c(1,1,2,1,2,3)
n_distinct(x)





# FUNCTIONS WITH VECTORS---------------------------------------------------------

# some useful functions for dealing with vectors

# how long is the vector
length(x)

# average of vector
mean(x)

# sum of vector
sum(x)

# variance and standard deviation 
var(x)
sd(x)

# min and maximum
min(x)
min(1:10)
max(x)
max(1:10)

# unique values
x=c(1,1,2,3,2,3)
unique(x)

# often you want the count of unique values
length(unique(x))

# the package dplyr has a shortcut for this
library(dplyr)
n_distinct(x)

# you can also do more complex manipulations with these functions 
# arbitrary example
mean(x)/var(x)+sd(x)

# the results of all of these can be assigned
V <- var(x)
V

# some additional functions

# absolute value
abs(-2:2)

# vectors can also take on words but words is another big topic
x <- c('A','B','C')
max(x)

# it should be notes that vectors can often have missing/unknown values
a <- c(1,2,3,4,NA)

# many functions don't work by default if there is even just one NA value
mean(a)
sum(a)
sd(a)

# does work
Var(a)
length(a)

# you can tell R not to count NA values with na.rm=T
mean(a,na.rm=T)

# in general you can alter function arguments with partial from the purrr package
library(purrr)
# simply name the arguments you want to alter with commas
mean2=partial(mean,na.rm=T)

mean2(a)

# MATRICES-------------------------------------------------------------------
# you can create a matrix from a vector
matrix(1:12,nrow=3,ncol=4)

# be default it goes by column but you can specify to go by row
matrix(1:12,nrow=3,ncol=4,byrow=T)

# if it is a square matrix you don't need to specify both rows and columns
matrix(1:9,nrow=3)
matrix(1:9,ncol=3)

# here are some matrix operations
A <- matrix(1:9,nrow=3)
B <- matrix(10:18,nrow=3)
A
# transpose
t(A)
# multiplication
A %*% B

# elementwise operations
A * B
A / B
A^2

# DATAFRAMES--------------------------------- 

# real world data is often stored in data tables
# The atvantage of data frames is that there are many tools to manipulate and clean data frames
# It is also a useful way for organizing a lot of information at once
# the disatvantage is that advanced statistical algorithms often use matrices instead

# there are a number of data frames that come with R and with packages in R
# to access these data frames simply just type in the name

# examples 
iris

# you can quickly veiw information about each column with summary
summary(iris)

# view the data type of each column and innitial values
str(iris)

# View the innitial rows
head(iris)

# View the entire table
View(iris)

# number of rows
nrow(iris)

# number of columns
ncol(iris)

# rows and columns
dim(iris)

# column names
names(iris)

# alter column names
dat=iris
names(dat)
# the names of the data frame can be altered like any vector

# change all the names
names(dat)=c('A','B','C','D','E')
names(dat)

# change the third name
# instead use names(dat)[3] <- 'C'

# change the first and third name
# instead use names(dat)[c(1,3)]=c('A','B')

# access a single column as a vector
iris$Sepal.Length

# equivelent;y 
iris[['Sepal.Length']]

# this is not the same as the data type is still data frame and not a vector
# this will not always work with functions that use vectors
iris['Sepal.Length']

iris['Sepal.Length'] %>% class

# here I used %>%. for function F F(x) is the same as x %>% F or x %>% F()
x <- 1:5
mean(x)
x %>% mean
x %>% mean()
# unfortunately the hotkey is 3 buttons. I personally changed mine to 'alt' + '>'

# the point of the pipe operator is to simplify more complex operations with many parenthesis

# you can create your own data frame from vectors
a <- 1:4
b <- 5:8
c <- 9:12

data.frame(a,b,c)

# the names are by defualt the ones you inputed but you can assign different names
data.frame(A=a,B=b,c)

# cbind will add a column to an existing data frame and retian the structure of data frame
dat <- data.frame(a,b,c)
dat <- cbind(dat,d=13:16)
class(dat)

# like vectors data frames can also have NA and other types of missing/ unknown values

# in R there are multiple approaches to data manipulation

# 1 there is the functions that come from R. 
# It is perhaps the most common form of data manipulation

# 2 there is a popular package called dplyr. It is faster than base R and is popular for its
# intuitive syntax and readability.It also contains some functionality not present in base R

# 3 another pakcage called data.tables. This package is known to have short syntax and to 
# fast on very large data but the syntax is less readable. It also contains functionality not
# present in base R

# 4 you can use sql within R using the sqldf package. Useful if you are communicating with
# people who are familiar with sql or if there is some functionality in sql not present within
# base R or the other packages.

# I will discuss the two most popular base R and dplyr

# ordering data
order(iris)

# dplyr
# the first argument of most of dplyr's functions is the data frame
# and the result genneraly outputs the entire data frame

head(iris)
# 

# arrange accending
arrange(iris,Sepal.Length)
# arrange descending
arrange(iris,desc(Sepal.Length))
# arrange by column1 and then column 2
arrange(iris,Sepal.Length,desc(Petal.Length))

# selecting columns
# column subsetting procedures are used for both removing unused columns and for ordering
# in general you can select by the name of a column and by the indice of the column

# I am selecting 2 columns and reversing the order
head(iris)
select(iris,Species,Sepal.Width)
# alternatively
select(iris,5,2)
# or 
select(iris,c(5,2))
# or
i <- c(5,2)
select(iris,i)

# filtering data
# essentially input a logical vector and it will output the rows where the vector is TRUE

head(iris)

# dplyr
# syntax is df than any number of conditions seperated by commas
filter(iris,Sepal.Length < 5, !Petal.Width==.2)

# conditions can also make sure of the & and | operators
# arbitrary example of either virginica or sepal width > 3 (or both)
filter(iris, Species=='virginica' | Sepal.Width > 3)

# when you filter in general the NA values and other special values will be removed

a=c(1:4,NA)
b=5:9
c=c(NA,10:13)
dat=data.frame(a,b,c)

filter(dat,a < 3)

# You can also select the top rows and sample from it just like in base R
# top_n syntax is df, n ,variable
# if n is positive it will take the highest number if n is negative is will take 
# the lowest numbers.
top_n(dat,2,a)
top_n(dat,-2,a)

# notice that top produces more than 100 rows here
top_n(iris,100,Sepal.Width)

# in general that is the case because it includes the extra ties
# also notice that it doesn't change the order of the data

# also you can randomly select rows from the data with sample n
sample_n
sample_frac
# TIBBLES--------------------------------------------------------------
# IF STATEMEMT-------------------------------------------------------------------
# if statement is used when you only want R to do something if certain conditions are met

# Here are some examples of the syntac of if statements
rm(x,y,a,b)
a=5
b=10
if (a < 2) {x=3}
x
# notice that x doesn't exist

# another example
if (a > 2 & a < 7){
x <- 3
y <- 4
}
# x and y are both properly assigned
# if statements quite literally just execute anything inside if the condition is met

# the condition must result in only one value of True or False

# the following below don't work

a=NA
a > 3
# result is NA

a=1:3
a > 2
# the results is a vector which is not excepted by the if statement

# this does work
if (any(a > 2)) {x=5}
x

# you can do nested if statements and if else statements

# There is also vectorized if statements which is completely different

# the basic syntac is ifelse(condition, value if true,value if false)

# here is an arbitrary silly example
x=1:10
y=10:1

ifelse(y > x, y^2,x^2)

# you can nest them

ifelse( y > x,
ifelse(x=1)
  
  )

# an alternative to complex nested ifelse is case when from dplyr

# in these examples I am using the between function from dplyr 
# short hand for x >=low & x <=high
# syntax is between(vector,low,high) or x %>% between(low,high)

x <- 1:50
y <- rep(1,50)
case_when(
x %>% between(41,50)~'A',
x %>% between(31,40)~'B',
x %>% between(21,30)~'C',
TRUE ~ as.character(x)
)

# the example below shows how genneraly when using a vector you need to specify its class
case_when(
x %>% between(41,50)~5,
x %>% between(31,40)~4,
x %>% between(21,30)~3,
TRUE ~ as.double(x+y)
)

# the condition and outputs can involve multiple vectors


# LOOPING------------------------------------------------------------

# looping is iterating a process multiple times

# when starting a loop you should innitialize an empty structure (vector,matrix,list etc)
# you can use the vector function to do this

# list
vector('list',5)

# number vectors
vector('numeric',5)
vector('double',5)

# logical vector
vector('logical',5)

# string vector
vector('character',5)

# matrix
matrix(,5,5)
# or
matrix(NA,5,5)




