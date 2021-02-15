# An Introduction to Statistical Learning,
# Sec. 2.3 Lab: Introduction to R

## ==== Sec. 2.3.1 Basic Commands ==================================================

# Create a vector containing 3 numbers
x = c(1, 6, 2)
x
y = c(1, 4, 3)
y

# Make sure that x and y have the same
# length before adding them
length(x)
length(y)
x + y

# List the variables in your workspace
# (can also look at the GUI for this)
ls()
# Remove x and y from the workspace
rm(x, y)
# Remove all variables from workspace
# (same as Clear button in GUI)
rm(list=ls())
ls()

# Display the help page for matrix()
?matrix
x = matrix(data=c(1, 2, 3, 4),
           nrow=2, ncol=2)
# Same: x = matrix(c(1, 2, 3, 4), 2, 2)
# Populate row by row instead
matrix(data=c(1, 2, 3, 4),
       nrow=2, ncol=2, byrow=TRUE)
# This is not assigned to a variable,
# so it is directly printed instead

sqrt(x)
x^2

# Sample of n = 50, from normal
# distribution with mean 0, std. dev. 1
x = rnorm(50)
# Std. dev. is square root of the variance
# Variance is the average of the squared
# distances of each point to the mean
x
# WHAT DOES THIS STATEMENT DO?
y = x + rnorm(50, mean=50, sd=.1)
y

# Plot 2 graphs side by side
par(mfrow=c(1, 2))
plot(x)
plot(y)
# Move back to plotting 1 graph at a time
cor(x, y)
# Correlation is the extent to which two
# variables have a linear relationship
# with each other
par(mfrow=c(1, 1))

# Seed the random number generator with a
# specific (arbitrary) value, so that
# commands such as rnorm() always yield
# the same values. This is for reproducibility
# of the results
set.seed(1303)
rnorm(50)

set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

## ==== Sec. 2.3.2 Graphics ========================================================

?plot
x = rnorm(100)
y = rnorm(100)
plot(x, y)
plot(x, y, xlab="this is the x-axis",
     ylab="this is the y-axis", main ="Plot of X vs Y")

# Store plot as PDF
pdf("Figure.pdf")
plot(x, y, col="green")
# Tell R that we are done plotting and that
# the file can be written to disk
dev.off()

# Store plot as JPG
jpeg("Figure.jpg")
plot(x, y, col="lightblue")
dev.off()

# You can also use the GUI for storing plots
# as files!

x = seq(1, 10)
x
x = 1:10
x
x = seq(-pi, pi, length=50)
x
x = seq(-50, 50, by=10)
x

# Now for some more advanced plots.

?contour
x = seq(-pi, pi, length=50)
y = x
f = outer(x, y, function(x, y)cos(y)/(1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels=45, add=T)
fa = (f - t(f)) / 2
contour(x, y, fa, nlevels =15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta=30)
persp(x, y, fa, theta=30, phi=20)
persp(x, y, fa, theta=30, phi=70)
persp(x, y, fa, theta=30, phi=40)

## ==== Sec. 2.3.3 Indexing Data ===================================================

A = matrix(1:16, 4, 4)
A
dim(A)
A[2, 3]
# A[2, 3, drop=FALSE]
A[c(1, 3), c(2, 4)]  # Behaves like AND
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]
# Sometimes, for subsequent matrix
# manipulations, you want to keep it as a
# matrix, even if it has only one dimension
A[-c(1, 3), -c(1, 3, 4), drop=FALSE]

## ==== Sec. 2.3.4 Loading Data ====================================================

?read.table()
# write.table(): export data, write data to file
# Menu: Session -> Set Working Directory ->
#       To Source File Location
#
# SHOW DATA IN TEXT FILE FIRST
#
Auto <-  read.table("./data/Auto.data")
# fix(Auto)
# Interpret first row as header,
# interpret "?" as NA (not applicable)
Auto <-  read.table("Auto.data", header=TRUE,
                  na.strings="?")
# fix(Auto)

# Before you do this, you can look at the data in a text editor.
Auto <- read.csv("./data/Auto.csv", header=TRUE,
                na.strings="?")
# fix(Auto)
dim(Auto)
Auto [1:4, ]
# Omit rows containing NA (not applicable)
Auto <-  na.omit(Auto)
dim(Auto)
names(Auto)
str(Auto)

## ==== Sec. 2.3.5 Additional Graphical and Numerical Summaries ====================

# Error: object not found
plot(cylinders, mpg)
# But this works
plot(Auto$cylinders, Auto$mpg)
# Or this
attach(Auto)
plot(cylinders, mpg)

# Convert cylinders from quantitative
# to qualitative (categorical), because
# it can take only a few values
cylinders = as.factor(cylinders)
# Among other things, this leads to the
# generation of boxplots instead of scatterplots
# if the qualitative variable is on the x-axis
plot(cylinders, mpg)
# If varwidth is TRUE, the boxes are drawn with
# widths proportional to the square-roots of the
# number of observations in the groups
plot(cylinders, mpg, col="green", varwidth=TRUE,
     horizontal=TRUE, xlab="cylinders", ylab="MPG")

hist(mpg, col=2)  # 2 is "red"
hist(mpg, col =2, breaks=15)

pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight +
      acceleration, Auto)
pairs(Auto[1:4])

plot(horsepower[1:20], mpg[1:20])
identify(horsepower, mpg, name)
# Click one or more points, then click finish
# The corresponding names will apear,
# and their indices in the console

summary(Auto)
summary(mpg)

Auto$year <- as.factor(Auto$year)
Auto$origin <- as.factor(Auto$origin)
Auto$cylinders <- as.factor(Auto$cylinders)

sapply(Auto[,c(1, 3:6)], range,USE.NAMES = T)
sapply(Auto[,c(1, 3:6)], mean, USE.NAMES = T)
sapply(Auto[,c(1, 3:6)], sd, USE.NAMES = T)

sub_Auto <- Auto[-c(10:85),] 
summary(sub_Auto)


pairs(Auto[,c(1, 3:6)])
par(mfrow = c(2,2))
for (i in c(2,7:8)) {
  for (j in c(1,3:6)) plot(Auto[[j]] ~ Auto[[i]], xlab = names(Auto[j]), ylab = names(Auto[i]))
}


# When exiting RStudio, we have the option to
# save the current workspace so that all objects
# in this R session will be available next time.
# Before exiting R , we may want to save a record
# of all of the commands that we typed in the most
# recent session (if we were working in the console
# instead of in a file); this can be accomplished
# using the savehistory() function. Next time we
# enter R, we can load that history using loadhistory()
