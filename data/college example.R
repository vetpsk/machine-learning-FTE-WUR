getwd()
install.packages("ISLR")
require(ISLR)

summary(College[,1:10])
pairs(College[,1:10])

plot(College$Outstate ~ College$Private)

college <- College
rm(College)

college$Elite <- rep("No",nrow(college))
college$Elite[college$Top10perc >50] <- " Yes"
college$Elite <- as.factor(college$Elite)
## college <- data.frame(college , Elite)

summary(college$Elite)

plot(college$Outstate ~ college$Elite)
par(mfrow = c(2,3))

# hist(college$Apps, breaks = seq(0, 50000, 2000))
# hist(college$Accept, breaks = seq(0, max(college$Accept), max(college$Accept)/10))
# hist(college$Enroll, breaks = seq(0, max(college$Enroll), max(college$Enroll)/10))
# hist(college$Top10perc, breaks = seq(0, max(college$Top10perc), max(college$Top10perc)/10))
# hist(college$Top25perc, breaks = seq(0, max(college$Top25perc), max(college$Top25perc)/10))
# hist(college$F.Undergrad, breaks = seq(0, max(college$F.Undergrad), max(college$F.Undergrad)/10))
# hist(college$P.Undergrad, breaks = seq(0, max(college$P.Undergrad), max(college$P.Undergrad)/10))
# hist(college$Outstate, breaks = seq(0, max(college$Outstate), max(college$Outstate)/10))

sapply(college[,2:18], hist)

