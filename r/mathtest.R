library(mosaic)	# nice for its overloading of the mean() function

mathtest = read.csv('../data/mathtest.csv')

# Ensure that school is treated as a categorical variable
mathtest$school = factor(mathtest$school)

# This won't work without mosaic loaded
schoolmeans = mean(mathscore~school, data=mathtest)
schoolsizes = as.numeric(summary(mathtest$school))

# Notice the extremes tend to be for schools with low sample sizes
plot(schoolsizes, schoolmeans, pch=19)
