#### Part1: College DataSet ####

# Read College.csv
college = read.csv("../datasets/College.csv")

# Converting College data to Categorical Data
college$Private = factor(college$Private, c("Yes","No"), labels = c("Yes","No"))

# Assign row names
rownames (college )=college [,1]

# Drop first column
college = college[,-1]

# Summary of the dataset
summary(college)
dim(college)
fix(college)

# Plot Scatter-plot of first 10 columns
pairs(college[1:10])

# Plotting OutState v/s Private
summary(college$Private)
plot(college$Outstate,college$Private)

# Adding Column Elite
Elite = rep ("No",nrow(college ))
Elite [college$Top10perc >50]="Yes"
Elite =as.factor (Elite)
college =data.frame(college ,Elite)

# Plotting Outstate v/s Elite
plot(college$Outstate, college$Elite)

# Plotting SubPlots
par(mfrow=c(1,2))

# 1. Private v/s Elite
plot(college$Private, college$Elite, xlab = "Private", ylab = "Elite", main = "Private v/s Elite")

# 2. Acceptance Rate v/s %age of students in cohort from top 10 %ile in High school
plot(college$Accept/college$Apps
     ,college$Top10perc
     ,xlab = "Acceptance Rate"
     ,ylab = "%age Top students"
     ,main = "Acceptance Rate v/s Quality"
     ,pch = "*"
     ,col = ifelse(as.character(college$Elite) == "Yes", "red","green")
     #,legend = legend("topright", col=c("red","green"),c("Elite","Not Elite"))
     )

#### Part2: Auto DataSet ####

# Read Auto Data
auto = read.csv("../datasets/Auto.csv")

# Assign Rownames
        # Duplicate entries for "plymouth reliant 81 - 4 cylinders"
rownames(auto) = paste(as.character(auto$car.name)
                       ,as.character(auto$model.year)
                       ,"-",as.character(auto$cylinders),"cylinders")
fix(auto)

# Summary
summary(auto)
