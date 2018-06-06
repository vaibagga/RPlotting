# data files
library(xlsx)
library(ggplot2)
setwd('C:/Users/lenovo/Downloads')
## loading the dataset
data_control1 <- read.xlsx('project.xlsx', 6)
data_control2 <- read.xlsx('project.xlsx', 8)
## create a new dataset
## taking data from sheet 6
combined_control <- data.frame(matrix(0, nrow = 103), stringsAsFactors = FALSE)
combined_control$Subject = data_control1$UNCONTROLLED.HBA1C..DEPRESSION.AND.ANXIETY.LEVELS.
combined_control$HBA1C = data_control1$NA.
combined_control$noDep = data_control1$NA..1
combined_control$minDep = data_control1$NA..2
combined_control$mildDep = data_control1$NA..3
combined_control$modDep = data_control1$NA..4
combined_control$sevDep = data_control1$NA..5
combined_control$noAnx = data_control1$NA..6
combined_control$minAnx = data_control1$NA..7
combined_control$mildAnx = data_control1$NA..8
combined_control$modAnx = data_control1$NA..9
combined_control$sevAnx = data_control1$NA..10
combined_control = combined_control[4:103, ]
## taking data from sheet 8
combined_control$Age = data_control2$NA..1[6:105]
combined_control$Sex = data_control2$NA..2[6:105]
combined_control$normBMI = data_control2$NA..3[6:105]
combined_control$ovwtBMI = data_control2$NA..4[6:105]
combined_control$ob1 = data_control2$NA..5[6:105]
combined_control$ob2 = data_control2$NA..6[6:105]
combined_control$SCS = data_control2$NA..7[6:105]
combined_control$matrix.1..nrow...103. <- NULL
## filling NAs

## age distribution in data
hist(as.numeric(as.character(combined_control$Age)), 
     main = 'Age Distribution in Uncontrolled Data', 
     xlab = 'Age', 
     breaks = 20, 
     col = 'blue')
## sex distribution in data

x = c(sum(combined_control$Sex == 'M'), "Female" = sum(combined_control$Sex == 'F'))
labels <-  c("Male", "Female")

piepercent<- round(100*x/sum(x), 1)

# Give the chart file a name.
png(file = "Uncontrol Data Sex.jpg")

# Plot the chart.
pie(x, labels = piepercent, main = "Sex Distribution in Unontrolled Data",col = rainbow(length(x)))
legend("topright", c("Male", "Female"), cex = 0.8,
       fill = rainbow(length(x)))

# Save the file.
dev.off()

## Sugar vs Age
plot(as.numeric((as.character(combined_control$Age))),
     as.numeric((as.character(combined_control$HBA1C))),
     main = "Age vs HBA1C levels in Uncontrolled Data",
     xlab = "Age",
     ylab = "HBA1C levels",
     col = "red",
     pch = 4)
abline(lm(as.numeric(as.character(combined_control$HBA1C)) ~ as.numeric(as.character(combined_control$Age))))
## Sugar vs Gender

is_male = combined_control$Sex == "M"
is_female = combined_control$Sex == "F"    
males = combined_control[is_male, ]
females = combined_control[is_female, ]
hist(as.numeric(as.character(males$HBA1C)), col = 'blue', breaks = 10, main = "HBA1C levels in Males in Uncontrolled Data", xlab = "HBA1C level")
hist(as.numeric(as.character(females$HBA1C)), col = 'blue', breaks = 10, main = "HBA1C levels in Females in Uncontrolled Data", xlab = "HBA1C level")

## Overall Distrinbution of HBA1C levels
##hist(as.numeric(as.character(combined_control$HBA1C)), col = 'blue', breaks = 10, main = "Distribution of HBA1C levels in Controlled Data", xlab = "HBA1C level")
##y = sum(as.numeric(as.character(combined_control$HBA1C)) <= 6.4) - sum(as.numeric(as.character(combined_control$HBA1C)) < 5.6)
##z = sum(as.numeric(as.character(combined_control$HBA1C)) > 6.4)
##x = c(y, z)
##labels <-  c("Between 5.6 and 6.4", "More than 5.7")

##piepercent<- round(100*x/sum(x), 1)

# Give the chart file a name.
##png(file = "HBA1C Controlled Data.jpg")

# Plot the chart.
##pie(x, labels = piepercent, main = "HBA1C levels in Controlled Data",col = rainbow(length(x)))
##legend("topright", c("Between 5.6 and 6.4", "More than 6.5"), cex = 0.8,


# Save the file.
##dev.off()

## Dep vs HBA1C levels
depLevel = c(as.numeric(as.character(combined_control$noDep)),
             as.numeric(as.character(combined_control$minDep)),
             as.numeric(as.character(combined_control$mildDep)),
             as.numeric(as.character(combined_control$modDep)),
             as.numeric(as.character(combined_control$sevDep)))

depLevel[is.na(depLevel)] = 0
depLevel = depLevel[1:100] + depLevel[101:200] + depLevel[201:300] + depLevel[301:400] + depLevel[401:500]
hist(depLevel, breaks = 20, col = 'blue', main = 'Depression Levels in Uncontrolled Data', xlab = 'Depression Level')


## varaition of depn with sugar
plot(depLevel, as.numeric(as.character(combined_control$HBA1C)),
     col = 'red',
     pch = 4,
     main = 'HBA1C Level vs Depression Levels in Uncontrolled Data',
     xlab = 'Depression Level',
     ylab = 'HBA1C level')
##Anxiety Level
abline(lm(as.numeric(as.character(combined_control$HBA1C)) ~ depLevel))

anxLevel = c(as.numeric(as.character(combined_control$noAnx)),
             as.numeric(as.character(combined_control$minAnx)),
             as.numeric(as.character(combined_control$mildAnx)),
             as.numeric(as.character(combined_control$modAnx)),
             as.numeric(as.character(combined_control$sevAnx)))

anxLevel[is.na(anxLevel)] = 0
anxLevel = anxLevel[1:100] + anxLevel[101:200] + anxLevel[201:300] + anxLevel[301:400] + anxLevel[401:500]
hist(anxLevel, breaks = 20, col = 'blue', main = 'Anxiety Levels in Uncontrolled Data', xlab = 'Anxiety Level')

plot(anxLevel, as.numeric(as.character(combined_control$HBA1C)),
     col = 'red',
     pch = 4,
     main = 'HBA1C Level vs Anxiety Level in Uncontrolled Data',
     xlab = 'Anxiety Level',
     ylab = 'HBA1C level')
abline(lm(as.numeric(as.character(combined_control$HBA1C)) ~ anxLevel))
BMILevel = c(as.numeric(as.character(combined_control$normBMI)),
             as.numeric(as.character(combined_control$ovwtBMI)),
             as.numeric(as.character(combined_control$ob1)),
             as.numeric(as.character(combined_control$ob2)))
BMILevel[is.na(BMILevel)] = 0
BMILevel = BMILevel[1:100] + BMILevel[101:200] + BMILevel[201:300] + BMILevel[301:400]

## BMI Level Distribution
hist(BMILevel, breaks = 20, col = 'blue', main = 'BMI in Uncontrolled Data', xlab = 'BMI')

## sugar vs BMI
plot(BMILevel, as.numeric(as.character(combined_control$HBA1C)),
     col = 'red',
     pch = 4,
     main = 'HBA1C Level vs BMI in Uncontrolled Data',
     xlab = 'BMI',
     ylab = 'HBA1C Level')
abline(lm(as.numeric(as.character(combined_control$HBA1C))~BMILevel))


## SCS/ MSES
x = c(sum(combined_control$SCS == 'MSES'), "Female" = sum(combined_control$SCS == 'USES'))
labels <-  c("MSES", "USES")

piepercent<- round(100*x/sum(x), 1)

# Give the chart file a name.
png(file = "Uncontrol Data SCS-MSES-USES.jpg")

# Plot the chart.
pie(x, labels = piepercent, main = "SCS-MSES/USES",col = rainbow(length(x)))
legend("topright", c("MSES", "USES"), cex = 0.8,
       fill = rainbow(length(x)))

# Save the file.
dev.off()

is_scs = combined_control$SCS == "MSES"
is_uses = combined_control$SCS == "USES"    
scs = combined_control[is_scs, ]
uses = combined_control[is_uses, ]
hist(as.numeric(as.character(scs$HBA1C)), col = 'blue', breaks = 8, main = "HBA1C levels in SCS in Uncontrolled Data", xlab = "HBA1C level")
hist(as.numeric(as.character(uses$HBA1C)), col = 'blue', breaks = 8, main = "HBA1C levels in USES in Uncontrolled Data", xlab = "HBA1C level")
