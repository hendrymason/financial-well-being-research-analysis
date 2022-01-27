#Creating the data frame
project_df <- data.frame(fwb$FWBscore, fwb$KHscore, fwb$LMscore, fwb$FSscore, fwb$SWB_1, fwb$SWB_2, fwb$SWB_3, fwb$MATERIALISM_1, fwb$MATERIALISM_2, fwb$MATERIALISM_3, fwb$SELFCONTROL_1, fwb$SELFCONTROL_2, fwb$SELFCONTROL_3, fwb$DISTRESS, fwb$CONNECT)

#Renaming the Variables
names(project_df) <-c("Financial Well-Being Score" , 
                      "KH Score" , "LM Score" , "Financial Skills Score" , 
                      "Satisfaction" , " Optimism" , "Effort" , "Lux Admiration" , 
                      "Things" , "Impress" , "Impulse" , "Delay Gratification" , 
                      "Diligence" , "Distress" , "PsyConnect")

#Set Quantitative Variables as Numeric
project_df$`Financial Well-Being Score` <- as.numeric(project_df$`Financial Well-Being Score`)
project_df$`Financial Skills Score` <- as.numeric(project_df$`Financial Skills Score`)
project_df$`KH Score` <- as.numeric(project_df$`KH Score`)
project_df$`LM Score` <- as.numeric(project_df$`LM Score`)

#Adjusted Data for Cleaning
adjusted_data <- project_df[-c(885)] # remove an unresponsive row
adjusted_data <- subset(project_df$`Financial Well-Being Score`, project_df$`Financial Well-Being Score` != "-4, -1" )
adjusted_data <- subset(project_df, project_df$`Financial Skills Score` != "Refused" )
adjusted_data <- subset(project_df, project_df$Satisfaction != "Refused")
adjusted_data <- subset(project_df, project_df$` Optimism` != "Refused")
adjusted_data <- subset(project_df, project_df$Effort!= "Refused")
adjusted_data <- subset(project_df, project_df$`Lux Admiration` != "Refused")
adjusted_data <- subset(project_df, project_df$Things != "Refused")
adjusted_data <- subset(project_df, project_df$Impress != "Refused")
adjusted_data <- subset(project_df, project_df$Impulse != "Refused")
adjusted_data <- subset(project_df, project_df$`Delay Gratification` != "Refused")
adjusted_data <- subset(adjusted_data, project_df$Diligence != "Refused")
adjusted_data <- subset(adjusted_data, project_df$Distress != "Refused")
adjusted_data <- subset(adjusted_data, project_df$PsyConnect != "Refused")
adjusted_data <- subset(adjusted_data, project_df$` Optimism` != "Refused")
# LM score & KH score did not need readjustment

# Categorical Data Analysis - Boxplots and Numerical Analysis
boxplot(project_df$`Financial Well-Being Score` ~ project_df$Satisfaction, 
         xlab = 'Satisfaction', ylab= 'Financial Well-Being Score')
project_df$Satisfaction <- as.numeric(project_df$Satisfaction)
summary(fivenum(project_df$Satisfaction))
tapply(project_df$`Financial Well-Being Score`, project_df$Satisfaction, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$Effort, 
        xlab = 'Percieved Impact of Effort', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$Effort, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$`Lux Admiration`,
        xlab = 'Level of Admiration for Luxury', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$`Lux Admiration`, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$Things,
        xlab - 'Percieved Value of Material Thigns', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$Things, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$Impress, 
        xlab = 'Value of Things to Impress Others', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$Impress, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$Impulse, 
        xlab='Resistance to Impulse', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$Impulse, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$`Delay Gratification`,
        xlab = 'Degree of Ability to Delay Gratification', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$`Delay Gratification`, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$Diligence,
        xlab='Degree of Diligence', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$Diligence, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$Distress, 
        xlab='Level of Distress', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$Distress, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$PsyConnect, 
        xlab='Level of Psychological Connectedness', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$PsyConnect, mean, na.rm=T)

boxplot(project_df$`Financial Well-Being Score` ~ project_df$` Optimism`, 
        xlab='Level of Optimism', ylab= 'Financial Well-Being Score')
tapply(project_df$`Financial Well-Being Score`, project_df$` Optimism`, mean, na.rm=T)

# Quantitative Variable Analysis
plot(project_df$`Financial Well-Being Score` ~ project_df$`KH Score`, 
     xlab = 'KH Score', ylab = 'Financail Well-Being Score')
cor(project_df$`KH Score`, project_df$`Financial Well-Being Score`, use = "complete.obs")

plot(project_df$`Financial Well-Being Score` ~ project_df$`LM Score`, 
     xlab = 'LM Score', ylab = 'Financail Well-Being Score')
cor(project_df$`LM Score`, project_df$`Financial Well-Being Score`, use = "complete.obs")

plot(project_df$`Financial Well-Being Score` ~ project_df$`Financial Skills Score`, 
     xlab = 'Financial Skills Score', ylab = 'Financail Well-Being Score')
cor(project_df$`Financial Skills Score`, project_df$`Financial Well-Being Score`, use = "complete.obs")
