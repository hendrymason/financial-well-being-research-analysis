## INSTALL PACKAGES ##
library(colorRamps)
library(car)
library(moments)
library(leaps)


## CREATE PROJECT DATE FRAME ##
project_df <- data.frame(fwb$FWBscore, fwb$KHscore, fwb$LMscore, fwb$FSscore, 
                      fwb$SWB_1, fwb$SWB_2, fwb$MATERIALISM_1, 
                      fwb$MATERIALISM_2, fwb$MATERIALISM_3, fwb$SELFCONTROL_1, 
                      fwb$SELFCONTROL_2, fwb$SELFCONTROL_3, fwb$DISTRESS, 
                      fwb$CONNECT)


## RENAME VARIABLES ##
names(project_df) <-c("FWBscore" , 
                      "KHscore" , "LMscore" , "FSscore" , 
                      "Satisfaction" , "Optimism" , "LuxAdm" , 
                      "Things" , "Impress" , "Impulse" , "DelayGrat" , 
                      "Diligence" , "Distress" , "PsyConnect")

## ENSURE ALL QUANTITATIVE DATA IS READ AS NUMERIC ##
# FWBscore
project_df$FWBscore<- as.numeric(project_df$FWBscore)
# KHscore
project_df$KHscore<- as.numeric(project_df$KHscore)
# LMscore
project_df$LMscore <- as.numeric(project_df$LMscore)
# FSscore
project_df$FSscore <- as.numeric(project_df$FSscore)
# PsyConnect
project_df$PsyConnect <- as.numeric(project_df$PsyConnect)


## CLEAN DATA BY REPLACING NO RESPONSES & REFUSALS WITH NA ##
# FWBscore
project_df$FWBscore[project_df$FWBscore == "Response not written to database"] <- NA
project_df$FWBscore[project_df$FWBscore == "Refused"] <- NA
# FSscore
project_df$FSscore[project_df$FSscore == "Refused"] <- NA
# PsyConnect
project_df$PsyConnect[project_df$PsyConnect == "Refused"] <- NA
# Satisfaction
project_df$Satisfaction[project_df$Satisfaction == "Response not written to database"] <- NA
project_df$Satisfaction[project_df$Satisfaction == "Refused"] <- NA
# Optimism
project_df$Optimism[project_df$Optimism == "Response not written to database"] <- NA
project_df$Optimism[project_df$Optimism == "Refused"] <- NA
# Lux-Admiration
project_df$LuxAdm[project_df$LuxAdm == "Refused"] <- NA
# Things
project_df$Things[project_df$Things == "Refused"] <- NA
# Impress
project_df$Impress[project_df$Impress == "Refused"] <- NA
# Impulse
project_df$Impulse[project_df$Impulse == "Refused"] <- NA
# Delay Gratification
project_df$DelayGrat[project_df$DelayGrat == "Refused"] <- NA
# Diligence
project_df$Diligence[project_df$Diligence == "Refused"] <- NA
# Distress
project_df$Distress[project_df$Distress == "Refused"] <- NA
## RESPONSE VARIABLE (Q): FWBscore ##
  ## GENERATE INDIVIDUAL BOXPLOT ##
  FWBscore <- project_df$FWBscore
  boxplot(FWBscore, main="Financial Well-Being Score Boxplot", col = "green")
  ## GENERATE COLORS FOR USE THROUGHOUT ##
  colors<- c("red", "blue")
  colors2 <- c("purple", "green")
  colors3 <- c("blue", "yellow")
  colors4 <- c("red", "orange")
  ## GENERATE HISTOGRAM ##
  hist(FWBscore, xlab="Financial Well-Being Score", 
       main="Financial Well-Being Score Histogram", 
       col=colors,
       breaks = sqrt(nrow(project_df)))
  ## GENERATE NUMERICAL DATA ##
  summary(FWBscore, na.rm=T) # 5 NUMBER SUMMARY
  sd(FWBscore, na.rm=T) # STANDARD DEVIATION
  
## EXPLANATORY VARIABLE (Q): KHscore ##
  KHscore <- project_df$KHscore
  ## GENERATE HISTORGAM ##
  hist(KHscore, xlab = "KH Score",
       main= "KH Score Histogram",
       col = colors2)
  ## GENERATE SCATTERPLOT BETWEEN KHSCORE & FWBSCORE ##
  plot(KHscore, FWBscore,
       cex = 0.5,
       xlab="KH score",
       xlim = c(-2.1, 1.5),
       ylab="Financial Well-Being Score",
       ylim = c(0,85),
       pch=0, col="orange")
  ## ANALYZE NUMERICAL DATA ##
  cor(FWBscore, KHscore, "complete.obs")
  summary(KHscore, na.rm=T)
  sd(KHscore, na.rm=T)
  
## EXPLANATORY VARIABLE (Q): LMscore ##
  LMscore <- project_df$LMscore
  ## GENERATE HISTOGRAM ##
  hist(LMscore, xlab="LM Score",
       main="LM Score Histogram",
       col=colors3)
  kurtosis(LMscore)
  ## GENERATE SCATTERPLOT BETWEEN LMSCORE & FWBSCORE ##
  plot(LMscore, FWBscore,
       cex = 0.5,
       xlab="LM Score",
       ylab="Financial Well-Being Score",
       pch=1, col="red")
  ## ANALYZE NUMERICAL DATA ##
  cor(LMscore, FWBscore, "complete.obs")
  summary(LMscore, na.rm=T)
  sd(LMscore, na.rm=T)

## EXPLANATORY VARIABLE (Q): FSscore ##
  FSscore <- project_df$FSscore
  ## GENERATE HISTOGRAM ##
  hist(FSscore, xlab="Financial Skills Score",
       main="Financial Skill Score Histogram",
       col=colors4)
  ## GENERATE SCATTERPLOT BETWEEN FSSCORE & FWBSCORE ##
  plot(FSscore, FWBscore,
       cex = 0.5,
       xlab="Financial Skills Score",
       ylab="Financial Well-Being Score",
       pch=24, col="purple")
  ## ANALYZE NUMERICAL DATA ##
  cor(FSscore, FWBscore, "complete.obs")
  summary(FSscore, na.rm=T)
  sd(FSscore, na.rm=T)

  ## EXPLANATORY VARIABLE (Q): PsyConnect ##
  psyconnect <- project_df$PsyConnect
  ## GENERATE HISTOGRAM ##
  hist(psyconnect, xlab="Degree of Psychological Connectedness",
       main="Psychological Connectedness Histogram",
       col=colors)
  ## GENERATE SCATTERPLOT BETWEEN PSYCONNECT & FWBSCORE ##
  plot(psyconnect, FWBscore,
       cex = 0.5,
       xlab="Degree of Psychological Connectedness",
       ylab="Financial Well-Being Score",
       pch=25, col="black")
  ## ANALYZE NUMERICAL DATA ##
  cor(psyconnect, FWBscore, "complete.obs")
  summary(psyconnect, na.rm=T)
  sd(psyconnect, na.rm=T)
 
## EXPLANATORY VARIABLE (C): Satisfaction ##
  ## CREATE BARPLOT FOR SATISFACTION ##
  project_df$Satisfaction <- droplevels(project_df$Satisfaction)
  Satisf <- project_df$Satisfaction
  Satisf_tble <- table(Satisf)
  barplot(Satisf_tble, ylab = "Frequency",
          xlab= "Level of Life Satisfaction",
          names =c("1", "2", "3", "4", "5", "6", "7"),
          col = colors2)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR SATISFACTION VS. FWBSCORE ##
  boxplot(FWBscore ~ Satisf, 
          xlab = 'Life Satisfaction Level (Low to High)',
          ylab= 'Financial Well-Being Score',
          main = "Life Satisfaction level Boxplot",
          col = colors2,
          names =c("1", "2", "3", "4", "5", "6", "7"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, Satisf, fivenum)
  
## EXPLANATORY VARIABLE (C): Optimism ##
  project_df$Optimism <- droplevels(project_df$Optimism)
  Opt <- project_df$Optimism
  ## CREATE BAR PLOT FOR OPTIMISM ##
  Opt_tble <- table(Opt)
  barplot(Opt_tble, ylab = "Frequency",
          xlab= "Level of Optimism",
          names =c("1", "2", "3", "4", "5", "6", "7"),
          col = colors)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR OPTIMISM VS. FWBSCORE ##
  boxplot(FWBscore ~ Opt, 
          xlab = 'Life Optimism Level (Low to High)',
          ylab= 'Financial Well-Being Score',
          main = "Life Optimism level Boxplot",
          col = colors,
          names =c("1", "2", "3", "4", "5", "6", "7"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, Opt, fivenum)
  
## EXPLANATORY VARIABLE (C): Lux-Admiration ##
  project_df$LuxAdm <- droplevels(project_df$LuxAdm)
  lux <- project_df$LuxAdm
  ## CREATE BAR PLOT FOR LUX-ADMIRATION ##
  lux <- project_df$LuxAdm
  lux_tble <- table(lux)
  barplot(lux_tble, ylab = "Frequency",
          xlab= "Level of Value Towards Luxury",
          names = c("1", "2", "3", "4", "5"),
          col = colors4)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR LUX-ADMIRATION VS. FWBSCORE ##
  boxplot(FWBscore ~ lux, 
          xlab = 'Level of Admiriation to Luxury (Low to High)',
          ylab= 'Financial Well-Being Score',
          main = "Value of Luxury vs. FWBscore Boxplot",
          col = colors4,
          names =c("1", "2", "3", "4", "5"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, lux, fivenum)

## EXPLANATORY VARIABLE (C): Things ##
  project_df$Things <- droplevels(project_df$Things)
  things <- project_df$Things
  ## CREATE BAR PLOT FOR THINGS ##
  things_tble <- table(things)
  barplot(things_tble, ylab = "Frequency",
          xlab= "Value Towards Material Things (Low to High)",
          names = c("1", "2", "3", "4", "5"),
          col = colors)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR THINGS VS. FWBSCORE ##
  boxplot(FWBscore ~ things, 
          xlab = 'Value Towards Material Things (Low to High)',
          ylab= 'Financial Well-Being Score',
          main = "Value of Material Possessions vs. FWBscore Boxplot",
          col = colors,
          names =c("1", "2", "3", "4", "5"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, things, fivenum)
  
## EXPLANATORY VARIABLE (C): Impress ##
  project_df$Impress <- droplevels(project_df$Impress)
  impress <- project_df$Impress
  ## CREATE BAR PLOT FOR IMPRESS ##
  impress_tble <- table(impress)
  barplot(impress_tble, ylab = "Frequency",
          xlab= "Value Towards Impressing Others (Low to High)",
          names = c("1", "2", "3", "4", "5"),
          col = colors2)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR IMPRESS VS. FWBSCORE ##
  boxplot(FWBscore ~ impress, 
          xlab = 'Value Towards Material Things (Low to High)',
          ylab= 'Financial Well-Being Score',
          main = "Value of Material Possessions vs. FWBscore Boxplot",
          col = colors2,
          names =c("1", "2", "3", "4", "5"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, impress, fivenum)
  
## EXPLANATORY VARIABLE (C): Impulse ##
  project_df$Impulse <- droplevels(project_df$Impulse)
  impulse <- project_df$Impulse
  ## CREATE BAR PLOT FOR IMPULSE ##
  impulse_tble <- table(impulse)
  barplot(impulse_tble, ylab = "Frequency",
          xlab= "Level of Impulsive Decision-Making (Low to High)",
          names = c("1", "2", "3", "4"),
          col = colors3)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR IMPULSE VS. FWBSCORE ##
  boxplot(FWBscore ~ impulse, 
          xlab = "Level of Impulsive Decision-Making (Low to High)",
          ylab= 'Financial Well-Being Score',
          main = "Degree of Impulsive Decision-Making vs. FWBscore Boxplot",
          col = colors3,
          names =c("1", "2", "3", "4"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, impulse, fivenum)
  
## EXPLANATORY VARIABLE (C): Delay Gratification ##
  project_df$DelayGrat <- droplevels(project_df$DelayGrat)
  delay_grat <- project_df$DelayGrat
  ## CREATE BAR PLOT FOR DELAY GRATIFICATION ##
  delay_grat_tble <- table(delay_grat)
  barplot(delay_grat_tble, ylab = "Frequency",
          xlab= "Ability to Delay Gratification (Low to High)",
          names = c("1", "2", "3", "4"),
          col = colors4)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR DELAY GRATIFICATION VS. FWBSCORE ##
  boxplot(FWBscore ~ delay_grat, 
          xlab = "Ability to Delay Gratification (Low to High)",
          ylab= 'Financial Well-Being Score',
          main = "Ability to Delay Gratification (Low to High) vs. FWBscore Boxplot",
          col = colors4,
          names =c("1", "2", "3", "4"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, delay_grat, fivenum)

## EXPLANATORY VARIABLE (C): Diligence ##
  project_df$Diligence <- droplevels(project_df$Diligence)
  diligence <- project_df$Diligence
  ## CREATE BAR PLOT FOR DILIGENCE ##
  diligence_tble <- table(diligence)
  barplot(diligence_tble, ylab = "Frequency",
          xlab= "Ability to Work Diligently for Long-Term Goals (Low to High)",
          names = c("1", "2", "3", "4"),
          col = colors)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR DILIGENCE VS. FWBSCORE ##
  boxplot(FWBscore ~ diligence, 
          xlab = "Ability to Work Diligently for Long-Term Goals (Low to High)",
          ylab= 'Financial Well-Being Score',
          main = "Ability to Work Diligently vs. FWBscore Boxplot",
          col = colors,
          names =c("1", "2", "3", "4"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, diligence, fivenum)
## EXPLANATORY VARIABLE (C): Distress ##
  project_df$Distress <- droplevels(project_df$Distress)
  distress <- project_df$Distress
  ## CREATE BAR PLOT FOR DISTRESS ##
  distress_tble <- table(distress)
  barplot(distress_tble, ylab = "Frequency",
          xlab= "Degree of Stress Experienced (Low to High)",
          names = c("1", "2", "3", "4", "5"),
          col = colors2)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR DISTRESS VS. FWBSCORE ##
  boxplot(FWBscore ~ distress, 
          xlab = "Degree of Stress Experienced (Low to High)",
          ylab= 'Financial Well-Being Score',
          main = "Degree of Stress vs. FWBscore Boxplot",
          col = colors2,
          names =c("1", "2", "3", "4", "5"))
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, distress, fivenum)
  
## TEST / TRAINING DATA SPLIT ##
  # TESTING SET #
  .3 * nrow(project_df)
  set.seed(1)
  test.cases <- sample(1:6394, 1918)
  sort(test.cases)
  test.set <- project_df[test.cases,]
  # TRAINING SET #
  training.cases <- setdiff(1:6394, test.cases)
  training.set <- project_df[training.cases,]
  
  # COMPARE MEANS OF TEST / TRAINING SETS #
  test_mean <- mean(test.set$FWBscore)
  training_mean <- mean(training.set$FWBscore)
  abs(test_mean - training_mean)
  # ^COMPARISON REVEALS SIGNIFICANTLY CLOSE MEANS & SETS ARE GOOD #
  
## FUNNEL DOWN TRAINING DATA TO BECOME EFFECTIVELY PARSIMONIOUS ##
  # GENERATE SUBSET PLOT #
  plot(regsubsets(FWBscore ~ KHscore + LMscore + FSscore +
                    PsyConnect + Satisfaction + LuxAdm + Impress +
                    Impulse + DelayGrat + Diligence + Distress,
                  data = training.set),
       scale = "adjr2") #INDICATES POOR adjr^2 with KHscore, FSscore-> EXCLUDE FROM MODEL TO MAKE MORE PARSIMONIOUS #
  # CREATE TRAINING MODEL #
  training_mod1 <- lm(FWBscore ~ KHscore + LMscore + FSscore +
                        PsyConnect + Satisfaction + LuxAdm + Impress +
                        Impulse + DelayGrat + Diligence + Distress,
                      data = training.set)
  # INTERPRET TRAINING MODEL #
  vif(training_mod1) # NO SIGNIFICANT COLLINEARITY
  summary(training_mod1) # HIGH P-VALUE OF LUX / IMPRESS / IMPULSE -> REMOVE FROM VARIABLE SET
  plot(regsubsets(FWBscore ~ LMscore + PsyConnect + Satisfaction  + 
                    DelayGrat + Diligence + Distress, 
                  data = training.set),
       scale = "adjr2")
  # CREATE NEW TRAINING MODEL #
  training_mod2 <- lm(FWBscore ~ LMscore + PsyConnect + Satisfaction  + 
                        DelayGrat + Diligence + Distress, 
                      data = training.set)
  # INTERPRET TRAINING MODEL 2 #
  vif(training_mod2) # NO COLLINEARITY
  summary(training_mod2) # ALL SIGNIFICANT P-VALUES (IGNORING DUMMY VARIABLE)
  # RESTRUCTURE COLUMNS ACCORDING TO NEW VARIABLES#
  keeps <- c("FWBscore", "LMscore", "PsyConnect", "Satisfaction", "DelayGrat", 
             "Diligence", "Distress")
  training.set <- training.set[keeps]
  
  # COMPARE MEAN ABSOLUTE ERROR #
  # TRAINING SET MAE #
  train_mae <- mean(abs(residuals(training_mod2)))
  train_mae
  # TEST SET MAE #
  FWBscore.hat <- predict(training_mod2, test.set) # PREDICTED MAE FROM THE TEST SET
  test_mae <- mean(abs(FWBscore.hat - test.set$FWBscore), na.rm=T)
  test_mae
  mae_diff <- abs(test_mae - train_mae)
  mae_diff
  # ^MAE IS ONLY 0.08899 OFF #
  # COMPARE R-SQUARED VALUES #
  train_rsq <- summary(training_mod2)$r.squared # COLLECT R-SQUARED
  train_rsq
  test_rsq <- cor(test.set$FWBscore, FWBscore.hat, use = "complete.obs")^2
  test_rsq
  rsquared_diff = abs(test_rsq - train_rsq)
  rsquared_diff
  #R^2 IS ONLY 0.02154 OFF #
## EXAMPLE PREDICTION & PREDICTION INTERVAL ##
  # CREATE TEST MODEL #
  test_model <- lm(FWBscore ~ LMscore + PsyConnect + Satisfaction  + 
                     DelayGrat + Diligence + Distress, 
                   data = test.set)
  # RUN PREDICTION & COMPARE#
  actual_FWB <- 25
  predict.lm(test_model, list(LMscore=3, PsyConnect=57, Satisfaction="5", 
                              DelayGrat="Not at all", Diligence="Not very well", 
                              Distress="Agree"), 
             interval = "prediction")
  summary(test_model)
  fit_FWB <- 33.49863
  resid <- actual_FWB - fit_FWB
  resid
  # THE PREDICTED WAS NOT THE ACTUAL BUT WAS STILL WITHIN THE PREDICTION INTERVAL #
  
## CHECK REGRESSION ASSUMPTIONS ##
  # REFRAME DATA #
  final_model <- lm(FWBscore ~ LMscore + PsyConnect + Satisfaction  + 
                      DelayGrat + Diligence + Distress, 
                    data = training.set)
## EVALUATE FINAL VARIABLES OF MODEL ##
  # FWBscore #
  FWBscore <- training.set$FWBscore
  boxplot(FWBscore, main="Financial Well-Being Score Boxplot", col = "green")
  ## GENERATE HISTOGRAM ##
  hist(FWBscore, xlab="Financial Well-Being Score", 
       main="Financial Well-Being Score Histogram", 
       col=colors,
       breaks = sqrt(nrow(training.set)))
  skewness(FWBscore)
  kurtosis(FWBscore)
  ## GENERATE NUMERICAL DATA ##
  summary(FWBscore, na.rm=T) # 5 NUMBER SUMMARY
  sd(FWBscore, na.rm=T) # STANDARD DEVIATION
  
  # LMscore #
  ## EXPLANATORY VARIABLE (Q): LMscore ##
  LMscore <- training.set$LMscore
  ## GENERATE HISTOGRAM ##
  hist(LMscore, xlab="LM Score",
       main="LM Score Histogram",
       col=colors3)
  skewness(LMscore)
  kurtosis(LMscore)
  ## GENERATE SCATTERPLOT BETWEEN LMSCORE & FWBSCORE ##
  plot(LMscore, FWBscore,
       cex = 0.5,
       xlab="LM Score",
       ylab="Financial Well-Being Score",
       pch=1, col="red")
  model_lm <- lm(FWBscore ~ LMscore, data = training.set)
  plot(residuals(model_lm))
  plot(model_lm)
  ## ANALYZE NUMERICAL DATA ##
  cor(LMscore, FWBscore, "complete.obs")
  summary(LMscore, na.rm=T)
  sd(LMscore, na.rm=T)
  
  # PsyConnect #
  psyconnect <- training.set$PsyConnect
  ## GENERATE HISTOGRAM ##
  hist(psyconnect, xlab="Degree of Psychological Connectedness",
       main="Psychological Connectedness Histogram",
       col=colors)
  skewness(psyconnect)
  kurtosis(psyconnect)
  ## GENERATE SCATTERPLOT BETWEEN PSYCONNECT & FWBSCORE ##
  plot(psyconnect, FWBscore,
       cex = 0.5,
       xlab="Degree of Psychological Connectedness",
       ylab="Financial Well-Being Score",
       pch=25, col="black")
  model_psy <- lm(FWBscore ~ PsyConnect, data = training.set)
  plot(residuals(model_psy))
  plot(model_psy)
  ## ANALYZE NUMERICAL DATA ##
  cor(psyconnect, FWBscore, "complete.obs")
  summary(psyconnect, na.rm=T)
  sd(psyconnect, na.rm=T)
  
  # Satisfaction #
  ## CREATE BARPLOT FOR SATISFACTION ##
  training.set$Satisfaction <- droplevels(training.set$Satisfaction)
  Satisf <- training.set$Satisfaction
  Satisf_tble <- table(Satisf)
  barplot(Satisf_tble, ylab = "Frequency",
          xlab= "Level of Life Satisfaction",
          names =c("1", "2", "3", "4", "5", "6", "7"),
          col = colors2)
  skewness(Satisf_tble)
  kurtosis(Satisf_tble)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR SATISFACTION VS. FWBSCORE ##
  boxplot(FWBscore ~ Satisf, 
          xlab = 'Life Satisfaction Level (Low to High)',
          ylab= 'Financial Well-Being Score',
          main = "Life Satisfaction level Boxplot",
          col = colors2,
          names =c("1", "2", "3", "4", "5", "6", "7"))
  model_satisf <- lm(FWBscore ~ Satisfaction, data = training.set)
  plot(residuals(model_satisf))
  plot(model_satisf)
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, Satisf, fivenum)
  
  # Delay Gratification #
  training.set$DelayGrat <- droplevels(training.set$DelayGrat)
  delay_grat <- training.set$DelayGrat
  skewness(delay_grat_tble)
  kurtosis(delay_grat_tble)
  ## CREATE BAR PLOT FOR DELAY GRATIFICATION ##
  delay_grat_tble <- table(delay_grat)
  barplot(delay_grat_tble, ylab = "Frequency",
          xlab= "Ability to Delay Gratification (Low to High)",
          names = c("1", "2", "3", "4"),
          col = colors4)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR DELAY GRATIFICATION VS. FWBSCORE ##
  boxplot(FWBscore ~ delay_grat, 
          xlab = "Ability to Delay Gratification (Low to High)",
          ylab= 'Financial Well-Being Score',
          main = "Ability to Delay Gratification (Low to High) vs. FWBscore Boxplot",
          col = colors4,
          names =c("1", "2", "3", "4"))
  model_delay <- lm(FWBscore ~ DelayGrat, data = training.set)
  plot(residuals(model_delay))
  plot(model_delay)
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, delay_grat, fivenum)
  
  # Diligence #
  training.set$Diligence <- droplevels(training.set$Diligence)
  diligence <- training.set$Diligence
  ## CREATE BAR PLOT FOR DILIGENCE ##
  diligence_tble <- table(diligence)
  barplot(diligence_tble, ylab = "Frequency",
          xlab= "Ability to Work Diligently for Long-Term Goals (Low to High)",
          names = c("1", "2", "3", "4"),
          col = colors)
  skewness(diligence_tble)
  kurtosis(diligence_tble)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR DILIGENCE VS. FWBSCORE ##
  boxplot(FWBscore ~ diligence, 
          xlab = "Ability to Work Diligently for Long-Term Goals (Low to High)",
          ylab= 'Financial Well-Being Score',
          main = "Ability to Work Diligently vs. FWBscore Boxplot",
          col = colors,
          names =c("1", "2", "3", "4"))
  model_diligence <- lm(FWBscore ~ Diligence, data = training.set)
  plot(residuals(model_diligence))
  plot(model_diligence)
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, diligence, fivenum)
  
  # Distress #
  training.set$Distress <- droplevels(training.set$Distress)
  distress <- training.set$Distress
  ## CREATE BAR PLOT FOR DISTRESS ##
  distress_tble <- table(distress)
  barplot(distress_tble, ylab = "Frequency",
          xlab= "Degree of Stress Experienced (Low to High)",
          names = c("1", "2", "3", "4", "5"),
          col = colors2)
  skewness(distress_tble)
  kurtosis(distress_tble)
  ## CREATE A SIDE BY SIDE BOX PLOT FOR DISTRESS VS. FWBSCORE ##
  boxplot(FWBscore ~ distress, 
          xlab = "Degree of Stress Experienced (Low to High)",
          ylab= 'Financial Well-Being Score',
          main = "Degree of Stress vs. FWBscore Boxplot",
          col = colors2,
          names =c("1", "2", "3", "4", "5"))
  model_distress <- lm(FWBscore ~ Distress, data = training.set)
  plot(residuals(model_distress))
  plot(model_distress)
  ## GENERATE NUMERICAL DATA ##
  tapply(FWBscore, distress, fivenum)

## PERFORM REGRESSION ##
  plot(final_model)
  # ^PLOTS INCLUDE RESIDUALS VS FITTED / QQNORM / SCALE-LOCATION / LEVERAGE
  # RESIDUALS VS FITTED & QQNORM REVEALS OUTLIER AT 3221 / 5200
  # SCALE-LOCATION REVEALS OUTLIERS AT 3221 / 5200
  # RESIDUALS VS. LEVERAGE REVEALS OUTLIERS AT 3221 
## REMOVE OUTLIERS & HIGH LEVERAGE POINTS
  training.set <- training.set[-c(3221, 5200),] # apparently R does not want to delete these points out of stubbornness or technical malfunction
  # RECREATE MODEL, NOW WITHOUT OUTLIERS
  final_model <- lm(FWBscore ~ LMscore + PsyConnect + Satisfaction  + 
                      DelayGrat + Diligence + Distress, 
                    data = training.set)
  # RECREATE NEW PLOT WITH NO OUTLIERS #
  plot(final_model)
  # DISPLAYS SOME NEW FOUND OUTLIERS BUT NOT SIGNIFICANT ENOUGH TO REMOVE #
  # RESIDUALS VS. FITTED REVEALS NO PATTERNS = EQUAL VARIANCE SATISFIED #
  # NORMALITY SATISFIED DUE TO STRAIGHT QQPLOT = NO TRANSFORMATION NEEDED #
  # SCALE-LOCATION IS PREDOMINANTLY RANDOMLY SPREAD = NO TRANSFORMATION NEEDED #
  # RESIDUALS VS LEVERAGE CONTAINS POINTS ONLY WITH NO/INSIGNIFICANT LEVERAGE = NO TRANSFORMATION NEEDED #

## COLLECT INFERENCE DATA ON NEW MODEL - COEFFICIENTS, R-SQ, STANDARD ERRORS & CONF INTVL ##
  final_model
  summary(final_model)
  # SIGNIFICANT ENOUGH P-VALUE TO REJECT NULL -> CONTINUE INFERENCE #
  # CREATE CONFIDENCE INTERVAL #
  confint(final_model)
  # EVERY CONFINT OF DISTRESS IS NEGATIVE -> CANNOT PREDICT DISTRESS CONFIDENTLY #
  # ALSO POOR PREDICTOR OF NOT VERY WELL DILIGENCE DUE TO (-) NEGATIVE 2.5% BEGINNING INTERVAL #
