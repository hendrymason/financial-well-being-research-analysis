#Response Variable: FWB Score (Financial Well Being Score)


Project1Frame <- data.frame(NFWBS_PUF_2016_data$FWBscore,NFWBS_PUF_2016_data$FSscore,NFWBS_PUF_2016_data$BENEFITS_5,
                            NFWBS_PUF_2016_data$PPETHM,NFWBS_PUF_2016_data$REJECTED_1,NFWBS_PUF_2016_data$PPGENDER,NFWBS_PUF_2016_data$LMscore,
                            NFWBS_PUF_2016_data$COLLECT,NFWBS_PUF_2016_data$FINSOC2_3,NFWBS_PUF_2016_data$PPEDUC, NFWBS_PUF_2016_data$generation,
                            NFWBS_PUF_2016_data$KHscore, NFWBS_PUF_2016_data$FRAUD2, NFWBS_PUF_2016_data$CONNECT, NFWBS_PUF_2016_data$HEALTH)



names(Project1Frame) <- c("FWS", "Financial Skill Score","Work/Life Balance","Race","Credit Rejection","Gender", 
                          "LM Score","Debt Collector","Credit Discussion","Education Level", "Generation","KH Score",
                          "Fraud","Psychological Connectedness","Health")


#Response Variable:Financial Wellbeing Score

   Project1Frame$`FWS`[Project1Frame$`FWS` <= -4] <- NA  #taking out for no reponse 
   Project1Frame$`FWS`[Project1Frame$`FWS` <= -1] <- NA  #taking out codes refused answer
   
   FWS<- Project1Frame$FWS
   boxplot(FWS, main="Financial Wellbeing Score Boxplot", col="orange" ) #shows outliers 
   
   # 5-Num summary for Financial Wellbeing Score
   summary(FWS, na.rm= T) 
   sd(FWS, na.rm= T)
   
   # Financial Wellbeing Score Histogram
   hist(FWS, xlab ="Financial Wellbeing Score", main = "Financial Wellbeing Score Histogram", col = rainbow(6), breaks = sqrt(nrow(Project1Frame)))

   
   
   
   
   
   
# Explanatory Variable #1: Financial Skills Score (Quantitative)

   Project1Frame$`Financial Skill Score`[Project1Frame$`Financial Skill Score` <= -1] <- NA  #taking out codes refused answer
   FSS <- Project1Frame$`Financial Skill Score`
   
   # 5-Num summary for Financial Skill Score
   summary(FSS, na.rm=T)
   sd(FSS, na.rm= T)
   
   boxplot(FSS, main="Financial Skill Score Boxplot", col="orange" ) #shows outliers 
   
   # Financial Wellbeing Skill Histogram
   hist(FSS, xlab ="Financial Skill Score", main = "Financial Skill Score Histogram", col = rainbow(6))

#Relationship b/w FWS (Response Variable) & FSS (Explanatory Variable #1)
      plot(FSS,FWS, xlab = "Financial Skill Score",
           ylab = "Financial Wellbeing Score",
           main= "FWS vs FSS", pch=20, col="purple")
   
      cor(FWS, FSS, "complete.obs")
      
      
      
      
      
      
      
# Explanatory Variable #2: Race/Ethnicity (Categorical)
   
   #(create a percentage pie chart of the race makeup do the dataframe )
   Race <-table(Project1Frame$`Race`)
   lbls <- c("White", "Black", "Other", "Hispanic")
   pct <- round(Race/sum(Race)*100)
   lbls <-paste(lbls,pct)
   lbls <-paste(lbls, "%", sep = "")
   pie(Race,labels = lbls, col = rainbow(length(lbls)), main= "Race Pie Chart" )
   Race
   
   #Relationship b/w FWS (Response Variable) & Race (Explanatory Variable #2)
         boxplot(FWS~ Race, data= Project1Frame,
          xlab = "Race", ylab = "Financial Wellbeing Score", 
          main= "Race & Financial Wellbeing Score",col = rainbow(4),
          names = c("White", "Black", "Other", "Hispanic"))
         
   #5 number summary of with the various race catergory 
         tapply(Project1Frame$FWS, Project1Frame$Race, fivenum)
   
   
       
         
           
         
# Explanatory Variable #3: Generation (Categorical)
   
   #(create a percentage pie chart of the generation group makeup do the dataframe )
   Generation <-table(Project1Frame$Generation)
   lbs <- c("Pre-Boomer", "Boomer", "Gen X", "Millennial")
   pct <- round(Generation/sum(Generation)*100)
   lbs <-paste(lbs,pct)
   lbs <-paste(lbs, "%", sep = "")
   pie(Generation,labels = lbs, col = rainbow(length(lbs)), main= "Generation Pie Chart" )
   Generation
   
    #Relationship b/w FWS (Response Variable) & Generations (Explanatory Variable #3)
   
            boxplot(FWS~ Generation, data= Project1Frame,
              xlab = "Generation Groups", ylab = "Financial Wellbeing Score", 
              main= "Generation Groups & Financial Wellbeing Score",col = rainbow(4),
              names = c("Pre-Boomer", "Boomer", "Gen X", "Millennial"))
   
     #5 number summary of with the various race catergory 
             tapply(Project1Frame$FWS, Project1Frame$Generation, fivenum)
   
   
   
   
   
   
   