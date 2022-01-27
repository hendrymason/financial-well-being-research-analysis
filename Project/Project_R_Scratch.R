rm(list = ls())
View(fwb)
model_FS
resid_kh <- resid(model_kh)
resid_lm <- resid(model_lm)
plot(resid_kh ~ project_df$`KH Score`)

New_FG <- ifelse(project_df$`Financial Goals` == "Yes", 1, 0)
Model_FG <- lm(project_df$`Financial Well-Being Score` ~ New_FG, data = project_df)
Model_FG
model_KH <- lm(project_df$`Financial Well-Being Score` ~ project_df$`KH Score`, data = project_df) 
model_LM <- lm(project_df$`Financial Well-Being Score` ~ project_df$`LM Score`, data = project_df)
model_KH
summary(model_KH)
model_LM
summary(model_LM)
dual_scores <- lm(project_df$`Financial Well-Being Score` ~ project_df$`KH Score` + project_df$`LM Score`, data = project_df)
dual_scores
summary(dual_scores)
plot(project_df$`KH Score`, project_df$`Financial Well-Being Score`)
plot(project_df$`LM Score`, project_df$`Financial Well-Being Score`)
kh_resid <- resid(model_KH)
lm_resid <- resid(model_LM)
plot(kh_resid ~ project_df$`KH Score`)

# -- CODE FOR FINANCIAL SKILL SCORE (QUANT VAR)
# Ensure Quantitative quality of Financial Skill Score Data
project_df$`Financial Skills Score` <- as.numeric(project_df$`Financial Skills Score`)
model_FS <- lm(project_df$`Financial Well-Being Score` ~ project_df$`Financial Skills Score`, data = project_df)
plot(project_df$`Financial Well-Being Score` ~ project_df$`Financial Skills Score`, data = project_df)
model_FS
summary(model_FS)
resid_FS <- resid(model_FS)
plot(resid_FS ~ project_df$`Financial Skills Score`)

# -- CODE FOR SATISFACTION --
model_sf <- lm(project_df$`Financial Well-Being Score` ~ project_df$Satisfaction, data = project_df)
model_sf
