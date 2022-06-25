# diplom ae rodionova 

install.packages("dplyr")
install.packages("survminer")
install.packages("survival")
install.packages("stargazer")
library(dplyr)
library(survminer)
library(survival)
library(stargazer)

### Doing Cox proportional hazards regression models
data<-read.csv("C:/Users/ËÈÇÀ/Documents/diploma_full.csv", sep=';')
attach(data)
data$region_str <- as.factor(data$region_str)
# Model 1: 2000-2020 divided into 3 periods by date of appointment (P3 (2000-2005), P3 (2005-2012) and P3 (2012-2020))
data$periods_3_app <- as.factor(data$periods_3_app)
# Model 2: 2000-2020 divided into 2 periods by date of resignation (P2 (2000-2012) and P2 (2012-2020))
data$periods_2_dis <- as.factor(data$periods_2_dis)
# This data is also for model 2, but it assigns nothing in the period column if the governor is still in office (i.e. strict 2012-2020, not just after 2012, as in the 1st case)
data$periods_2_dis_corrected <- as.factor(data$periods_2_dis_corrected)
data$population <- as.numeric(data$population)
data$GRPgrowth <- as.numeric(data$GRPgrowth_)
data$GRPgrowth_ <- scale(data$GRPgrowth_)
data$population <- scale(data$population)

data_00_12<-subset(data, periods_2_dis=="1")
attach(data_00_12)

data_12_20<-subset(data, periods_2_dis=="2")
attach(data_12_20)

# Model 1 (3 periods)
cox_3p <- coxph(Surv(total_days, status)~region_str+periods_3_app+ethnic+local_ties+pres_vote+pres_turnout+GRPgrowth_+population+age_app, data=data)
cox_3p
# Model 2 (2 periods)
cox_2p <- coxph(Surv(total_days, status)~region_str+periods_2_dis+ethnic+local_ties+pres_vote+pres_turnout+GRPgrowth_+population+age_app, data=data)
cox_2p
# Model 3 (without any periods)
cox_np <- coxph(Surv(total_days, status)~region_str+ethnic+local_ties+pres_vote+pres_turnout+GRPgrowth_+population+age_app, data=data)
cox_np
# Model 4 (without the type of economic structure of the region as a separate parameter)
cox_nst <- coxph(Surv(total_days, status)~ethnic+local_ties+pres_vote+pres_turnout+GRPgrowth_+population+age_app, data=data)
cox_nst
# Model 5 (within 2000-2012)
cox_00_12 <- coxph(Surv(total_days, status)~region_str+ethnic+local_ties+pres_vote+pres_turnout+GRPgrowth_+population+age_app, data=data_00_12)
cox_00_12
# Model 6 (within 2012-2020)
cox_12_20 <- coxph(Surv(total_days, status)~region_str+ethnic+local_ties+pres_vote+pres_turnout+GRPgrowth_+population+age_app, data=data_12_20)
cox_12_20

# Plot them all in a single table 
stargazer(cox_3p, cox_2p, cox_np, cox_nst, cox_00_12, cox_12_20, 
          type="html",
          model.numbers = FALSE,
          title="Cox Regression Results",
          dep.var.labels=c("Governor's Survival (days)"),
          column.labels=c("Model 1 <br>(3 periods)","Model 2 <br>(2 periods)","Model 3 <br>(without periods)","Model 4 <br>(without ec srt)","Model 5 <br>(2000-2012)","Model 6 <br>(2012-2020)"),
          model.names=FALSE,
          covariate.labels=c("Dominant", "Plural", "P3 (2005-2012)", "P3 (2012-2020)", "P2 (2012-2020)", "Ethnic", "Local ties", "Vote (pres)", "Turnout (pres)", "GRP growth", "Population size", "Governor's age"),
          omit.stat = c("ser", "f"),
          column.sep.width = "30pt",
          out="di12_check.html")

## Testing proportionality assumption (the one without periods)
check <- cox.zph(cox_np)
print(check)
plot(check, resid=F)
ggcoxzph(check)

## Compare the values of the Akaike information criterion (AIC) for two models: the model is better, the AIC for which is less
AIC(cox_3p, cox_2p, cox_np, cox_00_12, cox_12_20)

### Creating Kaplan-Meier Survival curve - probability of survival changes over time under the influence of the economic structure of the region 
## Can see how the probability changes by days or by months 
sfit <- survfit(Surv(total_months, status)~region_str, data=data)
plot(sfit)
ggsurvplot(sfit, data=data)

sfit <- survfit(Surv(total_days, status)~region_str, data=data)
plot(sfit)
ggsurvplot(sfit, data=data)

#Plot it
ggsurvplot(sfit, data=data, conf.int=TRUE, pval=TRUE, 
           legend.labs=c("Neglected", "Dominant", "Plural"), 
           legend.title="Economic structure of the region",  
           palette=c("cadetblue3", "darkseagreen3", "coral1"), 
           title="Kaplan-Meier Curve for Governor's Survival")
