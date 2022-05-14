#thesis
install.packages('stargazer')
install.packages('lubrdidate')
install.packages('tidyverse')
install.packages('readxl')
library(readxl)
library(stargazer)
library(lubridate)
library(tidyverse)


logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  out <- c(round(R.l, 3), round(R.cs, 3), round(R.n, 3))
  names(out) <- c("R2 (Hosmer & Lemeshow)", "R2 (Cox & Snell)", "R2 (Nagelkerke)")
  out
}


#subset = subset %>% dplyr::select(-text)
#write_xlsx(subset, 'subset.xlsx')

#subset = subset %>% filter(year >= 2009)

#RCC <- read_excel("thesis_stuff/RCC.xlsx")

#RCC$Works2state = ifelse(RCC$t.lawenf == 1 | RCC$t.military == 1, 1, 0)
#1, 0)
#RCC$Jurisdiction = ifelse(RCC$t.courts == 1 | RCC$t.govtDispute == 1 | RCC$t.gosduma == 1 | RCC$t.govt == 1 | RCC$t.president == 1,
#RCC$SpecialStatus = ifelse(RCC$t.chernobyl == 1 | RCC$t.veterans == 1 | RCC$t.repression == 1 | RCC$t.farNorth == 1, 1,0)
#RCC$Social_Rights = ifelse(RCC$t.housing == 1 | RCC$t.welfare == 1 | RCC$t.pensions == 1 | RCC$t.labour == 1, 1,0)

#RCC = RCC %>% dplyr::select(-c('t.lawenf', 't.military', 't.courts', 't.govtDispute', 't.gosduma', 't.govt', 't.president',
#'t.chernobyl', 't.veterans', 't.repression', 't.farNorth', 't.housing', 't.welfare', 't.pensions', 't.labour'))



RCC_FINAL_1992 <- read_excel("RCC_FINAL_1992.xlsx")
RCC = RCC_FINAL_1992

#descrrr.stat = RCC %>% dplyr::select(-c(n.x, year))







#descriptive stat

#summary(descrrr.stat)
#stargazer(as.data.frame(descrrr.stat), type="html",
#          title="Table 2. Variables for the RCC", 
#          digits=2, flip = F, summary.logical = T,
#          summary.stat=c('n', "mean", "sd", "min", "max","median"),
#          out="desc_stat_big_hometask.htm",
#          covariate.labels = c("Constitutional",
#                               "Level Federal",
#                               "Level Regional",
#                               "Level International",
#                               "Type Criminal",
#                               'Type Civil',
#                               "Type Elections",
#                               'Type Social Welfare',
#                               'Type Military',
#                               'Type Property',
#                               'Type Administrative',
#                               'Type Law Enforcement',
#                               'Type Governing Structure',
                               # 'Type Economics',
                               # 'Type Salient',
                               # 'Petinioner Citizens',
                               # 'Petitioner Federal',
                               # 'Pet Regional',
                               # 'Pet Local Gov',
                               # 'Pet NGO',
                               # 'Pet Firms',
                               # 'Pet Government total',
                               # 'Control number of pet',
                               # 'Control caseload',
                               # 'Control proportion of justices',
                               # 'Number of justices',
                               # 'Number of dis. opinions'))



# a = c(sum(RCC$level.fed), sum(RCC$level.reg), sum(RCC$level.intl))
# b = c(sum(RCC$t.Social_Welfare), sum(RCC$t.criminal), sum(RCC$t.property), sum(RCC$t.lawenf), sum(RCC$t.salient))
# c = RCC %>% filter(n_dissent != 0)
library(ggplot2)

# ggplot(data = RCC) + 
#   geom_bar(aes(x = as.factor(year), fill = outcome), stat = 'count') +
#   theme_minimal() + 
#   labs(title = 'Figure 1',
#        subtitle = "Number of Judgements Issued by the RCC \n by the Outcome",
#        y = "Number",
#        x = "Year", caption = "Source: Yulia Khalikova, ‘Constitutional Review and Judicial Behavior under
#     Authoritarianism: Evidence from Russia’ (Universität Bremen, n.d.), \n author's calculations")  + scale_fill_grey()
# 


# sum(RCC$t.lawenf)
# RCC_fact_year = RCC

# RCC_fact_year$year = as.factor(RCC_fact_year$year)
# t2 = RCC_fact_year %>% group_by(year) %>% summarise(count_of_cases = n())
# 
# t1 = RCC_fact_year %>% group_by(year) %>% summarise(mean_unconstitut = mean(dv_dummy))
# t3 = t1 %>% left_join(t2, by = 'year')
# t3$standard = t3$mean_unconstitut/t3$count_of_cases
# 
# t3$year_num = as.character(t3$year)
# t3$year_num = as.numeric(t3$year_num)
# t3$befoe_2009 = ifelse(t3$year_num <= 2008, 1, 0)
# t3$time_lines = ifelse(t3$year_num <= 2008, 1,
#                        ifelse(t3$year_num > 2008 & t3$year_num < 2014, 2, 3))
# 
# 
# t3 %>% group_by(time_lines) %>% summarise(me = mean(count_of_cases)) 
# t3 %>% group_by(time_lines) %>% summarise(me = mean(mean_unconstitut) * 100) 
# 
# 
# t1$year = as.factor(t1$year)
# stargazer(as.data.frame(t1), type="html", summary = FALSE,
#           out="YEAR_BY_CONSTITUTIONAL.htm") ### 1 for UNCONSTITUTIONAL
# 
# install.packages('eatATA')
# library(eatATA)

RCC$type = ifelse(RCC$t.salient == 1, "Salient",
                  ifelse(RCC$t.civil == 1, "Civil",
                         ifelse(RCC$t.criminal == 1, "Criminal",
                                ifelse(RCC$t.Social_Welfare == 1, "Social Welfare",
                                       ifelse(RCC$t.econom == 1, "Economic", "Other")))))

RCC$type = as.factor(RCC$type)

n = RCC %>% group_by(type) %>% summarise(n = n())

# ggplot(data = RCC) +
#   geom_bar(aes(y = type, fill = outcome)) + theme_minimal() + facet_wrap(~year) +  scale_fill_grey() +
#   labs(title = 'Figure 2', 
#        subtitle = "Judgements by Legal Area and Year",
#        y = "Legal Area",
#        x = "Number", caption = "Source: Yulia Khalikova, ‘Constitutional Review and Judicial Behavior under
#     Authoritarianism: Evidence from Russia’ (Universität Bremen, n.d.), \n author's calculations")


###########
regresssions



log_legal = glm(RCC$dv_dummy ~ RCC$t.admin + RCC$t.elections + RCC$t.Gov_Structure + RCC$t.lawenf +
                  RCC$contol_n_pet + RCC$control_caseload2 + RCC$control_prop_judges,
                family = "binomial")
summary(log_legal)

log_petit = glm(data = RCC, dv_dummy ~ pet_citizens + pet_ngo + pet_firm + pet_gov_total
                + contol_n_pet + control_caseload2 + control_prop_judges, family = "binomial")
summary(log_petit)

log_enlarged = glm(RCC$dv_dummy ~ RCC$t.salient + RCC$pet_gov_total +
                     RCC$contol_n_pet + RCC$control_caseload2 + RCC$control_prop_judges,
                   family = "binomial")
summary(log_enlarged)

log_legal_pure = glm(RCC$dv_dummy ~ RCC$t.salient +
                       RCC$contol_n_pet + RCC$control_caseload2 + RCC$control_prop_judges,
                     family = "binomial")
summary(log_legal_pure)

# Table with coefficients
stargazer(log_legal, ci = T, single.row = T, type = "text")

OR.vector <- exp(log_legal$coef)
CI.vector <- exp(confint(log_legal))
p.values <- summary(log_legal)$coefficients[, 4]

# Table with ORs and CIs
stargazer(log_legal, coef = list(OR.vector), ci = T, 
          ci.custom = list(CI.vector), p = list(p.values), 
          single.row = F, type = "html", digits = 2,
          title="Table 3. Regression Results for the RCC by Area of Law",
          out="log_legal.htm",
          covariate.labels = c("Admin. law",
                               "Election law",
                               "Gov. structure",
                               "Law Enforcement",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"),
          dep.var.labels = "Unconstitutional")
logisticPseudoR2s(log_legal)


OR.vector_PETIT <- exp(log_petit$coef)
CI.vector_PETIT <- exp(confint(log_petit))
p.values_PETIT <- summary(log_petit)$coefficients[, 4]

stargazer(log_petit, coef = list(OR.vector_PETIT), ci = T, 
          ci.custom = list(CI.vector_PETIT), p = list(p.values_PETIT), 
          single.row = F, type = "html", digits = 2,
          title="Table 4. Regression Results for the RCC by Petitioner",
          out="log_petit.htm",
          covariate.labels = c("Citizens",
                               "NGOs",
                               "Companies/State Firms",
                               "Government (Enlarged)",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"),
          dep.var.labels = "Unconstitutional")


OR.vector_large <- exp(log_enlarged$coef)
CI.vector_large <- exp(confint(log_enlarged))
p.values_large <- summary(log_enlarged)$coefficients[, 4]

stargazer(log_enlarged, coef = list(OR.vector_large), ci = T, 
          ci.custom = list(CI.vector_large), p = list(p.values_large), 
          single.row = F, type = "html", digits = 2,
          title="Table 5. Regression Results for the RCC Enlarged",
          out="log_enlarged.htm",
          covariate.labels = c("Salient Area of Law",
                               "Government (Enlarged)",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"),
          dep.var.labels = "Unconstitutional")




stargazer(log1, log2, type="html",
          digits = 2,
          #model.numbers = FALSE,
          title="Table 3. Regression Results for the RCC",
          #ci = T,
          #omit.stat = "ser",
          out="regression_Sidorov.htm",
          covariate.labels = c("Salient",
                               "Admin. law",
                               "Election law",
                               "Gov. structure",
                               "Law Enforcement",
                               "Petitioner Gov",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"),
          dep.var.labels = "Unconstitutional")
exp(coef(log1))







logisticPseudoR2s(log_legal)



compareLogisticModels(models = list("model1" = log1, "model2" = log2))


compareLogisticModels<-function(models){
  
  pseudor2s <- data.frame(t(sapply(models, logisticPseudoR2s)))
  names(pseudor2s)<-c("Hosmer.Lemeshow", "Cox.Snell", "Nagelkerke")
  rownames(pseudor2s)<-names(models)
  
  modelsums<-lapply(models, summary)
  
  chis<-paste0(round(unlist(lapply(models, function(x) x$null.deviance - x$deviance)), 2), paste0(" ("), paste0(unlist(lapply(models, function(x) x$df.null - x$df.residual)), ")"))
  
  chisq.prob <-unlist(lapply(models, function(x) 1 - pchisq((x$null.deviance - x$deviance), (x$df.null - x$df.residual))))
  
  test<-models
  trythis<-NULL
  
  for(i in 1:length(models)){
    out<-""
    for(r in c(1:length(models))[-i]){
      test_results <- anova(test[[i]], test[[r]], test ="Chisq")
      if(!is.na(test_results[["Pr(>Chi)"]][2])){
        if(test_results$`Pr(>Chi)`[2]<.05){out<-paste0(out, r)}
      }
    }
    trythis<-c(trythis, out)
  }
  
  modeltable2<-data.frame(unlist(lapply(models, function(x) x$null.deviance - x$deviance)), unlist(lapply(models, function(x) x$df.null - x$df.residual)), unlist(lapply(models, function(x) 1 - pchisq((x$null.deviance - x$deviance), (x$df.null - x$df.residual)))), unlist(lapply(models, function(x) x$aic)), pseudor2s, trythis)
  names(modeltable2)[c(1:4, 8)]<-c("Chi square", "df", "p", "AIC", "Sig. diff from")
  
  modeltable2
}



dis_op_legal = lm(RCC$n_dissent ~  RCC$t.admin + RCC$t.elections + RCC$t.Gov_Structure + RCC$t.lawenf +
                    RCC$contol_n_pet + RCC$control_caseload2 + RCC$control_prop_judges)
dis_op_petit = lm(data = RCC, n_dissent ~ pet_citizens + pet_ngo + pet_firm + pet_gov_total
                  + contol_n_pet + control_caseload2 + control_prop_judges)
summary(dis_op_legal)

summary(dis_op_petit)


stargazer(dis_op_legal, dis_op_petit, type="html",
          digits = 2,
          model.numbers = FALSE,
          title="Table 6. Regression Results for the RCC Dissent Rate",
          ci = T,
          omit.stat = "ser",
          out="dis_op_legal.htm",
          covariate.labels = c("Admin. law",
                               "Election law",
                               "Gov. structure",
                               "Law Enforcement",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices",
                               "Citizens",
                               "NGOs",
                               "Companies/State Firms",
                               "Government (Enlarged)",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"),
          dep.var.labels = "   Number of Dissenting Opinions")



##### UKSC

UKSC_FULL <- read_excel("UKSC_FULL.xlsx")
UKSC_FULL$n_justices = lengths(gregexpr(",", UKSC_FULL$justices)) + 1
UKSC_FULL$prop_justices = round(UKSC_FULL$n_justices/12, 2)
UKSC_FULL$year = year(UKSC_FULL$date)

q = UKSC_FULL %>% group_by(year) %>% summarise(n = n())
avg_cases_per_year = mean(q$n)



q$caseload = round(q$n/avg_cases_per_year, 3)

UKSC_FULL = UKSC_FULL %>% dplyr::select(-caseload)
UKSC_joined = UKSC_FULL %>% left_join(q, by = 'year')

UKSC_joined = UKSC_joined %>% dplyr::select(-c('Judgement', 'press_summary', 'Notes'))
length(unique(UKSC_joined$citation))



ggplot(data = UKSC_joined) + 
  geom_bar(aes(x = as.factor(year), fill = factor(outcome)), stat = 'count') +
  theme_minimal() +
  labs(title = 'Figure 3',
       subtitle = "Number of Judgements Issued by the UKSC \n by the Outcome",
       y = "Number",
       x = "Year", caption = "Source: The Supreme Court of the United Kingdom, \n author's calculations")  + scale_fill_grey(name = 'Outcome', labels = c("dismissed", "allowed", "other", "on reference"))

UKSC_joined %>% filter(outcome == 1 | outcome == 0) %>% summarise(mean = mean(outcome))
UKSC_joined %>% filter(outcome == 1 | outcome == 0) %>% group_by(year) %>% summarise(mean = mean(outcome)) %>% 
  arrange(-mean) %>% round(2)

UKSC_joined %>% group_by(Appellant) %>% summarise(n = n()) %>% arrange(-n)
UKSC_joined %>% group_by(respondent) %>% summarise(n = n()) %>% arrange(-n)
UKSC_joined %>% group_by(Appellant, respondent) %>% summarise(n = n()) %>% arrange(-n)


UKSC_joined %>% filter(Appellant == 1 | respondent == 1) %>% count()
UKSC_joined %>% filter(Appellant == 2 | respondent == 2) %>% count()
UKSC_joined %>% filter(Appellant == 3 | respondent == 3) %>% count()
UKSC_joined %>% filter(Appellant == 4 | respondent == 4) %>% count()

UKSC_joined$legal_area_char = ifelse(UKSC_joined$type == 1, "Criminal",
                                     ifelse(UKSC_joined$type == 2, "Civil",
                                            ifelse(UKSC_joined$type == 3, "Social Welfare",
                                                   ifelse(UKSC_joined$type == 5, "Economic",
                                                          ifelse(UKSC_joined$type == 6, "Salient", "Other")))))







ggplot(data = UKSC_joined) +
  geom_bar(aes(y = factor(legal_area_char), fill = factor(outcome))) + theme_minimal() + facet_wrap(~year) +  scale_fill_grey(name = 'Outcome', labels = c("dismissed", "allowed", "other", "on reference")) +
  labs(title = 'Figure 4', 
       subtitle = "Judgements by Legal Area and Year",
       y = "Legal Area",
       x = "Number", caption = "Source: The Supreme Court of the United Kingdom, \n author's calculations")

UKSC_joined %>% group_by(type, outcome) %>% summarise(n = n()) %>% arrange(-n)

UKSC_joined_for_regression = UKSC_joined %>% filter(outcome == 1 | outcome == 0)

UKSC_joined_for_regression$t.criminal = ifelse(UKSC_joined_for_regression$type == 1, 1, 0)
UKSC_joined_for_regression$t.civil = ifelse(UKSC_joined_for_regression$type == 2, 1, 0)
UKSC_joined_for_regression$t.social_welfare = ifelse(UKSC_joined_for_regression$type == 3, 1, 0)
UKSC_joined_for_regression$t.economic = ifelse(UKSC_joined_for_regression$type == 5, 1, 0)
UKSC_joined_for_regression$t.salient = ifelse(UKSC_joined_for_regression$type == 6, 1, 0)
UKSC_joined_for_regression$other = ifelse(UKSC_joined_for_regression$type == 0, 1, 0)
UKSC_joined_for_regression$ap_gov = ifelse(UKSC_joined_for_regression$Appellant == 2 | UKSC_joined_for_regression$Appellant == 3 | UKSC_joined_for_regression$Appellant == 4, 1,0)
UKSC_joined_for_regression$resp_gov = ifelse(UKSC_joined_for_regression$respondent == 2 | UKSC_joined_for_regression$respondent == 3 | UKSC_joined_for_regression$respondent == 4, 1,0)



log_uksc = glm(data = UKSC_joined_for_regression, as.factor(outcome) ~ t.salient+as.factor(ap_gov)+n_pet+caseload+prop_justices, family = 'binomial')

summary(log_uksc)

OR.vector <- exp(log_uksc$coef)
CI.vector <- exp(confint(log_uksc))
p.values <- summary(log_uksc)$coefficients[, 4]

# Table with ORs and CIs
stargazer(log_uksc, coef = list(OR.vector), ci = T, 
          ci.custom = list(CI.vector), p = list(p.values), 
          single.row = F, type = "html", digits = 2,
          title="Table 7. Regression Results for the UKSC",
          out="log_uksc.htm",
          covariate.labels = c("Salient Issue",
                               "Appellant Gov",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"),
          dep.var.labels = "Appeal Allowed")
logisticPseudoR2s(log_uksc)

log_uksc_petit = glm(data = UKSC_joined_for_regression, as.factor(outcome) ~ as.factor(Appellant)+n_pet+caseload+prop_justices, family = 'binomial')
summary(log_uksc_petit)

log_uksc_law = glm(data = UKSC_joined_for_regression, as.factor(outcome) ~ as.factor(type)+n_pet+caseload+prop_justices, family = 'binomial')
summary(log_uksc_law)

log_uksc_gover = glm(data = UKSC_joined_for_regression, as.factor(outcome) ~ as.factor(ap_gov)+ as.factor(resp_gov)+n_pet+caseload+prop_justices, family = 'binomial')
summary(log_uksc_gover)

OR.vector <- exp(log_uksc_gover$coef)
CI.vector <- exp(confint(log_uksc_gover))
p.values <- summary(log_uksc_gover)$coefficients[, 4]

# Table with ORs and CIs
stargazer(log_uksc_gover, coef = list(OR.vector), ci = T, 
          ci.custom = list(CI.vector), p = list(p.values), 
          single.row = F, type = "html", digits = 2,
          title="Table 8. Regression Results for the UKSC when Government is involved",
          out="log_uksc_gover.htm",
          covariate.labels = c("Appellant Gov",
                               "Respondent Gov",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"),
          dep.var.labels = "Appeal Allowed")
logisticPseudoR2s(log_uksc_gover)


uksc_dissent = lm(data = UKSC_joined_for_regression, n_dissent ~ as.factor(t.salient) + as.factor(ap_gov)+ as.factor(resp_gov) +
                    n_pet+caseload+prop_justices)
summary(uksc_dissent)

stargazer(uksc_dissent, type="html",
          digits = 2,
          model.numbers = FALSE,
          title="Table 9. Regression Results for the UKSC for dissenting opinions",
          ci = T, 
          out="uksc_dissent.htm",
          covariate.labels = c("Salient Issue",
                               "Appellant Gov",
                               "Respondent Gov",
                               "Control: #Pet",
                               "Control: Caseload",
                               "Control: Share of Justices"), dep.var.labels = "Dissent Rate")

library(readxl)
aa <- read_excel("Downloads/табл.xlsx")

library(stringr)
a1 = aa$Presidents
a2 = aa$`Foreign Ministers`
a3= aa$`Prime ministers`
remove_special_chars <- function(covar_labels){
  covar_labels %>% 
    str_replace_all("\\\\", "") %>% 
    str_replace_all("\\^",  "") %>%
    str_replace_all("_",   " ") %>% 
    str_replace_all("\\$",  "") %>% 
    str_replace_all("`",   "'") 
}

stargazer(as.data.frame(aa), summary = FALSE, rownames = FALSE, title = "sss", out="NAME.htm")
