load("C:/Users/Hanna/Downloads/final_data.RData")

### REPLICATION PART
data_summary <- data.frame("Variable" = rep(0,20), "Definition" = rep(0,20),
                           "Mean" = rep(0, 20), "SD" = rep(0,20))
not_centered <- final_com[,c(5:8,18,9,10,17,11:15,19,16,20:23,2,4)]
a <- c("Female","Age","Hispanic","Black","Two-parent household","Mother's education",
       "Number of siblings","Length of family residence","Depression","Emotionality",
       "Impulsitivity","Sociability","Substance abuse","Exposure to suicidal behaviors",
       "Family attachment and support","Concentrated poverty","Immigrant concentration",
       "Residential stability","Collective efficacy", "Collecttive efficacy")
b <- c("Dummy variable, return 1 if female","Age in years","Dummy variable, return 1 if Hispanic",
       "Dummy variable, return 1 if Black","Dummy variable, return 1 if living with two parents",
       "Mother's highest education, from 1 to 5","Number of siblings aged under 19",
       "Length of residence in years in current address","Extent of depressive behavior, from 0 to 2",
       "Level of emotionality, from 1 to 5","Level of impulsivity, from 1 to 5",
       "Level of sociability, from 1 to 5","Sum of dichotomous responses to 3 types of drugs, from 0 to 3",
       "Dummy variable, return 1 if affirmative","Extent to which family members provide support, from 1 to 3",
       "Percentages of residents below poverty line, unemployed, female-led","Percentages of Latino and foriegn-born",
       "Percentages of residents living in the same house as in 1985","Scales of social cohesion, intergenerational closure and informal social control in the neighborhood"
       )
for (i in 1:20) {
  data_summary[i,3] <- mean(not_centered[,i],na.rm=TRUE)
  data_summary[i,4] <- sd(not_centered[,i],na.rm=TRUE)
  data_summary[i,1] <- a[i]
  data_summary[i,2] <- b[i]
}
# demographics
female <- mean(not_centered$female)
black <- mean(not_centered$black)
hispanic <- mean(not_centered$hispanic)
age <- mean(not_centered$age)
two_parents <- mean(not_centered$two_parents)
mom_edu <- mean(not_centered$mom_edu)

# replication
model1 <- glmer(suicide ~ female_c+age_c+black_c+hispanic_c+two_parents_c+mom_edu_c+sibs_c+res_yrs_c+
                   depression_c+emotionality_c+impulsivity_c+sociability_c+sub_abuse_c+exp_suic_c
                 +CPOV90_c+CFORBORN_c+fam_attach_c+(1+fam_attach_c|nc),
                 family = binomial("logit"), final_com)

model2 <- glmer(suicide ~ female_c+age_c+black_c+hispanic_c+two_parents_c+mom_edu_c+sibs_c+res_yrs_c+
                     depression_c+emotionality_c+impulsivity_c+sociability_c+sub_abuse_c+exp_suic_c
                   +CPOV90_c+CFORBORN_c+fam_attach_c+collective_c+(1+fam_attach_c|nc),
                   family = binomial("logit"), final_com)

model3 <- glmer(suicide ~ female_c+age_c+black_c+hispanic_c+two_parents_c+mom_edu_c+sibs_c+res_yrs_c+
                  depression_c+emotionality_c+impulsivity_c+sociability_c+sub_abuse_c+exp_suic_c
                +CPOV90_c+CFORBORN_c+fam_attach_c*collective_c+(1+fam_attach_c|nc),
                family = binomial("logit"), final_com)











### EXTENSION PART
library(rbounds)
library(lme4)
library(Matching)
library(rgenoud)

names(final_com)
X <- cbind(final_com$age,final_com$black,final_com$hispanic,final_com$two_parents,final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
  depression_c,final_com$emotionality,final_com$impulsivity,final_com$sociability,final_com$sub_abuse,final_com$exp_suic
,final_com$CPOV90,final_com$CFORBORN,final_com$fam_attach*final_com$collective)

Tr<- final_com$female
Y<- final_com$suicide
genout<-GenMatch(Tr=Tr, X=X,BalanceMatrix=X, estimand="ATT", M=1,pop.size = 200, max.generations=100,
                 wait.generations=4)
mout<-Match(Tr=Tr, X=X, estimand = "ATT", Weight.matrix = genout)
summary(mout)
mb<-MatchBalance(final_com$female~final_com$age+final_com$black+final_com$hispanic+final_com$two_parents+final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
depression_c+final_com$emotionality+final_com$impulsivity+final_com$sociability+final_com$sub_abuse+final_com$exp_suic
+final_com$CPOV90_c+final_com$CFORBORN+final_com$fam_attach*final_com$collective, match.out = mout, nboots = 500)

summary(mb)

mout_ii<- Match(Y=final_com$suicide,Tr=Tr, X=X, estimand = "ATT", Weight.matrix = genout)
summary(mout_ii)


#final_com$sub_abuse[which(final_com$sub_abuse!=0)]<-1


###PARTII-FOR SUBSTANCE ABUSE



X2 <- cbind(final_com$female,final_com$age,final_com$black,final_com$hispanic,final_com$two_parents,final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
             depression_c,final_com$emotionality,final_com$impulsivity,final_com$sociability,final_com$exp_suic
           ,final_com$CPOV90,final_com$CFORBORN,final_com$fam_attach*final_com$collective)

Tr2<- final_com$sub_abuse
Y<- final_com$suicide
genout2<-GenMatch(Tr=Tr2, X=X2,BalanceMatrix=X, estimand="ATT", M=1,pop.size = 200, max.generations=100,
                 wait.generations=4)
mout2<-Match(Tr=Tr2, X=X2, estimand = "ATT", Weight.matrix = genout2)
summary(mout2)
mb2<-MatchBalance(final_com$sub_abuse~final_com$female+final_com$age+final_com$black+final_com$hispanic+final_com$two_parents+final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
                   depression_c+final_com$emotionality+final_com$impulsivity+final_com$sociability+final_com$exp_suic
                 +final_com$CPOV90_c+final_com$CFORBORN+final_com$fam_attach*final_com$collective, match.out = mout2, nboots = 500)

summary(mb2)

mout2_ii<- Match(Y=final_com$suicide,Tr=Tr2, X=X2, estimand = "ATT", Weight.matrix = genout2)
summary(mout2_ii)



psens(mout_ii)

psens(mout2_ii)


#lines(density(non_sub_abuse_suicide), col = "blue", lwd = 2)
new_set_female<- final_com[which(final_com$female==1),]
new_set_male<- final_com[which(final_com$female==0),]

# Multilevel, All observation, Collective efficacy-family attachment interaction

model_female <- glmer(suicide ~ female_c+age_c+black_c+hispanic_c+two_parents_c+mom_edu_c+sibs_c+res_yrs_c+
                        depression_c+emotionality_c+impulsivity_c+sociability_c+sub_abuse_c+exp_suic_c
                      +CPOV90_c+CFORBORN_c+fam_attach_c*collective_c+(1+fam_attach_c|nc),
                      family = binomial("logit"), new_set_female)
sum_female <- summary(model_female)
sum_female


model_male <- glmer(suicide ~ female_c+age_c+black_c+hispanic_c+two_parents_c+mom_edu_c+sibs_c+res_yrs_c+
                      depression_c+emotionality_c+impulsivity_c+sociability_c+sub_abuse_c+exp_suic_c
                    +CPOV90_c+CFORBORN_c+fam_attach_c*collective_c+(1+fam_attach_c|nc),
                    family = binomial("logit"), new_set_male)
sum_male <- summary(model_male)
sum_male
