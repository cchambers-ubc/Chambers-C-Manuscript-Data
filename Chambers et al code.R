#Drug use stigma, accidental pet poisonings, and veterinary care: Results from a survey of pet owners in Vancouver, British Columbia
#Chloe Chambers, Lexis Ly, Alexandra Protopopova

#Code used for analysis of data
#All data is available in the Github. If you are unable to access it, please do not hesitate to reach out.
#ccham022@uottawa.ca

######## Packages #########
library(dplyr)
library(ggplot2)
library(tidyr)
library(summarytools) 
library(psych)
library(likert)
library(table1)
library(readr)
library(stats)
library(corrplot)
library(Hmisc)

##### Raw data filtering #######
rawdata<- read.csv("C:\\Users\\chloe\\OneDrive\\Documents\\Hons thesis\\Survey data\\RAW DATA de-ID and cleaned October 2024.csv")
#Or should I do smthn like
#rawdata<- read.csv("C:\\your file path\\RAW DATA de-ID and cleaned October 2024.csv")

#Filter based on completion
rawdata<-filter(rawdata, Finished == TRUE)
#Q4: Primary guardian of a pet
rawdata<-filter(rawdata, Q4 == "Yes", na.rm = TRUE)
#Q5: Regularly use drugs
rawdata<-filter(rawdata, Q5 == "Yes", na.rm = TRUE)
#Filter based on reCaptcha score
rawdata<-filter(rawdata, Q_RecaptchaScore > 0.5|Q_RecaptchaScore == "")
#Q28_4: Attention check, must select "Somewhat disagree"
rawdata<- filter(rawdata, Q28_4 == "Somewhat disagree", na.rm = TRUE)

#Export to Excel for short answer analysis

#Online survey data
online<- read.csv("C:\\Users\\chloe\\OneDrive\\Documents\\Hons thesis\\Survey data\\R Import Online Oct 2024.csv")
#online<- read.csv("C:\\your file path\\R Import Online Oct 2024.csv")

#Remove Attention Check question
online_2<-subset(online, select = -Q28_4)
#Assign respondent ID
online_2<-online_2%>%
  mutate(resp_id = 1:n())%>%
  select(resp_id,everything())

#In-person survey data
#Because all of the responses were collected in-person, data quality was ensured, minimal filtering was necessary
in_person<- read.csv("C:\\Users\\chloe\\OneDrive\\Documents\\Hons thesis\\Survey data\\R Import In-Person Oct 2024.csv")
#in_person<- read.csv("C:\\your file path\\R Import In-Person Oct 2024.csv")

in_person <- filter(in_person, Q4 == "Yes")
in_person <- filter(in_person, Q5 == "Yes")
in_person_2<-subset(in_person, select = -Q28_4)
in_person_2<-in_person_2%>%
  mutate(resp_id = 1:n())%>%
  select(resp_id,everything())

####### Frequency calculations for descriptive statistics ######

#Multiple choice question example: age

#Verify n of each question
sum(online_2$Q4.1!="")
sum(in_person_2$Q4.1!="")

#Tabulate results
age_online<-online_2%>%filter(online_2$Q4.1!="")%>%
  group_by(Q4.1)%>%
  summarise(n=n()) %>%
  mutate(Freq = n/sum(n)*100)

age_irl<-in_person_2%>%filter(in_person_2$Q4.1!="")%>%
  group_by(Q4.1)%>%
  summarise(n=n()) %>%
  mutate(Freq = n/sum(n)*100)

#The same code was applied to questions: 
  #Gender identity
  #Housing
  #Neighborhood
  #Drug use frequency
  #Household drug use
  #Measures for preventing access to drugs
  #Frequency of poisoning occurrences


#Select all question example: Racial or Ethnic Background

sum(online_2$Q5.1!="")
sum(in_person_2$Q5.1!="")

race_eth_online<-online_2%>%
  select(resp_id, Q5.1)%>%
  separate_rows(Q5.1, sep = ",")%>%
  group_by(Q5.1)%>%
  summarise(count = n_distinct(resp_id), freq = count/82*100)

race_eth_irl<-in_person_2%>%
  select(resp_id, Q5.1)%>%
  separate_rows(Q5.1, sep = ",")%>%
  group_by(Q5.1)%>%
  summarise(count = n_distinct(resp_id), freq = count/32*100)

#The same code was applied to questions: 
  #Employment
  #Pet type (Bird, Reptile, and Other were grouped into Other pet)
  #Drugs used
  #Method of drug use

#Numeric value input example: Income

sum(!is.na(online_2$Q7))
sum(!is.na(in_person_2$Q7))

income_online<- online_2 %>% 
  summarise(mean = (mean(Q7, na.rm = T)), sd = (sd(Q7, na.rm = T)))
income_online

income_irl<- in_person_2 %>% 
  summarise(mean = (mean(Q7, na.rm = T)), sd = (sd(Q7, na.rm = T))) 
income_irl

#The same code was applied to questions: 
  #Number of pets

#Mean rank calculation: Q39
online_ranked_39 <-colMeans(online_2[,47:55], na.rm = TRUE)
Q39_headings<- c("Emergency animal hospital", 
                 "Local veterinary clinic", 
                 "Call local veterinary clinic", 
                 "Call animal poison line", 
                 "Call non-animal poison line",
                 "Call drug overdose helpline",
                 "Community-based organization",
                 "Research treatment/treat at home",
                 "Monitor symptoms/no immediate treatment")
names(online_ranked_39) <- Q39_headings
online_ranked_39 <- sort(online_ranked_39)
online_ranked_39

irl_ranked_39 <-colMeans(in_person_2[,50:58], na.rm = TRUE)
names(irl_ranked_39) <- Q39_headings
irl_ranked_39 <- sort(irl_ranked_39)
irl_ranked_39


####### Re-coding text to numeric for Likert questions ######

likert_recode <- function(x) {
  as.numeric(case_when(
    x == "Strongly disagree" ~ 1,
    x == "Somewhat disagree" ~ 2,
    x == "Neither agree nor disagree" ~ 3,
    x == "Somewhat agree" ~ 4,
    x == "Strongly agree" ~ 5,
  ))
}

never_recode <- function(x) {
  as.numeric(case_when(
    x == "Never" ~1,
    x == "Sometimes" ~2,
    x == "About half the time" ~3,
    x == "Most of the time" ~ 4,
    x == "Always" ~5
  ))
}

experience_recode <- function(x) {
  as.numeric(case_when(
    x == "Extremely bad" ~1,
    x == "Somewhat bad" ~2,
    x == "Neither good nor bad" ~3,
    x == "Somewhat good" ~ 4,
    x == "Extremely good" ~5
  ))
}

concern_recode <- function(x) {
  as.numeric(case_when(
    x == "Not at all concerned" ~1,
    x == "Somewhat concerned" ~2,
    x == "Moderately concerned" ~3,
    x == "Quite concerned" ~ 4,
    x == "Extremely concerned" ~5
  ))
}


#Online survey
online_likert_recode <- mutate_at(online_2, vars
                                  (c("Q27_1", "Q27_2", "Q27_3", "Q27_4", "Q27_5", 
                                     "Q28_1", "Q28_2", "Q28_3", "Q28_5", 
                                     "Q37_1", "Q37_2", "Q42_1", "Q42_2", "Q42_3", 
                                     "Q44_1", "Q44_2", "Q44_3", "Q44_4", "Q44_5", 
                                     "Q44_6", "Q44_7", "Q44_8", "Q44_9", "Q46_1", 
                                     "Q46_2", "Q46_3", "Q46_4", "Q46_5", "Q46_6",
                                     "Q46_7", "Q93_1", "Q93_2", "Q96_1", "Q96_2",
                                     "Q96_1", "Q96_2", "Q96_3", "Q96_4", "Q96_5",
                                     "Q96_6", "Q96_7", "Q96_8", "Q96_9", "Q61_1",
                                     "Q61_2", "Q61_3", "Q65_1", "Q65_2", "Q65_3", "Q65_4", "Q65_5", 
                                     "Q65_6", "Q65_7", "Q65_8", "Q65_9", "Q71")), likert_recode)

online_likert_recode <- mutate_at(online_likert_recode, vars("Q70"), experience_recode)
online_likert_recode <- mutate_at(online_likert_recode, vars(c("Q36", "Q41")), concern_recode)
online_recoded <- mutate_at(online_likert_recode, vars(c("Q29_1", "Q29_2", "Q29_3", "Q29_4", "Q29_5", "Q29_6")), never_recode)

#We did not do any inferential statistical analysis with the in-person data given the small sample size 

online_hyp <- filter(online_recoded, Q33 == "Option 1: I have not experienced an animal emergency where my pet has accidentally ingested drugs.")

###### Likert means #########
likert_online<- online_recoded[,c(26:40)] #Q27
likertmeans_online<-summarize_all(likert_online, mean, na.rm=TRUE)
likertsd_online<-summarize_all(likert_online, sd, na.rm=TRUE)
likertrange_online<-summarize_all(likert_online, range, na.rm=TRUE)

#Repeated for Questions 28 and 29

###### Intra-question correlations #######

#Relationships within domains of stigma

#Question 27: discrimination related to drug use
corr.test(online_hyp[26:30], method = "spearman")$r
corr.test(online_hyp[26:30], method = "spearman")$p
cor_test_27 <- corr.test(online_hyp[26:30], method = "spearman")$r
mean(cor_test_27)
sd(cor_test_27)

online_hyp<- mutate(online_hyp, avg_27 = rowMeans(online_hyp[26:30], na.rm=TRUE), .after=30)

#Question 28: barriers to various services
#Note: Q28_4 was the attention check question and was removed in previous filtering
corr.test(online_hyp[32:35], method = "spearman")$r
corr.test(online_hyp[32:35], method = "spearman")$p
cor_test_28 <- corr.test(online_hyp[c(32:35)], method = "spearman")$r
mean(cor_test_28)
sd(cor_test_28)

online_hyp<- mutate(online_hyp, avg_28 = rowMeans(online_hyp[32:35], na.rm=TRUE), .after=35)

#Question 29: discrimination in healthcare
corr.test(online_hyp[37:42], method = "spearman")
cor_test_29 <- corr.test(online_hyp[37:42], method = "spearman")$r
mean(cor_test_29)
sd(cor_test_29)
online_hyp<- mutate(online_hyp, avg_29 = rowMeans(online_hyp[37:42], na.rm=TRUE), .after=42)

######### Correlations between stigma scores and 46 (veterinary concerns), 42 (sharing information), 39 (ranked actions) ########

#Q27 average
rcorr_matrix_27 <- rcorr(as.matrix(online_hyp[ , c("avg_27", "Q46_1", "Q46_2", "Q46_3", "Q46_4", "Q46_5", "Q46_6", "Q46_7")]))
rcorr_matrix_27
coef_matrix<-rcorr_matrix_27$r
p_matrix<-rcorr_matrix_27$P


#Q28 average
rcorr_matrix_28 <- rcorr(as.matrix(online_hyp[ , c("avg_28", "Q46_1", "Q46_2", "Q46_3", "Q46_4", "Q46_5", "Q46_6", "Q46_7")]))
rcorr_matrix_28
coef_matrix_28<-rcorr_matrix_28$r
p_matrix_28<-rcorr_matrix_28$P

#Q29 average
rcorr_matrix_29 <- rcorr(as.matrix(online_hyp[ , c("avg_29", "Q46_1", "Q46_2", "Q46_3", "Q46_4", "Q46_5", "Q46_6", "Q46_7")]))
rcorr_matrix_29
coef_matrix_29<-rcorr_matrix_29$r
p_matrix_29<-rcorr_matrix_29$P

#Q42
rcorr_matrix_42a <- rcorr(as.matrix(online_hyp[ , c("avg_27", "Q42_1", "Q42_2", "Q42_3")]))
rcorr_matrix_42a
coef_matrix_42a<-rcorr_matrix_42a$r
p_matrix_42a<-rcorr_matrix_42a$P

rcorr_matrix_42b <- rcorr(as.matrix(online_hyp[ , c("avg_28", "Q42_1", "Q42_2", "Q42_3")]))
rcorr_matrix_42b
coef_matrix_42b<-rcorr_matrix_42b$r
p_matrix_42b<-rcorr_matrix_42b$P

rcorr_matrix_42c <- rcorr(as.matrix(online_hyp[ , c("avg_29", "Q42_1", "Q42_2", "Q42_3")]))
rcorr_matrix_42c
coef_matrix_42c<-rcorr_matrix_42c$r
p_matrix_42c<-rcorr_matrix_42c$P

#Q39 ranking
rcorr_matrix_39a <- rcorr(as.matrix(online_hyp[ , c("avg_27", "Q39_1", "Q39_2", "Q39_3", "Q39_4", "Q39_5", "Q39_6", "Q39_7")]))
rcorr_matrix_39a
#No correlations/significance

rcorr_matrix_39b <- rcorr(as.matrix(online_hyp[ , c("avg_27", "Q39_1", "Q39_2", "Q39_3", "Q39_4", "Q39_5", "Q39_6", "Q39_7")]))
rcorr_matrix_39b
#No correlations/significance

rcorr_matrix_39c <- rcorr(as.matrix(online_hyp[ , c("avg_29", "Q39_1", "Q39_2", "Q39_3", "Q39_4", "Q39_5", "Q39_6", "Q39_7")]))
rcorr_matrix_39c
#Wait and monitor symptoms - significant - R=-0.28

#Thanks for reading :)
