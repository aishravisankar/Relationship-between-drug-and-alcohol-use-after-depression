
#REASEARCH QUESTION: In American adults, is there a higher prevalence of drug use among those who are depressed, compared to those 
#who are not? Does the use of alcohol confound this relationship? 

#NULL HYPOTHESIS: Drug and alcohol use are not correlated with depression

#ALTERNATIVE HYPOTHESIS: Drug and alcohol use are correlated with depression

library(tidyverse) # data management (tidyr, dplyr) and plots (ggplot2)
library(foreign) # reading xport files

# Downloading data from NHANES ######################################################################################################################
# Demographics
demofile = tempfile()

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT", 
              destfile = demofile,
              mode = "wb") 

demodf = read.xport(demofile) 


# Mental health
####################Note: participant age 18 and older only (we should limit our dataset to 18 and over)
mhfile = tempfile()

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DPQ_J.XPT",
              destfile = mhfile,
              mode = 'wb')

mhdf = read.xport(mhfile)

# Alcohol
alcfile = tempfile()

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/ALQ_J.XPT",
              destfile = alcfile,
              mode = 'wb')

alcdf = read.xport(alcfile)

# Drugs
drugfile = tempfile()

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DUQ_J.XPT",
              destfile = drugfile,
              mode = 'wb')

drugdf = read.xport(drugfile)


# Merging using "piping" + selecting variables ###############################################################################################################################

nhanesdata = 
  left_join(demodf, mhdf, by="SEQN") %>% 
  left_join(alcdf,   by="SEQN") %>%
  left_join(drugdf,     by="SEQN")

#Variables needed:
##################################DEMOGRAPHICS
#RIDAGEYR #Age in years
#RIAGENDR #Gender (1=male, 2=female)
#RIDRETH3 #Race/Hispanic origin (1=Mex. American, 2=Other Hispanic, 3=NH White, 4=NH Black, 6=Asian, 7=OTher/Multi-racial)
#DMQMILIZ #Served active duty in US Armed Forces (1=Yes, 2=No, 7=Refused, 9=Don't know)
#DMDEDUC2 #education (1=less than 9th, 2=9-11, 3=high school/GED, 4=some college/AA, 5=college grad+, 7/9=missing)
#DMDEDUC3 #Education level - Children/Youth 6-19 (0=never attended/kindergarted, 1-12=grades 1-12, 13/14=HS/GED, 15=gt HS, 55=lt 5th, 66=lt 9th, 77/99=refuse/don't know)
#DMDMARTL #marital status (1=married, 2=widowed, 3=divorced, 4=separated, 5=nvr married, 6=living w/ partner, 77/99=refused/don't know)
#INDFMPIR #Ratio of family income to poverty (values 0-5; 5 can represent gt 5)
##################################MENTAL HEALTH (0=not at all, 1=several days, 2=more than half the days, 3=nearly every day, 7/9=refused/don't know)
##############NOTE: Composite scores of 0-27 can be calculated (PHQ-9 scores of 5, 10, 15, and 20 represented mild, moderate, moderately severe, and severe depression, respectively.)
#DPQ010 #Have little interest in doing things
#DPQ020 #Feeling down, depressed, or hopeless
#DPQ030 #Trouble sleeping or sleeping too much
#DPQ040 #Feeling tired or having little energy
#DPQ050 #Poor appetite or overeating
#DPQ060 #Feeling bad about yourself
#DPQ070 #Trouble concentrating on things
#DPQ080 #Moving or speaking slowly or too fast
#DPQ090 #Thought you would be better off dead
#DPQ100 #Difficulty these problems have caused (0=not at all, 1=somewhat, 2=very, 3=extremely, 7/9=refused/don't know)
##################################ALCOHOL
#ALQ111 - Ever had a drink of any kind of alcohol
#ALQ121 - Past 12 mo how often have alcohol drink
##################################DRUGS
#DUQ240 - Ever used cocaine/heroin/methamphetamine
#DUQ370 - Ever use a needle to inject illegal drug

nhanesdata = nhanesdata %>%
  select(
    RIDAGEYR, #Age in years
    RIAGENDR, #Gender (1=male, 2=female)
    RIDRETH3, #Race/Hispanic origin (1=Mex. American, 2=Other Hispanic, 3=NH White, 4=NH Black, 6=Asian, 7=OTher/Multi-racial)
    DMQMILIZ, #Served active duty in US Armed Forces (1=Yes, 2=No, 7=Refused, 9=Don't know)
    DMDEDUC2, #education (1=less than 9th, 2=9-11, 3=high school/GED, 4=some college/AA, 5=college grad+, 7/9=missing)
    DMDEDUC3, #Education level - Children/Youth 6-19 (0=never attended/kindergarted, 1-12=grades 1-12, 13/14=HS/GED, 15=gt HS, 55=lt 5th, 66=lt 9th, 77/99=refuse/don't know)
    DMDMARTL, #marital status (1=married, 2=widowed, 3=divorced, 4=separated, 5=nvr married, 6=living w/ partner, 77/99=refused/don't know)
    INDFMPIR, #Ratio of family income to poverty (values 0-5; 5 can represent gt 5)
    DPQ010, #Have little interest in doing things
    DPQ020, #Feeling down, depressed, or hopeless
    DPQ030, #Trouble sleeping or sleeping too much
    DPQ040, #Feeling tired or having little energy
    DPQ050, #Poor appetite or overeating
    DPQ060, #Feeling bad about yourself
    DPQ070, #Trouble concentrating on things
    DPQ080, #Moving or speaking slowly or too fast
    DPQ090, #Thought you would be better off dead
    DPQ100, #Difficulty these problems have caused (0=not at all, 1=somewhat, 2=very, 3=extremely, 7/9=refused/don't know)
    ALQ111, #Ever had a drink of any kind of alcohol
    ALQ121, #Past 12 mo how often have alcohol drink
    DUQ240, #Ever used cocaine/heroin/methamphetamine (1=Yes, 2=No, 7=Refused, 9=Don't know)
    DUQ370 #Ever use a needle to inject illegal drug (1=Yes, 2=No, 7=Refused, 9=Don't know)
  )%>%
  mutate(PHQ = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090 + DPQ100)


# Exclusion/Inclusion Criteria ######################################################################################################################

#Adults only
nhanesdata <- nhanesdata[!(nhanesdata$RIDAGEYR < 18), ]

#PHQ score not missing 
nhanesdata<-nhanesdata[(!is.na(nhanesdata$PHQ)==TRUE), ]

#Drug use not missing
nhanesdata<-nhanesdata[(!is.na(nhanesdata$DUQ370)==TRUE)&(!is.na(nhanesdata$DUQ240)==TRUE), ]%>%
  
  #Define the depression groups
  mutate(deplvl = case_when(
    PHQ < 5 ~ "Not depressed",
    (PHQ>=5) & (PHQ<10) ~ "Mild depression",
    (PHQ>=10) ~ "Moderate to severe depression"
  )) %>%
  
  
  #Defining missing 
  mutate_at (c("RIAGENDR", "DMDEDUC2", "DMQMILIZ", "DUQ240", "DUQ370", "ALQ111"), na_if, 7) %>%
  mutate_at (c("RIAGENDR", "DMDEDUC2", "DMQMILIZ", "DUQ240", "DUQ370", "ALQ111"), na_if, 9) %>%
  mutate_at (c("DMDMARTL", "DMDEDUC3"),  na_if, 77) %>%
  mutate_at (c("DMDMARTL", "DMDEDUC3"),  na_if, 99) 


# Defining variables #################################################################################################################################

nhanesdata <- nhanesdata %>% 
  
  mutate(race = case_when(
    RIDRETH3 %in% (1:2)   ~ "Hipanic",
    RIDRETH3==3           ~ "NH White",
    RIDRETH3==4           ~ "NH Black",
    RIDRETH3==6           ~ "Multiracial/Other",
    RIDRETH3==7           ~ "Multiracial/Other"
  ))%>% 
  
  mutate(marriage = case_when(
    (DMDMARTL==1) | (DMDMARTL==6) ~ "Married or Living With Partner",
    DMDMARTL==2               ~ "Widowed",
    DMDMARTL %in% (3:4)       ~ "Divorced or Separated",
    DMDMARTL==5               ~ "Never Married"
  ))%>% 
  
  mutate(education = case_when(
    DMDEDUC3 %in% (0:8) | DMDEDUC3==55 | DMDEDUC3==66 | DMDEDUC2==1 ~ "Less than 9th grade",
    DMDEDUC3 %in% (9:12) | DMDEDUC2==2 ~ "9th-11th grade",
    DMDEDUC3 %in% (13:14) | DMDEDUC2==3 ~ "High School/GED",
    DMDEDUC3==15 | DMDEDUC2==4 ~ "Some college",
    DMDEDUC2==5 ~ "College grad +"
  ))%>%
  
  mutate(military = case_when(
    DMQMILIZ==1 ~ "Yes",
    DMQMILIZ==2 ~ "No"
  ))%>%
  
  mutate(poverty = case_when(
    INDFMPIR > 1 ~ "Not in poverty",
    INDFMPIR <= 1 ~ "In poverty"
  ))%>%
  
  mutate(druguse = case_when(
    (DUQ370==1) | (DUQ240==1) ~ "Yes",
    (DUQ370==2) & (DUQ240==2) ~ "No"
  ))%>%
  
  
  mutate(drinkfreq = case_when(
    ALQ121 %in% c(1,2) ~ "Every day",
    ALQ121 %in% c(3,4,5) ~ "Weekly",
    ALQ121 %in% c(6,7) ~ "Monthly",
    ALQ121 %in% c(0,8,9,10) ~ "Less than once a month"
  ))%>%
  
  mutate(sex = case_when(
    RIAGENDR==1 ~ "Male",
    RIAGENDR==2 ~ "Female"
  ))%>%
  
  mutate(age = cut(RIDAGEYR, 
                       breaks = quantile(RIDAGEYR, probs = c(0, 1/3, 2/3, 1)), 
                       include.lowest = T, 
                       labels = c("18-39", "40-61", "62+")))
  

 
  nhanesdata$sex       <-as.factor(nhanesdata$sex)
  nhanesdata$drinkfreq <-as.factor(nhanesdata$drinkfreq)
  nhanesdata$druguse   <-as.factor(nhanesdata$druguse)
  nhanesdata$military  <-as.factor(nhanesdata$military)
  nhanesdata$education <-as.factor(nhanesdata$education)
  nhanesdata$marriage  <-as.factor(nhanesdata$marriage)
  nhanesdata$race      <-as.factor(nhanesdata$race)
  nhanesdata$deplvl    <-as.factor(nhanesdata$deplvl)
  nhanesdata$poverty   <-as.factor(nhanesdata$poverty)
  nhanesdata$age       <-as.factor(nhanesdata$age)

  str(nhanesdata)  
  
# defining reference levels
  nhanesdata$deplvl  <- relevel(nhanesdata$deplvl, ref = "Not depressed")
  nhanesdata$age  <- relevel(nhanesdata$age, ref = "62+")  
  nhanesdata$poverty  <- relevel(nhanesdata$poverty, ref = "Not in poverty")
  nhanesdata$race  <- relevel( nhanesdata$race, ref = "NH white")
  nhanesdata$marriage  <- relevel(nhanesdata$marriage, ref = "Married or Living With Partner")
  nhanesdata$education  <- relevel(nhanesdata$education, ref = "College Grad +")
  nhanesdata$military  <- relevel(nhanesdata$military, ref = "No")
  nhanesdata$drinkfreq  <- relevel(nhanesdata$drinkfreq, ref = "Not depressed")
  nhanesdata$sex  <- relevel(nhanesdata$sex, ref = "Female")

#####not needed 
#install.packages("epitools")
library(epitools)

  oddsratio.wald(nhanesdata$deplvl, nhanesdata$druguse ,
                 conf.level = 0.95,
                 rev = c("neither", "rows", "columns", "both"),
                 correction = FALSE,
                 verbose = FALSE)
  
  oddsratio.wald(nhanesdata$deplvl, nhanesdata$race, nhanesdata$druguse,
                 conf.level = 0.95,
                 rev = c("neither", "rows", "columns", "both"),
                 correction = FALSE,
                 verbose = FALSE)
  
#Crude model (main exposure - depression level, outcome = drug abuse) 
Crude_model <- glm(druguse ~ deplvl, data = nhanesdata, family ="binomial"(link=logit))
summary(Crude_model)
coef(Crude_model)
exp(coef(Crude_model))
confint(Crude_model)
exp(confint(Crude_model))
#with(Crude_model, null.deviance - deviance)
#with(Crude_model, df.null, df.residual)
with(Crude_model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - age) 
Model1 <- glm(druguse ~ deplvl + age , data = nhanesdata, family ="binomial")
summary(Model1)
coef(Model1)
exp(coef(Model1))
confint(Model1)# to get odds ratio
exp(confint(Model1))# confidence interval
# p-value of chi sqaure
with(Model1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - race) 
Model2 <- glm(druguse ~ deplvl + race , data = nhanesdata, family ="binomial")
summary(Model2)
coef(Model2)
exp(coef(Model2))
confint(Model2)
exp(confint(Model2))
with(Model2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - sex) 
Model3 <- glm(druguse ~ deplvl + sex , data = nhanesdata, family ="binomial")
summary(Model3)
coef(Model3)
exp(coef(Model3))
confint(Model3)
exp(confint(Model3))
with(Model3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - education) 
Model4 <- glm(druguse ~ deplvl + education , data = nhanesdata, family ="binomial")
summary(Model4)
coef(Model4)
exp(coef(Model4))
confint(Model4)
exp(confint(Model4))
with(Model4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - marriage) 
Model5 <- glm(druguse ~ deplvl + marriage , data = nhanesdata, family ="binomial")
summary(Model5)
coef(Model5)
exp(coef(Model5))
confint(Model5)
exp(confint(Model5))
with(Model5, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - Military) 
Model6 <- glm(druguse ~ deplvl + military , data = nhanesdata, family ="binomial")
summary(Model6)
coef(Model6)
exp(coef(Model6))
confint(Model6)
exp(confint(Model6))
with(Model6, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - Poverty) 
Model7 <- glm(druguse ~ deplvl + poverty , data = nhanesdata, family ="binomial")
summary(Model7)
coef(Model7)
exp(coef(Model7))
confint(Model7)
exp(confint(Model7))
with(Model7, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Crude model (main exposure - depression level, outcome = drug abuse, adjusted - Drinkfreq) 
Model8 <- glm(druguse ~ deplvl + drinkfreq , data = nhanesdata, family ="binomial")
summary(Model8)
coef(Model8)
exp(coef(Model8))
confint(Model8)
exp(confint(Model8))
with(Model8, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



















  
  