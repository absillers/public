###State rankings 
library(dplyr)
library(tidyverse)
library(readxl)

#HR survey 

#Title IV, US only - 

HR_1 <- read.csv("C:/Users/annas/Downloads/Data_9-12-2022---168.csv")
HR_1$X <- NULL

colnames(HR_1) <- gsub("\\.\\.(SAL|FLAGS|HD)2020", "", colnames(HR_1))

#find HR family. Only schools with HR indicator of 2 need to be group together. all else are separate units

HR_1$familyID <- ifelse(HR_1$Parent.child == 2, 
                        HR_1$ID.of.institution.where.data.are.reported.for.the.Human.Resources, 
                        HR_1$UnitID)

#add up professors (full, associate, assistant ) and admin positions
                    
HR_1$sum_prof <- apply(HR_1[,c(3:5)], 1, sum)
HR_1$sum_admin <- apply(HR_1[,c(8:10)], 1, sum)

#find what type of family & sum of professors/admin by family unit

HR_1 <- HR_1 %>%
  group_by(familyID) %>%
  mutate(family_type =  max(Parent.child..indicator...Human.Resources..HR..component., na.rm=TRUE))

HR_1 <- HR_1 %>%
  group_by(familyID) %>%
  mutate(family_prof = sum(sum_prof, na.rm=TRUE))

HR_1 <- HR_1 %>%
  group_by(familyID) %>%
  mutate(family_admin = sum(sum_admin, na.rm=TRUE))

#filter by IDs
staterankings2022_IDs <- read_excel("C:/Users/annas/OneDrive - The American Council of Trustees Alumni/staterankings2022_IDs.xlsx")
HR_1 <- subset(HR_1, HR_1$UnitID %in% staterankings2022_IDs$IPEDS)

#make sure all "2 families" have a parent with the data
HR_1 %>%
  select(UnitID, Parent.child..indicator...Human.Resources..HR..component., family_type) %>%
  filter(family_type == 2)

#find admins and professors by state - ignore schools with 2, since they will be double counted. their information is in their parent
HR_1 <- HR_1 %>%
  group_by(State.abbreviation.) %>%
  mutate(state_admin = sum(sum_admin[Parent.child..indicator...Human.Resources..HR..component. != 2], na.rm=TRUE))

HR_1 <- HR_1 %>%
  group_by(State.abbreviation.) %>%
  mutate(state_prof = sum(sum_prof[Parent.child..indicator...Human.Resources..HR..component. != 2], na.rm=TRUE))

HR_1$state_ratio <- HR_1$state_admin/HR_1$state_prof

HR_1 <- unique(HR_1[c(11, 20)])
HR_1$rank <- rank(HR_1$state_ratio)


###finance survey


#Spending amounts are calculated using what institutions report to IPEDS as "institutional support" expenses, or 
#those for the "day-to-day operational support of the institution." Institutional support commonly includes costs 
#for executive management, legal departments, fiscal operations, public relations, and development. 
#To calculate administrative spending on a per-student basis, the total state spending is divided by the
#"Full-time equivalent enrollment (Fall enrollment derivation)" variable reported in IPEDS. Higher rankings correspond with lower amounts of per-student spending.
#[Note: Some institutions that operate multiple campuses choose to report spending data in a single record 
#(usually the main campus). In these rare cases, we divided the total spending by all students within the system, 
#even students at schools outside of our survey, to account for the total per-student spending.]

finance_survey_staterankings <- read.csv("C:/Users/annas/OneDrive - The American Council of Trustees Alumni/Desktop/Data_9-13-2022---391.csv")
finance_survey_staterankings$X <- NULL

HR_2 <- read.csv("C:/Users/annas/Downloads/Data_9-12-2022---168.csv")

finance_survey_staterankings$state <- HR_2[match(finance_survey_staterankings$UnitID, HR_2$UnitID), 11]

colnames(finance_survey_staterankings) <- gsub("\\.\\.(FL|F1|D).*", "", colnames(finance_survey_staterankings))

finance_survey_staterankings$familyID <- ifelse(finance_survey_staterankings$Parent.child.indicator...Finance == 2, 
                                                finance_survey_staterankings$ID.number.of.parent.institution...Finance, 
                                                finance_survey_staterankings$UnitID)

finance_survey_staterankings <- finance_survey_staterankings %>%
  group_by(familyID) %>%
  mutate(family_type = max (Parent.child.indicator...Finance, na.rm=TRUE))

finance_survey_staterankings <- finance_survey_staterankings %>%
  group_by(familyID) %>%
  mutate(family_FTE = sum(Full.time.equivalent.fall.enrollment, na.rm=TRUE))

staterankings2022_IDs <- read_excel("C:/Users/annas/OneDrive - The American Council of Trustees Alumni/staterankings2022_IDs.xlsx")
finance_survey_staterankings <- subset(finance_survey_staterankings, finance_survey_staterankings$UnitID %in% staterankings2022_IDs$IPEDS)

finance_survey_staterankings <- finance_survey_staterankings %>%
  group_by(state) %>%
  mutate(state_SS = sum(Student.services...Current.year.total, na.rm=TRUE))

finance_survey_staterankings <- finance_survey_staterankings %>%
  group_by(state) %>%
  mutate(state_admin = sum(Institutional.support...Current.year.total, na.rm=TRUE))

finance_survey_staterankings <- finance_survey_staterankings %>%
  group_by(state) %>%
  mutate(state_fte = sum(family_FTE, na.rm=TRUE))


finance_survey_staterankings$admin_fte_state <- finance_survey_staterankings$state_admin/
                                                  finance_survey_staterankings$state_fte

finance_survey_staterankings$SS_fte_state <- finance_survey_staterankings$state_SS/
  finance_survey_staterankings$state_fte

finance_survey_staterankings <- unique(finance_survey_staterankings[,c(8, 15,16)])



