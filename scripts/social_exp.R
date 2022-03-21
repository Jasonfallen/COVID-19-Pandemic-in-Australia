#### Preamble ####
# Purpose: Clean the survey data downloaded from [https://www.abs.gov.au/statistics/people/people-and- communities/general-social-survey-summary-results-australia/2020#data-download]
# Author: Haocheng Xu, Jing Li
# Data: 15 March, 2021
# Contact: haocheng.xu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####

library(knitr)
library(tidyverse)
res <- data.frame(Response=c("Fully Responding", "Refusal", "Non-Response", "Part-Response", "Total Non-Response", "Sample Size"), 
                  Number=c(5304, 79, 3327, 58, 3464, 8768), 
                  Proportion=c(60.5, 0.9, 37.9, 0.7, 39.5, 100))
knitr::kable(res, caption="2020 GSS Response Distribution")

# Reading the open source data file from package
data <- readxl::read_xlsx("data/GSS_Table2.xlsx", sheet=4, skip=6)
data.total <- readxl::read_xlsx("data/GSS_Table2.xlsx", sheet=2, skip=6)

# Preparing the data: extract each question's response into a separate data frame
#     and convert the first column value type from string to double
#     also separate male and female according to structrue of the XLSX file
comm_involvement_m <- data[4:6,1:6]
comm_involvement_m$`15�C24` <- as.double(comm_involvement_m$`15�C24`)
comm_groups_involvement_m <- data[9:11,1:6]
comm_groups_involvement_m$`15�C24` <- as.double(comm_groups_involvement_m$`15�C24`)
comm_say_m <- data[14:16,1:6]
comm_say_m$`15�C24` <- as.double(comm_say_m$`15�C24`)
comm_support_m <- data[19:23,1:6]
comm_support_m$`15�C24` <- as.double(comm_support_m$`15�C24`)
comm_cultural_tolerance_m <- data[26:27,1:6]
comm_cultural_tolerance_m$`15�C24` <- as.double(comm_cultural_tolerance_m$`15�C24`)
trust_people_m <- data[31:33,1:6]
trust_people_m$`15�C24` <- as.double(trust_people_m$`15�C24`)
trust_hc_system_m <- data[36:38,1:6]
trust_hc_system_m$`15�C24` <- as.double(trust_hc_system_m$`15�C24`)
trust_police_m <- data[41:43,1:6]
trust_police_m$`15�C24` <- as.double(trust_police_m$`15�C24`)
trust_justice_sys_m <- data[46:48,1:6]
trust_justice_sys_m$`15�C24` <- as.double(trust_justice_sys_m$`15�C24`)
stressors_m <- data[51:54,1:6]
stressors_m$`15�C24` <- as.double(stressors_m$`15�C24`)
crime_and_safety_m <- data[57:58,1:6]
crime_and_safety_m$`15�C24` <- as.double(crime_and_safety_m$`15�C24`)
health_m <- data[62:64,1:6]
health_m$`15�C24` <- as.double(health_m$`15�C24`)

comm_involvement_f <- data[74:76,1:6]
comm_involvement_f$`15-24` <- as.double(comm_involvement_f$`15-24`)
comm_groups_involvement_f <- data[79:81,1:6]
comm_groups_involvement_f$`15-24` <- as.double(comm_groups_involvement_f$`15-24`)
comm_say_f <- data[84:86,1:6]
comm_say_f$`15-24` <- as.double(comm_say_f$`15-24`)
comm_support_f <- data[89:93,1:6]
comm_support_f$`15-24` <- as.double(comm_support_f$`15-24`)
comm_cultural_tolerance_f <- data[96:97,1:6]
comm_cultural_tolerance_f$`15-24` <- as.double(comm_cultural_tolerance_f$`15-24`)
trust_people_f <- data[101:103,1:6]
trust_people_f$`15-24` <- as.double(trust_people_f$`15-24`)
trust_hc_system_f <- data[106:108,1:6]
trust_hc_system_f$`15-24` <- as.double(trust_hc_system_f$`15-24`)
trust_police_f <- data[111:113,1:6]
trust_police_f$`15-24` <- as.double(trust_police_f$`15-24`)
trust_justice_sys_f <- data[116:118,1:6]
trust_justice_sys_f$`15-24` <- as.double(trust_justice_sys_f$`15-24`)
stressors_f <- data[121:124,1:6]
stressors_f$`15-24` <- as.double(stressors_f$`15-24`)
crime_and_safety_f <- data[127:128,1:6]
crime_and_safety_f$`15-24` <- as.double(crime_and_safety_f$`15-24`)
health_f <- data[132:134,]
health_f$`15-24` <- as.double(health_f$`15-24`)

total_male <- data.total[70,1:6]; total_male$`15-24` <- as.double(total_male$`15-24`)
total_female <- data.total[140,1:6]; total_female$`15-24` <- as.double(total_female$`15-24`)
total_ppl <- data.total[210,1:6]


#### Drawing Figures ####

## Figure 1
total_male %>%
  pivot_longer(-...1, names_to="age_group", values_to="count") %>%
  mutate(Sex="M") %>%
  bind_rows(total_female %>% 
              pivot_longer(-...1, names_to="age_group", values_to="count") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=count, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") +
  labs(
    x="Sex", y="Estimasted Number of Individuals, in Thousands"
  )

## Figure 2
comm_involvement_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(comm_involvement_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question Category: Community Involvement"
  )

## Figure 3
comm_groups_involvement_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(comm_groups_involvement_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question: Has been involved in groups in the last 12 months"
  )

## Figure 4
comm_support_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(comm_support_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question Category: Family and community support"
  )

## Figure 5
trust_people_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(trust_people_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question: Feels most people can be trusted"
  )

## Figure 6
trust_hc_system_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(trust_hc_system_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question: Feels the healthcare system can be trusted"
  )

## Figure 7
trust_police_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(trust_police_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question: Feels police can be trusted"
  )

## Figure 8
trust_justice_sys_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(trust_justice_sys_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question: Feels the justice system can be trusted"
  )

## Figure 9
stressors_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(stressors_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question Category:	Stressors"
  )

## Figure 10
crime_and_safety_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(crime_and_safety_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question Category:	Crime and Safety"
  )

## Figure 11
health_m %>%
  pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
  mutate(Sex="M") %>%
  bind_rows(health_f %>%
              pivot_longer(-...1, names_to="age_group", values_to="proportion") %>%
              mutate(Sex="F")) %>%
  rename(Response=...1) %>%
  ggplot(aes(x=Response, y=proportion, fill=age_group)) +
  geom_bar(stat="identity", width=.8, position="dodge") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  facet_wrap(~Sex) + 
  labs(
    x="Response", y="Proportion", 
    title="Question Category:	Self-assessed health status"
  )
