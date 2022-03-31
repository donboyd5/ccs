

# TODO --------------------------------------------------------------------

# graduation rate percentiles


# links -------------------------------------------------------------------

# https://pad.human.cornell.edu/
# https://pad.human.cornell.edu/schools/datadownload.cfm 

# Please note that Year in the data files refers to the beginning year of the
# school year. E.g. 2010 refers to 2010/2011 school year.

# https://www.nyruralschools.org/


# libraries ---------------------------------------------------------------

library(tidyverse)
tprint <- 50  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

library(fs)

# tools
library(vroom)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(RcppRoll)
library(fredr)
library(tidycensus)

# boyd libraries
library(btools)
library(bdata)
library(bggtools)
library(bmaps)

# graphics
library(scales)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)

# tables
library(knitr)
library(kableExtra)
library(DT)
library(gt)

# maps
library(maps)
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)

# locations ---------------------------------------------------------------
dpad <- r"(E:\data\cornell_pad\)"
dschools <- path(dpad, "schools")


# district identifiers ----------------------------------------------------

ccsid <- "641610"
gwid <- "640801"
saraid <- "521800"
schuyid <- "521701"
locals <- c(ccsid, gwid, saraid, schuyid)


# constants ---------------------------------------------------------------



# mappings ----------------------------------------------------------------

# Subgroup,Description
# 1,All Students
# 2,Male
# 3,Female
# 4,White
# 5,Black or African American
# 6,American Indian or Alaska Native
# 7,Asian or Pacific Islander
# 8,Hispanic or Latino
# 9,Multiracial
# 10,Small Group Total
# 11,Students with Disabilities
# 111,General Education
# 12,Limited English Proficient
# 112,English Proficient
# 13,Economically Disadvantaged
# 113,Not Economically Disadvantaged
# 14,Migrant
# 114,Not Migrant


# download files ----------------------------------------------------------

# https://pad.human.cornell.edu/schools/datadownload.cfm 

# Please note that Year in the data files refers to the beginning year of the
# school year. E.g. 2010 refers to 2010/2011 school year.

# last download: 1/11/2022
# files:
# BasicInfo.csv
# Demographics_all.csv
# Enrollments_all.csv
# ELAMATH_all.csv 
# graduation_all.csv
# regents_all.csv
# apm_all.csv
# FARU_all.csv
# Subgroups codes

# BasicInfo.csv, https://pad.human.cornell.edu/schools/BasicInfo_csv.cfm
# Demographics_all.csv
# Enrollments_all.csv
# ELAMATH_all.csv
# graduation_all.csv
# regents_all.csv
# apm_all.csv
# FARU_all.csv
# Subgroups codes

url <- "https://pad.human.cornell.edu/schools/BasicInfo_csv.cfm"
download.file(url, path(dschools, "BasicInfo2.csv"), mode="wb")

url <- "https://pad.human.cornell.edu/schools/ELAMATH_csv.cfm"
download.file(url, path(dschools, "ELAMATH_all2.csv"), mode="wb")

# bdown=function(url, file){
#   # library('RCurl')
#   f = RCurl::CFILE(file, mode="wb")
#   a = RCurl::curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
#   RCurl::close(f)
#   return(a)
# }
# 
# ## ...and now just give remote and local paths     
# ret = bdown("http://www.example.com/bfile.zip", "path/to/bfile.zip")
# 
# url <- "https://pad.human.cornell.edu/schools/BasicInfo_csv.cfm"
# ret = bdown(url, path(dschools, "BasicInfo3.csv"))


# get data ---------------------------------------------------------------
#.. rules ----
# districtid should be 6 chars leading zeros
# dsb is 6 characters leading zeros
# geoid is 7 characters leading zero
# boces is 4 characters leading zeros
# syear is year ENDING, integer

#.. basic info ----
basic1 <- vroom(path(dschools, "BasicInfo.csv"),
               col_types = cols(.default = col_character()))
glimpse(basic1)

basic2 <- basic1 %>%
  setNames(str_to_lower(names(.))) %>%
  rename(dname=district_name, 
         grades=grade_range, 
         ineeds=needs_index, 
         boces=boces_cd) %>%
  mutate(year=as.integer(year),
         syear=year + 1,
         ineeds=as.integer(ineeds)) %>%
  select(districtid, dsb, geoid, dname, boces, grades, ineeds, year, syear)
glimpse(basic2)

basic <- basic2 %>%
  mutate(dname=str_to_title(dname),
         dname=str_replace(dname,  " Csd", " CSD"),
         dname=str_replace(dname,  " Sd", " SD"),
         dname=str_replace(dname,  "Ufsd", "UFSD"), # note no space so replace anywhere
         dname=str_replace(dname,  "Nyc", "NYC")
  )
unique(basic$dname) %>% sort
glimpse(basic)
saveRDS(basic, here::here("data", "basic.rds"))


#.. subgroups ----
subgroups <- vroom(path(dschools, "Subgroups.csv")) %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(subgroup=as.integer(subgroup))
subgroups

saveRDS(subgroups, here::here("data", "subgroups.rds"))

# Subgroup,Description
# 1,All Students
# 2,Male
# 3,Female
# 4,White
# 5,Black or African American
# 6,American Indian or Alaska Native
# 7,Asian or Pacific Islander
# 8,Hispanic or Latino
# 9,Multiracial
# 10,Small Group Total
# 11,Students with Disabilities
# 111,General Education
# 12,Limited English Proficient
# 112,English Proficient
# 13,Economically Disadvantaged
# 113,Not Economically Disadvantaged
# 14,Migrant
# 114,Not Migrant


#.. enrollment ----
enroll1 <- vroom(path(dschools, "Enrollments_all.csv"),
              col_types = cols(.default = col_character()))
glimpse(enroll1)

enroll2 <- enroll1 %>%
  setNames(str_to_lower(names(.))) %>%
  rename(districtid=distrid) %>%
  mutate(year=as.integer(year),
         across(-c(districtid, year), as.numeric))
glimpse(enroll2)

enroll <- enroll2 %>%
  pivot_longer(cols=-c(districtid, year))

saveRDS(enroll, here::here("data", "enroll.rds"))


#.. demographics ----
demographics1 <- vroom(path(dschools, "Demographics_all.csv"),
                 col_types = cols(.default = col_character()))
glimpse(demographics1)

demographics2 <- demographics1 %>%
  setNames(str_to_lower(names(.))) %>%
  rename(districtid=district) %>%
  mutate(year=as.integer(year),
         across(-c(districtid, year), as.numeric))
glimpse(demographics2)

demographics <- demographics2 %>%
  pivot_longer(cols=-c(districtid, year)) %>%
  mutate(name=str_remove(name, "num_")) %>%
  filter(!is.na(value))
glimpse(demographics)
summary(demographics)  # 1993-2000 year
count(demographics, name)

saveRDS(demographics, here::here("data", "demographics.rds"))


#.. apm not sure what this is ----


#.. FARU == finance ----
finance1 <- vroom(path(dschools, "FARU_all.csv"),
                       col_types = cols(.default = col_character()))
glimpse(finance1)

finance2 <- finance1 %>%
  setNames(str_to_lower(names(.))) %>%
  rename(districtid=district) %>%
  mutate(year=as.integer(year),
         across(-c(districtid, year), as.numeric))
glimpse(finance2)
# note that this has OPEB!!!

finance <- finance2 %>%
  pivot_longer(cols=-c(districtid, year)) %>%
  filter(!is.na(value))

summary(finance)  # 1993-2018 year
count(finance, name) # 55 values

saveRDS(finance, here::here("data", "finance.rds"))



#.. ELA and MATH ----
scores1 <- vroom(path(dschools, "ELAMATH_all.csv"))
glimpse(scores1)

scores <- scores1 %>%
  setNames(str_to_lower(names(.))) %>%
  rename(bedscode=beds_cd, 
         numtested=num_tested, 
         totscore=total_score) %>%
  mutate(bedscode=str_trim(bedscode),
         cocode=str_sub(bedscode, 1, 2) %>% str_trim) %>%
  left_join(info2 %>% filter(year==2016) %>% select(-year), by=c("bedscode")) %>%
  select(cocode, bedscode, dname, year, everything())

scores2 <- scores %>%
  filter(!is.na(dname))

ela8 <- scores2 %>%
  filter(subject=="ELA", grade==8, subgroup==1)
count(scores, year)


count(scores2, subject, grade)

comp <- ela8 %>%
  filter(numtested >= 40) %>%
  group_by(year) %>%
  mutate(ndists=n(), prank=percent_rank(avgscore))

comp %>%
  filter(str_detect_any(dname, c("HASTINGS", "CAMBRIDGE"))) %>%
  select(dname, year, avgscore,  prank) %>%
  arrange(year, dname)

tests <- scores2 %>%
  filter(subject %in% c("ELA", "MATH"), 
         grade %in% c(3, 8), 
         subgroup==1, 
         numtested >= 35) %>%
  unite(type, subject, grade, remove=FALSE) %>%
  group_by(year, type) %>%
  mutate(ndists=n(), prank=percent_rank(avgscore)) %>%
  ungroup
  
count(scores, year)

comp %>%
  filter(str_detect(dname, "CAMBRIDGE")) %>%
  ggplot(aes(year, prank)) +
  geom_line()

tests %>%
  filter(str_detect(dname, "CAMBRIDGE")) %>%
  ggplot(aes(year, prank, colour=type)) +
  geom_line(size=1) +
  geom_point(size=1) +
  theme_bw()

tests %>%
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE"))) %>%
  ggplot(aes(year, prank, colour=type)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = .5) +
  theme_bw() +
  facet_wrap(~dname, ncol=2)

tests %>%
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE", "SCHUYLERV")) |
           dname=="SALEM CSD") %>%
  ggplot(aes(year, prank, colour=dname)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = .5) +
  theme_bw() +
  facet_wrap(~type, ncol=2)


emts <- scores2 %>%
  filter(subject %in% c("ELA", "MATH"), 
         subgroup==1, 
         numtested >= 35) %>%
  unite(type, subject, grade, remove=FALSE) %>%
  group_by(year, type) %>%
  mutate(ndists=n(), prank=percent_rank(avgscore)) %>%
  ungroup

# create cohorts
emts %>%
  filter(subject=="ELA", str_detect(dname, "CAMBRIDGE"), year==2005)

cohorts <- emts %>%
  group_by(dname, subject) %>%
  mutate(cohortg3=year + 3 - grade,
         # student in g3 in 2010 (2010-11) graduates in 2019-2020 (class=2020)
         class=cohortg3 + 10) %>% 
  select(cocode, bedscode, dname, subject, year, cohortg3, class, grade, numtested, prank) %>%
  arrange(dname, subject, cohortg3, grade)

count(cohorts, cohortg3)

cohorts %>%
  filter(class==2023, str_detect_any(dname, c("CAMBRIDGE", "GREENWICH"))) %>%
  ggplot(aes(grade, prank, colour=subject)) +
  geom_line() +
  geom_point() +
  facet_wrap(~dname, nrow = 1)

cohorts %>%
  filter(cohortg3 %in% 2010:2013, str_detect_any(dname, c("CAMBRIDGE"))) %>%
  ggplot(aes(grade, prank, colour=subject)) +
  geom_line() +
  geom_point() +
  facet_wrap(~cohortg3, nrow = 2)

# 641610   CAMBRIDGE CSD

cohorts %>%
  # filter(cohortg3 %in% 2010:2013, str_detect_any(dname, c("CAMBRIDGE"))) %>%
  filter(class %in% 2020:2023, str_detect_any(dname, c("CAMBRIDGE"))) %>%
  ggplot(aes(grade, prank, colour=as.factor(class))) +
  geom_line() +
  geom_point() +
  facet_wrap(~subject, nrow = 2)



# graduation rates --------------------------------------------------------
gr1 <- vroom(path(dschools, "graduation_all.csv"))

gr2 <- gr1 %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(districtid=as.integer(str_trim(district_cd)),
         districtid=str_pad(districtid, width=6, side="left", pad="0"))
summary(gr2) # latest year is 2015

# 641610   CAMBRIDGE CSD
glimpse(gr2)
count(gr2, districtid) %>% ht
unique(gr2$districtid)

gr2 %>%
  filter(districtid=="641610", subgroup==1, outcome_yrs==4) %>%
  mutate(grate=gr_graduated / total) %>%
  select(districtid, cohortye, total, starts_with("gr"))

grates4 <- gr2 %>%
  filter(subgroup==1, outcome_yrs==4) %>%
  group_by(cohortye) %>%
  mutate(grate=gr_graduated / total,
         prank=percent_rank(grate)) %>%
  ungroup %>%
  select(districtid, cohortye, total, starts_with("gr"), prank)
glimpse(grates4)

grates4 %>%
  filter(districtid=="641610") # ends in 2015

grates4 %>%
  filter(districtid=="641610") %>%
  ggplot(aes(cohortye, grate)) +
  geom_line() +
  geom_point()



# enrollment --------------------------------------------------------------

summary(enr2)



enr2 %>%
  filter(districtid %in% locals) %>%
  ggplot(aes(year, total)) +
  geom_point() +
  geom_line() +
  facet_wrap(~districtid, scales="free", ncol=2)

enr2 %>%
  filter(districtid %in% locals) %>%
  group_by(districtid) %>%
  mutate(itotal=total / total[year==2010]) %>%
  ungroup %>%
  ggplot(aes(year, itotal, colour=districtid)) +
  geom_point() +
  geom_line()

enr2 %>%
  filter(districtid == ccsid) %>%
  pivot_longer(-c(districtid, year)) %>%
  filter(name %in% c("g10", "g11", "g12")) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_point() +
  geom_line()

names(enr2)
g1 <- c("k", "g1", "g2")
g2 <- c("g3", "g4", "g5")
g3 <- c("g6", "g7", "g8")
g4 <- c("g9", "g10", "g11")
g5 <- c("unelem", "unsec")

g <- g1

enr2 %>%
  filter(districtid == ccsid) %>%
  pivot_longer(-c(districtid, year)) %>%
  filter(name %in% g) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_point() +
  geom_line()



# BOCES -------------------------------------------------------------------

boces <- scores2 %>%
  filter(bocescode=="6490") %>%
  filter(subject %in% c("ELA", "MATH"), 
         grade %in% c(3, 8), 
         subgroup==1, 
         numtested >= 30) %>%
  unite(type, subject, grade, remove=FALSE) %>%
  group_by(year, type) %>%
  mutate(ndists=n(), rank=rank(-avgscore), prank=percent_rank(avgscore)) %>%
  ungroup

boces %>%
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE"))) %>%
  ggplot(aes(year, prank, colour=dname)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = .5) +
  theme_bw() +
  facet_wrap(~type, ncol=2)

boces %>%
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE"))) %>%
  ggplot(aes(year, rank, colour=dname)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = c(1, 10, 20)) +
  scale_y_reverse() +
  theme_bw() +
  facet_wrap(~type, ncol=2)

count(boces, year, type)

# all tests
alltests <- scores2 %>%
  filter(numtested >= 30) %>%
  unite(type, subject, grade, remove=FALSE) %>%
  group_by(year, type, subgroup) %>%
  mutate(ndists=n(), rank=rank(-avgscore), prank=percent_rank(avgscore)) %>%
  ungroup

allboces <- scores2 %>%
  filter(numtested >= 30, bocescode=="6490") %>%
  unite(type, subject, grade, remove=FALSE) %>%
  group_by(year, type, subgroup) %>%
  mutate(ndists=n(), rank=rank(-avgscore), prank=percent_rank(avgscore)) %>%
  ungroup

ccs <- alltests %>%
  filter(bedscode=="641610") %>%
  filter(year==2018) %>%
  arrange(subgroup, type, year)

ccs2 <- ccs %>%
  select(dname, year, type, subgroup, numtested, avgscore, rank, prank)

xccs <- expression(bedscode=="641610")

count(alltests, subject)

subj <- "ELA"
subj <- "MATH"
alltests %>%
  filter(subject==subj, subgroup==1, eval(xccs)) %>%
  # filter(year==2018) %>%
  filter(year %in% c(seq(1990, 2015, 5), 2016:2018)) %>%
  ggplot(aes(grade, prank, colour=as.factor(year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = .5) +
  ggtitle(subj)

check <- alltests %>%
  filter(subject==subj, subgroup==111, eval(xccs), grade==6) 

subj <- "ELA"
subj <- "MATH"
allboces %>%
  filter(subject==subj, subgroup==1, eval(xccs)) %>%
  # filter(year==2018) %>%
  filter(year %in% c(seq(1990, 2015, 5), 2016:2018)) %>%
  ggplot(aes(grade, prank, colour=as.factor(year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = .5) +
  ggtitle(subj)




# libraries ---------------------------------------------------------------

