

# libraries ---------------------------------------------------------------


library(tidyverse)
tprint <- 50  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

# tools
library(vroom)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(RcppRoll)
library(fredr)
library(btools)
library(tidycensus)

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


# get data ---------------------------------------------------------------
info1 <- read_csv(paste0(dpad, "BasicInfo.csv"))
info <- info1 %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(districtid=str_trim(districtid))

info2 <- info %>%
  rename(bedscode=districtid, dname=district_name, grades=grade_range, ineeds=needs_index, bocescode=boces_cd)

scores1 <- read_csv(paste0(dpad, "ELAMATH_all.csv"))
glimpse(scores1)

scores <- scores1 %>%
  setNames(str_to_lower(names(.))) %>%
  rename(bedscode=beds_cd, numtested=num_tested, totscore=total_score) %>%
  mutate(bedscode=str_trim(bedscode),
         cocode=str_sub(bedscode, 1, 2) %>% str_trim) %>%
  left_join(info2 %>% filter(year==2016) %>% select(-year), by=c("bedscode")) %>%
  select(cocode, bedscode, dname, year, everything())

scores2 <- scores %>%
  filter(!is.na(dname))

ela8 <- scores2 %>%
  filter(subject=="ELA", grade==8, subgroup==1)
count(scores, year)

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
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE"))) %>%
  ggplot(aes(year, prank, colour=dname)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = .5) +
  theme_bw() +
  facet_wrap(~type, ncol=2)


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

# libraries ---------------------------------------------------------------
