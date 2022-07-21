

# TODO --------------------------------------------------------------------

# graduation rate percentiles


# links -------------------------------------------------------------------

## NCES ----
# https://nces.ed.gov/programs/edge/Geographic/SchoolLocations

## Cornell ----
# https://pad.human.cornell.edu/
# https://pad.human.cornell.edu/schools/datadownload.cfm 

# Please note that Year in the data files refers to the beginning year of the
# school year. E.g. 2010 refers to 2010/2011 school year.

# https://www.nyruralschools.org/

# Jan Vink, Cornell PAD (3/31/2022): This is main page I use to download enrollment data:
#   https://www.p12.nysed.gov/irs/statistics/enroll-n-staff/home.html

# graduation rates:
# http://www.nysed.gov/news/2022/state-education-department-releases-2017-cohort-high-school-graduation-rates
# ...2017 cohort, those students who first entered 9th grade in New York’s public schools in 2017
# this means:
#   cohortye is the year a student first entered 9th grade in NY public schools


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
dpad <- r"(E:\data\cornell_pad\)"
dschools <- path(dpad, "schools")
dnysed <- r"(E:\data\nyschools\)"


# download files ----------------------------------------------------------

# https://pad.human.cornell.edu/schools/datadownload.cfm 

# Please note that Year in the data files refers to the beginning year of the
# school year. E.g. 2010 refers to 2010/2011 school year.

# last download: 7/20/2022
# on left is the name Cornell associates with a file, on right is its url

fnames <- read_csv("fname, url
BasicInfo.csv,            https://pad.human.cornell.edu/schools/BasicInfo_csv.cfm
Demographics_all.csv,     https://pad.human.cornell.edu/schools/demogr_csv.cfm
Enrollments_all.csv,      https://pad.human.cornell.edu/schools/enr_csv.cfm
graduation_all.csv,       https://pad.human.cornell.edu/schools/grad_csv.cfm
regents_all.csv,          https://pad.human.cornell.edu/schools/regents_csv.cfm
apm_all.csv,              https://pad.human.cornell.edu/schools/apm_csv.cfm
FARU_all.csv,             https://pad.human.cornell.edu/schools/FARU_csv.cfm
Subgroups.csv,          https://pad.human.cornell.edu/schools/download/Subgroups.csv
ELAMATH_all.csv,          https://pad.human.cornell.edu/schools/ELAMATH_csv.cfm
")
# note that I put ELAMATH last because it might timeout

tout <- getOption("timeout")
options(timeout=180)
#for(fnum in 1:nrow(fnames)){
for(fnum in 8){
  fname <- fnames$fname[fnum] |> str_to_lower()
  url <- fnames$url[fnum]
  print(fname)
  download.file(url, path(dschools, fname), mode="wb")
}
options(timeout=tout)


# constants ---------------------------------------------------------------



# district identifiers ----------------------------------------------------

ccsid <- "641610"
gwid <- "640801"
saraid <- "521800"
schuyid <- "521701"
locals <- c(ccsid, gwid, saraid, schuyid)



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


# get data ---------------------------------------------------------------
## rules ----
# districtid should be 6 chars leading zeros
# dsb is 6 characters leading zeros
# geoid is 7 characters leading zero
# boces is 4 characters leading zeros
# syear is year ENDING, integer

### basic info ----
basic1 <- vroom(path(dschools, "basicinfo.csv"),
               col_types = cols(.default = col_character()))
glimpse(basic1)
count(basic1, YEAR) # all data are from 2016-17

basic2 <- basic1 |>
  lcnames() |>
  rename(dname=district_name, 
         grades=grade_range, 
         ineeds=needs_index, 
         boces=boces_cd) |>
  mutate(ineeds=as.integer(ineeds)) |>
  select(districtid, dsb, geoid, dname, boces, grades, ineeds)
glimpse(basic2)

# do we need dsb AND districtid?
basic2 |> filter(districtid != dsb) # none
basic2 |> filter(is.na(districtid)) # none
basic2 |> filter(is.na(dsb)) # about 8
# no, we can drop dsb

basic <- basic2 |>
  mutate(dname=str_to_title(dname),
         dname=str_replace(dname,  " Csd", " CSD"),
         dname=str_replace(dname,  " Sd", " SD"),
         dname=str_replace(dname,  "Ufsd", "UFSD"), # note no space so replace anywhere
         dname=str_replace(dname,  "Nyc", "NYC")
  ) |> 
  select(-dsb)
unique(basic$dname) |> sort()
glimpse(basic)
saveRDS(basic, here::here("data", "basic.rds"))


### subgroups ----
subgroups <- vroom(path(dschools, "subgroups.csv")) |>
  lcnames() |> 
  mutate(subgroup=as.integer(subgroup))
subgroups

# subgroup description                     
# <int> <chr>                           
# 1        1 All Students                    
# 2        2 Male                            
# 3        3 Female                          
# 4        4 White                           
# 5        5 Black or African American       
# 6        6 American Indian or Alaska Native
# 7        7 Asian or Pacific Islander       
# 8        8 Hispanic or Latino              
# 9        9 Multiracial                     
# 10       10 Small Group Total               
# 11       11 Students with Disabilities      
# 12      111 General Education               
# 13       12 Limited English Proficient      
# 14      112 English Proficient              
# 15       13 Economically Disadvantaged      
# 16      113 Not Economically Disadvantaged  
# 17       14 Migrant                         
# 18      114 Not Migrant   

saveRDS(subgroups, here::here("data", "subgroups.rds"))

### enrollment ----
# Cornell (Jan Vink) source for the data is:
#   https://www.p12.nysed.gov/irs/statistics/enroll-n-staff/ArchiveEnrollmentData.html
#   Note that some of the web data are slightly updated vs Jan's data
# Plus, he only has some subsets of the data, not the crosses (e.g., not white-female)

enroll1 <- vroom(path(dschools, "enrollments_all.csv"),
              col_types = cols(.default = col_character()))
glimpse(enroll1)

enroll2 <- enroll1 |>
  lcnames() |> 
  rename(districtid=distrid) |>
  mutate(year=as.integer(year),
         across(-c(districtid, year), as.numeric))
glimpse(enroll2)

enroll <- enroll2 |>
  pivot_longer(cols=-c(districtid, year))

ht(enroll)

saveRDS(enroll, here::here("data", "enroll.rds"))

#### enrollwide ----
enrollwide <- enroll2
saveRDS(enrollwide, here::here("data", "enrollwide.rds"))


### demographics ----
# Cornell (Jan Vink) source:
#   https://www.p12.nysed.gov/irs/statistics/enroll-n-staff/ArchiveEnrollmentData.html
#   Note that some of the web data are slightly updated vs Jan's data
demographics1 <- vroom(path(dschools, "Demographics_all.csv"),
                 col_types = cols(.default = col_character()))
glimpse(demographics1)

demographics2 <- demographics1 |>
  lcnames() |> 
  rename(districtid=district) |>
  mutate(year=as.integer(year),
         across(-c(districtid, year), as.numeric))
glimpse(demographics2)

demographics <- demographics2 |>
  pivot_longer(cols=-c(districtid, year)) |>
  mutate(name=str_remove(name, "num_"),
         name=str_remove(name, "_")) |>
  filter(!is.na(value))
glimpse(demographics)
summary(demographics)  # 1993-2000 year
count(demographics, name)

demographics |> 
  filter(year==2019, districtid=="010100")

# subgroups, labels; vname
# 01  All Students; ? not included in this data, but in enrollment instead
# 02	Female; female
# 03	Male; male

# 04	American Indian/Alaska Native; amind
# 05	Black; black
# 06	Hispanic; hisp
# 07	Asian/Pacific Islander; asian
# 08	White; white
# 09	Multiracial; multi

# 10	General Education Students; ?? by subtraction?
# 11	Students with Disabilities; swd

# 12	Not English Language Learner; ?? by subtraction ?
# 13	English Language Learner; ell

# 15	Economically Disadvantaged; ecdis
# 16	Not Economically Disadvantaged; ???? - by subtraction?

# not found: lep, freelunch, reducedlunch
# lep: Limited English Proficient subgroup 12

saveRDS(demographics, here::here("data", "demographics.rds"))

#.... demo wide ----
demowide <- demographics |> 
  pivot_wider()
saveRDS(demowide, here::here("data", "demowide.rds"))


#.. FARU == finance ----
# https://stateaid.nysed.gov/st3/2020-2021_School_Year_2021-2022_SAMS_Blank_ST-3.htm

#.... finance NYSED ----
# https://www.oms.nysed.gov/faru/Profiles/profiles_cover.html
# ST-3 info
#   https://www.oms.nysed.gov/faru/Profiles/AGuidetotheHeadingsoftheFiscalProfile.htm
# NYSED data source: 
#   https://www.oms.nysed.gov/faru/Profiles/Masterfiles93-94to19-20_final_consistentformulasnopivot.xlsx
# glimpse(finwide)
# add enrollment 

# get mappping of colheadings to vnames
mapfn <- "Masterfiles93-94to19-20_final_consistentformulasnopivot_djb.xlsx"
vmap1 <- read_excel(path(dnysed, mapfn), sheet="vnames", range="a1:bj2")
vmap <- vmap1 |> 
  pivot_longer(cols=everything(), names_to = "colhead", values_to = "vname") |> 
  select(vname, colhead)
vmap
saveRDS(vmap, here::here("data", "finsedvmap.rds"))
vmap <- readRDS(here::here("data", "finsedvmap.rds"))


finfn <- "Masterfiles93-94to19-20_final_consistentformulasnopivot.xlsx"
df <- read_excel(path(dnysed, finfn))
df2 <- df |> 
  select(all_of(vmap$colhead)) |> 
  setNames(vmap$vname) |> 
  mutate(syear=str_sub(syear6, 1, 4) |> as.integer(),
         syear=syear + 1)
ns(df2)
# Total Fringe Benefits. The sum of Teacher Retirement, Health, and Other Fringe Benefits.
finsedwide <- df2
saveRDS(finsedwide, here::here("data", "finsedwide.rds"))



#.... finance Cornell ----

finance1 <- vroom(path(dschools, "FARU_all.csv"),
                       col_types = cols(.default = col_character()))
glimpse(finance1)

finance2 <- finance1 |>
  lcnames() |> 
  rename(districtid=district) |>
  mutate(year=as.integer(year),
         across(-c(districtid, year), as.numeric))
glimpse(finance2)
names(finance2)

#.... finance wide Cornell ----
finwide <- finance2
saveRDS(finwide, here::here("data", "finwide.rds"))


#.... finance long Cornell ----
finance <- finance2 |>
  pivot_longer(cols=-c(districtid, year)) |>
  filter(!is.na(value))

summary(finance)  # 1993-2018 year
count(finance, name) # 55 values

saveRDS(finance, here::here("data", "finance.rds"))


#.. ELA and MATH ----
elamath1 <- vroom(path(dschools, "ELAMATH_all.csv"),
                  col_types = cols(.default = col_character()))
glimpse(elamath1)
count(elamath1, SUBJECT)
count(elamath1, SUBGROUP)
count(elamath1, GRADE)
count(elamath1, YEAR)

elamath2 <- elamath1 |>
  lcnames() |> 
  rename(districtid=beds_cd, 
         numtested=num_tested, 
         totscore=total_score) |>
  mutate(districtid=str_pad(districtid, width=6, side="left", pad="0"),
         across(c(year, subgroup, grade, numtested), as.integer),
         across(c(starts_with("level"), totscore, avgscore), as.numeric)) |>
  select(districtid, year, subject, grade, subgroup, everything())
summary(elamath2) # looks like -1 should be set to NA
quantile(elamath2$totscore)
elamath2 |> filter(totscore < 0) # totscore looks suspicious

elamath3 <- elamath2 |>
  mutate(across(c(starts_with("level"), avgscore), ~ ifelse(.x==-1, NA_real_, .x)),
         level12=naz(level1) + naz(level2),
         level34=naz(level3) + naz(level4),
         totscore=ifelse(totscore < 0, NA_real_, totscore))
summary(elamath3)

elamath <- elamath3 |>
  pivot_longer(cols=c(numtested,
                      starts_with("level"),
                      totscore, avgscore))

saveRDS(elamath, here::here("data", "elamath.rds"))


#.. graduation  --------------------------------------------------------
grad1 <- vroom(path(dschools, "graduation_all.csv"),
               col_types = cols(.default = col_character()))
glimpse(grad1)
count(grad1, `_NAME_`)  # all na
count(grad1, SRCYEAR)
count(grad1, OUTCOME_YRS)

grad2 <- grad1 |>
  select(-`_NAME_`) |>
  lcnames() |> 
  mutate(districtid=str_pad(district_cd, width=6, side="left", pad="0"),
         cohortye=as.integer(cohortye),
         subgroup=as.integer(subgroup),
         outcome_yrs=as.numeric(outcome_yrs),
         across(c(total, starts_with("gr_")), as.numeric))
summary(grad2) # looks like -1 should be converted to NA

grad3 <- grad2 |> 
  select(-district_cd) |> 
  pivot_longer(cols=-c(districtid, cohortye, srcyear, subgroup, outcome_yrs)) |>
  mutate(value=ifelse(value==-1, NA_real_, value)) |>
  filter(!is.na(value)) |>
  select(districtid, cohortye, srcyear, subgroup, outcome_yrs, name, value)
summary(grad3) # latest year is 2015
# cohortye is the year a student first entered 9th grade in NY public schools
# so enter 9th in 2015 means grad in 2019 (generally)
count(grad3, outcome_yrs)

grad3 |>
  filter(cohortye==2015) |>
  count(srcyear) # 201906 for all, which happens to be the grad year for typical person

graduation <- grad3

saveRDS(graduation, here::here("data", "graduation.rds"))


#.. regents ----


#.. apm not sure what this is ----


# get previously saved data -----------------------------------------------
files <- c("basic", 
           "demographics", "demowide",
           "elamath",
           "enroll", "enrollwide",
           "finance", "finwide",
           "finsedwide", "finsedvmap",
           "graduation", "subgroups")
for(file in files){
  print(file)
  assign(file, readRDS(here::here("data", paste0(file, ".rds"))))
}

# basic <- readRDS(here::here("data", "basic.rds"))

# make a finance file -------------------
glimpse(finsedwide)

check <- finsedwide |> 
  filter(districtid==ccsid) |> 
  mutate(syear=str_sub(syear6, 1, 4) |> 
           as.integer() + 1) |> 
  select(districtid, dname, syear, subtotexp, fbtchretire, fbemphealth, fbtotal, dcaadm) |> 
  left_join(enrollwide |> 
              mutate(syear=year + 1) |> 
              select(districtid, syear, enroll=total),
            by = c("districtid", "syear"))

check |> 
  pivot_longer(cols=c(dcaadm, enroll)) |> 
  ggplot(aes(syear, value, colour=name)) +
  geom_line() +
  geom_point()

check |> 
  mutate(xfb=subtotexp - fbtotal,
         across(c(xfb, starts_with("fb")),
                ~ .x / enroll))
  





glimpse(basic)
glimpse(enrollwide)
fin2 <- finwide |> 
  left_join(basic |> select(districtid, dname, ineeds),
            by = "districtid") |> 
  left_join(enrollwide |> select(districtid, year, enroll=total),
            by = c("districtid", "year"))

fin2 |> 
  filter(districtid==ccsid) |> 
  select(districtid, dname, year, totrev, texp, tchr, heal) |>
  mutate(val=tchr,
         val=val / val[year==2010]) |> 
  ggplot(aes(year, val)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=seq(0, 2, .05))

fin2 |> 
  filter(districtid==ccsid) |> 
  select(districtid, dname, year, totrev, texp, tchr, heal) |>
  mutate(val=heal / texp) |> 
  ggplot(aes(year, val)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 2, .01))



# merge or stack files -------------------------------------------------------------------
glimpse(basic)  # 1 per district: geoid, dname, boces, grades included, and ineeds

glimpse(demographics) # gender, race, economic disadvantage
count(demographics, year) # 1993:2020
count(demographics, name) # enrollment counts of various types
# gender; race: amind, asian, black, hisp, white, multi; 
glimpse(demowide)

glimpse(enrollwide)

glimpse(finance)
count(finance, name)
glimpse(finwide)
ns(finwide)

finwide |> 
  filter(districtid==ccsid)

# state_aid_total_revenues state_revenues_total_revenues loc_eff_rate
finwide |> 
  filter(districtid==ccsid) |> 
  ggplot(aes(year, loc_eff_rate)) +
  geom_line() +
  geom_point()


# pipe investigation ----
# basic |> names(.) # YES
# basic |> names(_)  # NO
# basic |> names(x=_) # YES
# basic |> names() # YES
basic
# df <- tibble(YEAR=2000:2010, X=10:20, y=30:40)
# df |> lcnames()
# 
# vnames <- names(basic)
# vnames[1] <- "aaa"
# vnames
# basic |>
#   setNames(vnames) # YES
# basic |>
#   setNames(object=_, vnames) # YES

# f <- function(df) {
#   vnames <- str_to_lower(names(df))
#   setNames(df, vnames)
#   }
# basic |>
#   f(df=.) # YES
# basic |>
#   f(df=_)  # YES
# basic |>
#   f()  # YES


# basic |>
#   setNames(names(x=_) |> str_to_lower())
# 
# basic |>
#   rename_with(.fn=str_to_lower())
# 
# basic |> 
#   rename_with(., names(.), .fn=str_to_lower())



# play 1 --------------------------------------------------------------------


gr2 |>
  filter(districtid=="641610", subgroup==1, outcome_yrs==4) |>
  mutate(grate=gr_graduated / total) |>
  select(districtid, cohortye, total, starts_with("gr"))

grates4 <- gr2 |>
  filter(subgroup==1, outcome_yrs==4) |>
  group_by(cohortye) |>
  mutate(grate=gr_graduated / total,
         prank=percent_rank(grate)) |>
  ungroup |>
  select(districtid, cohortye, total, starts_with("gr"), prank)
glimpse(grates4)

grates4 |>
  filter(districtid=="641610") # ends in 2015

grates4 |>
  filter(districtid=="641610") |>
  ggplot(aes(cohortye, grate)) +
  geom_line() +
  geom_point()



# enrollment --------------------------------------------------------------

summary(enr2)



enr2 |>
  filter(districtid %in% locals) |>
  ggplot(aes(year, total)) +
  geom_point() +
  geom_line() +
  facet_wrap(~districtid, scales="free", ncol=2)

enr2 |>
  filter(districtid %in% locals) |>
  group_by(districtid) |>
  mutate(itotal=total / total[year==2010]) |>
  ungroup |>
  ggplot(aes(year, itotal, colour=districtid)) +
  geom_point() +
  geom_line()

enr2 |>
  filter(districtid == ccsid) |>
  pivot_longer(-c(districtid, year)) |>
  filter(name %in% c("g10", "g11", "g12")) |>
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

enr2 |>
  filter(districtid == ccsid) |>
  pivot_longer(-c(districtid, year)) |>
  filter(name %in% g) |>
  ggplot(aes(year, value, colour=name)) +
  geom_point() +
  geom_line()



# BOCES -------------------------------------------------------------------

boces <- scores2 |>
  filter(bocescode=="6490") |>
  filter(subject %in% c("ELA", "MATH"), 
         grade %in% c(3, 8), 
         subgroup==1, 
         numtested >= 30) |>
  unite(type, subject, grade, remove=FALSE) |>
  group_by(year, type) |>
  mutate(ndists=n(), rank=rank(-avgscore), prank=percent_rank(avgscore)) |>
  ungroup

boces |>
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE"))) |>
  ggplot(aes(year, prank, colour=dname)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = .5) +
  theme_bw() +
  facet_wrap(~type, ncol=2)

boces |>
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE"))) |>
  ggplot(aes(year, rank, colour=dname)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = c(1, 10, 20)) +
  scale_y_reverse() +
  theme_bw() +
  facet_wrap(~type, ncol=2)

count(boces, year, type)

# all tests
alltests <- scores2 |>
  filter(numtested >= 30) |>
  unite(type, subject, grade, remove=FALSE) |>
  group_by(year, type, subgroup) |>
  mutate(ndists=n(), rank=rank(-avgscore), prank=percent_rank(avgscore)) |>
  ungroup

allboces <- scores2 |>
  filter(numtested >= 30, bocescode=="6490") |>
  unite(type, subject, grade, remove=FALSE) |>
  group_by(year, type, subgroup) |>
  mutate(ndists=n(), rank=rank(-avgscore), prank=percent_rank(avgscore)) |>
  ungroup

ccs <- alltests |>
  filter(bedscode=="641610") |>
  filter(year==2018) |>
  arrange(subgroup, type, year)

ccs2 <- ccs |>
  select(dname, year, type, subgroup, numtested, avgscore, rank, prank)

xccs <- expression(bedscode=="641610")

count(alltests, subject)

subj <- "ELA"
subj <- "MATH"
alltests |>
  filter(subject==subj, subgroup==1, eval(xccs)) |>
  # filter(year==2018) |>
  filter(year %in% c(seq(1990, 2015, 5), 2016:2018)) |>
  ggplot(aes(grade, prank, colour=as.factor(year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = .5) +
  ggtitle(subj)

check <- alltests |>
  filter(subject==subj, subgroup==111, eval(xccs), grade==6) 

subj <- "ELA"
subj <- "MATH"
allboces |>
  filter(subject==subj, subgroup==1, eval(xccs)) |>
  # filter(year==2018) |>
  filter(year %in% c(seq(1990, 2015, 5), 2016:2018)) |>
  ggplot(aes(grade, prank, colour=as.factor(year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = .5) +
  ggtitle(subj)


# play ---------------------------------------------------------------


scores2 <- scores |>
  filter(!is.na(dname))

ela8 <- scores2 |>
  filter(subject=="ELA", grade==8, subgroup==1)
count(scores, year)


count(scores2, subject, grade)

comp <- ela8 |>
  filter(numtested >= 40) |>
  group_by(year) |>
  mutate(ndists=n(), prank=percent_rank(avgscore))

comp |>
  filter(str_detect_any(dname, c("HASTINGS", "CAMBRIDGE"))) |>
  select(dname, year, avgscore,  prank) |>
  arrange(year, dname)

tests <- scores2 |>
  filter(subject %in% c("ELA", "MATH"), 
         grade %in% c(3, 8), 
         subgroup==1, 
         numtested >= 35) |>
  unite(type, subject, grade, remove=FALSE) |>
  group_by(year, type) |>
  mutate(ndists=n(), prank=percent_rank(avgscore)) |>
  ungroup

count(scores, year)

comp |>
  filter(str_detect(dname, "CAMBRIDGE")) |>
  ggplot(aes(year, prank)) +
  geom_line()

tests |>
  filter(str_detect(dname, "CAMBRIDGE")) |>
  ggplot(aes(year, prank, colour=type)) +
  geom_line(size=1) +
  geom_point(size=1) +
  theme_bw()

tests |>
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE"))) |>
  ggplot(aes(year, prank, colour=type)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = .5) +
  theme_bw() +
  facet_wrap(~dname, ncol=2)

tests |>
  filter(str_detect_any(dname, c("GREENWICH", "CAMBRIDGE", "SCHUYLERV")) |
           dname=="SALEM CSD") |>
  ggplot(aes(year, prank, colour=dname)) +
  geom_line(size=1) +
  geom_point(size=1) +
  geom_hline(yintercept = .5) +
  theme_bw() +
  facet_wrap(~type, ncol=2)


emts <- scores2 |>
  filter(subject %in% c("ELA", "MATH"), 
         subgroup==1, 
         numtested >= 35) |>
  unite(type, subject, grade, remove=FALSE) |>
  group_by(year, type) |>
  mutate(ndists=n(), prank=percent_rank(avgscore)) |>
  ungroup

# create cohorts
emts |>
  filter(subject=="ELA", str_detect(dname, "CAMBRIDGE"), year==2005)

cohorts <- emts |>
  group_by(dname, subject) |>
  mutate(cohortg3=year + 3 - grade,
         # student in g3 in 2010 (2010-11) graduates in 2019-2020 (class=2020)
         class=cohortg3 + 10) |> 
  select(cocode, bedscode, dname, subject, year, cohortg3, class, grade, numtested, prank) |>
  arrange(dname, subject, cohortg3, grade)

count(cohorts, cohortg3)

cohorts |>
  filter(class==2023, str_detect_any(dname, c("CAMBRIDGE", "GREENWICH"))) |>
  ggplot(aes(grade, prank, colour=subject)) +
  geom_line() +
  geom_point() +
  facet_wrap(~dname, nrow = 1)

cohorts |>
  filter(cohortg3 %in% 2010:2013, str_detect_any(dname, c("CAMBRIDGE"))) |>
  ggplot(aes(grade, prank, colour=subject)) +
  geom_line() +
  geom_point() +
  facet_wrap(~cohortg3, nrow = 2)

# 641610   CAMBRIDGE CSD

cohorts |>
  # filter(cohortg3 %in% 2010:2013, str_detect_any(dname, c("CAMBRIDGE"))) |>
  filter(class %in% 2020:2023, str_detect_any(dname, c("CAMBRIDGE"))) |>
  ggplot(aes(grade, prank, colour=as.factor(class))) +
  geom_line() +
  geom_point() +
  facet_wrap(~subject, nrow = 2)
