

# TODO --------------------------------------------------------------------

# graduation rate percentiles


# links -------------------------------------------------------------------

## NCES ----
# https://nces.ed.gov/programs/edge/Geographic/SchoolLocations

## NYSED ----
# https://www.p12.nysed.gov/irs/schoolDirectory/
# https://www.oms.nysed.gov//sedref/home.html
# https://eservices.nysed.gov/sedreports/list?id=1
# https://cognos.nysed.gov/ibmcognos/bi/v1/disp?b_action=cognosViewer&ui.tool=CognosViewer&ui.action=run&ui.object=storeID(%27iE2F9F18D05114622ABE308D0AA265CE9%27)&cv.header=false&cv.toolbar=false&run.outputFormat=HTML

# https://data.nysed.gov/downloads.php
# https://www.p12.nysed.gov/irs/statistics/enroll-n-staff/home.html
# https://www.oms.nysed.gov//sedref/documents/GRADE-Organization-DESC.pdf
# https://www.p12.nysed.gov/irs/pmf/
# https://www.p12.nysed.gov/irs/pmf/PersonnelMasterFileStatisticalRuns2019-20.xlsx  has needs-resource codes

# https://data.ny.gov/browse?q=state%20education%20department&sortBy=relevance
# https://data.ny.gov/Government-Finance/New-York-State-School-Aid-Beginning-School-Year-19/9pb8-dg53


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
dnysed <- r"(E:\data\nysed\)"
# dschools <- path(dpad, "schools")
# dnysed <- r"(E:\data\nyschools\)"


# id crosswalk ------------------------------------------------------------
# https://eservices.nysed.gov/sedreports/list?id=1  landing page
# https://eservices.nysed.gov/sedreports/view?rpt=%2Fcontent%2Ffolder%5B%40name%3D%27NYSED+Reports%27%5D%2Ffolder%5B%40name%3D%27SEDREF%27%5D%2Ffolder%5B%40name%3D%27SEDREF+Reports+for+Public+Website%27%5D%2Freport%5B%40name%3D%27School+Districts%2C+Public+%26+Charter+Schools%3A+NCES+IDs%27%5D&format=CSV&reportId=iE0680D8ED23E4088B5F288AEE1B63E23
fn <- "School Districts, Public & Charter Schools_ NCES IDs.csv"

# character info (from Emeditor)
# 8
# U+0038
# UTF-16LE: 0x0038
# DIGIT EIGHT
# Unicode Script: Zyyy (Common)
# Unicode General Category: Nd (Decimal Number)
# File position: 1,896 bytes


# get district ids --------------------------------------------------------
# the col_names are not valid
ids1 <- vroom(path(dnysed, fn),
             col_types = cols(.default = col_character()),
             # col_names = FALSE,
             locale=locale(encoding="UTF-16LE"),
             delim="\t", trim_ws = TRUE)
names(ids1)
(vnames <- str_replace_all(names(ids1), " ", "_"))
# [1] "Institution_ID"            "Legal_Name"                "Popular_Name"              "SED_Code"                 
# [5] "SchDistofLoc_Code"         "SchDistofLoc_Description"  "County_Code"               "County_Description"       
# [9] "INST_Type_Code"            "INST_Type_Description"     "INST_Sub_Type_Code"        "INST_Sub_Type_Description"
# [13] "Active_Date"               "Inactive_Date"             "EDEN_NCES_LEA_ID"          "EDEN_LEA_Type_CODE"       
# [17] "EDEN_LEA_OP_Status_Code"   "EDEN_NCES_SCH_ID"          "EDEN_Sch_Type_Code"        "EDEN_Sch_OP_Status_Code"  

ids2 <- ids1 |> 
  setNames(vnames)

count(ids2, INST_Type_Code, INST_Type_Description)
# INST_Type_Code INST_Type_Description     n
# <chr>          <chr>                 <int>
#   1 10             GOVERNMENT AGENCIES      53
# 2 16             SCHOOL DISTRICTS        734
# 3 17             PUBLIC SCHOOLS         5291
# 4 18             BOCES                    38
# 5 21             CHILD NUTRITION           5

count(ids2, INST_Type_Code, INST_Type_Description, INST_Sub_Type_Code, INST_Sub_Type_Description)

count(ids2 |> filter(INST_Type_Code=="16"), INST_Type_Code, INST_Type_Description, INST_Sub_Type_Code, INST_Sub_Type_Description)
# INST_Type_Code INST_Type_Description INST_Sub_Type_Code INST_Sub_Type_Description     n
# <chr>          <chr>                 <chr>              <chr>                     <int>
#   1 16             SCHOOL DISTRICTS      1                  CITY                         89
# 2 16             SCHOOL DISTRICTS      10                 100% CONTRACT                 3
# 3 16             SCHOOL DISTRICTS      2                  UNION FREE                   62
# 4 16             SCHOOL DISTRICTS      3                  INDEPENDENT UNION FREE       86
# 5 16             SCHOOL DISTRICTS      4                  CENTRAL                     268
# 6 16             SCHOOL DISTRICTS      5                  COMMON                        8
# 7 16             SCHOOL DISTRICTS      6                  CITY CENTRAL                  7
# 8 16             SCHOOL DISTRICTS      7                  INDEPENDENT CENTRAL         195
# 9 16             SCHOOL DISTRICTS      8                  SPECIAL ACT                  13
# 10 16             SCHOOL DISTRICTS      9                  CENTRAL HIGH SCHOOL           3

# convert all-numeric codes to integers for more-convenient sorting
ids3 <- ids2 |> 
  mutate(across(c(INST_Type_Code, INST_Sub_Type_Code), as.integer))

# focus on school districts
sdids1 <- ids3 |> 
  filter(INST_Type_Code==16) |> 
  mutate(across(c(Active_Date, Inactive_Date), as.Date))

summary(sdids1)
# which codes can we drop?
count(sdids1, EDEN_LEA_Type_CODE) # keep
count(sdids1, EDEN_LEA_OP_Status_Code) # keep
count(sdids1, EDEN_NCES_SCH_ID) # all NA, drop
count(sdids1, EDEN_Sch_Type_Code)  # all NA, drop
count(sdids1, EDEN_Sch_OP_Status_Code)  # all NA, drop

sdids2 <- sdids1 |> 
  select(-c(EDEN_NCES_SCH_ID, EDEN_Sch_Type_Code, EDEN_Sch_OP_Status_Code))

saveRDS(sdids2, here::here("data", "sedcodes_xwalk.rds"))
