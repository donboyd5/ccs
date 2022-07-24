
# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
dpad <- r"(E:\data\cornell_pad\)"
dschools <- path(dpad, "schools")
dnysed <- r"(E:\data\nysed\)"

# get locale (rural) markings ----------------------------------------------------------------
sedcodes_xwalk <- readRDS(here::here("data", "sedcodes_xwalk.rds")) # beds codes and nces codes
locales <- readRDS(here::here("data", "nylocales.rds"))

glimpse(sedcodes_xwalk)
glimpse(locales)

marked <- sedcodes_xwalk |> 
  left_join(locales, by=c("EDEN_NCES_LEA_ID" = "leaid")) |> 
  select(instid=Institution_ID, sedcode=SED_Code, sdcode=SchDistofLoc_Code, leaid=EDEN_NCES_LEA_ID,
         legalname=Legal_Name, ncesname=name,
         cocode=County_Code, coname=County_Description,
         cnty, nmcnty,
         locale, localef)
marked |> filter(is.na(ncesname))

marked |> filter(coname=="WASHINGTON")
marked |> filter(coname=="WASHINGTON") |> select(sdcode, legalname, coname, locale, localef)
saveRDS(marked, here::here("data", "sedcodes_locales.rds"))


# put locale marking on enrollment data -----------------------------------
marked <- readRDS(here::here("data", "sedcodes_locales.rds"))
enroll <- readRDS(here::here("data", "enrollwide.rds"))

glimpse(marked)
glimpse(enroll)

enroll_marked <- enroll |>
  left_join(marked |> 
              select(districtid=sdcode, leaid, legalname, cnty, nmcnty, locale, localef), 
                     by="districtid")

saveRDS(enroll_marked, here::here("data", "enroll_marked.rds"))



# investigate -------------------------------------------------------------

df <- readRDS(here::here("data", "enroll_marked.rds"))
count(df, locale, localef)

df |> 
  filter(locale %in% c("41", "42", "43")) |> 
  group_by(year) |>
  summarise(total=sum(total)) |> 
  ggplot(aes(year, total)) +
  geom_line() +
  geom_point() 
  








