
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

df2 <-df |> 
  arrange(year, districtid) |> 
  group_by(year, districtid) |> 
  filter(row_number()==1) |> 
  ungroup()

df2 |> 
  filter(locale %in% c("41", "42", "43")) |> 
  group_by(year) |>
  summarise(total=sum(total)) |> 
  ggplot(aes(year, total)) +
  geom_line() +
  geom_point() 

df2 |> 
  filter(locale %in% c("41", "42", "43")) |> 
  group_by(year) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point() 
  

df2 |> 
  filter(!is.na(locale)) |> 
  mutate(locale=as.integer(locale),
         locgrp=case_when(locale %in% 11:13 ~ "city",
                          locale %in% 21:23 ~ "suburb",
                          locale %in% 31:33 ~ "town",
                          locale %in% 41:43 ~ "rural",
                          TRUE ~ "error"
                            )) |> 
  filter(locgrp != "city") |> 
  group_by(year, locgrp) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value / value[year==1995], colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels=scales::percent_format(accuracy = .1))

df2 |> 
  filter(!is.na(locale)) |> 
  mutate(locale=as.integer(locale),
         locgrp=case_when(locale %in% 11:13 ~ "city",
                          locale %in% 21:23 ~ "suburb",
                          locale %in% 31:33 ~ "town",
                          locale %in% 41:43 ~ "rural",
                          TRUE ~ "error"
         )) |> 
  filter(locgrp != "city") |> 
  group_by(year, locgrp) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(0, NA), labels=scales::comma_format())

df2 |> 
  filter(!is.na(locale)) |> 
  mutate(locale=as.integer(locale),
         locgrp=case_when(districtid=="641610" ~ "Cambridge",
                          locale %in% 11:13 ~ "city",
                          locale %in% 21:23 ~ "suburb",
                          locale %in% 31:33 ~ "town",
                          locale %in% 41:43 ~ "rural",
                          TRUE ~ "error"
         )) |> 
  filter(locgrp %in% c("rural", "Cambridge")) |> 
  group_by(year, locgrp) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(0, NA), labels=scales::comma_format())


df2 |> 
  filter(locale %in% c("41", "42", "43")) |> 
  group_by(year) |>
  summarise(value=median(g1)) |> 
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point() 

df2 |> 
  filter(locale %in% c("41", "42", "43")) |> 
  select(year, g1, g12) |> 
  pivot_longer(-year) |> 
  group_by(year, name) |>
  summarise(value=median(value)) |> 
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() 

df2 |> 
  filter(locale %in% c("41", "42", "43")) |> 
  select(year, g1, g12) |> 
  pivot_longer(-year) |> 
  mutate(year=ifelse(name=="g12", year, year + 11)) |> 
  group_by(year, name) |>
  summarise(value=median(value)) |> 
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() 

df2 |> 
  filter(locale %in% c("41", "42", "43")) |> 
  select(year, districtid, g1, g12) |> 
  pivot_longer(-c(year, districtid)) |> 
  mutate(year=ifelse(name=="g12", year, year + 11)) |> 
  group_by(year, districtid) |> 
  pivot_wider() |> 
  mutate(ratio=g12 / g1) |> 
  group_by(year) |>
  summarise(ratio=median(ratio, na.rm=TRUE)) |> 
  ggplot(aes(year, ratio)) +
  geom_line() +
  geom_point() 


df2 |> 
  filter(!is.na(locale)) |> 
  mutate(locale=as.integer(locale),
         locgrp=case_when(districtid=="641610" ~ "Cambridge",
                          locale %in% 11:13 ~ "city",
                          locale %in% 21:23 ~ "suburb",
                          locale %in% 31:33 ~ "town",
                          locale %in% 41:43 ~ "rural",
                          TRUE ~ "error"
         )) |> 
  filter(locgrp %in% c("rural", "Cambridge")) |> 
  group_by(year, locgrp) |>
  summarise(value=median(g12)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 120, 10), limits=c(0, NA), labels=scales::comma_format())

df2 |> 
  filter(!is.na(locale)) |> 
  mutate(locale=as.integer(locale),
         locgrp=case_when(locale %in% 11:13 ~ "city",
                          locale %in% 21:23 ~ "suburb",
                          locale %in% 31:33 ~ "town",
                          locale %in% 41:43 ~ "rural",
                          TRUE ~ "error"
         )) |> 
  filter(locgrp != "city") |> 
  group_by(year, locgrp) |>
  summarise(value=p25(g12)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 300, 10), limits=c(0, NA), labels=scales::comma_format())


df2 |> 
  filter(!is.na(locale)) |> 
  mutate(locale=as.integer(locale),
         locgrp=case_when(locale %in% 11:13 ~ "city",
                          locale %in% 21:23 ~ "suburb",
                          locale %in% 31:33 ~ "town",
                          locale %in% 41:43 ~ "rural",
                          TRUE ~ "error"
         )) |> 
  filter(locgrp == "rural") |> 
  group_by(year, locgrp) |>
  summarise(p25=p25(g12),
            p50=p50(g12),
            p75=p75(g12)) |> 
  pivot_longer(-c(year, locgrp)) |> 
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 300, 10), limits=c(0, NA), labels=scales::comma_format())


