
# get rural indicators and other district-level info from NCES

## NCES ----
# https://nces.ed.gov/programs/edge/Geographic/SchoolLocations
# https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICLEA_2021.zip
# EDGE_GEOCODE_PUBLICLEA_2021.zip
#   EDGE_GEOCODE_PUBLICLEA_2021.xlsx
#   EDGE_GEOCODE_PUBLICLEA_2021.TXT
#   EDGE_GEOCODE_PUBLICLEA_2021.sas7bdat



# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
library(haven)
library(labelled)

# locations ---------------------------------------------------------------
dnces <- r"(E:\data\nces\)"


# get geocode data --------------------------------------------------------
fnz <- "EDGE_GEOCODE_PUBLICLEA_2021.zip"
zdir <- "EDGE_GEOCODE_PUBLICLEA_2021" # the directory within the zip archive
fn <- "EDGE_GEOCODE_PUBLICLEA_2021.sas7bdat"

df <- read_sas(unz(description = path(dnces, fnz), filename = path(zdir, fn)))
glimpse(df)

df2 <- df |> 
  lcnames() |> 
  select(leaid, name, stabbr=state, cnty, nmcnty, cbsa, nmcbsa, cbsatype, csa, nmcsa, syear=schoolyear, locale, starts_with("pct_"))
glimpse(df2)
count(df2, stabbr)
count(df2, locale)
attributes(df2$pct_rural42) 
dictionary <- labelled::generate_dictionary(df2)

# create a factor from locale
f <- function(s) str_extract(s, "(?<=\\()([^()]*?)(?=\\)[^()]*$)")
# f("abc(def)ghi")

labs <- dictionary |> 
  filter(str_detect(variable, "pct_")) |> 
  select(variable, label) |> 
  mutate(value=str_sub(variable, -2, -1),
         vlab=f(label))
labs


df3 <- df2 |> 
  filter(stabbr=="NY") |> 
  mutate(localef=factor(locale, levels=labs$value, labels=labs$vlab)) |> 
  relocate(localef, .after=locale)
glimpse(df3)
count(df3, locale, localef)localecount(df3, locale, localef)

saveRDS(df3, here::here("data", "nylocales.rds"))

# zap_label(x) to get rid of variable labels
# https://raw.githubusercontent.com/rstudio/cheatsheets/main/labelled.pdf
attr(df3$pct_rural41, 'label')
df3$pct_rural41 |> attr('label')



as_factor(names(df3))

count(df3, locale)

attributes(df3$pct_rural42) 
attributes(df3)

tmp <- df3 |> 
  filter(str_detect(name, coll("Cambridge", ignore_case = TRUE)))
