# Social Spending ETL
# These codes are deprecated, but I may be use them if something goes wrong

## 2.2 Real GDP Growth --------------------------------------------------
# International Monetary Fund (https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD)

real_gdp <- read_xls("raw_data/imf_real_gdp.xls", sheet = 1, na = 'no data') %>%
  slice(-c(1, 198:231)) %>%
  pivot_longer(cols = c(2:50)) %>%
  rename(country = 1, year = 2, real_gdp = 3) %>%
  dplyr::filter(country != "Kosovo") %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.double(year)) %>%
  relocate(1, 4, 2, 3)

## 2.3 GDP per capita at constant prices --------------------------------
# Source: World Bank
gdp_pcp <- read_xls("raw_data/gdp_pcp_constant.xls", sheet = 1, skip = 2) %>%
  pivot_longer(cols = 5:67) %>%
  select(-c(2:4)) %>%
  rename(country = 1, year = 2, gdp_pcp = 3) %>%
  #filter(country %in% latam_countries) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.double(year))

## 2.8 Unemployment -------------------------------------------------
unemp <- wb_etl(y = 'SL.UEM.TOTL.NE.ZS', w = 1980, z = 2024) %>% 
  rename(unemp = 5)

## 2.9 Inflation --------------------------------------------------------
inflation <- wb_etl(y = 'FP.CPI.TOTL.ZG', w = 1980, z = 2024) %>% 
  rename(inflation = 5)