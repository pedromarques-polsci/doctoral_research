# Packages ----------------------------------------------------------------
if(require(countrycode) == F) install.packages('countrycode'); require(countrycode)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(gridExtra) == F) install.packages('gridExtra'); require(gridExtra)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(Hmisc) == F) install.packages('Hmisc'); require(Hmisc)
if(require(priceR) == F) install.packages('priceR'); require(readxl)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)

# Important conflicts
# dplyr::filter(), Hmisc::sumarize()

# 1. SOCIAL SPENDING -------------------------------------------

## 1.1 Social Spending per capita ------------------------------------------
# Economic Commission for Latin America and the Caribbean # Statistics > Demographic and Social
social_spending <- read_xlsx("raw_data/cepal_social_spending_pcp_constant.xlsx", sheet = 1, col_types = c(
  "text", "text", "text", "text", "numeric", "numeric", "text", "text", "numeric")
) %>%
  select(c(-1, -2, -7, -8, -9)) %>%
  pivot_wider(names_from = 2,
              values_from = value,
              values_fill = NA) %>%
  rename(country = 1, year = 2, cult_pcp = 3, edu_pcp = 4, soc_pcp = 5, sprot_pcp = 6, health_pcp = 7, house_pcp = 8, envir_pcp = 9) %>%
  dplyr::filter(country != "Caribbean")

social_spending$country[social_spending$country == "Bolivia (Plurinational State of)"] <- 'Bolivia'

tmp <- data.frame(year=rep(seq(min(social_spending$year),
                                max(social_spending$year)),
                            each=length(unique(social_spending$country))),
                  country=unique(social_spending$country))

tmp2 <- data.frame(year=rep(seq(min(social_spending$year),
                               max(social_spending$year)),
                            each=3),
country= c('Venezuela', 'Puerto Rico', 'Peru'))

social_spending <- merge(social_spending, tmp, by=c('year','country'),all=T) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  relocate(iso3c, country, year, soc_pcp) %>%
  arrange(country, year)

social_spending <- merge(social_spending, tmp2, by=c('year','country'),all=T) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  relocate(iso3c, country, year, soc_pcp) %>%
  arrange(country, year)

# dv_sample <- unique(social_spending$country)

## 1.2 Current Social Spending ----------------------------
socialx_current <- read_xlsx("raw_data/cepal_social_spending_current.xlsx") %>%
  filter(.[[2]] %in% c('Central government', 'General government'),
         .[[3]] %in% c('Venezuela (Bolivarian Republic of)', 'Peru'),
         .[[4]] == 'Social expenditure') %>%
select(3:6) %>%
  pivot_wider(names_from = 2,
              values_from = value,
              values_fill = NA) %>%
  rename(country = 1, year = 2, soc_nom = 3) %>%
  arrange(country, year) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"))
# The only data available to Peru is General Government

# 2. POLITICAL ECONOMY ---------------------------------------------------
## 2.1 Terms of trade ------------------------------------------------------
# Bertrand Gruss; Suhaib Kebhaj. (2019) "Commodity Terms of Trade: A New Database"
terms_of_trade <- read.csv2("raw_data/terms_of_trade_net_exports.csv", sep = ",", dec = ".") %>%
  select(1, 3, 5, 10) %>% # Commodity Net Export Price Index, Individual Commodities Weighted by Ratio of Net Exports to GDP (xm_gdp)
  dplyr::filter(Type.Name == "Historical, Rolling Weights, Index") %>%
  rename(country = 1, year = 3, commtot = 4) %>%
  select(-Type.Name) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

setdiff(unique(social_spending$country), unique(terms_of_trade$country))

## 2.2 Real GDP Growth ------------------------------------------------------
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

## 2.3 GDP per capita at constant prices ------------------------------------
# Source: World Bank
gdp_pcp <- read_xls("raw_data/gdp_pcp_constant.xls", sheet = 1, skip = 2) %>%
  pivot_longer(cols = 5:67) %>%
  select(-c(2:4)) %>%
  rename(country = 1, year = 2, gdp_pcp = 3) %>%
  filter(country %in% unique(social_spending$country)) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.double(year))

## 2.4 Government Revenue Dataset ----------------------------------------
gov_revenue <- read_dta('raw_data/unu_gov_revenue.dta') %>%
  filter(country != 'Kosovo') %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),       year = as.double(year))
         
## 2.5 Central Government Debt -------------------------------------------
cgov_debt <- read_xls("raw_data/imf_central_debt.xls", sheet = 1, na = 'no data') %>%
  slice(-c(1, 176:177)) %>%
  pivot_longer(cols = c(2:73)) %>%
  rename(country = 1, year = 2, cgov_debt = 3) %>%
  dplyr::filter(country != "Kosovo") %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.double(year)) %>%
  relocate(1, 4, 2, 3)

# 3. NON-CONTRIBUTORY POLICIES -------------------------------------------
# Economic Commission for Latin America and the Caribbean
# Non-contributory Social Protection Programmes Database
## 3.1 Conditional Cash Transfer -----------------------------------------
ncp_cct <- "https://dds.cepal.org/bpsnc/cct" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(cct = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

ncp_cct$year <- substr(x = gsub(pattern = '\\D', "", ncp_cct$cct), 1, 4) # '\\D' removes non-numerical characters

ncp_cct$year[ncp_cct$cct == "Aid Brazil Programme"] <- 2021

## 3.2 Non-contributory pensions -----------------------------------------
ncp_sp <- "https://dds.cepal.org/bpsnc/sp" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(sp = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

ncp_sp$year <- substr(x = gsub(pattern = '\\D', "", ncp_sp$sp), 1, 4)

## 3.3 Labour inclusion programmes ---------------------------------------
ncp_lpi <- "https://dds.cepal.org/bpsnc/lpi" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(lpi = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

ncp_lpi$year <- substr(x = gsub(pattern = '\\D', "", ncp_lpi$lpi), 1, 4)

# 4. ELECTION RESULTS -----------------------------------------------------
## 4.1 Database of Political Institutions ----------------------------------
dpi <- read.csv2("raw_data/dpi.csv", sep = ",") %>%
  rename(country = countryname) %>%
  mutate(year = as.double(year),
         maj = as.numeric(maj)) %>%
  # There are no iso3c for the following countries
  dplyr::filter(country != 'GDR',
         country != "Yemen People's Republic",
         country != "Yugoslavia") %>%
  mutate(country = ifelse(country == 'Cent. Af. Rep.', 'Central African Republic', country),
         country = ifelse(country == 'Dom. Rep.', 'Dominican Republic', country),
         country = ifelse(country == 'PRC', "People's Republic of China", country),
         country = ifelse(country == 'PRK', "Korea (the Democratic People's Republic of)", country),
         country = ifelse(country == 'ROK', 'Republic of Korea', country),
         country = ifelse(country == 'S. Africa', 'South Africa', country),
         )%>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

setdiff(unique(social_spending$country), unique(dpi$country))

## 4.2 Leaders Global  ---------------------------------------------------------
# Dupont, Nils; Doring, Holger; Bederke, Paul. (2021). "Leaders Global: Party affiliations of leaders (HoS/HoG) in 183 countries, 1880–2020"

leadglob <- read.csv2("raw_data/leadglob.csv", sep = ",") %>%
  mutate(year = as.double(year)) %>%
  # There are no iso3c for the following countries below
  dplyr::filter(country != 'German Democratic Republic',
                country != 'Kosovo',
                country != 'Somaliland',
                country != 'German Democratic Republic',
                country != "Yemen People's Republic",
                country != "Zanzibar",
                country != "Vietnam, South",
                country != "Czechoslovakia") %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

leadglob <- leadglob[!duplicated(leadglob[c("country","year")]),] # Removing duplicates

setdiff(unique(social_spending$country), unique(leadglob$country))

# 5. PARTY IDEOLOGY -------------------------------------------------------
## 5.1. V-Party --------------------------------------------------------------
# Staffan I. Lindberg et al. (2022) “Codebook Varieties of Party Identity and Organization (V–Party) V2”
vparty <- readRDS("raw_data/v_party.rds") %>%
  rename(country = country_name) %>%
  dplyr::filter(country %in% unique(social_spending$country)) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

# 6. DATA ENRICHMENT ----------------------------------------------------------
## 6.1 Political Institutions --------------------------------------------------
db_socialx_pcp <- dpi %>%
  select(country, iso3c, year, system, maj) %>% # Maj = Margin of Majority
  right_join(social_spending) %>%
  arrange(country, iso3c, year)

## 6.2 Elections ---------------------------------------------------------------
# Government party
db_socialx_pcp <- leadglob %>%
  mutate(year = as.double(year)) %>%
  select(iso3c, country, year, HoS_name, HoS_party_short, HoS_party_english, HoS_party_id, HoG_name, HoG_party_short, HoG_party_english, HoG_party_id) %>%
  right_join(db_socialx_pcp) %>%
  arrange(country, iso3c, year)

db_socialx_pcp <- db_socialx_pcp %>%
  mutate(leader = ifelse(system == 'Presidential', HoS_name, NA),
         leader = ifelse(system == 'Assembly-Elected President', HoS_name, leader),
         leader = ifelse(system == 'Parliamentary', HoG_name, leader),
         party_name = ifelse(system == 'Presidential', HoS_party_english, NA),
         party_name = ifelse(system == 'Assembly-Elected President', HoS_party_english, party_name),
         party_name = ifelse(system == 'Parliamentary', HoG_party_english, party_name),
         pf_party_id = ifelse(system == 'Presidential', HoS_party_id, NA),
         pf_party_id = ifelse(system == 'Assembly-Elected President', HoS_party_id, pf_party_id),
         pf_party_id = ifelse(system == 'Parliamentary', HoG_party_id, pf_party_id),
         party_short = ifelse(system == 'Presidential', HoS_party_short, NA),
         party_short = ifelse(system == 'Assembly-Elected President', HoS_party_short, party_short),
         party_short = ifelse(system == 'Parliamentary', HoG_party_short, party_short)) 
  
db_socialx_pcp <- db_socialx_pcp %>% 
  select(-HoS_name, -HoS_party_short, -HoS_party_english, -HoS_party_id, -HoG_name, -HoG_party_short, -HoG_party_english, -HoG_party_id)

## 6.3 Party Ideology -------------------------------------------------------------
db_socialx_pcp <- db_socialx_pcp %>%
   mutate(year1 = year)

vparty_sel <- vparty %>%
  select(iso3c, country, year, pf_party_id, v2pariglef_ord, v2pawelf_ord, v2paclient_ord, v2pagroup_2, v2pagroup_3, v2palocoff_ord, v2paactcom_ord, v2pasoctie_ord, v2paind_ord) %>%
   mutate(year2 = year)
 
db_socialx_pcp <- db_socialx_pcp %>%
   left_join(vparty_sel,
           by = join_by(country, pf_party_id, closest(year1 >= year2))) %>%
  mutate(region = countrycode(country, origin = "country.name", destination = "region23"),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

db_socialx_pcp$region[db_socialx_pcp$country == "Mexico"] <- 'North America'

db_socialx_pcp <- db_socialx_pcp %>%
  select(-c(iso3c.x, iso3c.y, year.x, iso3c.y, year.y, year2)) %>%
  rename(year = year1)
  
## 6.5 Non-Contributory Policies -----------------------------------------------
ncp_cct <- ncp_cct %>%
  group_by(country, year) %>%
  mutate(year = as.double(year)) %>%
  dplyr::summarise(n_cct=n()
  )

ncp_lpi <- ncp_lpi %>%
  group_by(country, year) %>%
  mutate(year = as.double(year)) %>%
  dplyr::summarise(n_lpi=n()
  )

ncp_sp <- ncp_sp %>%
  group_by(country, year) %>%
  mutate(year = as.double(year)) %>%
  dplyr::summarise(n_sp=n()
  )

db_socialx_pcp <- db_socialx_pcp %>%
  left_join(ncp_sp) %>%
  left_join(ncp_cct) %>%
  left_join(ncp_lpi)

db_socialx_pcp <- db_socialx_pcp %>%
  mutate(n_cct = coalesce(n_cct, 0),
         n_sp = coalesce(n_sp, 0),
         n_lpi = coalesce(n_lpi, 0),
         n_ncp = n_cct + n_sp + n_lpi)

## 6.6 Economic variables ------------------------------------------------------
db_socialx_pcp <- db_socialx_pcp %>%
  left_join(terms_of_trade) %>%
  left_join(real_gdp) %>%
  left_join(gdp_pcp) %>%
  left_join(cgov_debt)

db_socialx_pcp <- gov_revenue %>%
  select(country, iso3c, year, tax_inc_sc, nrtax_inc_sc) %>%
  right_join(db_socialx_pcp)

db_socialx_pcp <- db_socialx_pcp %>%
  relocate(region, system, iso3c, country, year, commtot, soc_pcp, cult_pcp, edu_pcp, sprot_pcp, health_pcp, house_pcp, envir_pcp, n_sp, n_cct, n_lpi, n_ncp, leader, party_name, pf_party_id, party_short, maj, v2pariglef_ord, v2pawelf_ord, v2paclient_ord, v2pagroup_2, v2pagroup_3, v2paind_ord, v2palocoff_ord, v2paactcom_ord, v2pasoctie_ord, v2paind_ord, real_gdp) %>%
  arrange(region, country, year)

# 7. DATASET EXPORT -----------------------------------------------------------
write_excel_csv2(db_socialx_pcp, "final_data/db_socialx_pcp.csv", na = '')
saveRDS(db_socialx_pcp, "final_data/db_socialx_pcp.RDS")
write_dta(db_socialx_pcp, "final_data/db_socialx_pcp.dta")

# CODEBOOK ----------------------------------------------------------------
db_socialx_pcp <- readRDS('final_data/db_socialx_pcp.RDS')
latin <- c("Cuba", "Dominican Republic", "Haiti", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Mexico", "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")

db_socialx_pcp <- db_socialx_pcp %>%
  mutate(macroregion = case_when(country %in% latin ~ 'latin',
                                 country %in% setdiff(unique(db_socialx_pcp$country), 
                                                      latin) ~ 'non_latin'))

x <- db_socialx_pcp %>%
  dplyr::filter(year %in% 1990:2019,
                macroregion == 'latin')

colSums(!is.na(db_socialx_pcp)) %>%
  View()

colSums(!is.na(x)) %>%
  View()

# LEFTOVERS ---------------------------------------------------------------
## The codes below are preserved for future reference.
## Select a desired chunk and press "Ctrl + Shift + C" to undo its hashtags. 

## 1.2 Social Spending (%Total Public Spending) -------------------------------
# Public spending by function ------------------------------------------
# public_spending <- read_xlsx("raw_data/cepal_public_spending.xlsx") %>%
#   select(c(-1, -2, -7, -8, -9)) %>%
#   pivot_wider(names_from = 2,
#               values_from = value,
#               values_fill = NA) %>%
#   rename(country = 1, year = 2, total_bdg = 3, def_bdg = 4, envir_bdg = 5, health_bdg = 6, cult_bdg = 7, edu_bdg = 8, sprot_bdg = 9, statdisc = 10) %>%
#   mutate(def_p = def_bdg * 100 / total_bdg,
#          welfare_p = (health_bdg + edu_bdg + sprot_bdg)*100 / total_bdg) %>%
#   arrange(country, year) %>%
#   mutate(country = countryname(country),
#          iso3c = countrycode(country, origin = "country.name", 
#                              destination = "iso3c")) %>%
#   relocate(13, 1, 2, 3:12)