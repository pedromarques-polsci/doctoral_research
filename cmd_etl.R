# Packages --------------------------------------------------------------
if(require(comtradr) == F) install.packages('comtradr'); require(comtradr)
if(require(concordance) == F) install.packages('concordance'); require(concordance)
if(require(countrycode) == F) install.packages('countrycode'); require(countrycode)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(xts) == F) install.packages('xts'); require(xts)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

# Potential conflicts
# dplyr::filter(), Hmisc::sumarize()

# SUMMARY --------------------------------------------------------------
# The current script is divided by three major parts 
# The first one builds our commodity price dataset by aggregating three sources.
# The second part extracts commodity trade data directly from UN Comtrade
# dataset through its API.
# The final one ties the previous databases together and builds up a Terms of Trade Index inspired by Gruss and Kebhaj's (2019) methodology,
# although our index has some minor, yet important differences.
# The current version of this code is 19/03/2024 13:52

# 1. COMMODITY PRICES --------------------------------------------------------
## 1.1 Source - IMF --------------------------------------------------------
commodity_code <- read_xlsx('raw_data/commodity_codes.xlsx', sheet = 1, skip = 1) %>%
  rename('commodity' = 1,
         'price_code' = 2)

index <- 1:nrow(commodity_code)

commodity_code <- commodity_code %>%
  mutate(id = index) %>%
  relocate(id) %>%
  filter(!id %in% c(90, 96, 98:100, 103:106, 109, 111))

commodity_code[commodity_code$id == '23', 'commodity'] <- 'Hard Logs'
commodity_code[commodity_code$id == '26', 'commodity'] <- 'Soft Logs'
commodity_code[commodity_code$id == '24', 'commodity'] <- 'Hard Sawn'
commodity_code[commodity_code$id == '27', 'commodity'] <- 'Soft Sawn'

commodity_full <- read_xlsx('raw_data/commodity_full.xlsx', sheet = 1, skip = 1) %>%
  rename('commodity' = 1,
         'fullname' = 2)

commodity_full <- commodity_full %>%
  mutate(id = index) %>%
  relocate(id) %>%
  filter(!id %in% c(90, 96, 98:100, 103:106, 109, 111))

commodity_full[commodity_full$id == '23', 'commodity'] <- 'Hard Logs'
commodity_full[commodity_full$id == '26', 'commodity'] <- 'Soft Logs'
commodity_full[commodity_full$id == '24', 'commodity'] <- 'Hard Sawn'
commodity_full[commodity_full$id == '27', 'commodity'] <- 'Soft Sawn'

commodity_prices <- read_xlsx('raw_data/cmd_raw_prices.xlsx', sheet = 1, skip = 1) %>%
  select(-seq(from = 3, to = 69, by = 2)) %>%
  rename('commodity' = 1)

commodity_prices <- commodity_prices %>%
  mutate(id = index) %>%
  relocate(id) %>%
  filter(!id %in% c(90, 96, 98:100, 103:106, 109, 111))

commodity_prices[commodity_prices$id == '23', 'commodity'] <- 'Hard Logs'
commodity_prices[commodity_prices$id == '26', 'commodity'] <- 'Soft Logs'
commodity_prices[commodity_prices$id == '24', 'commodity'] <- 'Hard Sawn'
commodity_prices[commodity_prices$id == '27', 'commodity'] <- 'Soft Sawn'

commodity_prices <- commodity_prices %>%
  left_join(commodity_code) %>%
  left_join(commodity_full) %>%
  relocate(id, commodity, price_code, fullname) %>%
  pivot_longer(cols = 5:38,
               names_to = 'year',
               values_to = 'cmd_price')

# We aggregate wood prices, given that trade data does not discriminate between Soft/Hard Sawnwood/Logs
wood <- commodity_prices %>% 
  filter(price_code == 'PTIMB') %>% 
  mutate(price_code = 'PWMEAN',
         fullname = 'Wood Prices Mean (Soft/Hard Sawnwood, Soft/Hard Logs',
         commodity = 'Wood')
  

logsk <- commodity_prices %>% 
  filter(price_code == 'PLOGSK')

sawmal <- commodity_prices %>% 
  filter(price_code == 'PSAWMAL')

logore <- commodity_prices %>% 
  filter(price_code == 'PLOGORE')

sawore <- commodity_prices %>% 
  filter(price_code == 'PSAWORE')

# The aggregation is done by taking the mean price
for (i in 1:34) {
  wood[i,6] <- (logsk[i,6] + sawmal[i,6] + logore[i,6] + sawore[i,6])/4
}

commodity_prices <- commodity_prices %>% bind_rows(wood)

## 1.2 Source - UNCTAD -----------------------------------------------------
unctad_prices <- read_xlsx('raw_data/unctad_commodity_prices.xlsx') %>%
  rename('year' = 1,
         'price_code' = 2,
         'fullname' = 3,
         'cmd_price' = 4) %>%
  select(-5, -6) %>% 
  mutate(year = as.character(year),
         cmd_price = as.double(cmd_price)) %>% 
  filter(price_code == "240100.01")

## 1.3 Source - FRED -----------------------------------------------------------
orange_prices <- read_xlsx('raw_data/fred_orange_prices.xlsx', sheet = 1, skip = 10) %>%
  rename(year = 1,
         cmd_price = 2) %>%
  mutate(year = format(year, format="%Y"),
         commodity = 'Orange',
         price_code = 'PORANGUSDM',
         fullname = 'U.S. Dollars per Pound')

## 1.4 Joining both datasets -----------------------------------------------
commodity_prices <- commodity_prices %>%
  bind_rows(unctad_prices, orange_prices)

commodity_prices[commodity_prices$price_code == '240100.01', 'price_code'] <- 'PTOBAC'
commodity_prices[commodity_prices$price_code == 'PTOBAC', 'id'] <- 111
commodity_prices[commodity_prices$price_code == 'PTOBAC', 'commodity'] <- 'Tobacco'
commodity_prices[commodity_prices$price_code == 'PORANGUSDM', 'id'] <- 112

## 1.5 Exporting prices dataset --------------------------------------------
saveRDS(commodity_prices, "final_data/commodity_prices.RDS")
write_excel_csv2(commodity_prices, "final_data/commodity_prices.csv", na = '')
#write_dta(commodity_prices, "final_data/commodity_prices.dta")

# 2. COMMODITY TRADE  ------------------------------------------------------

## 2.1 Set Up --------------------------------------------------------------
# Using my Comtrade Key to get access to data
set_primary_comtrade_key("81ac679de5e0407585798c83b93a1236")

# Reference tables
hs_stic_table <- hs_sitc3
hs_table <- comtradr::ct_get_ref_table('HS')
s3_table <- comtradr::ct_get_ref_table('S3')

## 2.2 Data extraction ---------------------------------------------------------

# Harmonized Commodity Description and Coding System
hs_code_vector <- c(
  # Agricultural materials
  "5201", "41", "4001", "4401", "4403", "4406", "2401", "5101", 
  
  # Food and beverages
  "0803", '1003', '0201', '0105', '1801', '0901', '1005', '160411', 
  '230120', '1202', '020430', '020410', '020423', '1509', '080510', '1511', 
  '0203', '1514', '1006', '030613', '2304', '1507', '1201', '1701', '1512', 
  '0902', '1001', 
  
  # Energy
  '2709', '2701', '2711',
  
  # Metals
  '2606', '7601', '2603', '7402', '7108', '2601', '2607', '2604',	
  '2609', '8001', '2612', '2608', '7901'
  )


price_code_vector <- c("PCOTTIND", "PHIDE", "PRUBB", # Commodity price codes
        'PWMEAN', 'PWMEAN', 'PWMEAN',
        'PTOBAC', 'PWOOLC',
        
        # Food and bevarages
        'PBANSOP', 'PBARL', 'PBEEF', 'PPOULT', 'PCOCO', 'PCOFFOTM', 'PMAIZMT',
        'PSALM', 'PFSHMEAL', 'PGNUTS', 'PLAMB', 'PLAMB', 'PLAMB', 'POLVOIL', 'PORANGUSDM',
        'PPOIL', 'PPORK', 'PROIL', 'PRICENPQ', 'PSHRI', 'PSMEA', 'PSOIL',
        'PSOYB', 'PSUGAISA', 'PSUNO', 'PTEA', 'PWHEAMT',
        
        # Energy
        'POILAPSP', 'PCOALAU', 'PNGASEU',
        
        # Metals
        'PALUM', 'PALUM', 'PCOPP', 'PCOPP', 'PGOLD', 'PIORECR', 'PLEAD',
        'PNICK', 'PTIN', 'PTIN', 'PURAN', 'PZINC', 'PZINC')

get_data <- function(x) {
  ct_get_data(
    reporter = 'all',
    flow_direction = c('import', 'export'),
    partner = 'World',
    frequency = 'A',
    start_date = x,
    end_date = x,
    commodity_classification = 'HS',
    commodity_code = c(hs_code_vector, "TOTAL")
  ) %>%
    select(reporterISO, reporterDesc, period, flowCode, cmdCode, cmdDesc, primaryValue) %>% 
    rename(iso3c = 1, country = 2, year = 3, flowcode = 4, hscode = 5, cmd_desc = 6, trade = 7)
    
}

# 1988 is the minimum year with available data
alltrade <- map(1988:2020, ~get_data(.x)) %>% list_rbind() %>%  
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) 

## 2.3 Data Transformation -----------------------------------------------------

# LATAM Country Codes
latam_iso <- readRDS("raw_data/latam_iso.RDS")

setdiff(latam_iso$iso3c, alltrade$iso3c) 
# As can be seen, our trade dataset does not contain info from Puerto Rico

latam_trade <- alltrade %>%
  filter(iso3c %in% latam_iso$iso3c)

latam_trade <- latam_trade %>% # Generating net exports
  reframe(
    trade = trade[flowcode == "X"] - trade[flowcode == "M"],
    flowcode = "NX",
    .by = c(country, iso3c, year, hscode)) %>% 
  bind_rows(latam_trade) %>% 
  group_by(country, year, hscode) %>% 
  arrange(flowcode, .by_group = TRUE)

latam_trade <- latam_trade %>% # Filling cmd description cells
  group_by(hscode) %>%
  fill(cmd_desc, .direction = "downup") %>%
  ungroup()

trade_price_bind <- data.frame(hscode = hs_code_vector,
                          price_code = price_code_vector)

latam_trade <- latam_trade %>% 
  left_join(y = trade_price_bind)

## 2.4 Exporting trade dataset --------------------------------------------

alltrade %>%
  saveRDS("final_data/alltrade.RDS")

latam_trade %>%
  saveRDS("final_data/latam_trade.RDS")

# 3. INDEX BUILDING ----------------------------------------------------------
# Only run this chunk if you haven't run all the code above.
# commodity_prices <- readRDS("final_data/commodity_prices.RDS")
# latam_trade <- readRDS('final_data/latam_trade.RDS')
# latam_iso <- readRDS('final_data/db_socialx_pcp.RDS') %>%
#   select(iso3c) %>%
#   unique()

euv_idx <- read.csv2("raw_data/worldbank_export_value_index.csv", 
                     sep = ',', na.strings = "..", dec = ".") %>% 
  slice(1:798)

names(euv_idx) <- substr(names(euv_idx), 2, 5)

euv_idx <- euv_idx %>% 
  rename(variable = 1,
         varcode =2,
         country = 3,
         iso3c = 4) %>% 
  pivot_longer(cols = 5:67,
               names_to = 'year', 
               values_to = 'value') %>% 
  pivot_wider(id_cols = c("country", "iso3c", "year"), names_from = 1,
              values_from = 'value') %>% 
  filter(iso3c %in% latam_iso$iso3c) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  rename(unitvalue_idx = 4, volume_idx = 5, value_idx = 6)

cmd_weight <- latam_trade %>% 
  mutate(hscode = ifelse(hscode == 'TOTAL', 0, hscode),
         price_code = ifelse(hscode == 0, 'TOTAL', price_code)) %>% 
  filter(flowcode == 'NX') %>% 
  group_by(country, iso3c, year, price_code) %>%
  reframe(cmd_trade = sum(trade)) %>%
  ungroup() %>% 
  group_by(country, iso3c, year) %>% 
  mutate(yweight = cmd_trade/cmd_trade[price_code=='TOTAL']) %>% 
  ungroup() %>% 
  left_join(commodity_prices) %>% 
  left_join(euv_idx)

cmd_weight <- cmd_weight %>% 
  mutate(ma_weight = rollmean(lag(yweight), 3, na.pad = TRUE, align = "right"), 
         .by = c(iso3c, price_code))

lvl_idx <- cmd_weight %>% 
  drop_na(ma_weight, unitvalue_idx) %>%
  group_by(country, iso3c, year) %>% 
  reframe(cmd_idx = 
            sum(cmd_price/unitvalue_idx * (ma_weight), 
                na.rm = TRUE)) %>% 
  ungroup()

# Exporting index dataset
lvl_idx %>%
  saveRDS("final_data/cmd_price_idx.RDS") 

lvl_idx %>% 
  write_dta("final_data/cmd_price_idx.dta")