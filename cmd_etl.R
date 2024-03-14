# Packages ----------------------------------------------------------------
if(require(comtradr) == F) install.packages('comtradr'); require(comtradr)
if(require(concordance) == F) install.packages('concordance'); require(concordance)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

# Potential conflicts
# dplyr::filter(), Hmisc::sumarize()

# SUMMARY -----------------------------------------------------------------
# The current script are divided by three major chunks. 
# The first one builds our commodity price dataset by aggregating three sources.
# The second chunk extracts commodity trade data directly from UN Comtrade 
# dataset through its API.
# The final one ties the previous databases together and builds up a Terms of 
# Trade Index inspired by Gruss and Kebhaj's (2019) methodology,  although our 
# index has some minor, yet important differences.

# 1. COMMODITY PRICES --------------------------------------------------------
## 1.1 Source - IMF --------------------------------------------------------
commodity_code <- read_xlsx('raw_data/commodity_codes.xlsx', sheet = 1, skip = 1) %>%
  rename('commodity' = 1,
         'code' = 2)

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
  relocate(id, commodity, code, fullname) %>%
  pivot_longer(cols = 5:38,
               names_to = 'year',
               values_to = 'com_prices')

wood <- commodity_prices %>% 
  filter(code == 'PTIMB') %>% 
  mutate(code = 'PWMEAN',
         fullname = 'Wood Prices Mean (Soft/Hard Sawnwood, Soft/Hard Logs',
         commodity = 'Wood')
  

logsk <- commodity_prices %>% 
  filter(code == 'PLOGSK')

sawmal <- commodity_prices %>% 
  filter(code == 'PSAWMAL')

logore <- commodity_prices %>% 
  filter(code == 'PLOGORE')

sawore <- commodity_prices %>% 
  filter(code == 'PSAWORE')

for (i in 1:34) {
  wood[i,6] <- (logsk[i,6] + sawmal[i,6] + logore[i,6] + sawore[i,6])/4
}

commodity_prices <- commodity_prices %>% bind_rows(wood)

## 1.2 Source - UNCTAD -----------------------------------------------------
unctad_prices <- read_xlsx('raw_data/unctad_commodity_prices.xlsx') %>%
  rename('year' = 1,
         'code' = 2,
         'fullname' = 3,
         'com_prices' = 4) %>%
  select(-5, -6) %>% 
  mutate(year = as.character(year),
         com_prices = as.double(com_prices)) %>% 
  filter(code == "240100.01")

## 1.3 Source - FRED -----------------------------------------------------------
orange_prices <- read_xlsx('raw_data/fred_orange_prices.xlsx', sheet = 1, skip = 10) %>%
  rename(year = 1,
         com_prices = 2) %>%
  mutate(year = format(year, format="%Y"),
         commodity = 'Orange',
         code = 'PORANGUSDM',
         fullname = 'U.S. Dollars per Pound')

## 1.4 Joining both datasets -----------------------------------------------
commodity_prices <- commodity_prices %>%
  bind_rows(unctad_prices, orange_prices)

commodity_prices[commodity_prices$code == '240100.01', 'code'] <- 'PTOBAC'
commodity_prices[commodity_prices$code == 'PTOBAC', 'id'] <- 111
commodity_prices[commodity_prices$code == 'PTOBAC', 'commodity'] <- 'Tobacco'
commodity_prices[commodity_prices$code == 'PORANGUSDM', 'id'] <- 112

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
hs_codes <- c(
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


cmd_price_code <- c("PCOTTIND", "PHIDE", "PRUBB", # Commodity price codes
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
    commodity_code = c(hs_codes, "TOTAL")
  ) %>%
    select(reporterISO, reporterDesc, period, flowCode, cmdCode, cmdDesc, primaryValue) %>% 
    rename(iso3c = 1, country = 2, year = 3, flowcode = 4, hscode = 5, cmd_desc = 6, value = 7)
    
}

alltrade <- map(1988:2020, ~get_data(.x)) %>% list_rbind() # 1988 is the minimum year with available data

## 2.3 Data Transformation -----------------------------------------------------

# LATAM Country Codes
latam_iso <- readRDS('final_data/db_socialx_pcp.RDS') %>%
  select(iso3c) %>%
  unique()

setdiff(latam_iso$iso3c, alltrade$iso3c) 
# As can be seen, our trade dataset does not contain info from Puerto Rico

latam_trade <- alltrade %>%
  filter(iso3c %in% latam_iso$iso3c)

latam_trade <- latam_trade %>% # Generating net exports
  reframe(
    value = value[flowcode == "X"] - value[flowcode == "M"],
    flowcode = "NX",
    .by = c(country, iso3c, year, hscode)) %>% 
  bind_rows(latam_trade) %>% 
  group_by(country, year, hscode) %>% 
  arrange(flowcode, .by_group = TRUE)

latam_trade <- latam_trade %>% # Filling cmd description cells
  group_by(hscode) %>%
  fill(cmd_desc, .direction = "downup") %>%
  ungroup()

price_codes <- data.frame(hscode = hs_codes,
                          code = cmd_price_code)

latam_trade <- latam_trade %>% 
  left_join(y = price_codes)

## 2.4 Exporting trade dataset --------------------------------------------

alltrade %>%
  saveRDS("final_data/alltrade.RDS")

latam_trade %>%
  saveRDS("final_data/latam_trade.RDS")

# 3. INDEX BUILDING ----------------------------------------------------------
commodity_prices <- readRDS("final_data/commodity_prices.RDS")
latam_trade <- readRDS('final_data/latam_trade.RDS')

trade_colapse <- latam_trade %>% 
  mutate(hscode = ifelse(hscode == 'TOTAL', 0, hscode),
         code = ifelse(hscode == 0, 'TOTAL', code)) %>% 
  filter(flowcode == 'NX') %>% 
  group_by(country, iso3c, year, code) %>%
  reframe(net_trade = sum(value)) %>%
  ungroup() %>% 
  group_by(country, iso3c, year) %>% 
  mutate(yweight = net_trade/net_trade[code=='TOTAL']) %>% 
  ungroup() %>% 
  left_join(commodity_prices)

trade_colapse <- trade_colapse %>% 
  mutate(ma = rollmean(yweight, 3, na.pad = TRUE, align = "right"), 
         .by = c(iso3c, code))
  
cmd_index <- trade_colapse %>% 
  group_by(country, iso3c, year) %>% 
  reframe(tot_index = 
            sum(com_prices * (ma), 
                na.rm = TRUE)) %>% 
  ungroup()

library(ggplot2)