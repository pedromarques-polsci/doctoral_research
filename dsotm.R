# Packages ----------------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(gghighlight) == F) install.packages('gghighlight'); require(gghighlight)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(gridExtra) == F) install.packages('gridExtra'); require(gridExtra)
if(require(Hmisc) == F) install.packages('Hmisc'); require(Hmisc)
if(require(QCA) == F) install.packages('QCA'); require(QCA)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)

# Important conflicts
# dplyr::filter(), Hmisc::sumarize()

# 1. Files --------------------------------------
db_socialx_pcp <- readRDS('final_data/db_socialx_pcp.RDS')

# 2. Descriptive statistics -------------------------------------------------

## 2.1 Terms of trade ---------------------------
plotot <- function(x){
  db_socialx_pcp %>%
    filter(year %in% 2000:2011) %>%
    filter(country == x) %>%
    ggplot(aes(x=year, y=commtot)) +
    xlab(x) + ylab('') +
    theme_classic() +
    geom_line(linetype = 'dashed') +
    geom_point()
}

p1 <- plotot('Costa Rica')
p2 <- plotot('El Salvador')
p3 <- plotot('Guatemala')
p4 <- plotot('Honduras')
p5 <- plotot('Panama')

grid.arrange(p1, p2, p3, p4, p5, ncol = 2, nrow = 3)

g <- arrangeGrob(p1, p2, p3, p4, p5, ncol = 2, nrow = 3)
ggsave('product/dsotm/tot.png', dpi = 300, height = 5, width = 10, unit = 'in', g)

## 2.2 Social spending ------------------------------------------
socx <- db_socialx_pcp %>%
  filter(!is.na(soc_pcp))

latam <- socx %>%
  group_by(year) %>%
  select(year, soc_pcp) %>%
  dplyr::summarise(soc_pcp=mean(soc_pcp)) %>%
  mutate(region = 'Latin America and Caribbean') %>%
  relocate(region, year, soc_pcp)

latam_r <- aggregate(socx$soc_pcp, by=list(region=socx$region, year=socx$year), FUN=mean) %>%
  rename(soc_pcp = x) %>%
  arrange(region, year) %>%
  mutate(region = as.character(region))

latam <- rbind(latam, latam_r)

latam <- socx %>%
  filter(region=='Central America') %>%
  select(year, country, soc_pcp) %>%
  rename(region = country) %>%
  rbind(latam)

latam %>%
  filter(region %in% c('Latin America and Caribbean', 'Central America', 'Caribbean', 'South America', 'North America')) %>%
  ggplot(aes(x=year, y=soc_pcp, group=region, color=region)) +
  theme_minimal() +
  geom_line(linewidth = 1, aes(color=region)) +
  geom_point(size = 2) +
  xlab('Ano') + ylab('Média de Gasto Social per Capita') + 
  #ggtitle('Média de Gasto Social') +
  scale_color_manual(values = c("#ca7dcc",
                                "orange",
                                "darkblue",
                                'darkgreen',
                                'deeppink',
                                'red'),
                     name = 'Região')

ggsave('product/dsotm/region_plot.jpeg', dpi = 300, height = 5, width = 10, unit = 'in')

latam %>%
  filter(region != 'Caribbean',
         region != 'South America',
         region != 'Latin America and Caribbean',
         region != 'North America') %>%
  ggplot(aes(x=year, y=soc_pcp, group=region, color=region)) +
  theme_minimal() +
  geom_line(linewidth = 1, aes(color=region)) +
  geom_point(size = 1) +
  #ggtitle('Gasto Social Per Capita') +
  xlab('Ano') +
  ylab('Gasto Social per Capita') +
  scale_color_manual(values = c("#ca7dcc",
                                "darkblue",
                                "#353436",
                                "darkgreen",
                                'orange',
                                'deeppink',
                                'red',
                                'lightblue'),
                     name = 'Região')

ggsave('product/dsotm/plot_c.jpeg', dpi = 300, height = 5, width = 10, unit = 'in')

# 3. QCA -------------------------------------------------------------
## 3.1 Data Management -----------------------------------------------
# Tax reform
db_socialx_pcp <- db_socialx_pcp %>%
  group_by(country) %>%
  mutate(tax_diff = tax_inc_sc - lag(tax_inc_sc, default = first(tax_inc_sc))) %>%
  arrange(region, country, year)

dsotm <- db_socialx_pcp %>%
  filter(region == 'Central America',
         year > 2002,
         year < 2014) %>%
  select(region, system, iso3c, country, year, soc_pcp, n_sp, n_cct, n_lpi, n_ncp, leader, party_name, pf_party_id, party_short, maj, v2pariglef_ord, tax_inc_sc, gdp_pcp, real_gdp, tax_diff)

# Central America Means
dsotm <- dsotm %>%
  group_by(year) %>%
  mutate(sx_ca = mean(soc_pcp, na.rm = T),
         rgdp_ca = mean(real_gdp, na.rm = T),
         gdpcp_ca = mean(gdp_pcp, na.rm = T))

# Renaming second terms
dsotm$leader[dsotm$year > 2018 & dsotm$leader == 'Juan Orlando Hernández Alvarado'] <- 'Juan Orlando Hernández Alvarado 2'
dsotm$leader[dsotm$year > 2011 & dsotm$year < 2017 & dsotm$leader == 'José Daniel Ortega Saavedra'] <- 'José Daniel Ortega Saavedra 2'
dsotm$leader[dsotm$year > 2016 & dsotm$leader == 'José Daniel Ortega Saavedra'] <- 'José Daniel Ortega Saavedra 3'

# Term means
dsotm <- dsotm %>%
  group_by(leader) %>%
  mutate(soc_pcp = mean(soc_pcp, na.rm = T),
         tax_diff = mean(tax_diff, na.rm = T),
         v2pariglef_ord = mean(v2pariglef_ord, na.rm = T),
         real_gdp = mean(real_gdp, na.rm = T),
         gdp_pcp = mean(gdp_pcp, na.rm = T),
         n_sp = sum(n_sp),
         n_cct = sum(n_cct),
         n_lpi = sum(n_lpi),
         n_ncp = sum(n_ncp),
         maj = mean(maj, na.rm = T)) %>%
  ungroup()

dsotm <- dsotm[!duplicated(dsotm$leader), ]

dsotm <- dsotm[complete.cases(dsotm[,c('n_ncp', 'tax_diff', 'v2pariglef_ord', 'real_gdp', 'n_sp', 'n_cct')]),]

## 3.2 Data Recoding -----------------------------------------------
dsotmc <- dsotm

dsotmc$n_ncp <- calibrate(dsotmc$n_ncp, type = 'crisp', thresholds = 1)
dsotmc$n_cct <- calibrate(dsotmc$n_cct, type = 'crisp', thresholds = 1)
dsotmc$n_lpi <- calibrate(dsotmc$n_lpi, type = 'crisp', thresholds = 1)
dsotmc$n_sp <- calibrate(dsotmc$n_sp, type = 'crisp', thresholds = 1)

dsotmc$tax_diff <- calibrate(dsotmc$tax_diff, type = 'crisp', thresholds = 0)

dsotmc$v2pariglef_ord[dsotmc$v2pariglef_ord < 3] <- 1
dsotmc$v2pariglef_ord[dsotmc$v2pariglef_ord > 2.9] <- 0

quantile(dsotmc$real_gdp, c(.05, .5, .95))

dsotmc$real_gdp <- round(calibrate(dsotmc$real_gdp, type = 'fuzzy', thresholds = c(1.4000, 4.0875, 7.1020)), digits = 2)

## 3.3 Necessity Analysis ----------------------------------------------
dsotmc <- dsotmc %>%
  select(country,
         year,
         leader,
         party_name,
         party_short,
         n_cct,
         n_lpi,
         n_sp,
         n_ncp,
         v2pariglef_ord,
         real_gdp,
         tax_diff)

# Necessity is default

# DV = Conditional Cash Transfer
pof('tax_diff -> n_cct', data = dsotmc)
pof('~tax_diff -> n_cct', data = dsotmc)
pof('tax_diff -> ~n_cct', data = dsotmc)
pof('~tax_diff -> ~n_cct', data = dsotmc)

pof('v2pariglef_ord -> n_cct', data = dsotmc)
pof('~v2pariglef_ord -> n_cct', data = dsotmc)
pof('v2pariglef_ord -> ~n_cct', data = dsotmc)
pof('~v2pariglef_ord -> ~n_cct', data = dsotmc)

pof('real_gdp -> n_cct', data = dsotmc)
pof('~real_gdp -> n_cct', data = dsotmc)
pof('real_gdp -> ~n_cct', data = dsotmc)
pof('~real_gdp -> ~n_cct', data = dsotmc)

# DV = Labour inclusion programmes
pof('tax_diff -> n_lpi', data = dsotmc)
pof('~tax_diff -> n_lpi', data = dsotmc)
pof('tax_diff -> ~n_lpi', data = dsotmc)
pof('~tax_diff -> ~n_lpi', data = dsotmc)

pof('v2pariglef_ord -> n_lpi', data = dsotmc)
pof('~v2pariglef_ord -> n_lpi', data = dsotmc)
pof('v2pariglef_ord -> ~n_lpi', data = dsotmc)
pof('~v2pariglef_ord -> ~n_lpi', data = dsotmc)

pof('real_gdp -> n_lpi', data = dsotmc)
pof('~real_gdp -> n_lpi', data = dsotmc)
pof('real_gdp -> ~n_lpi', data = dsotmc)
pof('~real_gdp -> ~n_lpi', data = dsotmc)

## 3.4 Sufficiency Analysis --------------------------------------------
# VD = Conditional Cash Transfer
pof('tax_diff -> n_cct', data = dsotmc, relation = 'sufficiency')
pof('~tax_diff -> n_cct', data = dsotmc, relation = 'sufficiency')
pof('tax_diff -> ~n_cct', data = dsotmc, relation = 'sufficiency')
pof('~tax_diff -> ~n_cct', data = dsotmc, relation = 'sufficiency')

pof('v2pariglef_ord -> n_cct', data = dsotmc, relation = 'sufficiency')
pof('~v2pariglef_ord -> n_cct', data = dsotmc, relation = 'sufficiency')
pof('v2pariglef_ord -> ~n_cct', data = dsotmc, relation = 'sufficiency')
pof('~v2pariglef_ord -> ~n_cct', data = dsotmc, relation = 'sufficiency')

pof('real_gdp -> n_cct', data = dsotmc, relation = 'sufficiency')
pof('~real_gdp -> n_cct', data = dsotmc, relation = 'sufficiency')
pof('real_gdp -> ~n_cct', data = dsotmc, relation = 'sufficiency')
pof('~real_gdp -> ~n_cct', data = dsotmc, relation = 'sufficiency')

# VD = Labour inclusion programme
pof('tax_diff -> n_lpi', data = dsotmc, relation = 'sufficiency')
pof('~tax_diff -> n_lpi', data = dsotmc, relation = 'sufficiency')
pof('tax_diff -> ~n_lpi', data = dsotmc, relation = 'sufficiency')
pof('~tax_diff -> ~n_lpi', data = dsotmc, relation = 'sufficiency')

pof('v2pariglef_ord -> n_lpi', data = dsotmc, relation = 'sufficiency')
pof('~v2pariglef_ord -> n_lpi', data = dsotmc, relation = 'sufficiency')
pof('v2pariglef_ord -> ~n_lpi', data = dsotmc, relation = 'sufficiency')
pof('~v2pariglef_ord -> ~n_lpi', data = dsotmc, relation = 'sufficiency')

pof('real_gdp -> n_lpi', data = dsotmc, relation = 'sufficiency')
pof('~real_gdp -> n_lpi', data = dsotmc, relation = 'sufficiency')
pof('real_gdp -> ~n_lpi', data = dsotmc, relation = 'sufficiency')
pof('~real_gdp -> ~n_lpi', data = dsotmc, relation = 'sufficiency')

# 3.5 Truth Table ----------------------------------------------------
# DV = Conditional cash transfer
as.data.frame(dsotmc) %>%
  truthTable(outcome = "n_cct", conditions = 'v2pariglef_ord, real_gdp, tax_diff', incl.cut = 0.85, sort.by = 'incl, n', complete = T, show.cases = T)

truth_notcct <- as.data.frame(dsotmc) %>%
  truthTable(outcome = "~n_cct", conditions = 'v2pariglef_ord, real_gdp, tax_diff', incl.cut = 0.85, sort.by = 'incl, n', complete = T, show.cases = T)

minimize(truth_notcct, details = TRUE, show.cases = TRUE)

truth_lpi <- as.data.frame(dsotmc) %>%
  truthTable(outcome = "n_lpi", conditions = 'v2pariglef_ord, real_gdp, tax_diff', incl.cut = 0.85, sort.by = 'incl, n', complete = T, show.cases = T)

truth_notlpi <- as.data.frame(dsotmc) %>%
  truthTable(outcome = "~n_lpi", conditions = 'v2pariglef_ord, real_gdp, tax_diff', incl.cut = 0.85, sort.by = 'incl, n', complete = T, show.cases = T)

minimize(truth_lpi, details = TRUE, show.cases = TRUE)
minimize(truth_notlpi, details = TRUE, show.cases = TRUE)

as.data.frame(dsotmc) %>%
  truthTable(outcome = "n_sp", conditions = 'v2pariglef_ord, real_gdp, tax_diff,', incl.cut = 0.9, sort.by = 'incl, n', complete = T, show.cases = T)

truth_notsp <- as.data.frame(dsotmc) %>%
  truthTable(outcome = "~n_sp", conditions = 'v2pariglef_ord, real_gdp, tax_diff,', incl.cut = 0.9, sort.by = 'incl, n', complete = T, show.cases = T)

minimize(truth_notsp, details = TRUE, show.cases = TRUE)

# leftover
# v_party %>%
#   filter(pf_party_id == 1367) %>%
#   select(pf_party_id, v2pariglef_ord, year)
# 
# dsotm %>%
#   select(year, pf_party_id, v2pariglef_ord) %>%
#   View()