# Packages ----------------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)

# 1. Files --------------------------------------
db_socialx_pcp <- readRDS('final_data/db_socialx_pcp.RDS')

# Quali Presentation ------------------------------------------------------
gg1 <- db_socialx_pcp %>%
  filter(year %in% 2002:2013) %>%
  filter(country %in% c('Argentina', 'Bolivia', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Mexico', 'Paraguay', 'Costa Rica', 'Dominican Republic', 'El Salvador', 'Guatemala', 'Haiti', 'Honduras', 'Nicaragua', 'Panama', 'Peru', 'Uruguay', 'Venezuela')) %>%
  ggplot(aes(x=year, y=commtot)) +
  facet_wrap(~factor(country, levels=c('Argentina', 'Bolivia', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Mexico', 'Paraguay', 'Peru', 'Venezuela', 'Costa Rica', 'Dominican Republic', 'El Salvador', 'Guatemala', 'Haiti', 'Honduras', 'Nicaragua', 'Panama', 'Uruguay')), drop = T, ncol = 5, scales = "free_y") +
  xlab("Ano") + ylab("Termos de Troca (Ano Base = 2012)") +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('product/dsotm/tot_all.jpeg', dpi = 300, height = 5, width = 10, unit = 'in', gg1)

# 2. Descriptive statistics -------------------------------------------------
latam <- db_socialx_pcp %>%
  filter(!is.na(soc_pcp)) %>%
  group_by(year) %>%
  select(year, soc_pcp) %>%
  dplyr::summarise(soc_pcp=mean(soc_pcp), ) %>%
  relocate(year, soc_pcp)

gg2 <- latam %>% #Por enquanto, nao ha dados de Venezuela, Puerto Rico
  ggplot(aes(x=year, y=soc_pcp, )) +
  theme_minimal() +
  geom_line(linewidth = 1) +
  xlab('Ano') + ylab('Média de Gasto Social per Capita')
  #ggtitle('Média de Gasto Social') +

ggsave('product/dsotm/socx.jpeg', dpi = 300, height = 5, width = 10, unit = 'in', gg2)