library(dplyr)
library(dagitty)
library(ggdag)
library(ggplot2)

theme_set(theme_dag())

# DAG 1 --------------------------------------------------------------
## Deconfounding 1 ---------------------------------------------------
dag_deconfounder <- dagify(
  welfare ~ tax + open + unitconfounder + war1 + neutral,
  tax ~ tot + capacity + unitconfounder,
  tot ~ unitconfounder + open,
  open ~ war1 + policy,
  war1 ~ welfare,
  exposure = "tot",
  outcome = "welfare",
  latent = "capacity",
  labels = c(welfare = "Social Spending", tot = "Terms of Trade", 
             tax = "Tax Revenue",
             open = "Trade Openness",
             policy = "Trade Policy",
             capacity = "State Capacity",
             commodity = "Commodity Prices",
             unitconfounder = "Unit Unobserved Confounder",
             war1 = "Warfare",
             neutral = "Neutral Controls",
             iv = "Resource Depletion"),
  coords = list(x = c(tot = 0, policy = 0, unitconfounder = 0.7,
                      capacity = 0.7, tax = 0.7, 
                      open = 0.5,
                      welfare = 1.4, commodity = 0, war1 = 0.7, 
                      war2 = 1.6, neutral = 1.11, 
                      iv = -0.2),
                y = c(tot = 0, neutral = -0.1,
                      open = 0.2, iv = -0.05,
                      policy = 0.2, capacity = 0.1, 
                      tax = 0, welfare = 0, commodity = -0.1,
                      unitconfounder = -0.1, war1 = 0.3, war2 = 0.2)
                )
  )

gg1 <- dag_deconfounder %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) +
  geom_dag_label(aes(label = label), show.legend = F)

ggsave('product/dagdec.jpeg', dpi = 300, height = 5, width = 10, unit = 'in', gg1)

## Black Box --------------------------------------------------------
blackbox <- dagify(
  welfare ~ debtdrive + taxdrive,
  tax ~ tot + capacity,
  will ~ tax + fav + unfav,
  debtdrive ~ will,
  taxdrive ~ will,
  exposure = "tot",
  outcome = "welfare",
  latent = "capacity",
  labels = c(welfare = "Social Spending", tot = "Terms of Trade", 
             tax = "Tax Revenue",
             capacity = "State Capacity",
             tot = "Terms of Trade",
             will = "Political Will",
             fav = "Pro-Expansion Factors",
             unfav = "Anti-Expansion Factors",
             debtdrive = "Debt Driven Expansion",
             taxdrive = "Tax Driven Expansion"),
  coords = list(x = c(tot = 0, tax = 1, capacity = 1,
                      will = 2, fav = 1.5, unfav = 2.5,
                      welfare = 4,
                      debtdrive = 3, taxdrive = 3),
                y = c(tot = 0, tax = 0, capacity = 1,
                      will = 0, fav = -1, unfav = -1,
                      debtdrive = 0.5, taxdrive = -0.5,
                      welfare = 0)
  )
)

gg2 <- blackbox %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) +
  geom_dag_label(aes(label = label), show.legend = F)

ggsave('product/blackbox.jpeg', dpi = 300, height = 5, width = 10, unit = 'in', gg2)

## Neutral Controls 1 ---------------------------------------------------
dag_neutral <- dagify(
  welfare ~ tax + left + unemp + dependent + inflation
  + debt + urban + confounder,
  tax ~ tot + capacity + unemp,
  inflation ~ debt + unemp,
  tot ~ confounder,
  debt ~ welfare,
  exposure = "tot",
  outcome = "welfare",
  latent = "capacity",
  labels = c(welfare = "Social Spending", tot = "Terms of Trade", 
             tax = "Tax Revenue",
             confounder = "Confounders",
             capacity = "State Capacity",
             confounder = "Confounder Controls",
             left = "Left Government",
             unemp = "Unemployment",
             dependent = "Dependent Pop.",
             inflation = "Inflation",
             debt = "Public Debt",
             urban = "Urbanization"),
  coords = list(x = c(tot = 0, capacity = 0.7, tax = 0.7, 
                      welfare = 1.5, confounder = 0.7,
                      left = 1.3, unemp = 0.7,
                      dependent = 1.5, debt = 1.0,
                      inflation = 0.7, urban = 1.5),
                y = c(tot = 0, capacity = 0.02, 
                      tax = 0, welfare = 0,
                      confounder = 0.035, left = 0.02,
                      unemp = -0.025, dependent = 0.04,
                      debt = -0.06, inflation = -0.05, urban = -0.04)
  )
)

gg3 <- dag_neutral %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) +
  geom_dag_label(aes(label = label), show.legend = F)

ggsave('product/dag_neutral.jpeg', dpi = 300, height = 5, width = 10, unit = 'in', gg3)

## Unemployment Dag -------------------------------------------------
dag_unemp <- dagify(
  welfare ~ unemp + inflation + debt,
  inflation ~ debt + unemp,
  debt ~ welfare,
  exposure = "unemp",
  outcome = "welfare",
  labels = c(welfare = "Social Spending", tot = "Terms of Trade", 
             tax = "Tax Revenue",
             confounder = "Confounders",
             capacity = "State Capacity",
             confounder = "Confounder Controls",
             left = "Left Government",
             unemp = "Unemployment",
             dependent = "Dependent Pop.",
             inflation = "Inflation",
             debt = "Public Debt",
             urban = "Urbanization"),
  coords = list(x = c(tot = 0, tax = 0.7, 
                      welfare = 1.5, confounder = 0.7,
                      left = 0.9, unemp = 0.7,
                      dependent = 0.9, debt = 1.0,
                      inflation = 0.9, urban = 1.3),
                y = c(tot = 0, tax = 0, welfare = 0,
                      confounder = 0.08, left = 0.02,
                      unemp = -0.03, dependent = 0.04,
                      debt = -0.06, inflation = -0.05, urban = -0.06)
  )
)

gg4 <- dag_unemp %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) +
  geom_dag_label(aes(label = label), show.legend = F)

ggsave('product/dag_unemp.jpeg', dpi = 300, height = 5, width = 10, unit = 'in', gg4)


# DAG 2 ------------------------------------------------------------
## Deconfounder 2 ---------------------------------------------------
dag_dec2 <- dagify(
  y ~ md + mt + w + uz,
  md ~ g,
  mt ~ g,
  g ~ w + t,
  t ~ w + p,
  x ~ t + uz,
  labels = c(w = "Wt-1"),
 coords = list(x = c(x = 0, p = 0,
                     t = 1,
                     w = 2, g = 2,
                     uz = 2.5,
                     mt = 3, md = 3,
                     y = 5,
                     t = 1),
                y = c(uz = -1.5,
                      x = 0, g = 0, y = 0,
                      mt = 0.5, md = -0.5,
                      t = 1,
                      p = 2, w = 2)
  )
)

gg5 <- dag_dec2 %>%
  ggdag() +
  geom_dag_label(aes(label = label), show.legend = F)

ggsave('product/dagdec2.jpeg', dpi = 300, height = 3.5, width = 8, unit = 'in', gg5)

## Neutral Controls 2 ------------------------------------------------
dag_neutral2 <- dagify(
  y ~ md + mt + g + un + i + uz + db + w + ot,
  mt ~ g + c,
  md ~ g,
  g ~ x + t,
  x ~ uz + t,
  un ~ g,
  t ~ w + p,
  i ~ un + db,
  labels = c(db = "Dbt-1", w = "Wt-1"),
  coords = list(x = c(x = 0, p = 0,
                      t = 1, 
                      g = 2, uz = 2, c = 2, un = 2, 
                      i = 2.5, w = 2.5,
                      md = 3, mt = 3, db = 3, 
                      ot = 3.5,
                      y = 4, l = 4, ur = 4),
                y = c(x = 0, g = 0, y = 0, ur = 0.7,
                      t = 1, un = 1, i = 1, db = 1, p = 1,
                      w = 2,
                      mt = -0.3, md = 0.3,
                      c = -0.7, l = -0.7, ot = -0.7,
                      uz = -1
                      ))
)

dag_neutral2 %>%
  ggdag() +
  geom_dag_label(aes(label = label), show.legend = F)

gg6 <- dag_neutral2 %>%
  ggdag() +
  geom_dag_label(aes(label = label), show.legend = F)

ggsave('product/dag2_completo.jpeg', dpi = 300, height = 8, width = 12, unit = 'in', gg6)

# Time Series Cross-Sectional DAG -----------------------------------
tscs_dag <- dagify(
  yt1 ~ dt1 + xt1 + ut1,
  yt2 ~ dt2 + xt2 + dt1 + yt1 + xt1 + ut2,
  yt3 ~ dt3 + xt3 + dt2 + yt2 + xt2 + ut3,
  dt1 ~ xt1 + ut1, 
  dt2 ~ dt1 + xt2 + ut2, 
  dt3 ~ dt2 + xt3 + ut3,
  xt1 ~ ut1,
  xt2 ~ xt1 + ut2, 
  xt3 ~ xt2 + ut3,
  ut2 ~ ut1,
  ut3 ~ ut2,
  labels = c(yt1 = "Yt-1", yt2 = "Yt", yt3 = "Yt+1",
             dt1 = "Dt-1", dt2 = "Dt", dt3 = "Dt+1", 
             xt1 = "Xt-1", xt2 = "Xt", xt3 = "Xt+1",
             ut1 = "Ut1", ut2 = "Ut2", ut3 = "Ut3"),
  coords = list(x = c(ut1 = 0.5, ut2 = 1.5, ut3 = 2.5,
                      dt1 = 0, dt2 = 1, dt3 = 2,
                      yt1 = 0.5, yt2 = 1.5, yt3 = 2.5,
                      xt1 = 0, xt2 = 1, xt3 = 2),
                y = c(ut1 = 2, ut2 = 2, ut3 = 2,
                      dt1 = 1, dt2 = 1, dt3 = 1,
                      yt1 = 0, yt2 = 0, yt3 = 0,
                      xt1 = -1, xt2 = -1, xt3 = -1)
  )
) #%>% tidy_dagitty() %>% 
  #dplyr::mutate(colour = ifelse(name == "U", "Unobserved", "Observed"))

tscs_dag %>%
  ggdag()
