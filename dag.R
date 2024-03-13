install.packages('dagitty')

library(dplyr)
library(dagitty)
library(ggdag)
library(ggplot2)

theme_set(theme_dag())

#controles demograficos: urbanizacao, gastos militares

tot_welfare <- dagify(
  welfare ~ tax + open + left + unitconfounder + dependent + unemp + inflation +
    debt + mob,
  inflation ~ debt,
  tax ~ tot + capacity + unemp,
  tot ~ trade + commodity + unitconfounder,
  trade ~ open,
  left ~ unemp,
  exposure = "tot",
  outcome = "welfare",
  latent = "capacity",
  labels = c(welfare = "Social Spending", tot = "Terms of Trade", 
             tax = "Tax Revenue",
             open = "Trade Openness",
             trade = "Trade Flow",
             left = "Left Government",
             capacity = "State Capacity",
             commodity = "Commodity Prices",
             unitconfounder = "Unit Unobserved Confounder",
             dependent = "Dependent Population",
             unemp = "Unemployment",
             inflation = "Inflation",
             debt = "Public Debt",
             mob = "Social Mobilization"),
  coords = list(x = c(tot = 0, open = 1, unitconfounder = 1, left = 2, 
                      trade = 0.5, capacity = 1, tax = 1, welfare = 2,
                      commodity = 0, dependent = 2, unemp = 1.5,
                      inflation = 1.7, debt = 1.8, mob = 1.8),
                y = c(tot = 0, 
                      open = 0.2, capacity = 0.1, left = 0.1, trade = 0.1, 
                      tax = 0, welfare = 0, commodity = -0.1,
                      unitconfounder = -0.1, dependent = -0.1,
                      unemp = -0.1, inflation = 0.1, debt = 0.2,
                      mob = -0.1)))

tot_welfare %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) +
  geom_dag_label_repel(aes(label = label), show.legend = F)


dagitty::paths(tot_welfare)

gg1 <- tot_welfare %>%
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name %in% c("unitconfounder", "mob"), "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) +
  geom_dag_label_repel(aes(label = label), show.legend = F)

ggsave('product/dag.jpeg', dpi = 300, height = 5, width = 10, unit = 'in', gg1)
