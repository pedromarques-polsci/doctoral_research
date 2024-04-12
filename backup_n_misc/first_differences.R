# Don't touch this \/
# cmd_index_fd %>% filter(iso3c == "ARG", code == "PALUM") %>% View()
# fd_idx <- cmd_weight %>% 
#   group_by(iso3c, price_code) %>% 
#   mutate(lag_prices = cmd_price - lag(cmd_price))
# 
# fd_idx <- fd_idx %>% 
#   group_by(iso3c, year) %>% 
#   drop_na(ma_weight, unitvalue_idx, lag_prices) %>%
#   reframe(fd_cmd_idx = 
#             sum(lag_prices/unitvalue_idx * (ma_weight), 
#                 na.rm = TRUE)) %>% 
#   ungroup()