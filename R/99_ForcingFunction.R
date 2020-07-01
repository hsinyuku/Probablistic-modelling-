library(tidyverse)
library(lubridate)

values <- readxl::read_xlsx("data/all_regions_forcing-function-parameters.xlsx")

dates <- readxl::read_xlsx("data/all_regions_time-parameters.xlsx")

forcing <- dplyr::left_join(values, dates) %>% 
  mutate(t = as.numeric(tmax - t1),
         tswitch = as.numeric(day_quarantine - t1)) %>% 
  group_by(region, value) %>% nest() %>% 
  mutate(forcing = map(data, function(x) {
    return(tibble(t = 1:x$t) %>% 
             mutate(forcing = x$eta + (1-x$eta)/(1+exp(x$xi*(t-x$tswitch-x$nu)))))
  })) %>% 
  unnest(cols = forcing) %>% 
  select(region, t, forcing)

pivot_wider(forcing, names_from = value, id_cols = c(region, t),
            values_from = forcing) %>% 
ggplot(aes(x = t, ymin = CrILower, y = median, ymax = CrIUpper, fill = region)) +
  facet_wrap(facets = "region") +
  geom_ribbon(alpha = 0.6) + geom_line() + theme_minimal()

