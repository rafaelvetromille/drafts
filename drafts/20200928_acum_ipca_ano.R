#' Author: Rafael Vetromille
#' Subject: Inflação Acumulada no Ano

# Pacotes
library(rbcb)
library(sidrar)
library(tidyverse)
library(lubridate)
library(magrittr)

# Import -----------------------------------------------------------------------

ipca <- rbcb::get_series(list(ipca = '433'))

# Tidy -------------------------------------------------------------------------

last <- month(last(ipca$date))

ipca <- ipca %>%
  group_by(year = lubridate::year(date)) %>%
  mutate(acum_ano = (cumprod((1 + ipca/100))-1)*100) %>%
  filter(month(date) <= last)

# Visualize --------------------------------------------------------------------

filter(ipca, year(date) >= 2020) %>%
ggplot() +
  geom_col(mapping = aes(x = date, y = acum_ano))

# Model ------------------------------------------------------------------------

# Export -----------------------------------------------------------------------

# readr::write_rds(d, "")
