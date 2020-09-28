#' Author: Rafael Vetromille
#' Subject: Inflação Acumulada no Ano

# Pacotes
library(tidyverse)
library(magrittr)

# Import -----------------------------------------------------------------------

ipca <- rbcb::get_series(list(ipca = '4333'))

# Tidy -------------------------------------------------------------------------

ipca <- ipca %>%
  group_by(year = lubridate::year(date)) %>%
  mutate(acum_ano = (cumprod((1 + ipca/100))-1)*100)

# Visualize --------------------------------------------------------------------

# Model ------------------------------------------------------------------------

# Export -----------------------------------------------------------------------

# readr::write_rds(d, "")
