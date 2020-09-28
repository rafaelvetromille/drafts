#' Author: Rafael Vetromille
#' Subject: Inflação segue controlada no Brasil?

library(sidrar)
library(rbcb)
library(tidyverse)
library(magrittr)

# remotes::install_github("leripio/tstools")
library(tstools)

# Import -----------------------------------------------------------------------

## Importar IPCA do SIDRA
ipca <- sidrar::get_sidra(api = '/t/1737/n1/all/v/63/p/all/d/v63%202') %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::select(date = mes_codigo, ipca = valor) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(date = parse_date(date, format = "%Y%m"))

## Núcleos de Inflação

series <- list(ipca_ms = '4466', ipca_ma = '11426', ipca_ex0 = '11427', ipca_ex1 = '16121',
  ipca_ex2 = '27838', ipca_ex3 = '27839', ipca_dp = '16122', ipca = '433', meta = '13521')

nucleos_vm <- rbcb::get_series(series) %>%
  purrr::reduce(full_join) %>%
  tidyr::fill(meta) %>%
  dplyr::arrange(date) %>%
  tidyr::drop_na()

nucleos_12m <- nucleos_vm %>%
  dplyr::mutate(across(
    .cols = starts_with("ipca"),
    .fns = ~ round(acum_p(.x, n = 12), digits = 2)
  )) %>%
  tidyr::drop_na() %>%
  rowwise() %>%
  mutate(nucleos_media = mean(c_across(starts_with("ipca_"))))


### API

api <- c(
  '/t/655/n1/all/v/all/p/all/c315/7171/d/v63%202',
  '/t/2938/n1/all/v/63/p/all/c315/7171/d/v63%202',
  '/t/1419/n1/all/v/63/p/all/c315/7171/d/v63%202',
  '/t/7060/n1/all/v/63/p/all/c315/7171/d/v63%202'
)

alim_dom <- api %>%
  purrr::map(.f = ~ sidrar::get_sidra(api = .x)) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  dplyr::select(date = mes_codigo, ipca_alim = valor) %>%
  dplyr::mutate(date = parse_date(date, format = "%Y%m"))

alim_dom_12m <- alim_dom %>%
  dplyr::mutate(across(
    .cols = starts_with("ipca"),
    .fns = ~ round(acum_p(.x, n = 12), digits = 2)
  )) %>%
  tidyr::drop_na()

## JOIN

data <- nucleos_12m %>%
  left_join(alim_dom_12m, by = 'date') %>%
  filter(date >= '2012-01-01')

# Visualize --------------------------------------------------------------------

ggplot(data = data, aes(x = date)) +
  geom_line(aes(y = ipca, colour = 'inf_cheia'), size = .8) +
  geom_line(aes(y = nucleos_media, colour = 'nucleos_media'), size= .8) +
  geom_line(aes(y = ipca_alim, colour = 'ipca_alim'), size = .8) +
  geom_ribbon(aes(ymin = meta - 1.5, ymax = meta + 1.5),
              linetype = 'dashed', color = 'grey40', fill = 'lightblue', alpha = 0.3) +
  geom_line(aes(y = meta, colour = 'meta'), size = .8) +
  scale_colour_manual(values = c("blue", "orange", "black", "red"),
                      labels = c("nucleos_media" = "Média dos Núcleos",
                                 "inf_cheia" = "Inflação Cheia",
                                 "ipca_alim" = "Alimentação no Domícilio",
                                 "meta" = "Meta de Inflação")) +
  labs(x = '', y = '%',
       title = 'Inflação Cheia, Núcleos e Inflação de Alimentos',
       caption = 'Fonte: analisemacro.com.br com dados do IBGE',
       fill = "Medidas") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.title = element_blank())


# Model ------------------------------------------------------------------------

# Export -----------------------------------------------------------------------

# readr::write_rds(d, "")
