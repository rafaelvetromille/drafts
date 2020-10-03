## 1. Pacotes

library(tidyverse)
library(readxl)
library(httr)
library(scales)
library(lubridate)

## 2. Baixar dados
GET(url = 'http://pdet.mte.gov.br/images/Novo_CAGED/Ago2020/3-tabelas.xlsx',
    config = write_disk(dados <- tempfile(fileext = ".xlsx")))

## 3. Tratar dados

# Método 1:
caged <- read_excel(dados, sheet = 'Tabela 5.1', skip = 4, na = '---') %>%
  janitor::clean_names() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(mes = parse_date(mes, format = '%B/%Y', locale = locale('pt')),
                variacao = ifelse(saldos < 0, 'negativo', 'positivo'))

# Método 2:
caged <- read_excel(dados, sheet = 'Tabela 5.1', skip = 4, na = '---')
  janitor::clean_names() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(mes = dmy(paste('01', mes), locale = "Portuguese_Brazil.1252"),
                variacao = ifelse(saldos < 0, 'negativo', 'positivo'))

## 4. Gráfico
ggplot(data = caged, aes(x = mes, y = saldos / 1000)) +
  geom_col(aes(fill = variacao)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_text(aes(label = round(saldos / 1000, 0), vjust = ifelse(saldos < 0, 1.5, -1)),
            size = 3.5, hjust = 0.5, colour = "black", fontface = 'bold') +
  scale_fill_manual(values = c('red', 'lightblue')) +
  labs(title = 'Saldo de Admissões e Demissões do CAGED',
       y = 'mil pessoas', x = '',
       caption = 'Fonte: Elaboração própria com dados do Novo CAGED.') +
  scale_x_date(breaks = '1 month',
               labels = date_format('%b/%y')) +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'white'),
    axis.line.x.bottom = element_line(colour = 'black'),
    axis.line.y.left = element_line(colour = 'black'),
    legend.position = "none",
    axis.title.y = element_text(vjust = 2),
    plot.caption = element_text(vjust = 5)
  )
