# Instalar e carregar pacotes necessários
install.packages("tidyverse")
library(tidyverse)
library(readxl)


# Importar dados do Excel
dados_luxo <- read_excel('C:/Users/jeferson-goncalves/Downloads/dados-uxo.xlsx') %>% 
  as_tibble()


# Inspeção inicial dos dados
glimpse(dados_luxo)
view(dados_luxo)


# Remover linhas completamente vazias
dados_luxo <- dados_luxo %>%
  filter(!if_all(everything(), is.na))


# Criar tabela de países por região (e remover linha problemática) - Aqui é apenas por curiosidade
paises_por_regiao <- dados_luxo %>%
  select(Region, Country) %>%
  distinct() %>%
  arrange(Region, Country) %>%
  slice(-51)  # remove a linha 51


view(paises_por_regiao)


# Verificar indústrias que não estão no nível mais detalhado - Por curiosidade também
dados_luxo %>%
  select(Industry, `Lowest Level`) %>%
  filter(`Lowest Level` == "no") %>%
  distinct() %>%
  arrange(Industry)


# Agrupar dados excluindo linhas redundantes (onde Category == Industry)
resumo_industria <- dados_luxo %>%
  filter(
    !is.na(Category),
    Category != Industry
  ) %>%
  group_by(Country, Industry) %>%
  summarise(
    total_2019 = sum(`2019`, na.rm = TRUE),
    total_2020 = sum(`2020`, na.rm = TRUE),
    total_2021 = sum(`2021`, na.rm = TRUE),
    total_2022 = sum(`2022`, na.rm = TRUE),
    total_2023 = sum(`2023`, na.rm = TRUE),
    total_2024 = sum(`2024`, na.rm = TRUE),
    .groups = "drop"
  )


# Visualizar dados filtrados (somente linhas com valores nos anos e nível granular)
dados_filtrados <- dados_luxo %>%
  select(Region, Country, Industry, Category, Subcategory, ProductId,
         `Lowest Level`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`) %>%
  filter(
    if_all(`2019`:`2024`, ~ !is.na(.)),
    Category != Industry
  )

view(dados_filtrados)

write_csv(dados_filtrados, "resumo_industria.csv")
