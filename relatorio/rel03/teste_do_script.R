# ============================================================
# DESAFIO FINAL — O ENGENHEIRO RESPONSÁVEL PELOS DADOS
# Controle tecnológico do concreto
# ============================================================


# ------------------------------------------------------------
# 1. CARREGAMENTO DOS PACOTES E IMPORTAÇÃO DA BASE
# ------------------------------------------------------------

library(tidyverse)

# Importando a base de dados.
dados <- read_csv2("base_de_dados.csv", trim_ws = FALSE)

# Inspeção inicial da base.
head(dados)
tail(dados)
glimpse(dados)

# Verificando as dimensões da base.
dim(dados)


# ------------------------------------------------------------
# 2. IDENTIFICAÇÃO DE POSSÍVEIS PROBLEMAS
# ------------------------------------------------------------

# Verificando os valores distintos das variáveis qualitativas.
dados |>
  select(id_corpo_prova, obra, tipo_concreto) |>
  map(unique)

# Procurando possíveis caracteres que indiquem erros de
# digitação ou formatação.
dados |>
  filter(
    if_any(
      everything(),
      ~ str_detect(as.character(.x), regex("O|,", ignore_case = TRUE))
    )
  )


# ------------------------------------------------------------
# 3. VERIFICAÇÃO DOS VALORES AUSENTES
# ------------------------------------------------------------

# Contando a quantidade de valores ausentes em cada variável.
dados |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variável",
    values_to = "quantidade_na"
  ) |>
  arrange(desc(quantidade_na))


# ------------------------------------------------------------
# 4. CRIAÇÃO DA BASE PARA LIMPEZA
# ------------------------------------------------------------

# Criando uma cópia para preservar os dados originais.
dados_limpos <- dados


# ------------------------------------------------------------
# 5. PADRONIZAÇÃO DAS VARIÁVEIS QUALITATIVAS
# ------------------------------------------------------------

# Removendo espaços desnecessários e padronizando a escrita.
dados_limpos <- dados_limpos |>
  mutate(
    obra = obra |>
      str_trim() |>
      str_to_upper(),
    
    tipo_concreto = tipo_concreto |>
      str_trim() |>
      str_to_upper()
  )


# ------------------------------------------------------------
# 6. TRATAMENTO DAS VARIÁVEIS QUANTITATIVAS
# ------------------------------------------------------------

# Corrigindo erros de digitação, separadores decimais e
# convertendo as variáveis para formato numérico.
dados_limpos <- dados_limpos |>
  mutate(
    idade_dias = parse_number(as.character(idade_dias)),
    
    resistencia_mpa = as.character(resistencia_mpa) |>
      str_replace_all(",", ".") |>
      str_replace_all("O", "0") |>
      parse_number(),
    
    cimento_kg_m3 = as.character(cimento_kg_m3) |>
      str_replace_all("O", "0") |>
      parse_number(),
    
    relacao_a_c = as.character(relacao_a_c) |>
      str_replace_all(",", ".") |>
      parse_number(),
    
    abatimento_mm = parse_number(as.character(abatimento_mm)),
    
    densidade_kg_m3 = parse_number(as.character(densidade_kg_m3)),
    
    absorção_agregado_pct =
      parse_number(as.character(absorção_agregado_pct))
  )


# ------------------------------------------------------------
# 7. PADRONIZAÇÃO E TRATAMENTO DOS VALORES AUSENTES
# ------------------------------------------------------------

# Transformando diferentes representações de ausência em NA.
dados_ausentes <- dados_limpos |>
  mutate(
    across(
      everything(),
      ~ na_if(str_trim(as.character(.x)), "")
    )
  ) |>
  mutate(
    across(
      everything(),
      ~ case_when(
        .x %in% c("NA", "N/A", "NULL", "null", "-") ~ NA_character_,
        TRUE ~ .x
      )
    )
  )

# Identificando as observações que possuem valores ausentes.
dados_excluidos <- dados_ausentes |>
  filter(if_any(everything(), is.na))

# Mantendo somente as observações completas.
dados_limpos <- dados_ausentes |>
  filter(if_all(everything(), ~ !is.na(.)))


# ------------------------------------------------------------
# 8. RECONVERSÃO DAS VARIÁVEIS QUANTITATIVAS
# ------------------------------------------------------------

# Garantindo que as variáveis quantitativas estejam em formato
# numérico após o tratamento dos valores ausentes.
dados_limpos <- dados_limpos |>
  mutate(
    idade_dias = parse_number(idade_dias),
    resistencia_mpa = parse_number(resistencia_mpa),
    cimento_kg_m3 = parse_number(cimento_kg_m3),
    relacao_a_c = parse_number(relacao_a_c),
    abatimento_mm = parse_number(abatimento_mm),
    densidade_kg_m3 = parse_number(densidade_kg_m3),
    absorção_agregado_pct = parse_number(absorção_agregado_pct)
  )


# ------------------------------------------------------------
# 9. VERIFICAÇÃO DA BASE APÓS A LIMPEZA
# ------------------------------------------------------------

# Conferindo a estrutura dos dados limpos.
glimpse(dados_limpos)

# Obtendo um resumo das variáveis.
summary(dados_limpos)


# ------------------------------------------------------------
# 10. ESTATÍSTICAS DESCRITIVAS
# ------------------------------------------------------------

# Calculando estatísticas da resistência por tipo de concreto.
resumo_resistencia <- dados_limpos |>
  group_by(tipo_concreto) |>
  summarise(
    quantidade = n(),
    média = mean(resistencia_mpa),
    mediana = median(resistencia_mpa),
    desvio_padrao = sd(resistencia_mpa),
    mínimo = min(resistencia_mpa),
    máximo = max(resistencia_mpa),
    .groups = "drop"
  ) |>
  arrange(tipo_concreto)

# Apresentando os resultados em uma tabela.
knitr::kable(
  resumo_resistencia,
  digits = 2,
  caption = "Estatísticas descritivas da resistência à compressão por tipo de concreto."
)


# ------------------------------------------------------------
# 11. CRIAÇÃO DE VARIÁVEIS DERIVADAS
# ------------------------------------------------------------

# Criando uma variável com a resistência de referência de cada
# classe de concreto e calculando a resistência relativa.
dados_limpos <- dados_limpos |>
  mutate(
    resistência_referência = case_when(
      tipo_concreto == "C25" ~ 25,
      tipo_concreto == "C30" ~ 30,
      tipo_concreto == "C35" ~ 35,
      tipo_concreto == "C40" ~ 40,
      TRUE ~ NA_real_
    ),
    
    resistencia_relativa =
      resistencia_mpa / resistência_referência,
    
    classificação = case_when(
      resistencia_relativa >= 1.00 ~
        "Igual ou superior à referência",
      resistencia_relativa >= 0.90 ~
        "Próxima da referência",
      TRUE ~
        "Abaixo da referência"
    ),
    
    acima_media =
      resistencia_mpa > mean(resistencia_mpa)
  )


# ------------------------------------------------------------
# 12. INFORMAÇÕES PARA O ENGENHEIRO
# ------------------------------------------------------------

# Resumo final por tipo de concreto.
resumo_engenheiro <- dados_limpos |>
  group_by(tipo_concreto) |>
  summarise(
    quantidade = n(),
    média_resistencia = mean(resistencia_mpa),
    desvio_padrao = sd(resistencia_mpa),
    menor_resistencia = min(resistencia_mpa),
    maior_resistencia = max(resistencia_mpa),
    .groups = "drop"
  ) |>
  arrange(tipo_concreto)

knitr::kable(
  resumo_engenheiro,
  digits = 2,
  caption = "Informações para acompanhamento da qualidade do concreto."
)


# ------------------------------------------------------------
# 13. GRÁFICO 1 — RESISTÊNCIA POR TIPO DE CONCRETO
# ------------------------------------------------------------

ggplot(
  dados_limpos,
  aes(
    x = tipo_concreto,
    y = resistencia_mpa
  )
) +
  geom_boxplot() +
  labs(
    title = "Distribuição da resistência por tipo de concreto",
    x = "Tipo de concreto",
    y = "Resistência à compressão (MPa)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 14. GRÁFICO 2 — CIMENTO X RESISTÊNCIA
# ------------------------------------------------------------

# O filtro evita que um valor muito elevado de cimento distorça
# a visualização da relação entre as variáveis.
ggplot(
  dados_limpos |>
    filter(cimento_kg_m3 < 1000),
  aes(
    x = cimento_kg_m3,
    y = resistencia_mpa
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relação entre consumo de cimento e resistência",
    x = "Cimento (kg/m³)",
    y = "Resistência à compressão (MPa)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 15. RESULTADO FINAL
# ------------------------------------------------------------

# Visualizando as principais informações geradas para apoiar
# o acompanhamento da qualidade do concreto.
dados_limpos |>
  count(tipo_concreto, classificação) |>
  arrange(tipo_concreto, classificação)


