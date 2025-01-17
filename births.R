library(tidyverse)
library(ggdark)

urls <- c(
  "https://repositoriodeis.minsal.cl/DatosAbiertos/VITALES/NACIMIENTOS/Serie_Nacimientos_1992_2000.zip",
  "https://repositoriodeis.minsal.cl/DatosAbiertos/VITALES/NACIMIENTOS/Serie_Nacimientos_2001_2019.zip",
  "https://repositoriodeis.minsal.cl/DatosAbiertos/VITALES/NACIMIENTOS/Serie_Nacimientos_2020_2021.zip"
)

file_names <- c("nacimientos1.zip", "nacimientos2.zip", "nacimientos3.zip")

for (i in seq_along(urls)) {
  download.file(urls[i], paste0("raw-data/", file_names[i]))
  unzip(paste0("raw-data/", file_names[i]), exdir = "raw-data")
}

nacimientos1 <- read_csv2("raw-data/Serie_Nacimientos_1992_2000.csv", locale = locale(encoding = "ISO-8859-1"))
nacimientos2 <- read_csv2("raw-data/Serie_Nacimientos_2001_2019.csv", locale = locale(encoding = "ISO-8859-1"))
nacimientos3 <- read_csv2("raw-data/BD_NAC_2020_2021.csv", locale = locale(encoding = "ISO-8859-1"))

todos_nac <- bind_rows(nacimientos1, nacimientos2, nacimientos3)

todos_nac <- todos_nac |>
  filter(GRUPO_ETARIO_MADRE != "NO ESPECIFICADO") |>
  mutate(
    GRUPO_ETARIO_MADRE = case_when(
      GRUPO_ETARIO_MADRE == "MENORES 15 AÑO" ~ "MENORES 15 AÑOS",
      TRUE ~ GRUPO_ETARIO_MADRE
    ),
    GRUPO_ETARIO_MADRE = fct_relevel(
      GRUPO_ETARIO_MADRE,
      "MENORES 15 AÑOS",
      "15 A 19 AÑOS",
      "20 A 24 AÑOS",
      "25 A 29 AÑOS",
      "30 A 34 AÑOS",
      "35 A 39 AÑOS",
      "40 A 44 AÑOS",
      "45 A 49 AÑOS",
      "50 O MAS AÑOS"
    )
  )

todos_nac |>
  group_by(GRUPO_ETARIO_MADRE, ANO_NAC) |>
  summarise(n = n()) |>
  ggplot(aes(ANO_NAC, n)) +
  geom_area(fill = "#ffafcc", alpha = 0.9) +
  geom_line(linewidth = 1, color = "#f72585") +
  labs(
    title = "Evolución de Nacimientos en Chile (1992 - 2021)",
    x = "Año",
    y = "Nacimientos",
    caption = "Fuente: Datos abiertos DEIS MINSAL"
  ) +
  dark_theme_gray() +
  facet_wrap(~GRUPO_ETARIO_MADRE, scales = "free_y")


todos_nac |> 
  janitor::clean_names() |> 
  group_by(ano_nac) |> 
  summarise(n = n()) |> 
  ggplot(aes(ano_nac, n)) +
  geom_line(color = "#f72585", linewidth = 2) +
  dark_theme_gray() +
    labs(
      title = "Evolución de Nacimientos en Chile (1992 - 2021)",
      x = "Año",
      y = "Nacimientos",
      caption = "Fuente: Datos abiertos DEIS MINSAL"
    )


todos_nac |> 
  janitor::clean_names() |> 
  group_by(ano_nac) |> 
  summarise(n = n()) |> 
  mutate(
    dif = n - lag(n),
    variacion_pct = (dif / lag(n)) * 100
  ) |> 
  na.omit() |> 
  ggplot(aes(x = ano_nac, y = variacion_pct, fill = ifelse(variacion_pct < 0, "Negativo", "Positivo"))) +
  geom_col() +
  scale_fill_manual(values = c("Negativo" = "#db2763", "Positivo" = "#b0db43")) +
  scale_x_continuous(breaks = seq(1992, 2021, 2)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5), labels = paste0(seq(-10, 10, 5), "%")) +
  labs(title = "Variación Porcentual Anual del Número de Nacimientos", 
  x = "Año", 
  y = "Variación Porcentual (%)", 
  fill = "Tipo de variación") +
  dark_theme_gray()
