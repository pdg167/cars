# Cargar librerías necesarias
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(dplyr)
library(tidyr)

# Cargar el dataset mtcars y convertirlo en un dataframe
data(mtcars)
df <- as.data.frame(mtcars)
print("Paso 1: Dataset cargado y convertido a dataframe")
print(head(df))

# 2. Selección de columnas y filtrado de filas
df_filtered <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)
print("Paso 2: Selección y filtrado")
print(head(df_filtered))

# 3. Ordenación y renombrado de columnas
df_ordered <- df_filtered %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)
print("Paso 3: Ordenación y renombrado")
print(head(df_ordered))

# 4. Creación de nuevas columnas y agregación de datos
df_ordered <- df_ordered %>%
  mutate(eficiencia = consumo / potencia)


df_agregado <- df_ordered %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo, na.rm = TRUE),
            potencia_maxima = max(potencia, na.rm = TRUE))

print("Paso 4: Nueva columna y agregación")
print(df_agregado)


# 5. Creación del segundo dataframe y unión
df_gear <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df_unido <- df_ordered %>%
  left_join(df_gear, by = "gear")
print("Paso 5: Unión de dataframes")
print(head(df_unido))


# 6. Transformación de formatos
df_largo <- df_unido %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia), 
               names_to = "medida", 
               values_to = "valor")
print("Paso 6.1: Transformación a formato largo")
print(head(df_largo))

# Manejo de duplicados y regreso a formato ancho
df_ancho <- df_largo %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = medida, values_from = valor)
print("Paso 6.2: Regreso a formato ancho manejando duplicados")
print(head(df_ancho))
