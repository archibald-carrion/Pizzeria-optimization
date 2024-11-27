# Instalar librerías necesarias
install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# No sé porque solo poniendo pizza_sales.xlsx no lo detecta
path <- "C:\\Users\\PC\\Desktop\\SimulacionProyecto\\pizza_sales.xlsx"
data <- read_excel(path, sheet = 1) 
# Leer los campos que contiene el excel
head(data)


# ---------------------------------
# Primera parte de la simulación:
# Determinar en promedio cuantas pizzas se venden por dia en el peak season (Octubre, Noviembre y Diciembre)

# El promedio va ser tomado del .xlsx que vienen a ser los datos historicos de nuestra pizzería 

# La columna 'order_date' tiene las fechas de las ventas, formatearla
data$order_date <- as.POSIXct(data$order_date, format = "%m/%d/%Y")
data$order_date <- as.Date(data$order_date)
# Castear las cantidades de pizzas vendidas a int, para saber cuanto se vende por día
data$quantity <- as.integer(data$quantity)

# Tomar solo los datos de esos meses
# Por las dudas el %>% funciona como un pipe, para los datos :)
# 10 = octubre, 11 = noviembre y 12 = diciembre
filtred_data_by_month <- data %>%
  filter(month(order_date) %in% c(10, 11, 12))

# Agrupar las ventas diarias apartir de la fecha y suamarlas
daily_sales <- filtred_data_by_month %>%
  group_by(order_date) %>%
  summarise(pizza_count = sum(quantity), .groups = 'drop') # Agregar la cantidad de pizzas vendidas cada día

# Calcular el promedio de cantidad de pizzas vendidas por día
daily_sales_mean <- mean(daily_sales$pizza_count)

cat("El promedio de cantidad de pizzas vendidas en peak season es:", daily_sales_mean, "\n")


# ------------------------------------
# Segunda parte de la simulación:
# Analizar el comportamiento de una temporada donde ocurre un aumento de precio en las
# pizzas más vendidas historicamente

days_count <- 90 # (30 días por mes aproximadamente)
regular_prop_buy <- 0.8     # Probabilidad de que un cliente compre con precios "normales" 
irregular_prop_buy <- 0.6  # Probabilidad de comprar cuando hay aumento de precios
additional_percentage <- 0.15 # porcentaje del aumento en el precio de las pizzas
# el lambda va ser el promedio de ventas diarias del historico
# Por qué? Porque de esta forma al usar poisson obtenemos valores que oscilan por esa media
lambda <- daily_sales_mean 

# Asociar pizza_id con su precio respectivo, precio de las pizzas
pizza_prices <- data %>%
  select(pizza_id, unit_price) %>%
  distinct()

# Obtener las 5 pizzas más vendidas en la temporada
top_pizzas <- filtred_data_by_month %>%
  group_by(pizza_id) %>%
  summarise(total_sold = sum(quantity), .groups = 'drop') %>%
  arrange(desc(total_sold)) %>%
  slice(1:5)

simulate <- function(prop_buy, additional_percentage = 0) {
  # crear un vector para almacenar los ingresos de cada día que la simulación corre
  season_income <- numeric(days_count)
  
  for (day in 1:days_count) {
    # usar poisson para obtener un número promedio del historico de posibles clientes
    possible_clients <- rpois(1, lambda)
    
    # Tomar del historico n cantidad de ordenes  random de pizzas, para simular las compras
    clients_orders <- sample(data$pizza_id, possible_clients, replace = TRUE)
    # Obtener los posibles ingresos de por pizza, si todos los clientes compran
    current_pizza_prices <- pizza_prices$unit_price[match(clients_orders, pizza_prices$pizza_id)]
    
    daily_income <- 0 
    
    if (possible_clients > 0) {
      # "Atender" a todos los posibles clientes
      for (i in 1:possible_clients) {
        # Si la orden pertenece al top de las pizzas más vendidas, existe la probabilidad
        # de que el cliente no la compre, ¿Por qué? Por que hubo un aumento de precio
        # entonces puede que cambie de opinión, se analiza este caso, si el cliente
        # decide comprarla se suma el aumento del precio al income
        if (clients_orders[i] %in% top_pizzas$pizza_id) {
          if (runif(1) < prop_buy) {
            daily_income <- daily_income + (current_pizza_prices[i] * additional_percentage)
            daily_income <- daily_income + current_pizza_prices[i]
          }
        } else {
          # Sí no es una de las del top, es un posible cliente del historico, por
          # que si la compra. Además el caso a analizar es cuando hay aumento de en
          # las más vendidas
          daily_income <- daily_income + current_pizza_prices[i]
        }
      }
    }
    
    season_income[day] <- daily_income
  }
  
  return(season_income)
}


# ------------------------------------------------
# Tercera Parte de la simulación:
# Correr entre 7000-10000 simulación de la temporada y analizar el promedio de ventas
# diarias

# Simulaciones
simulation_number <- 100
income_simulation_regular_price <- matrix(0, nrow = simulation_number, ncol = days_count)
income_simulation_higher_price <- matrix(0, nrow = simulation_number, ncol = days_count)

# Ejecutar las simulaciones con las n simulaciones para cada caso
for (i in 1:simulation_number) {
  income_simulation_regular_price[i, ] <- simulate(prop_buy = regular_prop_buy)
  income_simulation_higher_price[i, ] <- simulate(prop_buy = irregular_prop_buy, additional_percentage = additional_percentage) 
}


-----------------------------
# Analisis particular, se toma en cuenta cada día

# Calcular los ingresos promedio diarios tomando los resultados de cada simulación
mean_regular_income <- colMeans(income_simulation_regular_price)
mean_irregular_income <- colMeans(income_simulation_higher_price)

# Dataframe para graficar
df_mean_income <- data.frame(
  Day = 1:days_count,
  income_regular_price = mean_regular_income,
  income_irregular_price = mean_irregular_income
)

# Labels del grafico
type_with_change <- paste0(additional_percentage * 100, "%")
label_without_change <- "Sin aumento"
label_with_change <- "Con aumento"
type_without_change <- "0%"

# Crear gráfico con leyendas ajustadas
ggplot(df_mean_income) +
  geom_line(aes(x = Day, y = income_regular_price, color = label_without_change, linetype = type_without_change), size = 1) +
  geom_line(aes(x = Day, y = income_irregular_price, color = label_with_change, linetype = type_with_change), size = 1) +
  theme_minimal() +
  labs(
    title = "Precio Base vs Aumento en las Top Pizzas",
    x = "Día",
    y = "Ingreso Promedio (en dálares)",
    color = "Simulación",  # Label para colores
    linetype = "Porcentaje de aumento"  # label para tipos de línea
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = setNames(
    c("skyblue", "lightcoral"),
    c(label_without_change, label_with_change)
  )) +
  scale_linetype_manual(values = setNames(
    c("solid", "dashed"),
    c(type_without_change, type_with_change)
  ))

# ---------------------------------------
# Una vista más general del problema

# Analizar las densidades de los resultados
df_density <- data.frame(
  mean = c(mean_regular_income, mean_irregular_income),
  type = rep(c("Sin aumento", "Con aumento"), each = length(mean_regular_income))
)

# Crear gráfico de densidad
ggplot(df_density, aes(x = mean, fill = type, color = type)) +
  geom_density(alpha = 0.5) + 
  theme_minimal() +
  labs(
    title = "Densidad de Ingresos Promedio (Sin y Con aumento)",
    x = "Ingreso Promedio Diario (en dólares)",
    y = "Densidad",
    fill = "Simulación",
    color = "Simulación"
  ) +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +
  scale_color_manual(values = c("lightcoral", "skyblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))