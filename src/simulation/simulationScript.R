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
# Leer los datos del archivo .xlsx
path <- "C:\\Users\\PC\\Desktop\\SimulacionProyecto\\pizza_sales.xlsx"
data <- read_excel(path, sheet = 1)

# Formatear las fechas en la columna 'order_date'
data$order_date <- as.POSIXct(data$order_date, format = "%m/%d/%Y")
data$order_date <- as.Date(data$order_date)

# Castear las cantidades de pizzas vendidas a int
data$quantity <- as.integer(data$quantity)

# Agrupar las ventas diarias a partir de la fecha y sumarlas
daily_sales <- data %>%
  group_by(order_date) %>%
  summarise(pizza_count = sum(quantity), .groups = 'drop')

# Calcular el promedio de pizzas vendidas por día considerando todo el año
daily_sales_mean <- mean(daily_sales$pizza_count)

cat("El promedio de cantidad de pizzas vendidas por día durante todo el año es:", daily_sales_mean, "\n")


# ------------------------------------
# Segunda parte de la simulación:
# Analizar el comportamiento del primer año, recien abierta la pizzeria
# por lo que primero la cantidad de clientes será cero, pero progresivamente
# irán aumentando. Y del segundo año, con clientes más estables.
# Se utiliza poisson para calcular la cantidad de posibles clientes, además
# de la distribución normal para calcular un ruido y simular aún más, que pueden
# haber días donde lleguen más clientes que otros.
# también se utilizará como referencia un historico de precio de pizzas como
# referencia pero, también se simulará que tan conveniente es subir o bajar
# los precios a las pizzas que más se llegarían a vender


# Cuando se calcule el promedio diario de ingresos por día, se les restará los
# gastos diarios (costos de preparación, salarios, alquiler ...), cuando se tengan
# los promedios de ingresos diarios se multiplicaran por la cantidad de días
# 365 en la primer simulacion (el primer año con cliente más variables y en 
# aumento) y 365 en el segundo año (con una media de clientes ya más estable) y
# se determinará si en el primer año se consigue recuperar la inversión inicial o
# se requiere el segundo

#------- Datos ---------------

# Datos para la simulación
days_count <- 365 # un año
target_mean <- daily_sales_mean # promedio final de ventas diarias (meta estimada)
## Número de simulaciones
simulation_number <- 8500
# regular_prop_buy <- 0.8     # Probabilidad de que un cliente compre con precios "normales" 
# irregular_prop_buy <- 0.6  # Probabilidad de comprar cuando hay aumento de precios
# additional_percentage <- 0.15 # porcentaje del aumento en el precio de las pizzas
# el lambda va ser el promedio de ventas diarias del historico
# Por qué? Porque de esta forma al usar poisson obtenemos valores que oscilan por esa media


# Asociar pizza_id con su precio respectivo, precio de las pizzas
pizza_prices <- data %>%
  select(pizza_id, unit_price) %>%
  distinct()

# Obtener las 5 pizzas más vendidas en la temporada, para saber a cuales cambiarle
# el precio
top_pizzas <- data %>%
  group_by(pizza_id) %>%
  summarise(total_sold = sum(quantity), .groups = 'drop') %>%
  arrange(desc(total_sold)) %>%
  slice(1:5)

# -------------------------------------

# Esta simulación es para el primer año, considera una cantidad de clientes que se
# espera va en aumento, además se utiliza la distribución normal para simular
# que pueden haber días más buenos que otros
simulate_growth <- function(days_count, target_mean, prop_buy, change_percentage = 0, variability_factor = 0.3) {
  # Crear un vector para almacenar los ingresos diarios
  season_income <- numeric(days_count)
  daily_pizzas_sold <- numeric(days_count)
  # Todo: variable para calcular los costos diarios
  daily_bills <- 1
  
  for (day in 1:days_count) {
    # Calcular el promedio diario creciente, esto considera porque día del primer
    # año vamos para ir aumentando progresivamente la cantidad de clientes
    base_lambda <- (day / days_count) * target_mean
    # Además se agrega un ruido con la distribución normal, ya que no es algo
    # realmente lineal, por lo que con más aleatoriedad se puede simular los
    # días que son más buenos que otros
    noise <- rnorm(1, mean = 0, sd = base_lambda * variability_factor)
    # calcular el nuevo lambda que considera todo lo anterior descrito
    lambda <- max(0, base_lambda + noise)
    
    # Usar poisson para obtener posibles clientes
    possible_clients <- rpois(1, lambda)
    
    # Tomar del histórico n cantidad de órdenes aleatorias de pizzas
    # Importante: de acá podemos saber que pizzas se van a comprar, obtener
    # los ingredientes y asi calcular los gastos diarios, y bueno tambien agregar
    # lo de los salarios, alquiler y demás.
    clients_orders <- sample(data$pizza_id, possible_clients, replace = TRUE)
    current_pizza_prices <- pizza_prices$unit_price[match(clients_orders, pizza_prices$pizza_id)]
    
    daily_income <- 0
    pizzas_sold_today <- 0
    
    if (possible_clients > 0) {
      for (i in 1:possible_clients) {
        if (clients_orders[i] %in% top_pizzas$pizza_id) {
          # Caso con probabilidad de no comprar por cambio de precios
          if (runif(1) < prop_buy) {
            # Mantener precio, sumar o restarle al precio
            daily_income <- daily_income + (current_pizza_prices[i] * change_percentage)
            # restar los gatos, para obtener en si la ganancia, que sera lo que
            # cubra la inversión inicial
            daily_income <- daily_income + current_pizza_prices[i] - daily_bills
            pizzas_sold_today <- pizzas_sold_today + 1
          }
        } else {
          # Compra regular
          daily_income <- daily_income + current_pizza_prices[i] - daily_bills
          pizzas_sold_today <- pizzas_sold_today + 1
        }
      }
    }
    
    season_income[day] <- daily_income
    daily_pizzas_sold[day] <- pizzas_sold_today
  }
  
  list(income = season_income, pizzas_sold = daily_pizzas_sold)
}

# Matrices para almacenar los resultados diarios
income_simulation_regular_price_growth <- matrix(0, nrow = simulation_number, ncol = days_count)
income_simulation_higher_price_growth <- matrix(0, nrow = simulation_number, ncol = days_count)
income_simulation_discounted_price_growth <- matrix(0, nrow = simulation_number, ncol = days_count)

# Matrices para almacenar pizzas vendidas
pizzas_sold_simulation_regular_price_growth <- matrix(0, nrow = simulation_number, ncol = days_count)
pizzas_sold_simulation_higher_price_growth <- matrix(0, nrow = simulation_number, ncol = days_count)
pizzas_sold_simulation_discounted_price_growth <- matrix(0, nrow = simulation_number, ncol = days_count)


# Ejecutar simulaciones para los casos de crecimiento
for (i in 1:simulation_number) {
  # Precios regulares en top pizzas
  simulation_regular_price_result <- simulate_growth(
    days_count = days_count,
    target_mean = target_mean,
    prop_buy = 0.8
  )
  
  income_simulation_regular_price_growth[i, ] <- simulation_regular_price_result$income
  pizzas_sold_simulation_regular_price_growth[i, ] <- simulation_regular_price_result$pizzas_sold
  
  # Precios aumentados en top_pizzas
  simulation_higher_price_result <- simulate_growth(
    days_count = days_count,
    target_mean = target_mean,
    prop_buy = 0.6,
    change_percentage = 0.2 # 20% aumento
  )
  
  income_simulation_higher_price_growth[i, ] <- simulation_higher_price_result$income
  pizzas_sold_simulation_higher_price_growth[i, ] <- simulation_higher_price_result$pizzas_sold
  
  # Precios con descuento en top_pizzas
  simulation_discounted_price_result <- simulate_growth(
    days_count = days_count,
    target_mean = target_mean,
    prop_buy = 0.9, # Mayor probabilidad de compra
    change_percentage = -0.1 # 15% descuento
  )
  
  income_simulation_discounted_price_growth[i, ] <- simulation_discounted_price_result$income
  pizzas_sold_simulation_discounted_price_growth[i, ] <- simulation_discounted_price_result$pizzas_sold
}

# Promedio de ingresos diarios durante todas las simulaciones
average_income_regular_growth <- colMeans(income_simulation_regular_price_growth)
average_income_higher_growth <- colMeans(income_simulation_higher_price_growth)
average_income_discounted_growth <- colMeans(income_simulation_discounted_price_growth)

average_pizzas_sold_regular_price_growth <- colMeans(pizzas_sold_simulation_regular_price_growth)
average_pizzas_sold_higher_price_growth <- colMeans(pizzas_sold_simulation_higher_price_growth)
average_pizzas_sold_discounted_price_growth <- colMeans(pizzas_sold_simulation_discounted_price_growth)

# Mostrar resultados
cat("Promedio de ingresos diarios con precios regulares (crecimiento):", mean(average_income_regular_growth), "\n")
cat("Promedio de ingresos diarios con precios aumentados (crecimiento):", mean(average_income_higher_growth), "\n")
cat("Promedio de ingresos diarios con precios descontados (crecimiento):", mean(average_income_discounted_growth), "\n")

cat("Promedio de pizzas diarias vendidas con precios regulares (crecimiento):", mean(average_pizzas_sold_regular_price_growth), "\n")
cat("Promedio de pizzas diarias vendidas diarios con precios aumentados (crecimiento):", mean(average_pizzas_sold_higher_price_growth), "\n")
cat("Promedio de pizzas diarias vendidas diarios con precios descontados (crecimiento):", mean(average_pizzas_sold_discounted_price_growth), "\n")


# --------------------

# Esta simulación es para el segundo año, considera una cantidad de clientes que se
# estable (esperada), además se utiliza la distribución normal para simular
# que pueden haber días más buenos que otros
simulate_stable <- function(days_count, target_mean, prop_buy, change_percentage = 0, variability_factor = 0.3) {
  # Crear un vector para almacenar los ingresos diarios
  season_income <- numeric(days_count)
  daily_pizzas_sold <- numeric(days_count)
  # Todo: variable para calcular los costos diarios
  daily_bills <- 1
  
  for (day in 1:days_count) {
    # Además se agrega un ruido con la distribución normal, ya que no es algo
    # realmente lineal, por lo que con más aleatoriedad se puede simular los
    # días que son más buenos que otros
    noise <- rnorm(1, mean = 0, sd = target_mean * variability_factor)
    # calcular el nuevo lambda que considera todo lo anterior descrito
    lambda <- max(0, target_mean + noise)
    
    # Usar poisson para obtener posibles clientes
    possible_clients <- rpois(1, lambda)
    
    # Tomar del histórico n cantidad de órdenes aleatorias de pizzas
    # Importante: de acá podemos saber que pizzas se van a comprar, obtener
    # los ingredientes y asi calcular los gastos diarios, y bueno tambien agregar
    # lo de los salarios, alquiler y demás.
    clients_orders <- sample(data$pizza_id, possible_clients, replace = TRUE)
    current_pizza_prices <- pizza_prices$unit_price[match(clients_orders, pizza_prices$pizza_id)]
    
    daily_income <- 0
    pizzas_sold_today <- 0
    
    if (possible_clients > 0) {
      for (i in 1:possible_clients) {
        if (clients_orders[i] %in% top_pizzas$pizza_id) {
          # Caso con probabilidad de no comprar por cambio de precios
          if (runif(1) < prop_buy) {
            # Mantener precio, sumar o restarle al precio
            daily_income <- daily_income + (current_pizza_prices[i] * change_percentage)
            # restar los gatos, para obtener en si la ganancia, que sera lo que
            # cubra la inversión inicial
            daily_income <- daily_income + current_pizza_prices[i] - daily_bills
            pizzas_sold_today <- pizzas_sold_today + 1 
          }
        } else {
          # Compra regular
          daily_income <- daily_income + current_pizza_prices[i] - daily_bills
          pizzas_sold_today <- pizzas_sold_today + 1 
        }
      }
    }
    
    season_income[day] <- daily_income
    daily_pizzas_sold[day] <- pizzas_sold_today 
  }
  
  list(income = season_income, pizzas_sold = daily_pizzas_sold)
}

# Matrices para almacenar los resultados diarios
income_simulation_regular_price_stable <- matrix(0, nrow = simulation_number, ncol = days_count)
income_simulation_higher_price_stable <- matrix(0, nrow = simulation_number, ncol = days_count)
income_simulation_discounted_price_stable <- matrix(0, nrow = simulation_number, ncol = days_count)

# Matrices para almacenar pizzas vendidas
pizzas_sold_simulation_regular_price_stable <- matrix(0, nrow = simulation_number, ncol = days_count)
pizzas_sold_simulation_higher_price_stable <- matrix(0, nrow = simulation_number, ncol = days_count)
pizzas_sold_simulation_discounted_price_stable <- matrix(0, nrow = simulation_number, ncol = days_count)

# Ejecutar simulaciones para los casos de estable
for (i in 1:simulation_number) {
  # Precios regulares en top pizzas
  simulation_regular_price_result <- simulate_stable(
    days_count = days_count,
    target_mean = target_mean,
    prop_buy = 0.8
  )
  
  income_simulation_regular_price_stable[i, ] <- simulation_regular_price_result$income
  pizzas_sold_simulation_regular_price_stable[i, ] <- simulation_regular_price_result$pizzas_sold
  
  
  # Precios aumentados en top_pizzas
  simulation_higher_price_result <- simulate_stable(
    days_count = days_count,
    target_mean = target_mean,
    prop_buy = 0.6,
    change_percentage = 0.2 # 20% aumento
  )
  
  income_simulation_higher_price_stable[i, ] <- simulation_higher_price_result$income
  pizzas_sold_simulation_higher_price_stable[i, ] <- simulation_higher_price_result$pizzas_sold
  
  # Precios con descuento en top_pizzas
  simulation_discounted_price_result <- simulate_stable(
    days_count = days_count,
    target_mean = target_mean,
    prop_buy = 0.9, # Mayor probabilidad de compra
    change_percentage = -0.15 # 15% descuento
  )
  
  income_simulation_discounted_price_stable[i, ] <- simulation_discounted_price_result$income
  pizzas_sold_simulation_discounted_price_stable[i, ] <- simulation_discounted_price_result$pizzas_sold
}

# Promedio de ingresos diarios durante todas las simulaciones
average_income_regular_stable <- colMeans(income_simulation_regular_price_stable)
average_income_higher_stable <- colMeans(income_simulation_higher_price_stable)
average_income_discounted_stable <- colMeans(income_simulation_discounted_price_stable)

average_pizzas_sold_regular_price_stable <- colMeans(pizzas_sold_simulation_regular_price_stable)
average_pizzas_sold_higher_price_stable <- colMeans(pizzas_sold_simulation_higher_price_stable)
average_pizzas_sold_discounted_price_stable <- colMeans(pizzas_sold_simulation_discounted_price_stable)

# Mostrar resultados
cat("Promedio de ingresos diarios con precios regulares (estable):", mean(average_income_regular_stable), "\n")
cat("Promedio de ingresos diarios con precios aumentados (estable):", mean(average_income_higher_stable), "\n")
cat("Promedio de ingresos diarios con precios descontados (estable):", mean(average_income_discounted_stable), "\n")

cat("Promedio de pizzas diarias vendidas con precios regulares (estable):", mean(average_pizzas_sold_regular_price_stable), "\n")
cat("Promedio de pizzas diarias vendidas diarios con precios aumentados (estable):", mean(average_pizzas_sold_higher_price_stable), "\n")
cat("Promedio de pizzas diarias vendidas diarios con precios descontados (estable):", mean(average_pizzas_sold_discounted_price_stable), "\n")

-----------------------------
# Parte 3 de la simulacion analisis particular


# PRIMER AÑO
  
# Calcular los ingresos promedio diarios tomando los resultados de cada simulación
mean_regular_income <- colMeans(income_simulation_regular_price_growth)
mean_higher_income <- colMeans(income_simulation_higher_price_growth)
mean_discounted_income <- colMeans(income_simulation_discounted_price_growth)

# Dataframe para graficar los ingresos promedio diarios :)
df_mean_income <- data.frame(
  Day = 1:days_count,
  income_regular_price = mean_regular_income,
  income_higher_price = mean_higher_income,
  income_discounted_price = mean_discounted_income
)

# Labels del gráfico
label_without_change <- "Sin cambio"
label_with_increase <- "Con aumento"
label_with_discount <- "Con descuento"
type_without_change <- "0%"
type_with_increase <- paste0("Aumento ", 0.2 * 100, "%")
type_with_discount <- paste0("Descuento ", -0.15 * 100, "%")

# Gráfico de líneas
ggplot(df_mean_income) +
  geom_line(aes(x = Day, y = income_regular_price, color = label_without_change, linetype = type_without_change), size = 1) +
  geom_line(aes(x = Day, y = income_higher_price, color = label_with_increase, linetype = type_with_increase), size = 1) +
  geom_line(aes(x = Day, y = income_discounted_price, color = label_with_discount, linetype = type_with_discount), size = 1) +
  theme_minimal() +
  labs(
    title = "Primer Año: Comparación de Ingresos Promedio (Regular, Aumento, Descuento)",
    x = "Día",
    y = "Ingreso Promedio (en dólares)",
    color = "Simulación",
    linetype = "Tipo de Cambio"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = setNames(
    c("skyblue", "lightcoral", "lightgreen"),
    c(label_without_change, label_with_increase, label_with_discount)
  )) +
  scale_linetype_manual(values = setNames(
    c("solid", "dashed", "dotted"),
    c(type_without_change, type_with_increase, type_with_discount)
  ))

# Análisis de densidad para las tres simulaciones

# Dataframe para densidad
df_density <- data.frame(
  mean = c(mean_regular_income, mean_higher_income, mean_discounted_income),
  type = rep(c(label_without_change, label_with_increase, label_with_discount), each = length(mean_regular_income))
)

# Gráfico de densidad
ggplot(df_density, aes(x = mean, fill = type, color = type)) +
  geom_density(alpha = 0.5) + 
  theme_minimal() +
  labs(
    title = "Primer Año: Densidad de Ingresos Promedio (Sin cambio, Con aumento, Con descuento)",
    x = "Ingreso Promedio Diario (en dólares)",
    y = "Densidad",
    fill = "Simulación",
    color = "Simulación"
  ) +
  scale_fill_manual(values = c("skyblue", "lightcoral", "lightgreen")) +
  scale_color_manual(values = c("skyblue", "lightcoral", "lightgreen")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  




# SEGUNDO AÑO
  
# Calcular los ingresos promedio diarios tomando los resultados de cada simulación
mean_regular_income <- colMeans(income_simulation_regular_price_stable)
mean_higher_income <- colMeans(income_simulation_higher_price_stable)
mean_discounted_income <- colMeans(income_simulation_discounted_price_stable)

# Dataframe para graficar los ingresos promedio diarios :)
df_mean_income <- data.frame(
  Day = 1:days_count,
  income_regular_price = mean_regular_income,
  income_higher_price = mean_higher_income,
  income_discounted_price = mean_discounted_income
)

# Labels del gráfico
label_without_change <- "Sin cambio"
label_with_increase <- "Con aumento"
label_with_discount <- "Con descuento"
type_without_change <- "0%"
type_with_increase <- paste0("Aumento ", 0.2 * 100, "%")
type_with_discount <- paste0("Descuento ", -0.15 * 100, "%")

# Gráfico de líneas
ggplot(df_mean_income) +
  geom_line(aes(x = Day, y = income_regular_price, color = label_without_change, linetype = type_without_change), size = 1) +
  geom_line(aes(x = Day, y = income_higher_price, color = label_with_increase, linetype = type_with_increase), size = 1) +
  geom_line(aes(x = Day, y = income_discounted_price, color = label_with_discount, linetype = type_with_discount), size = 1) +
  theme_minimal() +
  labs(
    title = "Segundo Año: Comparación de Ingresos Promedio (Regular, Aumento, Descuento)",
    x = "Día",
    y = "Ingreso Promedio (en dólares)",
    color = "Simulación",
    linetype = "Tipo de Cambio"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = setNames(
    c("skyblue", "lightcoral", "lightgreen"),
    c(label_without_change, label_with_increase, label_with_discount)
  )) +
  scale_linetype_manual(values = setNames(
    c("solid", "dashed", "dotted"),
    c(type_without_change, type_with_increase, type_with_discount)
  ))

# Análisis de densidad para las tres simulaciones

# Dataframe para densidad
df_density <- data.frame(
  mean = c(mean_regular_income, mean_higher_income, mean_discounted_income),
  type = rep(c(label_without_change, label_with_increase, label_with_discount), each = length(mean_regular_income))
)

# Gráfico de densidad
ggplot(df_density, aes(x = mean, fill = type, color = type)) +
  geom_density(alpha = 0.5) + 
  theme_minimal() +
  labs(
    title = "Segundo Año: Densidad de Ingresos Promedio (Sin cambio, Con aumento, Con descuento)",
    x = "Ingreso Promedio Diario (en dólares)",
    y = "Densidad",
    fill = "Simulación",
    color = "Simulación"
  ) +
  scale_fill_manual(values = c("skyblue", "lightcoral", "lightgreen")) +
  scale_color_manual(values = c("skyblue", "lightcoral", "lightgreen")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))