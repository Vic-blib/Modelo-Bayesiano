########## Función para instalar, cargar y llamar paquetes ################

# Definir una función para instalar y cargar paquetes
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# Lista de paquetes necesarios
required_packages <- c(
  "readxl", "tidyverse", "ggplot2", "naniar", "caret", "GGally",
  "corrplot", "bayesplot", "rstanarm", "loo", "projpred", 
  "ROSE", "janitor", "BayesianTools", "bayesrules", "pROC", 
  "dplyr", "fastDummies", "gridExtra", "scales"
)

# Instalar y cargar los paquetes
install_and_load(required_packages)

# Ajustar opciones y tema para gráficos
theme_set(bayesplot::theme_default(base_family = "sans"))
options(mc.cores = 1)

# Llamar a cada una de las librerías explícitamente (opcional)
library(readxl)
library(tidyverse)
library(ggplot2)
library(naniar)
library(caret)
library(GGally)
library(corrplot)
library(bayesplot)
library(rstanarm)
library(loo)
library(projpred)
library(ROSE)
library(janitor)
library(BayesianTools)
library(bayesrules)
library(pROC)
library(dplyr)
library(fastDummies)
library(gridExtra)
library(scales)





######## lectura de los datos ###############


data <- read_excel("Detalle_base Regresión Bayes.xlsx")
# Depuración de la base de datos

head(data)

## variables a usar 

#v = c("genero", "actividad_cliente", "monto_total_aprobado",
#      "estrato", "estado_civil", "puntaje_cifin", "rangoingresos", 
#      "rangomontoaprobado", "regional", "BR")

summary(data)
data <- data %>%
  mutate(across(where(is.character), as.factor))
str(data)
## Valres faltantes

# Instalar e importar los paquetes necesarios

# Ver el total de valores faltantes por columna
# Ver el total de valores faltantes por columna y filtrar las que tienen al menos un NA
missing_values <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  select(where(~ . > 0))

# Mostrar los resultados
print(missing_values)


# Gráfico de los valores faltantes
gg_miss_var(data) + 
  labs(title = "Gráfico de valores faltantes por variable") +
  theme_minimal()


#Podemos notas que la variable que mas datos faltantes tiene es estrato

#Como la cantidad de datos faltantes reprensenta un porcentaje muy bajo de la información se procede a eliminarla.


# Omitir todas las filas con valores NA
data_clean <- na.omit(data)

# Verificar el resultado
print(data_clean)

summary(data_clean)

names(data_clean)


##### variables con las que se decidio trabajar 
v = c("genero", "actividad_cliente", "monto_total_aprobado",
      "estrato", "estado_civil", "puntaje_cifin", "rangoingresos", 
      "rangomontoaprobado", "regional", "BR")

new_data = data_clean %>% 
  select(genero, monto_total_aprobado, estrato, puntaje_cifin, estado_civil, actividad_cliente, puntaje_cifin, rangoingresos, rangomontoaprobado,regional, BR) %>% 
  mutate(puntaje_cifin = as.numeric(puntaje_cifin)) %>% 
  mutate(BR = as.factor(BR))


############### analisis descriptivo ##############

## para la variable de interes



# Crear el gráfico de barras
grafico <- ggplot(new_data, aes(x = BR)) +
  geom_bar(fill = "steelblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 4) +
  labs(title = "Distribución de la variable respuesta (BR)",
       x = "Valor de BR",
       y = "Cantidad de observaciones") +
  theme_minimal()

# Mostrar el gráfico
print(grafico)

# Guardar el gráfico como imagen (por ejemplo, en formato PNG)
ggsave("grafica1.png", plot = grafico, width = 8, height = 6)




## para la covariables



# Graficar la densidad de "monto_total_aprobado" sin notación científica
plot1 <- ggplot(new_data, aes(x = monto_total_aprobado)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(title = "Densidad de monto_total_aprobado", x = "Monto total aprobado") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank()) +
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)

# Graficar la densidad de "puntaje_cifin"
plot2 <- ggplot(new_data, aes(x = puntaje_cifin)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Densidad de puntaje_cifin", x = "Puntaje Cifin") +
  theme_minimal()+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank())  # Eliminar título del eje X


# Graficar barras para la variable "genero"
plot3 <- ggplot(new_data, aes(x = genero)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de genero") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10),
        axis.title.x = element_blank())  # Eliminar título del eje X

# Graficar barras para la variable "estrato"
plot4 <- ggplot(new_data, aes(x = estrato)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribución de estrato") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10),
        axis.title.x = element_blank())  # Eliminar título del eje X

# Graficar barras para la variable "estado_civil"
plot5 <- ggplot(new_data, aes(x = estado_civil)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribución de estado_civil") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10),
        axis.title.x = element_blank())  # Eliminar título del eje X

# Graficar barras para la variable "actividad_cliente"
plot6 <- ggplot(new_data, aes(x = actividad_cliente)) +
  geom_bar(fill = "pink") +
  labs(title = "Distribución de actividad_cliente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10),
        axis.title.x = element_blank())  # Eliminar título del eje X

# Graficar barras para la variable "rangoingresos"
plot7 <- ggplot(new_data, aes(x = rangoingresos)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribución de rangoingresos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10),
        axis.title.x = element_blank())  # Eliminar título del eje X

# Graficar barras para la variable "rangomontoaprobado"
plot8 <- ggplot(new_data, aes(x = rangomontoaprobado)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribución de rangomontoaprobado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 7.9),
        axis.title.x = element_blank())  # Eliminar título del eje X

# Graficar barras para la variable "regional"
plot9 <- ggplot(new_data, aes(x = regional)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Distribución de regional") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10),
        axis.title.x = element_blank())  # Eliminar título del eje X


# Organizar ambos gráficos en un solo mosaico
mosaico2= grid.arrange(plot1, plot2,plot5, ncol = 2)
ggsave("grafico2.png", mosaico2, width = 12, height = 8)

mosaico3=grid.arrange(plot6, plot7, plot8, plot9 , plot3,plot4, ncol = 3)

ggsave("grafico3.png", mosaico3, width = 12, height = 8)


################## grafica de correlación ##################
# Cargar la librería corrplot


# Calcular la matriz de correlación
cor_matrix <- cor(new_data[, c(2, 4)], use = "complete.obs")

# Guardar la gráfica como "grafico4.png"
png("grafico4.png", width = 800, height = 600)

# Graficar solo la parte inferior de la matriz de correlación con los números
# y rotar las etiquetas a 45 grados
corrplot(cor_matrix, method = "circle", 
         addCoef.col = "black", number.cex = 0.7, 
         tl.pos = "lt",    # Etiquetas solo en la izquierda y en la parte superior
         tl.cex = 0.8,     # Ajustar el tamaño de las etiquetas
         tl.srt = 45)      # Rotar las etiquetas a 45 grados

# Cerrar la conexión de gráficos y guardar el archivo
dev.off()


#### pasar a variables dummys###########

#install.packages("fastDummies")


# Importar el paquete

# Convertir las variables categóricas (factor) en columnas dummy
new_data_dummy <- dummy_cols(new_data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)


new_data_dummy$monto_total_aprobado <- scale(new_data_dummy$monto_total_aprobado)
new_data_dummy$puntaje_cifin <- scale(new_data_dummy$puntaje_cifin)


## Ajuste del modelo

new_data_dummy <- new_data_dummy #%>%
#sample_n(30000)



new_data_dummy  = new_data_dummy%>% clean_names()


new_data_dummy = new_data_dummy %>% 
  select(!c(estado_civil_union_libre, estrato_pirata))


############ partición ############



# Establecer una semilla para reproducibilidad
set.seed(123)

# Dividir el conjunto en 80% entrenamiento y 20% prueba
train_index <- createDataPartition(new_data_dummy$br_1, p = 0.8, list = FALSE)

# Crear el conjunto de entrenamiento y prueba
train_data <- new_data_dummy[train_index, ]
test_data <- new_data_dummy[-train_index, ]

# Verificar la dimensión de los conjuntos
dim(train_data)  # Debe tener aproximadamente el 80% de las filas originales
dim(test_data)   # Debe tener aproximadamente el 20% de las filas originales


################ balanceo de la base de datos ############

# Instalar el paquete ROSE si no lo tienes


# Verificar el número total de observaciones en el conjunto de entrenamiento
num_observaciones <- nrow(train_data)  # Total: 67865
num_observaciones_positivos <- sum(train_data$br_1 == 1)  # Número de casos positivos

# Definir N para ser mayor que el número actual de observaciones
# Aquí vamos a duplicar el número total de observaciones
N <- num_observaciones * 2  # Ajusta este número según sea necesario

# Usar ROSE para generar un conjunto balanceado (oversampling)
data_balanced <- ovun.sample(br_1 ~ ., data = train_data, method = "over", N = N)$data


dim(data_balanced)

data_balanced$br_1 %>% table()


########### modelo implementación ##########

# Convertir la variable de respuesta a factor
data_balanced$br_1 <- factor(data_balanced$br_1)


# Crear la fórmula del modelo dinámicamente
# Definir la fórmula del modelo
reg_formula <- formula(paste("br_1 ~", paste(names(data_balanced)[1:(ncol(data_balanced)-1)], collapse = " + ")))


# Definir la distribución a priori
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# Semilla para reproducibilidad
SEED <- 123

data_balanced <- data_balanced %>%
  sample_n(100000)
# Ajustar el modelo bayesiano de regresión logística
post2 <- stan_glm(reg_formula, 
                  data = data_balanced,
                  family = binomial(link = "logit"), 
                  prior = t_prior, 
                  prior_intercept = t_prior, 
                  QR = TRUE,  # Usar QR decomposition
                  seed = SEED, 
                  refresh = 0, chains = 4, iter = 5000*2, seed = 84735)

############ Mostrar los resultados del modelo#################
#print(post1)

# Guardar el modelo post1 en un archivo .rds

saveRDS(post2, file = "modelo_post1_rebalanceado_muestra.rds")

#saveRDS(post1, file = "modelo_post1_rebalanceado.rds")

# Cargar el modelo desde el archivo .rds

post1 <- readRDS(file = "modelo_post1_rebalanceado.rds")
post2 <- readRDS(file = "modelo_post1_rebalanceado_muestra.rds")





summary(post1)

# Cargar la librería bayesplot


# Extraer las cadenas del modelo ajustado
posterior <- as.array(post2)  # post1 es el modelo ajustado con stan_glm

# Definir los grupos de variables para los gráficos
variables_1 <- c("(Intercept)", "monto_total_aprobado", "puntaje_cifin", 
                 "genero_m", "estrato_cuatro", "estrato_dos")

variables_2 <- c("estrato_seis", "estrato_tres", "estrato_uno", 
                 "estado_civil_divorciado", "estado_civil_soltero", "estado_civil_viudo")

variables_3 <- c("actividad_cliente_empleados", "actividad_cliente_pensionados", 
                 "actividad_cliente_policia", "rangoingresos_02_mas_de_1mll_a_2mll", 
                 "rangoingresos_03_mas_de_2mll_a_3mll", "rangoingresos_04_mas_de_3mll_a_5mll")

variables_4 <- c("rangoingresos_05_mas_de_5mll_a_8mll", "rangoingresos_06_mas_de_8mll", 
                 "rangoingresos_indefinido", "rangomontoaprobado_02_mas_de_2mll_a_6mll", 
                 "rangomontoaprobado_03_mas_de_6mll_a_15mll", "rangomontoaprobado_04_mas_de_15mll_a_25mll")

variables_5 <- c("rangomontoaprobado_05_mas_de_25mll_a_50mll", "rangomontoaprobado_06_mas_de_50mll", 
                 "regional_centro_norte", "regional_centro_sur", "regional_digital", "regional_fidelizacion")

variables_6 <- c("regional_norte", "regional_occidental", "regional_santander", 
                 "regional_sur")

# Graficar las cadenas de MCMC para cada conjunto de variables

# Gráfico 1
trace1 <- mcmc_trace(posterior, pars = variables_1) + 
  ggtitle("Cadenas MCMC - Conjunto 1")
trace1

# Gráfico 2
trace2 <- mcmc_trace(posterior, pars = variables_2) + 
  ggtitle("Cadenas MCMC - Conjunto 2")
trace2

# Gráfico 3
trace3 <- mcmc_trace(posterior, pars = variables_3) + 
  ggtitle("Cadenas MCMC - Conjunto 3")
trace3

# Gráfico 4
trace4 <- mcmc_trace(posterior, pars = variables_4) + 
  ggtitle("Cadenas MCMC - Conjunto 4")
trace4

# Gráfico 5
trace5 <- mcmc_trace(posterior, pars = variables_5) + 
  ggtitle("Cadenas MCMC - Conjunto 5")
trace5

# Gráfico 6
trace6 <- mcmc_trace(posterior, pars = variables_6) + 
  ggtitle("Cadenas MCMC - Conjunto 6")
trace6

# Guardar cada gráfico como imagen
ggsave("mcmc_trace_conjunto_1.png", trace1, width = 10, height = 6)
ggsave("mcmc_trace_conjunto_2.png", trace2, width = 10, height = 6)
ggsave("mcmc_trace_conjunto_3.png", trace3, width = 10, height = 6)
ggsave("mcmc_trace_conjunto_4.png", trace4, width = 10, height = 6)
ggsave("mcmc_trace_conjunto_5.png", trace5, width = 10, height = 6)
ggsave("mcmc_trace_conjunto_6.png", trace6, width = 10, height = 6)




# Gráfico de densidades de las distribuciones posteriores para el conjunto 1
plot1 <- plot(post1, "areas", pars = variables_1, prob = 0.95, prob_outer = 1) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Densidades Posteriores - Conjunto 1")
plot1

# Gráfico de densidades de las distribuciones posteriores para el conjunto 2
plot2 <- plot(post1, "areas", pars = variables_2, prob = 0.95, prob_outer = 1) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Densidades Posteriores - Conjunto 2")
plot2

# Gráfico de densidades de las distribuciones posteriores para el conjunto 3
plot3 <- plot(post1, "areas", pars = variables_3, prob = 0.95, prob_outer = 1) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Densidades Posteriores - Conjunto 3")
plot3

# Gráfico de densidades de las distribuciones posteriores para el conjunto 4
plot4 <- plot(post1, "areas", pars = variables_4, prob = 0.95, prob_outer = 1) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Densidades Posteriores - Conjunto 4")
plot4

# Gráfico de densidades de las distribuciones posteriores para el conjunto 5
plot5 <- plot(post1, "areas", pars = variables_5, prob = 0.95, prob_outer = 1) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Densidades Posteriores - Conjunto 5")
plot5

# Gráfico de densidades de las distribuciones posteriores para el conjunto 6
plot6 <- plot(post1, "areas", pars = variables_6, prob = 0.95, prob_outer = 1) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Densidades Posteriores - Conjunto 6")
plot6



ggsave("densidades_conjunto_1.png", plot1, width = 10, height = 6)
ggsave("densidades_conjunto_2.png", plot2, width = 10, height = 6)
ggsave("densidades_conjunto_3.png", plot3, width = 10, height = 6)
ggsave("densidades_conjunto_4.png", plot4, width = 10, height = 6)
ggsave("densidades_conjunto_5.png", plot5, width = 10, height = 6)
ggsave("densidades_conjunto_6.png", plot6, width = 10, height = 6)

##### significancia de los parámetros #####


round(posterior_interval(post1, prob = 0.9), 2)

round(posterior_interval(post2, prob = 0.9), 2)


# validación del modelo


mtr = classification_summary(model = post1, data = test_data, cutoff = 0.5)


mtr$confusion_matrix
mtr$accuracy_rates

#cv_accuracy_1 <- classification_summary_cv(model = post1, data = test_data, cutoff = 0.2, k = 10)
#cv_accuracy_1$cv


y_test = test_data$br_1
y_pred <- posterior_predict(post1, newdata = test_data)
test_data = test_data %>% 
  select(!c(br_1))



y_pred <- posterior_predict(post1, newdata = test_data)






# Valores verdaderos (etiquetas de test)
#y_test <- test_data$BR_1

# Posterior_predict devuelve un conjunto de muestras, necesitamos promediar y hacer un corte en 0.5
y_pred_prob <- colMeans(y_pred)  # Promedio de las muestras de predicción para cada observación
y_pred_class <- ifelse(y_pred_prob > 0.5, 1, 0)  # Convertir probabilidades a clases usando un umbral de 0.5

# Convertir a factores para la matriz de confusión
y_test_factor <- factor(y_test, levels = c(0, 1))  # Asegurarse que las etiquetas verdaderas estén en factor
y_pred_factor <- factor(y_pred_class, levels = c(0, 1))  # Asegurarse que las predicciones estén en factor

# Crear la matriz de confusión
conf_matrix <- confusionMatrix(y_pred_factor, y_test_factor)

###### Mostrar la matriz de confusión ######


print(conf_matrix)





# Tabla de frecuencias para los valores reales (y_test)
y_test_freq <- table(y_test)

# Tabla de frecuencias para las predicciones (y_pred_class)
y_pred_freq <- table(y_pred_class)

# Mostrar las tablas de frecuencia
print("Frecuencia de y_test (valores reales):")
print(y_test_freq)

print("Frecuencia de y_pred_class (valores predichos):")
print(y_pred_freq)





# Calcular la curva ROC usando las probabilidades predichas y las etiquetas reales
roc_score <- roc(y_test, y_pred_prob)

# Graficar la curva ROC
plot(roc_score, main = "ROC curve -- Bayesian Logistic Regression")

# Calcular el AUC
auc_value <- auc(roc_score)
print(paste("AUC:", auc_value))



#### prueba del cpo######



# Calcular los valores de LOO, incluyendo el CPO
loo_result <- loo(post2, save_psis = TRUE)

# Extraer los valores de CPO (Conditional Predictive Ordinate)
cpo_values <- exp(-loo_result$pointwise[,"elpd_loo"])

# Mostrar un resumen de los valores de CPO
summary(cpo_values)

# Ver un histograma de los valores de CPO
hist(cpo_values, main = "Histograma de los valores de CPO", 
     xlab = "CPO", ylab = "Frecuencia", col = "lightblue")

# Calcular la proporción de observaciones con CPO > 0.4
mean_cpo_above_0.4 <- mean(cpo_values > 0.4)
print(paste("Proporción de observaciones con CPO > 0.4:", mean_cpo_above_0.4))

# Calcular el número de observaciones con CPO > 0.4 y el total de observaciones
num_cpo_above_0.4 <- sum(cpo_values > 0.4)
total_observations <- length(cpo_values)
print(paste("Número de observaciones con CPO > 0.4:", num_cpo_above_0.4))
print(paste("Total de observaciones:", total_observations))

# Mostrar las observaciones que tienen los valores de CPO más bajos (potencialmente atípicas)
low_cpo_indices <- which(cpo_values < 0.4)  # Ajustar el umbral a 0.4 como en la referencia
print("Observaciones con CPO bajos (posibles outliers):")
print(low_cpo_indices)



# Visualizar un histograma de los valores de PIT (posterior inclusion test)
# Nota: Esto se realiza manualmente para modelos ajustados con stan_glm
# y puede requerir cálculos adicionales de valores PIT si no se generan automáticamente.


#0 = un buen cliente (el cliente no ha incumplido)
#1 = (el clientes esta inclupimiento)









