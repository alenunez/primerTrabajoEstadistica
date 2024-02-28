library(ggplot2)
library(tidyverse)
datos <- read.csv('DataSetByGender_Aggregates_Excel_AggregateDS.csv')

##Segmentar el dataframe

Migration_stock <- data.frame(Origin = datos$Origin,
                              Year = datos$YEAR,
                              Total_migration_stock = datos$MS_ALL_TOT,
                              Males_All_education_groups = datos$MS_MAL_TOT,
                              Males_Low_skilled = datos$MS_MAL_LOW,
                              Males_Medium_skilled = datos$MS_MAL_MED,
                              Males_High_skilled = datos$MS_MAL_HIG,
                              Females_All_education_groups = datos$MS_FEM_TOT,
                              Females_Low_skilled = datos$MS_FEM_LOW,
                              Females_Medium_skilled = datos$MS_FEM_MED,
                              Females_High_skilled = datos$MS_FEM_HIG
                              )

Migration_rate <- data.frame(Origin = datos$Origin,
                              Year = datos$YEAR,
                              Total_migration_rate = datos$MR_ALL_TOT,
                              Males_All_education_groups = datos$MR_MAL_TOT,
                              Males_Low_skilled = datos$MR_MAL_LOW,
                              Males_Medium_skilled = datos$MR_MAL_MED,
                              Males_High_skilled = datos$MR_MAL_HIG,
                              Females_All_education_groups = datos$MR_FEM_TOT,
                              Females_Low_skilled = datos$MR_FEM_LOW,
                              Females_Medium_skilled = datos$MR_FEM_MED,
                              Females_High_skilled = datos$MR_FEM_HIG
)

Migration_native_labor <- data.frame(Origin = datos$Origin,
                             Year = datos$YEAR,
                             Total_native_labor_force = datos$NS_ALL_TOT,
                             Males_All_education_groups = datos$NS_MAL_TOT,
                             Males_Low_skilled = datos$NS_MAL_LOW,
                             Males_Medium_skilled = datos$NS_MAL_MED,
                             Males_High_skilled = datos$NS_MAL_HIG,
                             Females_All_education_groups = datos$NS_FEM_TOT,
                             Females_Low_skilled = datos$NS_FEM_LOW,
                             Females_Medium_skilled = datos$NS_FEM_MED,
                             Females_High_skilled = datos$NS_FEM_HIG
)

totales <- data.frame(Origin = datos$Origin,
                       Year = datos$YEAR,
                       Total_migration_stock = datos$MS_ALL_TOT, 
                       Total_migration_rate=datos$MR_ALL_TOT,
                       Total_native_labor_force=datos$NS_ALL_TOT)


Rate2<- Migration_rate[(Migration_rate$Year!=1990),]


##Analisis superficial


summary(Rate2$Total_migration_rate) #informacion basica del dato total

range(Rate2$Total_migration_rate) #rango

vari <- var(Rate2$Total_migration_rate) #varianza
vari

desv_est <- sd(Rate2$Total_migration_rate) #desviacion estandar
desv_est

coef_val <- desv_est/mean(Rate2$Total_migration_rate) #coeficiente de valoracion
coef_val


#Grafico de caja
box_plot <- boxplot(Rate2$Total_migration_rate, main="Porcentaje de migracion total", ylab="") 

# Histograma de la distribucion
hist_plot <- ggplot(Rate2, aes(x = Total_migration_rate)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de MR_ALL_TOT",
       x = "MR_ALL_TOT",
       y = "Frecuencia") +
  theme_minimal()



#################### por terminar ###################

####plot hombres vs mujeres 
# Crear un dataframe agregado con los valores totales de migración para hombres y mujeres
aggregated_data <- Rate2 %>%
  summarise(
    Males_All_education_groups = sum(Males_All_education_groups),
    Females_All_education_groups = sum(Females_All_education_groups)
  )

# Transformar el dataframe a formato largo
aggregated_data_long <- gather(aggregated_data, key = "Genero", value = "Valor", -1)

# Crear un gráfico de barras apiladas 
bar_plot <- ggplot(aggregated_data, aes(x = factor(1), y = Valor, fill = Genero)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  coord_flip() +
  labs(title = "Valores Totales de Migración para Hombres y Mujeres",
       x = "",
       y = "Valor",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mostrar el gráfico
print(bar_plot)



