library(ggplot2)
datos <- read.csv('DataSetByGender_Aggregates_Excel_AggregateDS.csv')


Migration_stock <- data.frame(Origen = datos$Origin,
                              Año = datos$YEAR,
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

Migration_rate <- data.frame(Origen = datos$Origin,
                              Año = datos$YEAR,
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

Migration_native_labor <- data.frame(Origen = datos$Origin,
                             Año = datos$YEAR,
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

totales <- data.frame(Origen = datos$Origin,
                       Año = datos$YEAR,
                       Total_migration_stock = datos$MS_ALL_TOT, 
                       Total_migration_rate=datos$MR_ALL_TOT,
                       Total_native_labor_force=datos$NS_ALL_TOT)
########CHATGPT por corregir

# Lee los datos desde el archivo CSV
data <- read.csv("migration_data.csv")

# Crea la gráfica de barras comparando el número de mujeres en MR
ggplot(data, aes(x = YEAR, y = MR_FEM_TOT)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(title = "Comparación del número de mujeres en Migration Rate (MR)",
       x = "Año",
       y = "Número de mujeres en MR") +
  theme_minimal()

