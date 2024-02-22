
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

summary(Migration_rate)
class(recorte1$Total_migration_stock)
