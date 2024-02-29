library(ggplot2)
library(tidyverse)
library(Rmisc)
datos <- read.csv('DataSetByGender_Aggregates_Excel_AggregateDS.csv')

##-------------------------Segmentar el dataframe---------------------------------

Migration_stock <- data.frame(Origin = datos$Origin,
                              YEAR = datos$YEAR,
                              MS_ALL_TOT = datos$MS_ALL_TOT,
                              MS_MAL_TOT = datos$MS_MAL_TOT,
                              MS_MAL_LOW = datos$MS_MAL_LOW,
                              MS_MAL_MED = datos$MS_MAL_MED,
                              MS_MAL_HIG = datos$MS_MAL_HIG,
                              MS_FEM_TOT = datos$MS_FEM_TOT,
                              MS_FEM_LOW = datos$MS_FEM_LOW,
                              MS_FEM_MED = datos$MS_FEM_MED,
                              MS_FEM_HIG = datos$MS_FEM_HIG
                              )

Migration_rate <- data.frame(Origin = datos$Origin,
                             YEAR = datos$YEAR,
                             MR_ALL_TOT = datos$MR_ALL_TOT,
                             MR_MAL_TOT = datos$MR_MAL_TOT,
                             MR_MAL_LOW = datos$MR_MAL_LOW,
                             MR_MAL_MED = datos$MR_MAL_MED,
                             MR_MAL_HIG = datos$MR_MAL_HIG,
                             MR_FEM_TOT = datos$MR_FEM_TOT,
                             MR_FEM_LOW = datos$MR_FEM_LOW,
                             MR_FEM_MED = datos$MR_FEM_MED,
                             MR_FEM_HIG = datos$MR_FEM_HIG
)

Migration_native_labor <- data.frame(Origin = datos$Origin,
                                     YEAR = datos$YEAR,
                                     NS_ALL_TOT = datos$NS_ALL_TOT,
                                     NS_MAL_TOT = datos$NS_MAL_TOT,
                                     NS_MAL_LOW = datos$NS_MAL_LOW,
                                     NS_MAL_MED = datos$NS_MAL_MED,
                                     NS_MAL_HIG = datos$NS_MAL_HIG,
                                     NS_FEM_TOT = datos$NS_FEM_TOT,
                                     NS_FEM_LOW = datos$NS_FEM_LOW,
                                     NS_FEM_MED = datos$NS_FEM_MED,
                                     NS_FEM_HIG = datos$NS_FEM_HIG
)

totales <- data.frame(Origin = datos$Origin,
                      YEAR = datos$YEAR,
                      MS_ALL_TOT = datos$MS_ALL_TOT, 
                      MR_ALL_TOT=datos$MR_ALL_TOT,
                      NS_ALL_TOT=datos$NS_ALL_TOT)


Rate2<- Migration_rate[(Migration_rate$YEAR!=1990),] #Eliminar datos del 1990 para trabajar con los del 2000


##-------------------------Analisis descriptivo-------------------------


summary(Rate2$MR_ALL_TOT) #informacion basica del dato total

range(Rate2$MR_ALL_TOT) #rango

vari <- var(Rate2$MR_ALL_TOT) #varianza
vari

desv_est <- sd(Rate2$MR_ALL_TOT) #desviacion estandar
desv_est

coef_val <- desv_est/mean(Rate2$MR_ALL_TOT) #coeficiente de valoracion
coef_val

#------------------Alguas graficas------------------
#Grafico de caja
box_plot <- boxplot(Rate2$MR_ALL_TOT, main="Porcentaje de migracion total", ylab="") 

# Histograma de la distribucion
hist_plot <- ggplot(Rate2, aes(x = MR_ALL_TOT)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de MR_ALL_TOT",
       x = "MR_ALL_TOT",
       y = "Frecuencia") +
  theme_minimal()


# Crear un dataframe largo para los valores de migración de hombres y mujeres
long_data <- Rate2 %>%
  select(Origin, MR_MAL_TOT, MR_FEM_TOT) %>%
  pivot_longer(cols = c(MR_MAL_TOT, MR_FEM_TOT), names_to = "Genero", values_to = "Valor")

# Crear un gráfico de barras apiladas
bar_plot <- ggplot(long_data, aes(x = Origin, y = Valor, fill = Genero)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  labs(title = "Valores Totales de Migración para Hombres y Mujeres por País de Origen",
       x = "País de Origen",
       y = "Valor",
       fill = "Género") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()
print(bar_plot)


#-------------------Calculo de los estimadores-------------------

muestra <- Rate2$MR_ALL_TOT
# Si suponemos que conocemos la varianza
media <- mean(muestra) # Pedimos la media
desv <- sd(muestra) # La desviación estándar
N <- length(muestra) # El tamaño válido de la muestra
error.est <- desv/sqrt(N) # Calculamos el error estándar
error <- 1.96*error.est # Fijamos Z=1.96 para indicar un nivel de confianza de 95%
lim.inf <- media-error # Límite inferior del intervalo
lim.sup <- media+error # Límite superior del intervalo

#-----------------------------------Evaluacion del estimador-------------------------------------


n <- length(muestra)
varmedia <- (1/(n^2)) * sum((muestra - mean(muestra))^2)
# Alternativamente: varmedia <- var(muestra)/n
desvmedia <- sqrt(varmedia)
# Remuestreo
B <- 1e+04
media <- numeric(B)
mediana <- numeric(B)
for (k in 1:B) {
  remuestra <- sample(muestra, n, replace = TRUE)
  media[k] <- mean(remuestra)
  # remordenada <- sort(remuestra)
  # mediana[k] <- remordenada[8]
  mediana[k] <- median(remuestra)
}


sesgomediaboot <- mean(media) - mean(muestra)
sesgomedianaboot <- median(mediana) - median(muestra)

#Eficiencia
var(media)
var(mediana)


#-----------Grafica de distintos remuestreos-----------
muestra_1<- sample(1:nrow(Rate2),size=10,replace=FALSE)
m_paises_1 <- Rate2[muestra_1,]

muestra_2<- sample(1:nrow(Rate2),size=100,replace=FALSE)
m_paises_2 <- Rate2[muestra_2,]

muestra_3<- sample(1:nrow(Rate2),size=150,replace=FALSE)
m_paises_3 <- Rate2[muestra_3,]

multiplot(
  ggplot(data = Rate2, aes(x = MR_ALL_TOT)) + geom_histogram(bins = 50) + theme_test() + ggtitle('Población') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  
  
  ggplot(data = m_paises_1, aes(x = MR_ALL_TOT)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 10') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  ggplot(data = m_paises_2, aes(x = MR_ALL_TOT)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 100') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  ggplot(data = m_paises_3, aes(x = MR_ALL_TOT)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 150') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  cols = 2)

