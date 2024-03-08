# Cargamos las librerías necesarias

install.packages("readr")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("ggplot2")
install.packages("remotes")
install.packages("caTools")
install.packages("class")
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(ggplot2)
library(remotes)
library(caTools)
library(class)

remotes::install_github("cran/DMwR")
library(DMwR)

# Cargamos los datos
datos <- read.csv("homeLoanAproval.csv")

# Verificamos la estructura y los tipos de los datos
str(datos)

# Resumen de los datos
summary(datos)

# * Preprocesamiento de datos

# Verificamos si hay valores faltantes
sum(is.na(datos))

# Imputamos los valores faltantes en variables numéricas con la mediana
datos$LoanAmount[is.na(datos$LoanAmount)] <- median(datos$LoanAmount, na.rm = TRUE)
datos$LoanAmountTerm[is.na(datos$LoanAmountTerm)] <- median(datos$LoanAmountTerm, na.rm = TRUE)

# Convertimos las variables numéricas a enteros
datos$LoanAmount <- as.integer(datos$LoanAmount)
datos$LoanAmountTerm <- as.integer(datos$LoanAmountTerm)

# Verificamos si hay valores faltantes
sum(is.na(datos))

# Convertimos los valores de la columna CoapplicantIncome a enteros
datos$CoapplicantIncome <- as.integer(datos$CoapplicantIncome)

# Verificamos si hay valores faltantes
sum(is.na(datos))

# Imputamos los valores faltantes en CoapplicantIncome con la mediana
datos$CoapplicantIncome[is.na(datos$CoapplicantIncome)] <- median(datos$CoapplicantIncome, na.rm = TRUE)

# Convertimos los valores de la columna CoapplicantIncome a enteros
datos$CoapplicantIncome <- as.integer(datos$CoapplicantIncome)

# Verificamos si hay valores faltantes
sum(is.na(datos))

# Eliminamos caracteres especiales de la columna Dependents
datos$Dependents <- gsub("\\+","", datos$Dependents)

# Convertimos los valores de la columna Dependents a enteros
datos$Dependents <- as.integer(datos$Dependents)

# Verificamos si hay valores faltantes
sum(is.na(datos))

# Imputamos los valores faltantes en Dependents con la mediana
datos$Dependents[is.na(datos$Dependents)] <- median(datos$Dependents, na.rm = TRUE)

# Verificamos si hay valores faltantes
sum(is.na(datos))

# Calculamos la moda de la columna Married (ignorando los valores en blanco)
moda_married <- names(sort(table(datos$Married), decreasing = TRUE))[1]

# Reemplazamos los valores en blanco en Married con la moda
datos$Married[datos$Married == ""] <- moda_married

# # Convertimos Married de nuevo a factor
# datos$Married <- factor(datos$Married)

# Calculamos la moda de la columna Gender (ignorando los valores en blanco)
moda_gender <- names(sort(table(datos$Gender), decreasing = TRUE))[1]

# Reemplazamos los valores en blanco en Gender con la moda
datos$Gender[datos$Gender == ""] <- moda_gender

# # Convertir Gender de nuevo a factor
# datos$Gender <- factor(datos$Gender)

# Calculamos la moda de la columna SelfEmployed (ignorando los valores en blanco)
moda_self_employed <- names(sort(table(datos$SelfEmployed), decreasing = TRUE))[1]

# Reemplazamos los valores en blanco en SelfEmployed con la moda
datos$SelfEmployed[datos$SelfEmployed == ""] <- moda_self_employed

# # Convertimos SelfEmployed de nuevo a factor
# datos$SelfEmployed <- factor(datos$SelfEmployed)

# Mostramos la estructura actualizada del dataframe
str(datos)

# Verificamos si hay valores faltantes
sum(is.na(datos))

# * Eliminación de outliers

# Creamos un boxplot de la variable ApplicantIncome
boxplot(datos$ApplicantIncome, main = "ApplicantIncome")

# Calculamos el rango intercuartílico (IQR) de la variable ApplicantIncome
Q1 <- quantile(datos$ApplicantIncome, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$ApplicantIncome, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Definimos los umbrales para identificar outliers
umbral_superior <- Q3 + 1.5 * IQR
umbral_inferior <- Q1 - 1.5 * IQR

# Identificamos outliers
outliers <- which(datos$ApplicantIncome > umbral_superior | datos$ApplicantIncome < umbral_inferior)

# Imputamos el valor de la mediana a los outliers
datos$ApplicantIncome[outliers] <- median(datos$ApplicantIncome, na.rm = TRUE)

# Creamos un nuevo boxplot después de tratar los outliers
boxplot(datos$ApplicantIncome, main = "ApplicantIncome después de tratar outliers")

# Creamos un boxplot de la variable
boxplot(datos$CoapplicantIncome, main = "CoapplicantIncome")

# Calculamos el rango intercuartílico (IQR) de la variable CoapplicantIncome
Q1 <- quantile(datos$CoapplicantIncome, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$CoapplicantIncome, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Definimos los umbrales para identificar outliers
umbral_superior <- Q3 + 1.5 * IQR
umbral_inferior <- Q1 - 1.5 * IQR

# Identificamos outliers
outliers <- which(datos$CoapplicantIncome > umbral_superior | datos$CoapplicantIncome < umbral_inferior)

# Imputamos el valor de la mediana a los outliers
datos$CoapplicantIncome[outliers] <- median(datos$CoapplicantIncome, na.rm = TRUE)

# Creamos un nuevo boxplot después de tratar los outliers
boxplot(datos$CoapplicantIncome, main = "CoapplicantIncome después de tratar outliers")

# Creamos un boxplot de la variable LoanAmount
boxplot(datos$LoanAmount, main = "LoanAmount")

# Calcular el rango intercuartílico (IQR) de la variable LoanAmount
Q1 <- quantile(datos$LoanAmount, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$LoanAmount, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Definimos los umbrales para identificar outliers
umbral_superior <- Q3 + 1.5 * IQR
umbral_inferior <- Q1 - 1.5 * IQR

# Identificamos outliers
outliers <- which(datos$LoanAmount > umbral_superior | datos$LoanAmount < umbral_inferior)

# Imputamos el valor de la mediana a los outliers
datos$LoanAmount[outliers] <- median(datos$LoanAmount, na.rm = TRUE)

# Creamos un nuevo boxplot después de tratar los outliers
boxplot(datos$LoanAmount, main = "LoanAmount después de tratar outliers")

# * Visualización y comprensión de variables

# Histograma de la variable ApplicantIncome
ggplot(datos, aes(x = ApplicantIncome)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Ingreso del Solicitante",
       x = "Ingreso del Solicitante",
       y = "Frecuencia")

# Gráfico de barras de la variable PropertyArea
ggplot(datos, aes(x = PropertyArea, fill = LoanStatus)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución del Área de Propiedad",
       x = "Área de Propiedad",
       y = "Número de Solicitantes",
       fill = "Estado del Préstamo") +
  theme_minimal()

# Boxplot de la variable LoanAmount agrupada por Education
ggplot(datos, aes(x = Education, y = LoanAmount, fill = Education)) +
  geom_boxplot() +
  labs(title = "Distribución del Préstamo por Educación",
       x = "Educación",
       y = "Préstamo",
       fill = "Educación") +
  theme_minimal()

# Convertimos las variables categóricas a factores
datos$Gender <- as.factor(datos$Gender)
datos$Married <- as.factor(datos$Married)
datos$Dependents <- as.factor(datos$Dependents)
datos$Education <- as.factor(datos$Education)
datos$SelfEmployed <- as.factor(datos$SelfEmployed)
datos$PropertyArea <- as.factor(datos$PropertyArea)
datos$LoanStatus <- as.factor(datos$LoanStatus)
datos$LoanAmountTerm <- as.factor(datos$LoanAmountTerm)

# Verificamos la estructura actualizada de los datos
str(datos)

# Eliminamos la columna Loan_ID
datos_sin_id <- datos[, -which(names(datos) == "Loan_ID")]

# * Balanceo de clases

# Verificamos si las clases están balanceadas
table(datos$LoanStatus)

# Aplicamos SMOTE al conjunto de datos para balancear las clases
datos_balanced <- SMOTE(LoanStatus ~ ., datos_sin_id, perc.over = 2000, k = 5, perc.under = 150)

# Verificamos si las clases están balanceadas después de aplicar SMOTE
table(datos_balanced$LoanStatus)

# * División de conjuntos de entrenamiento y prueba

# Dividimos los datos en conjuntos de entrenamiento y prueba
set.seed(123)

division <- sample.split(datos_balanced$LoanStatus, SplitRatio = 0.7)
entrenamiento <- subset(datos_balanced, division == TRUE)
prueba <- subset(datos_balanced, division == FALSE)

# * Modelado y evaluación

# * K-nn

# Entrenamos el modelo k-NN
modelo_knn <- knn(train = as.matrix(entrenamiento[, c("CoapplicantIncome", "LoanAmount", "ApplicantIncome")]),
                  test = as.matrix(prueba[, c("CoapplicantIncome", "LoanAmount", "ApplicantIncome")]),
                  cl = entrenamiento$LoanStatus,
                  k = 5)

# Calculamos la matriz de confusión y otras métricas de evaluación
confusionMatrix(modelo_knn, prueba$LoanStatus)

# * Árbol de clasificación

# Entrenamos el árbol de decisión
modelo_arbol <- rpart(LoanStatus ~ CoapplicantIncome + LoanAmount + ApplicantIncome + Married + Dependents + Education + SelfEmployed + LoanAmountTerm + PropertyArea,
                        data = entrenamiento,
                        method = "class",
                        control = rpart.control(cp = 0.001, minsplit = 50, minbucket = 10))

# Visualizamos el árbol de decisión
prp(modelo_arbol, type = 2, extra = "auto", nn = TRUE, branch = 1, varlen = 0, yesno = 2)

# Realizamos predicciones con el árbol en los datos de prueba
prediccion_arbol <- predict(modelo_arbol, newdata = prueba, type = "class")

# Calculamos la matriz de confusión y otras métricas de evaluación
confusionMatrix(prediccion_arbol, prueba$LoanStatus)

# * Naive bayes

# Entrenamos el modelo Naive Bayes
modelo_nb <- naiveBayes(LoanStatus ~ CoapplicantIncome + LoanAmount + ApplicantIncome + Married + Dependents + Education + SelfEmployed + LoanAmountTerm + PropertyArea,
                        data = entrenamiento)

# Realizamos predicciones con el modelo Naive Bayes en los datos de prueba
prediccion_nb <- predict(modelo_nb, newdata = prueba, type = "class")

# Calculamos la matriz de confusión y otras métricas de evaluación
confusionMatrix(prediccion_nb, prueba$LoanStatus)
