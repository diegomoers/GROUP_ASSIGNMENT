# Cargar las bibliotecas necesarias
library(dplyr)
library(tidyr)
library(stats)
library(cluster)

# Cargar los datos
ceo_data <- read.csv("/Users/carmencilla/Desktop/Master EUR/Block 2/Data Science and HR Analytics/Replication Assignment/GROUP_ASSIGNMENT/survey_response_data.csv", stringsAsFactors = FALSE)

# Eliminar el CEO con ID 1115
ceo_data <- ceo_data %>%
  filter(id != 1115) %>%
  filter(level1 == "interacting") %>%
  filter(type != "personal_family")

# Crear columnas adicionales para análisis
ceo_data <- ceo_data %>%
  mutate(
    ins_alone = ifelse(ins == 1 & out == 0, 1, 0),
    out_alone = ifelse(ins == 0 & out == 1, 1, 0),
    ins_out = ifelse(ins == 1 & out == 1, 1, 0),
    coordinate = ifelse(n_functions > 1, 1, 0),
    activity_dummy = 1
  )

# Crear tablas cruzadas
df1 <- table(ceo_data$id, ceo_data$F_duration)
df2 <- table(ceo_data$id[ceo_data$F_planned != "missing"], ceo_data$F_planned[ceo_data$F_planned != "missing"])
df3 <- table(ceo_data$id[ceo_data$F_participants != "missing"], ceo_data$F_participants[ceo_data$F_participants != "missing"])
df4 <- table(ceo_data$id, ceo_data$ins_alone)
df5 <- table(ceo_data$id, ceo_data$ins_out)
df6 <- table(ceo_data$id, ceo_data$coordinate)

df_groupcom <- table(ceo_data$id, ceo_data$groupcom)
df_bunits <- table(ceo_data$id, ceo_data$bunits)
df_coo <- table(ceo_data$id, ceo_data$coo)
df_cao <- table(ceo_data$id, ceo_data$cao)

# Crear dataset agregado
agg_data <- data.frame(row.names = unique(ceo_data$id))
agg_data$long <- as.numeric(df1[, "1hrplus"])  # Tiempo para actividades largas
agg_data$planned <- as.numeric(df2[, "planned"])  # Actividades planeadas
agg_data$large <- as.numeric(df3[, "two_plus_ppl"])  # Actividades con múltiples participantes
agg_data$out <- as.numeric(df5[, 2])  # Actividades externas
agg_data$coordinate1 <- as.numeric(df_groupcom[, 2]) + as.numeric(df_bunits[, 2]) + as.numeric(df_coo[, 2]) + as.numeric(df_cao[, 2])
agg_data$coordinate2 <- as.numeric(df6[, 2])

# Calcular participaciones de actividad
activities <- ceo_data %>%
  group_by(id) %>%
  summarise(activity_dummy_sum = sum(activity_dummy))

agg_data_share <- agg_data / activities$activity_dummy_sum

# Descomposición en Componentes Principales (PCA)
cor_matrix <- cor(agg_data_share, use = "complete.obs")
pca_result <- prcomp(agg_data_share, scale. = TRUE)
pca_values <- pca_result$x[, 1:2]  # Usar las dos primeras componentes principales

# Clustering con K-Means
set.seed(123)  # Reproducibilidad
k_means_result <- kmeans(agg_data_share, centers = 2)

# Crear resultados finales
ceo_type <- data.frame(
  id = rownames(agg_data_share),
  pca1 = pca_values[, 1],
  pca2 = pca_values[, 2],
  k_means = k_means_result$cluster
)

# Guardar los resultados en un archivo CSV
write.csv(ceo_type, "clusters.csv", row.names = FALSE)

