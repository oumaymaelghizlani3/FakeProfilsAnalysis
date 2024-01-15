# Charger les données
df <- read.csv('/home/oumayma/donnees.csv', stringsAsFactors = FALSE)

# Prétraitement des données (personnalisez cela en fonction de votre ensemble de données)

# Convertir 'Followers' en numérique, gérer les valeurs manquantes et considérer les profils avec plus de 1000 abonnés comme faux
df$Followers <- as.numeric(as.character(df$Followers))
df$IsFakeProfile <- ifelse(is.na(df$Followers), 0, as.factor(df$Followers > 1000))

# Imputer les valeurs manquantes dans 'Title' et 'Skills' en utilisant la valeur la plus fréquente
columns_to_impute <- c('Title', 'Skills')

for (column in columns_to_impute) {
  most_frequent_value <- names(sort(table(df[[column]]), decreasing=TRUE))[1]
  df[[column]][is.na(df[[column]])] <- most_frequent_value
}

# Calculer la similarité cosinus entre 'Title' et 'Skills'
title_skills_similarity <- numeric(nrow(df))

for (i in 1:nrow(df)) {
  title <- as.character(df$Title[i])
  skills <- as.character(df$Skills[i])
  
  if (nzchar(title) && nzchar(skills)) {
    title_tokens <- unlist(strsplit(tolower(title), " "))
    skills_tokens <- unlist(strsplit(tolower(skills), " "))
    similarity <- sum(title_tokens %in% skills_tokens) / sqrt(length(title_tokens) * length(skills_tokens))
  } else {
    similarity <- 0
  }
  
  title_skills_similarity[i] <- similarity
}

df$TitleSkillsSimilarity <- title_skills_similarity

# Sélectionner les caractéristiques pertinentes après le prétraitement
selected_features <- c('Full.name', 'Skills', 'Followers', 'Relationship', 'TitleSkillsSimilarity', 'IsFakeProfile')

# Assurer que toutes les colonnes spécifiées dans selected_features existent dans le dataframe
missing_columns <- setdiff(selected_features, colnames(df))
if (length(missing_columns) > 0) {
  stop(paste("Les colonnes suivantes spécifiées dans selected_features n'existent pas dans le dataframe :", paste(missing_columns, collapse = ", ")))
}

# Sélectionner les colonnes dans le dataframe
df_selected <- df[, selected_features, drop = FALSE]

# Supprimer les lignes avec des valeurs manquantes après la sélection des colonnes
df_selected <- na.omit(df_selected)

# Diviser les données en ensembles d'entraînement et de test
set.seed(42)
split_index <- createDataPartition(df_selected$IsFakeProfile, p = 0.8, list = FALSE)
train_data <- df_selected[split_index, ]
test_data <- df_selected[-split_index, ]

# Initialiser et entraîner un classificateur Random Forest
clf <- randomForest(IsFakeProfile ~ Followers + TitleSkillsSimilarity + Relationship + Skills, data = train_data)

# Faire des prédictions sur l'ensemble de test
y_pred <- predict(clf, newdata = test_data, type = "response")

# Ajouter une colonne 'IsFakeProfile' dans le dataframe résultant avec un seuil
seuil <- 1.5  # Vous pouvez ajuster ce seuil en fonction de vos besoins

result_df <- data.frame('Full.name' = test_data$`Full.name`, 'PredictedIsFakeProfile' = y_pred)
result_df$IsFakeProfile <- ifelse(result_df$PredictedIsFakeProfile > seuil, "Fake", "Non-Fake")

# Enregistrer le dataframe résultant dans le fichier CSV
write.csv(result_df, 'predicted_results.csv', row.names = FALSE)

cat('Les résultats prédits ont été enregistrés dans predicted_results.csv\n')

