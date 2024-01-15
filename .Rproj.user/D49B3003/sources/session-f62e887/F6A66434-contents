# Charger les résultats prédits
predicted_results <- read.csv('/home/oumayma/Machinelearning/predicted_results.csv', stringsAsFactors = FALSE)

# Vérifier la longueur de la colonne IsFakeProfile dans df_selected
length_is_fake_profile <- length(df_selected$IsFakeProfile)

# Vérifier le nombre de lignes dans predicted_results
num_rows_predicted_results <- nrow(predicted_results)

# Afficher les longueurs
print(paste("Longueur de IsFakeProfile:", length_is_fake_profile))
print(paste("Nombre de lignes dans predicted_results:", num_rows_predicted_results))

# Assurez-vous que la colonne TrueLabel a la même longueur que predicted_results
df_selected$TrueLabel <- df_selected$IsFakeProfile

# Vérifier à nouveau la longueur de la colonne TrueLabel
print(paste("Longueur de TrueLabel après correction:", length(df_selected$TrueLabel)))

# Utiliser match pour aligner les lignes en fonction des noms
df_selected$PredictedIsFakeProfile <- predicted_results$PredictedIsFakeProfile[match(rownames(df_selected), rownames(predicted_results))]

# Vérifier la longueur de la colonne PredictedIsFakeProfile
length_predicted_profile <- length(df_selected$PredictedIsFakeProfile)

# Afficher la longueur de la colonne PredictedIsFakeProfile
print(paste("Longueur de PredictedIsFakeProfile:", length_predicted_profile))

# Créer une matrice de confusion
conf_matrix <- table(df_selected$TrueLabel, df_selected$PredictedIsFakeProfile > 0.5)

# Créer un diagramme en barres
barplot(conf_matrix, beside = TRUE, col = c("lightblue", "lightgreen"), legend = rownames(conf_matrix),
        main = "Résultats du modèle", xlab = "Classe réelle", ylab = "Nombre de profils",
        ylim = c(0, max(conf_matrix) + 5), names.arg = c("Non Fake Profile", "Fake Profile"))

# Ajouter des étiquettes
text(col(barplot(conf_matrix, beside = TRUE, plot = FALSE)), conf_matrix + 1, labels = conf_matrix)
