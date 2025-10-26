library(dplyr)
library(lubridate)
library(tidyr)
library(randomForest)

# Importer les données depuis le fichier csv
elecData <- read.csv("Electricity_WHE.csv", header = TRUE, sep = ",")
eauData <- read.csv("Water_WHW.csv", header = TRUE, sep = ",")
gazData <- read.csv("NaturalGas_WHG.csv", header = TRUE, sep = ",")
meteoData <- read.csv("Climate_HourlyWeather.csv", header = TRUE, sep = ",")


# Garder les données utiles
elec <- elecData[, c("unix_ts", "St")]
eau  <- eauData[, c("unix_ts", "counter")]
gaz  <- gazData[, c("unix_ts", "counter")]
meteo <- meteoData[, c("Date.Time", "Temp..C.", "Rel.Hum....", "Wind.Spd..km.h.")]


# Convertir unix_ts en datetime POSIXct
elec <- elec %>%
  mutate(datetime = as.POSIXct(unix_ts, origin = "1970-01-01", tz = "UTC"))

eau <- eau %>%
  mutate(datetime = as.POSIXct(unix_ts, origin = "1970-01-01", tz = "UTC"))

gaz <- gaz %>%
  mutate(datetime = as.POSIXct(unix_ts, origin = "1970-01-01", tz = "UTC"))

meteo <- meteo %>%
  mutate(periode = as.POSIXct(Date.Time, tz = "UTC"))


# Créer une colonne "periode" qui représente la tranche horaire (arrondi à l’heure)
elec <- elec %>% mutate(periode = floor_date(datetime, "hour"))
eau  <- eau %>% mutate(periode = floor_date(datetime, "hour"))
gaz  <- gaz %>% mutate(periode = floor_date(datetime, "hour"))


# Regrouper par période et calculer les valeurs horaires
# Comme les compteurs (St, counter) sont cumulés, on calcule la différence par heure
elec <- elec %>%
  arrange(datetime) %>%
  group_by(periode) %>%
  summarise(energie_kWh = if (n() > 1) (max(St) - min(St)) / 1000 else NA_real_, .groups = "drop")

eau <- eau %>%
  arrange(datetime) %>%
  group_by(periode) %>%
  summarise(eau_L = if (n() > 1) (max(counter) - min(counter)) else NA_real_, .groups = "drop")

gaz <- gaz %>%
  arrange(datetime) %>%
  group_by(periode) %>%
  summarise(gaz_dm3 = if (n() > 1) (max(counter) - min(counter)) else NA_real_, .groups = "drop")

#Renommer les colonnes de meteo
meteo <- meteo %>%
  rename(
    temperature_C = Temp..C.,
    humidite      = Rel.Hum....,
    vent_kmh      = Wind.Spd..km.h.
  )

# Joindre toutes les données
data <- elec %>%
  full_join(eau, by = "periode") %>%
  full_join(gaz, by = "periode") %>%
  full_join(meteo, by = "periode") %>%
  arrange(periode)

# Ajout colonne de l'heure (0–23) à partir de la date/heure
data$heure <- as.numeric(format(data$periode, "%H"))

# Ajout colonne de transformation cyclique de l'heure
data$heure_sin <- sin(2 * pi * data$heure / 24)
data$heure_cos <- cos(2 * pi * data$heure / 24)

# Ajout colonne du jour de la semaine (lubridate)
data$jour <- wday(data$periode, label = TRUE, abbr = FALSE, week_start = 1)

# Renommer les jours
levels(data$jour) <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")

# convertir en facteur non ordonné
data$jour <- factor(as.character(data$jour),
                    levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
                    ordered = FALSE)


#-------------------------------------------------------------------------------
#Fonction pour voir les valeurs abérantes
#-------------------------------------------------------------------------------
resume_table <- function(table) {
  # Sélection des colonnes numériques
  num_data <- table[sapply(table, is.numeric)]
  
  # Construction du résumé
  res <- data.frame(
    Min = sapply(num_data, function(v) round(min(v, na.rm = TRUE), 1)),
    Moyenne = sapply(num_data, function(v) round(mean(v, na.rm = TRUE), 1)),
    Max = sapply(num_data, function(v) round(max(v, na.rm = TRUE), 1)),
    NAs = sapply(num_data, function(v) sum(is.na(v))),
    
    zeros = sapply(num_data, function(v) {
      round(sum(v == 0, na.rm = TRUE) / sum(!is.na(v)) * 100, 1)
    }),
    
    outliers = sapply(num_data, function(v) {
      Q1 <- quantile(v, 0.25, na.rm = TRUE)
      Q3 <- quantile(v, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      outliers <- sum(v < (Q1 - 1.5 * IQR) | v > (Q3 + 1.5 * IQR), na.rm = TRUE)
      round((outliers / sum(!is.na(v))) * 100, 1)
    }),
    
    # 10 plus petites valeurs
    Min = sapply(num_data, function(v) {
      vals <- sort(v, na.last = NA)
      paste(round(head(vals, 10), 1), collapse = ", ")
    }),
    
    # 10 plus grandes valeurs
    Max = sapply(num_data, function(v) {
      vals <- sort(v, decreasing = TRUE, na.last = NA)
      paste(round(head(vals, 10), 1), collapse = ", ")
    }),
    
    stringsAsFactors = FALSE
  )
  
  return(res)
}
#-------------------------------------------------------------------------------
# Fonction distribution + indicateur symétrie
# **La variable doit exister et être numérique**
#-------------------------------------------------------------------------------
plot_skewness <- function(data, var_name, bins = 60, save_path = NULL) {
  
  # Package requis
  require(ggplot2)
  require(e1071)
  
  # Extraction dynamique de la variable choisie
  variable <- data[[var_name]]
  
  # Calcul de l’asymétrie (skewness)
  val_skew <- skewness(variable, na.rm = TRUE)
  
  # Création du graphique
  p <- ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(bins = bins, fill = "steelblue", color = "white") +
    labs(
      title = paste("Distribution de la variable", var_name),
      x = var_name,
      y = "Fréquence"
    ) +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = paste("Skewness =", round(val_skew, 2)),
      hjust = 1.1, vjust = 2,
      size = 5, color = "red"
    ) +
    theme_minimal()
  
  # Affichage du graphique
  print(p)
  
  # Enregistrement du graphique si un chemin est fourni
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, width = 8, height = 6, dpi = 300)
  }
}
#-------------------------------------------------------------------------------
# Fonction pour la corrélation des variables numériques
#-------------------------------------------------------------------------------
correlation <- function(data, vars, save_path = NULL) {
  # Package requis
  require(corrplot)
  
  # Extraire les variables explicatives
  vars_df <- data[, vars, drop = FALSE]
  
  # Calcul de la matrice de corrélation
  cor_matrix <- cor(vars_df, use = "complete.obs")
  
  # Palette de couleur du vert (faible) au rouge (forte)
  col <- colorRampPalette(c("green", "yellow", "red"))(200)
  
  # Enregistrement du graphique si un chemin est fourni
  if (!is.null(save_path)) {
    png(filename = save_path, width = 800, height = 600, res = 120)
  }
  
  # Visualisation graphique avec coefficients arrondis à 3 décimales
  corrplot::corrplot(
    round(cor_matrix, 3),
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    col = col,
    addCoef.col = "black",
    number.cex = 0.8,
    diag = TRUE,
    mar = c(0, 0, 1, 0)
  )
  
  # Ferme le périphérique si un fichier est spécifié
  if (!is.null(save_path)) {
    dev.off()
    cat("\nGraphique enregistré dans :", save_path, "\n")
  }
}
#-------------------------------------------------------------------------------
#Fonction pour vérifier les conditions du modèle de regression linéaire
#-------------------------------------------------------------------------------
verif_model <- function(modele, data, var_name = NULL, save_path = NULL) {

  require(ggplot2)
  require(dplyr)
  require(lmtest)
  require(car)
  require(moments)
  require(patchwork)
  
  # Préparation
  res <- resid(modele)
  fit <- fitted(modele)
  df_res <- data.frame(fit = fit, resid = res)
  
  # Linéarité & Homoscédasticité
  bp <- bptest(modele)
  reset <- resettest(modele)
  
  p1 <- ggplot(df_res, aes(x = fit, y = resid)) +
    geom_point(color = "steelblue") +
    geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
    geom_hline(yintercept = 0, color = "red", linetype = 2) +
    labs(title = "Linéarité & Homoscédasticité",
         x = "Valeurs ajustées", y = "Résidus") +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = paste0(
        "Breusch–Pagan p = ", formatC(bp$p.value, digits = 4, format = "f"), "\n",
        ifelse(bp$p.value > 0.05, "Homoscédasticité", "Hétéroscédasticité"), "\n",
        "RESET p = ", formatC(reset$p.value, digits = 4, format = "f"), "\n",
        ifelse(reset$p.value > 0.05, "Linéaire", "Non-linéaire")
      ),
      hjust = 1.1, vjust = 1.2, size = 3, color = "red"
    ) +
    theme_minimal()
  
  # Q-Q Plot normalité
  qq_data <- data.frame(sample = sort(res), theoretical = qqnorm(res, plot.it = FALSE)$x)
  p2 <- ggplot(qq_data, aes(sample = sample)) +
    stat_qq(color = "steelblue") +
    stat_qq_line(color = "red") +
    labs(title = "Normalité des résidus",
         x = "Théorique", y = "Observé") +
    theme_minimal()
  
  # Histogramme des résidus
  skew_val <- skewness(res)
  skew_text <- paste0(
    "Skewness = ", round(skew_val, 2), " — ",
    ifelse(abs(skew_val) < 0.5, "Symétrique",
           ifelse(skew_val > 0, "Asymétrie droite", "Asymétrie gauche"))
  )
  
  p3 <- ggplot(df_res, aes(x = resid)) +
    geom_histogram(aes(y = ..density..), bins = 60,
                   fill = "skyblue", color = "white") +
    geom_density(color = "red", linewidth = 1.1) +
    labs(title = "Histogramme des résidus", x = "Résidus", y = "Densité") +
    annotate("text", x = Inf, y = Inf, label = skew_text,
             hjust = 1.1, vjust = 2, size = 3, color = "red") +
    theme_minimal()
  
  # Tests complémentaires
  dw <- dwtest(modele)
  vifs <- vif(modele)
  
  # Titre dynamique selon la variable
  titre_modele <- ifelse(is.null(var_name),
                         "Vérification du modèle de régression",
                         paste("Vérification du modèle pour", var_name))
  
  # Affichage graphique
  combined <- (p1 | p2 | p3) + plot_annotation( 
    title = titre_modele, 
    theme = theme(plot.title = element_text(face = "bold", size = 14), 
                  plot.subtitle = element_text(size = 11)) ) 
  print(combined)
  
  # Enregistrement du graphique si un chemin est fourni
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = combined, width = 12, height = 6, dpi = 300)
    cat("\nGraphique enregistré dans :", save_path, "\n")
  }
  
  # Résumé textuel
  cat("\n=== Vérification du modèle ===\n")
  cat("Breusch–Pagan :", formatC(bp$p.value, digits = 4, format = "f"),
      ifelse(bp$p.value > 0.05, " → Homoscédasticité", " → Hétéroscédasticité"), "\n")
  cat("RESET test :", formatC(reset$p.value, digits = 4, format = "f"),
      ifelse(reset$p.value > 0.05, " → Linéarité", " → Non-linéaire"), "\n")
  cat("Durbin–Watson :", formatC(dw$p.value, digits = 4, format = "f"),
      ifelse(dw$p.value > 0.05, " → Indépendance", " → Autocorrélation"), "\n")
  cat("Skewness :", skew_text, "\n\n")
  cat("=== Multicolinéarité (VIF) ===\n")
  print(vifs)
}
#-------------------------------------------------------------------------------
#Fonction qui affiche un graph des p value et R2 du modele linéaire
#-------------------------------------------------------------------------------
lm_graph <- function(modele, var_name = NULL, save_path = NULL) {
  
  # Packages requis
  require(broom)
  require(ggplot2)
  require(dplyr)
  
  # Extraction des coefficients
  coef_data <- broom::tidy(modele)
  r2_values <- summary(modele)
  r2 <- r2_values$r.squared
  r2_adj <- r2_values$adj.r.squared
  
  # Codes de significativité
  coef_data <- coef_data %>%
    mutate(signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE ~ ""
    ))
  
  # Titre dynamique
  titre_modele <- ifelse(is.null(var_name),
                         "Significativité des variables du modèle (lm)",
                         paste("Significativité des variables pour", var_name))
  
  # Graphique
  p <- ggplot(coef_data, aes(x = reorder(term, p.value), y = -log10(p.value), fill = p.value < 0.05)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02")) +
    geom_text(aes(label = signif), hjust = -0.3, size = 5, color = "black") +
    labs(
      title = titre_modele,
      subtitle = paste0("R² = ", round(r2, 3), " | R² ajusté = ", round(r2_adj, 3)),
      x = "Variables explicatives",
      y = expression(-log[10](p-value))
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30")
    )
  
  # Eregistrement du graphique si un chemin est fourni
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, width = 10, height = 6, dpi = 300)
    cat("\nGraphique enregistré dans :", save_path, "\n")
  }
  
  return(p)
}
#-------------------------------------------------------------------------------
# Fonction qui affiche un graph de l'importance des variables
# et l'indicateur de performance OOB R² d'un modèle Random Forest
#-------------------------------------------------------------------------------
rf_graph <- function(modele, var_name = NULL, save_path = NULL) {
  
  require(ggplot2)
  require(dplyr)
  
  # Indicateur de performance
  r2_oob <- tail(modele$rsq, 1)
  
  # Extraction de l’importance des variables
  imp_data <- as.data.frame(importance(modele))
  imp_data$variable <- rownames(imp_data)
  
  # Adapter selon les colonnes disponibles
  if ("MeanDecreaseGini" %in% names(imp_data)) {
    imp_data <- imp_data %>%
      mutate(Importance = MeanDecreaseGini)
  } else if ("IncNodePurity" %in% names(imp_data)) {
    imp_data <- imp_data %>%
      mutate(Importance = IncNodePurity)
  } else {
    stop("Aucune colonne d'importance reconnue trouvée dans les résultats.")
  }
  
  imp_data <- imp_data %>% arrange(desc(Importance))
  
  # Titre dynamique
  titre_modele <- ifelse(is.null(var_name),
                         "Importance des variables du modèle Random Forest",
                         paste("Importance des variables pour", var_name))
  
  # Graphique
  p <- ggplot(imp_data, aes(x = reorder(variable, Importance), y = Importance, fill = Importance)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_gradient(low = "#d95f02", high = "#1b9e77") +
    labs(
      title = titre_modele,
      subtitle = paste0("OOB R² = ", round(r2_oob, 3)),
      x = "Variables explicatives",
      y = "Importance"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30")
    )
  
  # Enregistrement du graphique si un chemin est fourni
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, width = 10, height = 6, dpi = 300)
    cat("\nGraphique enregistré dans :", save_path, "\n")
  }
  
  return(p)
}
#-------------------------------------------------------------------------------
# Fonction pour évaluer les modèles
#-------------------------------------------------------------------------------
evaluer_modeles <- function(models, data_test, save_path = "comparaison_modeles.png") {
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(Metrics)
  
  # Fonction interne pour évaluer un seul modèle
  evaluer <- function(modele, data_test, variable_cible, nom_modele) {
    pred <- predict(modele, newdata = data_test)
    rmse_val <- rmse(data_test[[variable_cible]], pred)
    mae_val  <- mae(data_test[[variable_cible]], pred)
    r2_val   <- cor(data_test[[variable_cible]], pred)^2
    data.frame(
      Modele = nom_modele,
      Variable = variable_cible,
      RMSE = rmse_val,
      MAE = mae_val,
      R2 = r2_val
    )
  }
  
  # Évaluation de tous les modèles fournis
  res <- do.call(rbind, lapply(models, function(m) {
    evaluer(m$modele, data_test, m$variable, m$nom)
  }))
  
  # Ajout du type de modèle
  res$Type <- ifelse(grepl("LM", res$Modele), "Linéaire", "Forêt Aléatoire")
  
  # Transformation des données pour le graphique
  res_long <- res %>%
    pivot_longer(
      cols = c("RMSE", "MAE", "R2"),
      names_to = "Metrique",
      values_to = "Valeur"
    )
  
  # Création du graphique comparatif
  p <- ggplot(res_long, aes(x = Metrique, y = Valeur, fill = Type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = round(Valeur, 3)),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3.5
    ) +
    facet_wrap(~ Variable, scales = "free_y") +
    labs(
      title = "Comparaison des performances : LM vs RF",
      x = "Métrique",
      y = "Valeur",
      fill = "Type de modèle"
    ) +
    theme_minimal(base_size = 13) +
    theme(strip.text = element_text(face = "bold"))
  
  # Eregistrement du graphique final
  ggsave(filename = save_path, plot = p, width = 10, height = 6, dpi = 300)
  cat("\nGraphique enregistré dans :", save_path, "\n")
}
#-------------------------------------------------------------------------------
# Fonction pour visualiser la consommation et les valeurs extrèmes
#-------------------------------------------------------------------------------
graph_conso <- function(data, variable = c("temperature_C", "heure", "humidite", "jour"), save_path = NULL) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  variable <- match.arg(variable)
  
  # Préparation selon la variable
  if (variable == "temperature_C") {
    data_var <- data %>% mutate(var_arrondie = round(temperature_C))
    titre <- "Évolution relative de la consommation selon la température"
    x_label <- "Température (°C, arrondie)"
    x_is_discrete <- FALSE
    # pour les positions min/max (arrondies sur l'axe)
    map_x <- function(x) round(x)
    
  } else if (variable == "heure") {
    data_var <- data %>% mutate(var_arrondie = heure)
    titre <- "Évolution relative de la consommation selon l'heure de la journée"
    x_label <- "Heure (0–24 h)"
    x_is_discrete <- FALSE
    map_x <- function(x) x
    
  } else if (variable == "humidite") {
    breaks_h <- seq(0, 100, by = 10)
    labels_h <- paste0(seq(0, 90, by = 10), "–", seq(10, 100, by = 10))
    data_var <- data %>%
      mutate(var_arrondie = cut(humidite,
                                breaks = breaks_h,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = labels_h))
    titre <- "Évolution relative de la consommation selon l'humidité"
    x_label <- "Tranche d'humidité (%)"
    x_is_discrete <- TRUE
    # convertit une humidité réelle en index de niveau (1..n) pour geom_vline
    map_x <- function(x) {
      lvl <- cut(x, breaks = breaks_h, include.lowest = TRUE, right = FALSE, labels = labels_h)
      match(as.character(lvl), labels_h)
    }
    
  } else { # jour
    niveaux_jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
    data_var <- data %>%
      mutate(var_arrondie = factor(as.character(jour), levels = niveaux_jours, ordered = FALSE))
    titre <- "Évolution relative de la consommation selon le jour de la semaine"
    x_label <- "Jour de la semaine"
    x_is_discrete <- TRUE
    map_x <- function(x) match(as.character(x), niveaux_jours)
  }
  
  # Moyennes, pivot long, standardisation
  data_moy <- data_var %>%
    group_by(var_arrondie) %>%
    summarise(
      energie_kWh = mean(energie_kWh, na.rm = TRUE),
      eau_L       = mean(eau_L,       na.rm = TRUE),
      gaz_dm3     = mean(gaz_dm3,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(energie_kWh, eau_L, gaz_dm3),
      names_to = "type",
      values_to = "moyenne"
    ) %>%
    group_by(type) %>%
    mutate(moyenne_centrée = (moyenne - mean(moyenne, na.rm = TRUE)) / sd(moyenne, na.rm = TRUE)) %>%
    ungroup()
  
  # Valeurs réelles min/max (sur les données brutes)
  extremes <- data %>%
    summarise(
      min_energie_x = get(variable)[which.min(energie_kWh)],
      max_energie_x = get(variable)[which.max(energie_kWh)],
      min_eau_x     = get(variable)[which.min(eau_L)],
      max_eau_x     = get(variable)[which.max(eau_L)],
      min_gaz_x     = get(variable)[which.min(gaz_dm3)],
      max_gaz_x     = get(variable)[which.max(gaz_dm3)]
    )
  
  # Convertir les positions x pour geom_vline
  if (x_is_discrete) {
    x_min_energie <- map_x(extremes$min_energie_x)
    x_max_energie <- map_x(extremes$max_energie_x)
    x_min_eau     <- map_x(extremes$min_eau_x)
    x_max_eau     <- map_x(extremes$max_eau_x)
    x_min_gaz     <- map_x(extremes$min_gaz_x)
    x_max_gaz     <- map_x(extremes$max_gaz_x)
  } else {
    x_min_energie <- map_x(extremes$min_energie_x)
    x_max_energie <- map_x(extremes$max_energie_x)
    x_min_eau     <- map_x(extremes$min_eau_x)
    x_max_eau     <- map_x(extremes$max_eau_x)
    x_min_gaz     <- map_x(extremes$min_gaz_x)
    x_max_gaz     <- map_x(extremes$max_gaz_x)
  }
  
  # Graphique
  p <- ggplot(data_moy, aes(x = var_arrondie, y = moyenne_centrée, color = type, group = type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(
      title = titre,
      subtitle = "Chaque série est centrée et réduite (moyenne = 0, écart-type = 1)\nLignes verticales : min (tiret) et max (pointillé) réels",
      x = x_label,
      y = "Consommation standardisée",
      color = ""
    ) +
    scale_color_manual(
      values = c("energie_kWh" = "#1f77b4", "eau_L" = "#2ca02c", "gaz_dm3" = "#d62728"),
      labels = c("Énergie (kWh)", "Eau (L)", "Gaz (dm³)")
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "top")
  
  # Ajouter les lignes min/max
  p <- p +
    geom_vline(xintercept = x_min_energie, color = "#1f77b4", linetype = "dashed",  linewidth = 0.8) +
    geom_vline(xintercept = x_max_energie, color = "#1f77b4", linetype = "dotted",  linewidth = 0.8) +
    geom_vline(xintercept = x_min_eau,     color = "#2ca02c", linetype = "dashed",  linewidth = 0.8) +
    geom_vline(xintercept = x_max_eau,     color = "#2ca02c", linetype = "dotted",  linewidth = 0.8) +
    geom_vline(xintercept = x_min_gaz,     color = "#d62728", linetype = "dashed",  linewidth = 0.8) +
    geom_vline(xintercept = x_max_gaz,     color = "#d62728", linetype = "dotted",  linewidth = 0.8)
  
  print(p)
  
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 9, height = 5, dpi = 300)
  }
}
#-------------------------------------------------------------------------------
# Vérifier les valeurs abérantes
data %>%
  dplyr::select(energie_kWh, eau_L, gaz_dm3, temperature_C, humidite, vent_kmh) %>%
  resume_table()

# Suppression des données
data <- data %>%
  # Supprimer les 2 valeurs maximales d'énergie
  filter(energie_kWh < sort(unique(energie_kWh), decreasing = TRUE)[2])
data <- data %>%
  # Supprimer toutes les lignes contenant au moins un NA
  drop_na()
  
#revérifier la data
resume_table(data)
#-------------------------------------------------------------------------------
# Vérification de la distribution
# Graphique distribution + indicateur de symétrie
plot_skewness(data, "energie_kWh")
plot_skewness(data, "eau_L")
plot_skewness(data, "gaz_dm3")

# Vérification de la corrélation
# Corrélation des variables explicatives numériques
correlation(data, vars = c("temperature_C", "humidite", "vent_kmh", "heure_sin", "heure_cos"))

#-------------------------------------------------------------------------------
# Préparation des données train/test
#-------------------------------------------------------------------------------
# tri chronologique
data <- data[order(data$periode), ]  
n <- nrow(data)
seuil <- floor(0.8 * n)

# Découpage train/test temporel
data_train <- data[1:seuil, ]
data_test  <- data[(seuil + 1):n, ]

#-------------------------------------------------------------------------------
# Modèle linéaire
#-------------------------------------------------------------------------------
# Électricité
#-------------------------------------------------------------------------------
lm_Elec <- lm(
  energie_kWh ~ temperature_C + humidite + vent_kmh + heure_sin + heure_cos + jour,
  data = data_train
)
# Vérification des hypothèses
verif_model(lm_Elec, data_train, 'energie_kWh', "Vérif_Elec.png")

# Représentation graphique de la significativité
lm_graph(lm_Elec, 'energie_kWh', "VarLM_Elec.png")
#-------------------------------------------------------------------------------
# Eau
#-------------------------------------------------------------------------------
lm_Eau <- lm(
  eau_L ~ temperature_C + humidite + vent_kmh + heure_sin + heure_cos + jour,
  data = data_train
)
# Vérification des hypothèses
verif_model(lm_Eau, data_train, 'eau_L', "Vérif_Eau.png")

# Représentation graphique de la significativité
lm_graph(lm_Eau, 'eau_L', "VarLM_Eau.png")
#-------------------------------------------------------------------------------
# Gaz
#-------------------------------------------------------------------------------
lm_Gaz <- lm(
  gaz_dm3 ~ temperature_C + humidite + vent_kmh + heure_sin + heure_cos + jour,
  data = data_train
)
# Vérification des hypothèses
verif_model(lm_Gaz, data_train, 'gaz_dm3', "Vérif_Gaz.png")

# Représentation graphique de la significativité
lm_graph(lm_Gaz, 'gaz_dm3', "VarLM_Gaz.png")
#-------------------------------------------------------------------------------
# Modèle Random Forest
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Électricité
#-------------------------------------------------------------------------------
rf_Elec <- randomForest(
  energie_kWh ~ temperature_C + humidite + vent_kmh + heure_sin + heure_cos + jour,
  data = data_train,
  importance = TRUE
)

# Variable d'importance et R2 du modele
rf_graph(rf_Elec, 'energie_kWh', "VarRF_Elec.png")

#-------------------------------------------------------------------------------
# Eau
#-------------------------------------------------------------------------------
rf_Eau <- randomForest(
  eau_L ~ temperature_C + humidite + vent_kmh + heure_sin + heure_cos + jour,
  data = data_train,
  importance = TRUE
)
# Variable d'importance et R2 du modele
rf_graph(rf_Eau, 'eau_L', "VarRF_Eau.png")

#-------------------------------------------------------------------------------
# Gaz
#-------------------------------------------------------------------------------
rf_Gaz <- randomForest(
  gaz_dm3 ~ temperature_C + humidite + vent_kmh + heure_sin + heure_cos + jour,
  data = data_train,
  importance = TRUE
)
# Variable d'importance et R2 du modele
rf_graph(rf_Gaz, 'gaz_dm3', "VarRF_Gaz.png")

#-------------------------------------------------------------------------------
# Prédictions sur le jeu de test
#-------------------------------------------------------------------------------
# Liste de tous les modèles à évaluer
models <- list(
  list(modele = lm_Elec, variable = "energie_kWh", nom = "LM - Électricité"),
  list(modele = rf_Elec, variable = "energie_kWh", nom = "RF - Électricité"),
  list(modele = lm_Eau, variable = "eau_L", nom = "LM - Eau"),
  list(modele = rf_Eau, variable = "eau_L", nom = "RF - Eau"),
  list(modele = lm_Gaz, variable = "gaz_dm3", nom = "LM - Gaz"),
  list(modele = rf_Gaz, variable = "gaz_dm3", nom = "RF - Gaz")
)

# Évaluer et enregistrer le graphique des résultats
evaluer_modeles(models, data_test, "résultats.png")

#-------------------------------------------------------------------------------
# Mettre en relation les différents services
#-------------------------------------------------------------------------------
# Graphique de corrélation
correlation(data, vars = c("energie_kWh", "eau_L", "gaz_dm3", "temperature_C", "humidite", "vent_kmh", "heure_sin", "heure_cos"))

#-------------------------------------------------------------------------------
# Graphique par température
graph_conso(data, variable = "temperature_C", "conso_Température.png")

# Graphique par heure
graph_conso(data, variable = "heure", "conso_Heure.png")

# Graphique par humidité (groupée par tranches de 10 %)
graph_conso(data, variable = "humidite", "conso_Humidité.png")

# Graphique par jour de la semaine
graph_conso(data, variable = "jour", "conso_Jour.png")
