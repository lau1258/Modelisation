# Modélisation de la consommation énergétique et hydrique

L’étude porte sur la modélisation et la prévision de la consommation d’électricité, d’eau et de gaz naturel à partir de la base de données AMPds2 (Makonin et al., 2016).

## Vue d’ensemble du projet

L’ensemble des analyses a été réalisé dans le langage R, en s’appuyant sur plusieurs bibliothèques spécialisées.  
Les packages "dplyr", "ggplot2", "lubridate" et "tidyr" ont été utilisés pour la manipulation, la transformation et la visualisation des données, tandis que "randomForest", "corrplot", "lmtest", "car" et "moments" ont servi à la modélisation et à la vérification statistique des modèles.

Le projet combine des fonctions intégrées de R, comme "lm()" pour la régression linéaire et "randomForest()" pour la construction des forêts aléatoires, avec plusieurs fonctions personnalisées conçues pour automatiser certaines étapes et uniformiser les analyses.

## Fonctions principales

- "resume_table()" : résume la qualité et la distribution des données numériques (valeurs manquantes, extrêmes, zéros, etc.)
- "plot_skewness()" : visualise la distribution et l’asymétrie (skewness) des variables
- "correlation()" : calcule et affiche la matrice de corrélation entre variables explicatives
- "verif_model()" : vérifie les hypothèses fondamentales de la régression linéaire à l’aide de tests statistiques et de graphiques
- "lm_graph()" : illustre la significativité des variables explicatives dans le modèle linéaire
- "rf_graph()" : affiche l’importance relative des variables dans la forêt aléatoire
- "evaluer_modeles()" : compare les performances des modèles selon les métriques R², RMSE et MAE
- "graph_conso()" : affiche les graphiques pour l'analyse final des données

## Objectif

Comparer la performance de deux approches de modélisation, 
la régression linéaire et la forêt aléatoire.

## Référence des données

Makonin, S. et al. (2016). Electricity, water, and natural gas consumption of a residential house in Canada from 2012 to 2014.  
Scientific Data, 3, 160037. https://doi.org/10.1038/sdata.2016.37

## Auteur

Projet réalisé par lau1258
