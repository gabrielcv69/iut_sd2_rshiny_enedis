# iut_sd2_rshiny_enedis

## Description

Cette application Shiny a été développée dans le cadre du projet *iut_sd2_rshiny_enedis*.
Nous avons réalisé ce projet en cours de BUT Science des Données.
Cette application permet d’analyser et de visualiser les données énergétiques des logements en France. L'utilisateur peut interagir avec des tableaux, graphiques, et cartes pour explorer les indicateurs clés de performance (KPI) selon le code postal et le type de logement (ancien, neuf ou les deux).

L'application propose également des fonctionnalités avancées comme l'export des graphiques, des données en .csv et en .txt, et la réalisation de régressions linéaires simples.

## Fonctionnalités principales

- **Filtrage dynamique** : Filtrage des données par code postal et type de logement.
- **Graphiques variés** : Histogrammes, boîtes à moustaches, nuages de points, etc.
- **Cartographie interactive** : Visualisation des données avec des markers sur une carte.
- **Export** : Export des graphiques en .png et des données en .csv et .txt.
- **Analyse statistique** : Sélection de deux variables pour calculer le coefficient de corrélation et faire une régression linéaire simple.
- **KPI** : Présentation de plusieurs indicateurs de performance clés.
- **Personnalisation** : Sélection de thèmes pour l’application.

## Déploiement

L’application est déployée sur [shinyapps.io](https://www.shinyapps.io). Pour la tester en ligne, rendez-vous sur [ce lien](https://gabrielclairacvuitton.shinyapps.io/GabrielKevin/).

## Installation locale

1. **Cloner le repository** :
    ```bash
    git clone https://github.com/tonnomutilisateur/iut_sd2_rshiny_enedis.git
    ```

2. **Installer les dépendances** :
    Ouvrez le fichier `app.R` dans RStudio et exécutez le code suivant pour installer les packages requis :
    ```R
    install.packages(c("shiny", "leaflet", "ggplot2", "dplyr", "shinydashboard", "shinymanager", "httr"))
    ```

3. **Lancer l'application** :
    Après avoir installé les packages, exécutez simplement :
    ```R
    shiny::runApp()
    ```

## Architecture de l’application

- **app.R** : Fichier principal contenant le code Shiny de l'application.
- **ui.R** : Interface utilisateur (UI) de l'application.
- **server.R** : Logique serveur pour traiter les données et gérer les interactions.
- **www/** : Contient les fichiers statiques comme les images et le CSS.

## Packages utilisés

L’application repose sur plusieurs packages R, parmi lesquels :
- `shiny` pour la création de l’interface utilisateur et serveur.
- `leaflet` pour la cartographie interactive.
- `ggplot2` pour la génération des graphiques.
- `dplyr` pour le traitement des données.
- `shinymanager` pour la gestion de l'authentification.


## Auteurs

- **Gabriel CLAIRAC-VUITTON & Kévin COSTISOR** - *Développeurs* - [Lien GitHub](https://github.com/gabrielcv69)
