#TD2
library(readxl)       # Pour lire les fichiers Excel
library(dplyr)        # Pour la manipulation des données
library(ggplot2)      # Pour la visualisation des données
library(margins)      # Pour les effets marginaux

setwd("/Users/kaledgoatache/Desktop/a programming/r/TD ECON R")
psy <- read_csv("Psy.csv")
view(psy)

# pour MEF -> 1 moins méfiant, 5 plus méfiant

# Pourcentage des hommes qui font des teleconsultations
nb <- (sum(psy$Fem == 0 & psy$Teleconsultations == 1))

# Proportion d'individus peu méfiants quant à la confidentialité (Mef = 1,2) ayant eu recours à la téléconsultation
Mefiants <- subset(psy, Mef %in% c(1, 2))
Proportion <- mean(Mefiants$Teleconsultations == 1)

Logit <- glm(Teleconsultations ~ Age + Fem + Urbain + Acces + Mef + Nb_Acts,
             data = psy,
             family = binomial(link = "logit"))
summary(Logit)

# le Estimate nous donne que le sens de l'effet (+ ou -) de la variable sur la variable dependente. (pas des effets quantifiables)

# Effet marginal au point moyen pour la variable moyen

# Une variation de la variable urbain entraine une diminution de la prob de réaliser de teleconsultation de 115% points
# On trouve un résultat incohérent(-115%) car les moyennes sont biassé car elles tendent vers les extrêmes

# Effets marginaux moyens

moyennes <- colMeans(psy[, c("Age", "Fem", "Urbain", "Acces", "Mef", "Nb_Acts")])

MEM <- margins(Logit, at = list(Age = moyennes["Age"],
                                Fem = moyennes["Fem"],
                                Urbain = moyennes["Urbain"],
                                Acces = moyennes["Acces"],
                                Mef = moyennes["Mef"],
                                Nb_Acts = moyennes["Nb_Acts"]))
summary(MEM)
# L'Effet Marginal moyen est la moyen des effets marginaux sur l'ensembles des evchantillons.

# en moyenne ceteris paribus, le fait  d'habiter dans une zone urbaine diminue le fait des faires de téléconsultation de -44,62%

# Pour prédire avec R, il faut créer un nouvel objet représentant Joe:
Joe <- data.frame ( Age = 27,   # 27 ans 
                     Fem = 0,  # Homme
                     Urbain = 0,  # Habite à la campagne
                     Acces = 0,  # Pas d'accès Internet 
                     Mef = 5,  # Méfiance = 5
                     Nb_Acts = 12  # 12 dus par ans
)
prJoe <- predict(Logit, newdata = Joe, type = "response")
print(prJoe)

Family <- glm(Teleconsultations ~ Urbain + Mef,
             data = psy,
             family = binomial(link = "logit"))
summary(Family)



