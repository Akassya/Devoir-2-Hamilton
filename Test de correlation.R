biblio = c("survival", "survminer", "corrplot", "psy")

for(i in biblio){
  if(!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  }
}

repet = "E:/Mon Drive/M2 Paris Saclay/Cours/Exercices/MOOC_R/MOOC_R/alcool.csv"

df = read.csv2(repet, sep = ";")

str(df)

#1 )comparer le risque de rechute de la maladie alcoolique dans deux sous-groupes: 
# le groupe des plus de 50 ans (strictement plus de 50 ans, recodé en “1”) et le 
# groupe des moins de 50 ans (50 ou moins, recodé en “0”)

# 1.1) Création de la variable AGE50
df$AGE50 = ifelse(df$AGE > 50, 1, 0)
table(df$AGE50)

# 1.2) Analyse de survie
# Installation et chargement des packages nécessaires
# install.packages("survival")
# install.packages("survminer")
library(survival)
library(survminer)
# Création de l'objet survfit
fit = survfit(Surv(t, SEVRE) ~ AGE50, data = df)
# Affichage des courbes de survie
ggsurvplot(
  fit,
  data = df,
  pval = TRUE,
  conf.int = TRUE,
  pval.method = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  linetype = "strata",
  xlab = "Temps en jours",
  ylab = "Probabilité de survie",
  title = "Courbes de survie selon l'âge",
  #legend.labs = c("<= 50 ans", "> 50 ans")
  legend.title = "Âge"
  #color = "Dark2",
  #ggtheme = theme_minimal()
)

# Test de log-rank
survdiff(Surv(t, SEVRE) ~ AGE50, data = df)


# # 2) tester l'association entre le risque de rechute de la maladie alcoolique et 
# les variables SEXE, AGE et l'interaction entre les variables SEXE et AGE. Donner 
# la p-value associée à l'interaction entre les variables SEXE et AGE dans le test 
# correspondant (2 chiffres après la virgule)

# Analyse de survie
# Création de l'objet survfit
mod = coxph(Surv(t, SEVRE) ~ AGE * SEXE, data = df)
summary(mod)

# Conditions de valité
table(df$SEVRE)
# au moins 5 à 10 évènements par variable
# ici 2 variables -> 10 à 20 évènements
# on a 27 évènements -> ok




fit = survfit(Surv(t, SEVRE) ~ 1, data = df)

plot(
  fit,
  mark.time = T
)

#test de log-rank
(fit2 = survfit(Surv(t, SEVRE) ~ SEXE, data = df))

# Affichage
ggsurvplot(
  fit2,
  data = df,
  pval = TRUE,
  conf.int = TRUE,
  pval.method = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  linetype = "strata",
  #surv.median.line = "hv",
  xlab = "Temps en jours",
  ylab = "Probabilité de survie",
  title = "Courbes de survie selon le sexe",
  legend.labs = c("Hommes", "Femmes"),
  legend.title = "Sexe"
  #color = "Dark2",
  #ggtheme = theme_minimal()
)

# Test de log-rank
survdiff(Surv(t, SEVRE) ~ SEXE, data = df)

# Modele de cox
mod = coxph(Surv(t, SEVRE) ~ AGE + SEXE + EDVNEG, data = df)
summary(mod)

# Conditions de valité
table(df$SEVRE)

# au moins 5 à 10 évènements par variable
# ici 3 variables -> 15 à 30 évènements 
# on a 27 évènements -> ok

# Verification de l'hypothese des risques proportionnels
(test.ph = cox.zph(mod))

# Affichage des résidus 
par(mfrow = c(2,2))
plot(test.ph)
par(mfrow = c(1,1))

#--------------------------------------------

# On va changer de jeu de données
# et on va faire une analyse de corrélation
repet2 = "E:/Mon Drive/M2 Paris Saclay/Cours/Exercices/MOOC_R/MOOC_R/smp2.csv"

mydata = read.csv2(repet2, sep = ";")

# Affichage de la structure des données
str(mydata)

# selection des variables à inclure pour la correlation
var = c("age", "n.enfant", "dep.cons", "ed", "dr", "scz.cons", "grav.cons", "rs")

#selection des variables à inclure pour la correlation
explicatives = var[!var %in% "dep.cons"]
expliquer = c("dep.cons")

#ACF
fpca(
  y = expliquer,
  x = explicatives,
  data = mydata,
  partial = "No"
)

#glm
mod = glm(dep.cons ~ age + n.enfant + grav.cons + ed + scz.cons + rs, data = mydata, family = "binomial")
summary(mod)

predict(mod)
# glm(z ~ y, data=d, family=binomial("logit"))
mod = glm(dep.cons ~ age + n.enfant + grav.cons + ed + scz.cons + rs, data = mydata, family=binomial("logit"))
summary(mod)

# 1) Nous considérons l'âge (age) des individus ayant 4 enfants ou plus (n.enfant) et dont la
# catégorie socio-professionnelle (prof) figure parmi les modalités suivantes : 
# « sans emploi », « ouvrier », « cadre » et « employé ». Pour ce sous-ensemble de l'échantillon 
# du data frame smp, le rapport entre les deux variances les plus extrêmes dans ces 4 groupes est

# 1.1) new data
newdata = subset(
  mydata,
  n.enfant >= 4 &
    (
      prof == "sans emploi" |
        prof == "ouvrier" | prof == "cadre" | prof == "employé"
    )
  ,
  select = c("age", "n.enfant", "prof")
)

# calculer le rapport entre les deux variances les plus extrêmes dans ces 4 groupes est
variance_by_prof = tapply(newdata$age, newdata$prof, var, na.rm = TRUE)
max(variance_by_prof) / min(variance_by_prof)
aggregate(age ~ prof, data = newdata, FUN = stats::var)
# 1.2) Test de Bartlett
bartlett.test(age ~ prof, data = newdata)

# 2 )Nous souhaitons réaliser une ANOVA à un facteur en considérant l'âge (age) comme 
# variable réponse, et la taille de la fratrie (n.fratrie) recodée en 3 classes (0-2, 3-4, 5+) 
# comme variable explicative. Les bornes des intervalles sont inclues pour chacune des 
# trois classes. Indiquer le résultat du test F de Fisher-Snedecor d’égalité des moyennes :

# 2.1) Recodage de la variable n.fratrie en 3 classes
mydata$n.fratrie.cat = cut(
  mydata$n.fratrie,
  breaks = c(-Inf, 2, 4, Inf),
  labels = c("0-2", "3-4", "5+")
)
table(mydata$n.fratrie.cat)
# 2.2) ANOVA
anova_result = aov(age ~ n.fratrie.cat, data = mydata)
summary(anova_result)
mod = lm(age ~ n.fratrie.cat, data = mydata)
summary(mod)
# Vérification des hypothèses
# 2.3) Test de Tukey pour les comparaisons multiples
TukeyHSD(anova_result)
# Vérification des hypothèses
# Homogénéité des variances
bartlett.test(age ~ n.fratrie.cat, data = mydata)
# Normalité des résidus
shapiro.test(residuals(anova_result))

# 3) Nous nous intéressons à la relation entre la variable séparation (separation) et 
# l'âge (age) des individus, que l'on modélise à l'aide d'une régression logistique. 
# Donner la borne inférieure de l'intervalle de confiance à 95 % pour l'odds-ratio (3 chiffres après la virgule).

# 3.1) Régression logistique
mod_log = glm(separation ~ age, data = mydata, family = binomial)
summary(mod_log)
# 3.2) Intervalle de confiance pour les coefficients
exp(confint(mod_log))
# 3.3) Odds-ratio pour une augmentation d'une unité d'âge
odds_ratio = exp(coef(mod_log)["age"])
odds_ratio
# 3.4) Intervalle de confiance pour l'odds-ratio
odds_ratio_ci = exp(confint(mod_log)["age", ])
odds_ratio_ci


aggregate(dur.interv ~ dep.cons, data = mydata, FUN = median)

cor(mydata$age, mydata$dur.interv, use = "complete.obs")
cor.test(mydata$age, mydata$dur.interv)

wilcox.test(dur.interv ~ suicide.past, data = mydata)


# selection des variables à inclure pour la correlation
var = c("age", "n.enfant", "dep.cons", "ed", "dr", "scz.cons", "grav.cons", "rs")

# Calcule de la corrélation et arrondi à 3
round(cor(mydata[, var], use = "complete.obs"), 3)

correlation = round(cor(mydata[, var], use = "complete.obs"), 3)

# Affichage de la matrice de corrélation
corrplot(
  correlation,
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addrect = 3
)

# calcule de la distance sur la l'hyper-sphère des corrélations
d = sqrt(2*(1 - correlation)) # sqrt(0.5 * (1 - correlation))
# clustering hiérarchique
cluster = hclust(as.dist(d), method = "ward.D2")

# fenêtrage pour afficher le dendrogramme et la matrice de corrélation
layout(matrix(c(1,1,2,3), nrow = 2))

# Affichage du dendrogramme
plot(cluster)
#Division en 3 clusters
rect.hclust(cluster, k = 3, border = "red")
# Réorganisation de la matrice de corrélation
corrplot(correlation, order = "hclust", addrect = 3)
# Affichage de la matrice de corrélation
corrplot(
  correlation,
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addrect = 3
)
# retour à une seule fenêtre
par(mfrow = c(1, 1))

#ACP
mdspca(mydata[, var])
sphpca(mydata[, var], v = 55, nbsphere = 1, method="rscal",output=TRUE)

#ACF
expliquer = "grav.cons"
explicatives = var[!var %in% "grav.cons"]

# graphique des corrélations avec la variable à expliquer
fpca(
  y = expliquer,
  x = explicatives,
  data = mydata,
  partial = "No"
)

fpca(
  dur.interv ~
    age +
    n.enfant +
    dep.cons +
    ed +
    dr +
    scz.cons +
    rs,
  partial = "No",
  data = mydata
)
# Modèle linéaire multiple
mod1 = lm(
  dur.interv ~
    age +
    n.enfant +
    dep.cons +
    ed +
    #dr +
    #scz.cons +
    rs,
  data = mydata
)
summary(mod1)
# Vérification des résidus
par(mfrow = c(1, 2))
hist(mod1$residuals, main = "Histogramme des résidus")
plot(mod1, which = 2) # Q-Q plot
# retour à une seule fenêtre
par(mfrow = c(1, 1))
# Test de normalité des résidus
shapiro.test(residuals(mod1))


corre = round(cor(mydata[, var], use = "complete.obs"), 2)

d = sqrt(2(1-corre))

cluster = hclust(as.dist(d), method = "ward.D2")

plot(cluster)

rect.hclust(cluster, k = 3, border = "red")

plot(hclust(dist(t(scale(mydata[, var])))))
heatmap(corre)
heatmap(corre, Colv = NA, scale = "row")
heatmap(corre, Colv = NA, scale = "column")
heatmap(corre, Colv = NA, scale = "none")
heatmap(corre, Colv = NA, scale = "none", margins = c(5,10))

mdspca(mydata[, var])
sphpca(mydata[, var], v = 55, nbsphere = 1)

