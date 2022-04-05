# Fallstudien II - Bericht 1 (Felix Fikowski, 280189)

################################################################################
#Laden benötigter Pakete
{
  library(ggplot2)
  library(xtable)
  library(pROC)
  library(car)
  library(readxl)
  library(Rcpp)
  library(ranger)
  library(tidyr)
  library(plyr)
  
}
################################################################################

#Setzen des Dateipfades
setwd("C:\\Users\\Felix\\Documents\\TU Dortmund\\Fallstudien_II_WS2122\\Projekt1")

################################################################################
#Reproduzierbarkeit
set.seed(102021)

################################################################################
#Datenaufbereitung
{
  df <- read_xls("view_export_messungen_2006-11-08.xls", 
                 sheet = 1, col_names = T)
  
  #Umwandlung in Dataframe
  df <- as.data.frame(df)
  #Warnungen:
  #Warnmeldungen:
  #1: In read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet,  ... :
  #                 Expecting numeric in H2596 / R2596C8: got 'null'
  
  #Überblick über Dimensionen des Datensatzes
  dim(df)
  # 16386    18
  
  
  # Untersuche die Daten
  
  # Struktur der Variablen
  
  # Betrachte die Struktur der Variablen 
  str(df)
  
  # null in den Ortsvariablen (Spalte 4-7) zu Ausland umbenennen
  for(i in 4:7){
    df[df[,i]=="null", i] <- "Ausland"
  }
    
  # null in den restlichen Variablen, wo "null" enthalten ist als NA festlegen:
  
  #bestimmen der Spalten mit fehlenden Werten:
  fehlend <- c()
  for(i in 8:18){
    if(!any(is.na(df[,i]))){
      if(any(df[,i] == "null")){
        fehlend <- c(fehlend, i)
      }
    }
  }
  fehlend
  # 10 15 16
  
  #die dazugehörigen Spaltennamen
  colnames(df)[fehlend]
  #"geschlecht"         "schaetzwert_bp_sys" "schaetzwert_by_dia"
  
  #Ersetzen von "null" zu NA
  for(i in fehlend){
    df[df[,i]=="null", i] <- NA
    print(sum(is.na(df[,i])))
  }
  # 23 fehlend bei spalte 10
  # 45 fehlend bei spalte 15
  # 56 fehlend bei spalte 16
  
  # Spalte 3-8, 10 in factor umwandeln
  for(i in c(3:8, 10)){
    df[,i] <- as.factor(df[,i])
  }
  
  # Spalte 15,16 in numeric umwandeln 
  for(i in 15:16){
    df[,i] <- as.numeric(df[,i])
  }
    
  str(df) # nun richtige Struktur der Variablen
    
  # sortiere die levels der Variable 'Bundesland'
  df$bundesland <- factor(as.character(df$bundesland), 
                           levels = unique(df$bundesland), 
                           labels = unique(df$bundesland))
  levels(df$bundesland)
  
  
  # Pruefe ob doppelte id
  any(duplicated(df$id)) 
  # FALSE
  
  
  # Fehlende Werte
  
  # ueberpruefe auf fehlende Werte
  summary(df)
  # jeweils 23 NA's bei befinden, geburtsjahr und geschlecht
  #   Sind dies die selben?
  all(which(is.na(df$befinden)) == which(is.na(df$geburtsjahr)))
  all(which(is.na(df$befinden)) == which(is.na(df$geschlecht)))
  #   Ja dies sind die selben
  
  # NA's auch bei schaetzwert_bp_sys uns schaetzwert_by_dia (45 und 56)
  which(is.na(df$schaetzwert_bp_sys)) %in% which(is.na(df$schaetzwert_by_dia))
  # alle die sys nicht geschaetzt haben haben auch dys nicht geschaetzt
  
  which(is.na(df$schaetzwert_by_dia)) %in% which(is.na(df$schaetzwert_bp_sys))
  # einige die dys nicht geschaetzt haben, haben aber sys geschaetzt
    
  which(is.na(df$schaetzwert_bp_sys)) %in% which(is.na(df$befinden))
  which(is.na(df$schaetzwert_by_dia)) %in% which(is.na(df$befinden))
  # nur einige die fehlende werte bei befinden (und damit auch bei geburtsjahr 
  # hatten), haben ihre blutwerte nicht geschaetzt (jeweils 2)
    
  # Welche 2?
  which(is.na(df$schaetzwert_bp_sys))[
    which(which(is.na(df$schaetzwert_bp_sys)) %in% which(is.na(df$befinden)))]
  # 2661, 2772
  which(is.na(df$schaetzwert_by_dia))[
    which(which(is.na(df$schaetzwert_by_dia)) %in% which(is.na(df$befinden)))]
  # 2661, 2772
  # das sind hier auch wieder die selben
  # dh 2661, 2772 hatten fehlenden werte in all diesen 5 Variablen 
  
  
  # Datenbereinigung
  
  (dimdat1 <- dim(df))
  #16386 18
  
  # Vorbereiten der Löschung der Daten mit einem Alter > 100 und < 15 in dem diese zwischen gespeichert werden in gr100 und kl15
  alter <- 2006 - df$geburtsjahr
  gr100 <- which(alter > 100)
  kl15 <- which(alter < 15)
  
  # Identifizieren der Daten vor dem Anfang und nach dem Ende der Ausstellung 
  head(df$zeit, 133) # bis 132 vor Anfang der Austellung
  tail(df$zeit, 10) # die letzten drei sind nach der Austellung
    
  # loesche Daten ausserhalb der Oeffnungszeiten
  z <- as.character(df$zeit)
  zsplit <- strsplit(z, " ")
  zsplit <- unlist(zsplit)
  ind <- seq(1, length(zsplit), 2)
  date <- zsplit[ind]
  datesplit <- unlist(strsplit(date, "-"))
  df$monat <- as.numeric(datesplit[seq(2, length(datesplit), 3)])
  time <- zsplit[-ind]
  timesplit <- unlist(strsplit(time, ":"))
  stunde <- as.numeric(timesplit[seq(1, length(timesplit), 3)])
  df$stunde <- stunde
  str(df$stunde)
  kl9 <- which(stunde < 9)
  gr18 <- which(stunde >= 18)
  
  # entferne ungueltige Daten
  df <- df[-c(gr100, kl15, 
                1:132, dim(df)[1], dim(df)[1]-1, dim(df)[1]-2,
                kl9, gr18),]
  dimdat1 - dim(df)
  # insgesamt 1755 Beobachtungen geloescht
  
    
  #stunde und monat als factor anlegen
  df$stunde <- as.factor(df$stunde)
  df$monat <- as.factor(df$monat)
  
  #Aufnahme von alter
  df$alter <- 2006 - df$geburtsjahr
  
  
  #Anlage von Altersgruppen für die spätere deskriptive Analyse
  df$altersgruppen <- cut(df$alter, 
                          breaks=c(14,20,30,40,50,60,70,80,90,100), 
                          labels=c("15-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100"))
  
  
  
  
  #Anlage der indikator Variablen bluthochdruck
  df$bluthochdruck <- df$messwert_bp_sys >= 140 | df$messwert_bp_dia >= 90
  
  
  colnames(df)
  dim(df)
  #14631    23
  
  #Entfernen der NAs aus dem Datensatz
  df <- na.omit(df)
  dim(df)
  #14578    23
}
##############################################################################

# DESKRIPTIVE ANALYSE

# terminal bis bluthochdruck betrachten

table(df$terminal)
table(df$bundesland)
table(df$befinden)
table(df$altersgruppen)
table(df$geschlecht)
table(df$raucher)
table(df$blutzucker_bekannt)
table(df$cholesterin_bekannt)
table(df$in_behandlung)
table(df$bluthochdruck)

#Übersicht der Indikator Variablen
xtable(cbind(c("raucher", "blutzucker_bekannt", "cholesterin_bekannt", "in_behandlung", "geschlecht", "bluthochdruck"),
             rbind(table(df$raucher)
                   ,table(df$blutzucker_bekannt)
                   ,table(df$cholesterin_bekannt)
                   ,table(df$in_behandlung)
                   ,table(df$geschlecht)
                   ,table(df$bluthochdruck)
             )
))

#Zusammanfassung der kontinuierlichen Variablen
xtable(summary(df[, c("schaetzwert_bp_sys","schaetzwert_by_dia",
                   "messwert_bp_sys", "messwert_bp_dia", "alter")]))

table(df$blutzucker_bekannt, df$bluthochdruck)
table(df$cholesterin_bekannt, df$bluthochdruck)
table(df$in_behandlung, df$bluthochdruck)
table(df$geschlecht, df$bluthochdruck)
table(df$bluthochdruck, df$bluthochdruck)

#Tabelle Ausprägungen befinden
xtable(t(table(df$befinden)))

#Tabelle eAusprägungen Bundesland
xtable(table(df$bundesland))

#Tabelle eAusprägungen Terminal
xtable(t(table(df$terminal)))

#Boxplot altersgruppen vs. messwert_bp_sys
box_alt_sys<-ggplot(df, aes(x=altersgruppen, y=messwert_bp_sys, fill=altersgruppen)) + 
  geom_boxplot(show.legend = FALSE) + 
  xlab("Altersgruppen") + 
  ylab("Messwert des sys. Blutdrucks")
box_alt_sys

#Boxplot befinden vs. messwert_bp_sys
box_bef_sys<-ggplot(df, aes(x=as.factor(befinden), y=messwert_bp_sys, fill=befinden)) +
  geom_boxplot(show.legend = FALSE) + 
  xlab("Befinden") + 
  ylab("Messwert des sys. Blutdrucks")+
  scale_x_discrete("Terminal",labels=c("1"="sehr gut","2"="gut","3"="normal","4"="schlecht","5"="sehr schlecht"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
box_bef_sys

#Boxplot monat vs. messwert_bp_sys
box_bef_sys<-ggplot(df, aes(x=as.factor(monat), y=messwert_bp_sys, fill=monat)) +
  geom_boxplot(show.legend = FALSE) + 
  xlab("Uhrzeit der Messung") + 
  ylab("Messwert des sys. Blutdrucks")+
  #scale_x_discrete("Terminal",labels=c("9"="sehr gut","10"="gut","3"="normal","4"="schlecht","5"="sehr schlecht"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
box_bef_sys
colnames(df)
#Boxplot terminal vs. messwert_bp_sys
box_ter_sys<-ggplot(df, aes(x=as.factor(terminal), y=messwert_bp_sys, fill=terminal) +
  geom_boxplot(show.legend = FALSE)+
  ylab("Messwert des sys. Blutdrucks"))
box_ter_sys

#Boxplot bundesland vs. messwert_bp_sys
box_bun_sys<-ggplot(df, aes(x=bundesland, y=messwert_bp_sys, fill=bundesland)) +
  geom_boxplot(show.legend = FALSE)+ 
  xlab("Bundesland") + 
  ylab("Messwert des sys. Blutdrucks")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
box_bun_sys

#histogram messwert_bp_sys
his_mes_sys <- ggplot(data=df, aes(messwert_bp_sys)) + 
  geom_histogram(binwidth=5,
                 col="black", 
                 fill="blue", 
                 alpha = .2)+ 
  xlab("Messwert des systolischen Blutdruckes") + 
  ylab("Anzahl")
his_mes_sys

#histogram messwert_bp_dia
his_mes_dia <- ggplot(data=df, aes(messwert_bp_dia)) + 
  geom_histogram(binwidth=5,
                 col="black", 
                 fill="blue", 
                 alpha = .2)+ 
  xlab("Messwert des diastolischen Blutdruckes") + 
  ylab("Anzahl")
his_mes_dia

#histogram schaetzwert_bp_sys
his_sch_sys <- ggplot(data=df, aes(schaetzwert_bp_sys)) + 
  geom_histogram(binwidth=5,
                 col="black", 
                 fill="blue", 
                 alpha = .2)+ 
  xlab("Schätzwert des systolischen Blutdruckes") + 
  xlim(50,200) +
  ylab("Anzahl")
his_sch_sys
dim(df)
#histogram schaetzwert_bp_dia
his_sch_dia <- ggplot(data=df, aes(schaetzwert_by_dia)) + 
  geom_histogram(binwidth=5,
                 col="black", 
                 fill="blue", 
                 alpha = .2)+ 
  xlab("Schätzwert des diastolischen Blutdruckes") + 
  ylab("Anzahl")
his_sch_dia

#histogram schaetzwert_bp_dia
his_alter <- ggplot(data=df, aes(alter)) + 
  geom_histogram(binwidth=5,
                 col="black", 
                 fill="blue", 
                 alpha = .2)+ 
  xlab("Alter") + 
  ylab("Anzahl")
his_alter

summary(df)
# betrachte Haeufigkeiten
plot(df$befinden)
# wie zu erwarten sind auf der Messe, eher Personen denen es gut geht 
# (es muss einem eher gut gehen, dass man ueberhaupt an solch einer 
# Freizeitaktivitaet teilnehmen kann)


# betrachte typische zweidimensionale deskriptive Methoden
table(df$befinden, df$terminal)
# mehr personen denen es schlechter geht (4,5) bei Terminal 1 (und 2)

plot(df$befinden, df$geburtsjahr)
# juegere personen bei randwerten (1 und 5)
plot(df$befinden, df$geschlecht)
# bei 3 frauen und bei 5 maenner mehr
plot(df$befinden, as.factor(df$raucher))
# mit schlechterem befinden steigt auch die anzahl der raucher
plot(df$befinden, as.factor(df$blutzucker_bekannt))
# mit schlechterem befinden steigt auch die anzahl derjenigen bei denen 
# blutzucker bekannt ist (ausser bei 5)
plot(df$befinden, as.factor(df$cholesterin_bekannt))
# mit schlechterem befinden steigt auch die anzahl derjenigen bei denen 
# cholesterin bekannt ist (ausser bei 5)
table(df$befinden, as.factor(df$in_behandlung))
# mit schlechterem befinden steigt auch die anzahl derjenigen die in 
# Bluthochdruckbehandlung sind (ausser bei 4)
plot(df$befinden, df$schaetzwert_bp_sys)
plot(df$befinden, df$schaetzwert_by_dia)
# geschaetzte Blutwerte scheinen ueber alle befindungsklassen hinweg aehnlich zu 
# sein (bei normalen werten von etwa 120 zu 80)
plot(df$befinden, df$messwert_bp_sys)
plot(df$befinden, df$messwert_bp_dia)
# Blutwerte scheinen ueber alle befindungsklassen hinweg aehnlich zu sein 
# (bei normalen werten von etwa 120 zu 80)

##### pruefe hier ob signifikant 

plot(df$schaetzwert_bp_sys, df$schaetzwert_by_dia, xlab = "geschätzter systomlischer Blutdruck", ylab = "geschätzter diastolischer Blutdruck")
# eher positive Korrelation zwischen den geschätzt Blutwerten
cor(df$schaetzwert_bp_sys, df$schaetzwert_by_dia, use = "complete.obs")
# 0.5221
plot(df$messwert_bp_sys, df$messwert_bp_dia, xlab = "gemessener systomlischer Blutdruck", ylab = "gemessener diastolischer Blutdruck")
# positive Korrelation zwischen den gemessen Blutwerten
cor(df$messwert_bp_sys, df$messwert_bp_dia)
# 0.6749

plot(df$alter, df$messwert_bp_sys, xlab = "Alter", ylab = "gemessener systolischer Blutdruck")
# mit zunehmendem Alter steigt der systolische Blutdruck
cor(df$alter, df$messwert_bp_sys, use = "complete.obs")
# -0.3878
plot(df$geburtsjahr, df$messwert_bp_dia)
# beim diastolischen nicht wirklich erkennbar/ nur sehr leicht
cor(df$geburtsjahr, df$messwert_bp_dia, use = "complete.obs")
# -0.1871

plot(df$alter, df$schaetzwert_bp_sys, xlab = "Alter", ylab = "geschätzter systolischer Blutdruck")
#erhöhte Varianz bei niedrigem Alter, evtl. Zeichen für Unwissenheit bei jungen Menschen
cor(df$geburtsjahr, df$schaetzwert_bp_sys, use = "complete.obs")
# -0.3234
plot(df$geburtsjahr, df$schaetzwert_by_dia)
cor(df$geburtsjahr, df$schaetzwert_by_dia, use = "complete.obs")
# -0.1753
# aehnliches auch bei den schaetzwerten

plot(df$stunde, df$messwert_bp_sys, xlab = "Stunde", ylab = "gemessener systolischer Blutdruck")
# mit zunehmendem Alter steigt der systolische Blutdruck
cor(df$stunde, df$messwert_bp_sys, use = "complete.obs")

str(df)

################################################################################
# Multikollinearitaet

#Auswahl der relevanten Spalten
colnames(df)
df_multi <- df[, c("terminal","bundesland","befinden","geschlecht",
             "raucher","blutzucker_bekannt","cholesterin_bekannt",
             "in_behandlung","schaetzwert_bp_sys","schaetzwert_by_dia",
             "messwert_bp_sys","alter","stunde",
             "monat","bluthochdruck")]
colnames(df_multi)

for(i in c("terminal","bundesland","befinden","stunde","monat")){
  df_multi[,i] <- as.factor(df_multi[,i])
}

#entfernen der Spalte bluthochdruck
df_multi <- df_multi[,-15]

dim(df) # 14631    20
dim(df_multi) # 14578    20


print(car::vif(lm(messwert_bp_sys ~ ., data = df_multi)))

################################################################################
#Anlage des Datensatzes für die Anwendung des Random Forest und der logistischen Regression

#Erstellen des Trainings- und Testdfensatzes
{
  dim(df)
  #14578    23
  
  #Auswahl der Spalten
  colnames(df)
  df <- df[, c("terminal","bundesland","befinden","geschlecht",
                     "raucher","blutzucker_bekannt","cholesterin_bekannt",
                     "in_behandlung","schaetzwert_bp_sys","schaetzwert_by_dia",
                     "alter","stunde","monat","bluthochdruck")]
  
  colnames(df)
  dim(df)
  #14578    14
  
  
  #Aufspaltung in Traningsdaten und Testdaten
  train <- sample(1:dim(df)[1], round(dim(df)[1]*0.8))
  training <- df[train, ]
  test <- df[-train,]
  xtrain <- training[, -14]
  ytrain <- training[, 14]
  dim(training)
  # 11662  14
  dim(test)
  # 2916  14
}
################################################################################
#logistic Regression

logreg <- glm(bluthochdruck ~ ., data=training, family = binomial)
logreg
logreg_tab <- cbind("Koeffizient" = round(logreg$coefficients, 4), 
                    "exp(Koeffizient)" = round(exp(logreg$coefficients), 4),
                    "Std. Error" = round(summary(logreg)$coefficients[,2],4),
                    "p-Wert" = ifelse(summary(logreg)$coefficients[,4] < 0.0001, "< 0.0001", 
                                      as.character(round(summary(logreg)$coefficients[,4],4))),
                    " " = ifelse(summary(logreg)$coefficients[,4] < 0.001, "***", 
                                 ifelse(summary(logreg)$coefficients[,4] < 0.01, "**",
                                        ifelse(summary(logreg)$coefficients[,4] < 0.05, "*",
                                               ifelse(summary(logreg)$coefficients[,4] < 0.1, ".", "")))))

logreg_tab

xtable(logreg_tab, type = "latex")

summary(logreg)
#Call:
#  glm(formula = bluthochdruck ~ ., family = binomial, data = training)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.8055  -0.8917  -0.6589   1.1382   2.5296  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                -6.167411   0.331152 -18.624  < 2e-16 ***
#  terminal2                  -0.151788   0.050277  -3.019 0.002536 ** 
# terminal3                   0.045920   0.050009   0.918 0.358498    
#bundeslandOberösterreich   -0.092886   0.177661  -0.523 0.601094    
#bundeslandAusland          -0.197453   0.165258  -1.195 0.232161    
#bundeslandWien             -0.034216   0.116976  -0.293 0.769904    
#bundeslandKärnten          -0.060028   0.192429  -0.312 0.755081    
#bundeslandNiederösterreich  0.010573   0.123538   0.086 0.931798    
#bundeslandSalzburg         -0.223424   0.316007  -0.707 0.479553    
#bundeslandVorarlberg       -0.056378   0.460155  -0.123 0.902488    
#bundeslandTirol            -0.315777   0.319370  -0.989 0.322785    
#bundeslandBurgenland        0.359143   0.227414   1.579 0.114279    
#befinden2                  -0.008136   0.046847  -0.174 0.862119    
#befinden3                  -0.021997   0.067034  -0.328 0.742796    
#befinden4                   0.253212   0.185937   1.362 0.173255    
#befinden5                   0.475650   0.339590   1.401 0.161315    
#geschlechtm                 0.289087   0.042186   6.853 7.25e-12 ***
#  raucherTRUE                -0.013949   0.059229  -0.236 0.813814    
#blutzucker_bekanntTRUE      0.091667   0.057674   1.589 0.111974    
#cholesterin_bekanntTRUE    -0.042686   0.053675  -0.795 0.426453    
#in_behandlungTRUE           0.390152   0.063657   6.129 8.85e-10 ***
#  schaetzwert_bp_sys          0.022257   0.001678  13.266  < 2e-16 ***
#  schaetzwert_by_dia          0.021329   0.002715   7.856 3.98e-15 ***
#  alter                       0.019365   0.001485  13.041  < 2e-16 ***
#  stunde10                    0.590942   0.173775   3.401 0.000672 ***
#  stunde11                    0.599636   0.167110   3.588 0.000333 ***
#  stunde12                    0.565092   0.167618   3.371 0.000748 ***
#  stunde13                    0.349124   0.168751   2.069 0.038558 *  
#  stunde14                    0.275489   0.168351   1.636 0.101757    
#stunde15                    0.268740   0.167554   1.604 0.108735    
#stunde16                    0.198975   0.168669   1.180 0.238130    
#stunde17                    0.230480   0.176008   1.309 0.190370    
#monat5                     -0.218041   0.178960  -1.218 0.223081    
#monat6                     -0.529025   0.180151  -2.937 0.003319 ** 
#  monat7                     -0.668924   0.180032  -3.716 0.000203 ***
#  monat8                     -0.262262   0.175570  -1.494 0.135234    
#monat9                     -0.413980   0.178002  -2.326 0.020034 *  
#  monat10                    -0.304185   0.177702  -1.712 0.086940 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15122  on 11661  degrees of freedom
#Residual deviance: 13718  on 11624  degrees of freedom
#AIC: 13794
#
#Number of Fisher Scoring iterations: 4


#Optimierung nach Trennparameter

#Funktion zur Optimierung der Gesamt fehlerrate
#c: Trennparameter
#pi: vorhergesakten Wahrscheinlichkeiten des Modells
#y: die Tatsächlichen Realisationen
fehler <- function(c, pi, y){
  pred <- ifelse(pi > c, 1, 0)
  return(mean(pred!=as.numeric(y)))
}

################################################################################
#Feststellung der optimalen Gesamtfehlerrate anhand der Trainingsdaten training:

#Gesamtfehlerrate

logreg$fitted.values

#Optimierung der Gesamtfehlerrate
copt_log1 <- optimize(fehler, lower = 0, upper = 1, pi = logreg$fitted.values, y = training$bluthochdruck)
copt_log1
#$minimum
#[1] 0.4648609
#
#$objective
#[1] 0.3079232



#runden der Gesamtfehlerrate
round(fehler(copt_log <- copt_log1$minimum, logreg$fitted.values, training$bluthochdruck), 4)
#0.3079
#der dazugehörige Trennparameter
round(copt_log, 4)
#0.4649

#Aufstellen der Wahrheitstabelle für die Traningsdaten
table(ifelse(logreg$fitted.values> copt_log, 1, 0), training$bluthochdruck)
#FALSE TRUE
#0  6423 2451
#1  1140 1648


#Bestimmen der Gesamtfehler
round(fehler(c = copt_log, y=training$bluthochdruck, pi =logreg$fitted.values), 4)
#0.3079

# sensitivitat:
round(table(ifelse(logreg$fitted.values > copt_log, 1, 0), training$bluthochdruck)[2,2]/
        (sum(table(ifelse(logreg$fitted.values > copt_log, 1, 0), training$bluthochdruck)[,2])), 4)
#0.402

# spezifitat:
round(table(ifelse(logreg$fitted.values > copt_log, 1, 0), training$bluthochdruck)[1,1]/
        (sum(table(ifelse(logreg$fitted.values > copt_log, 1, 0), training$bluthochdruck)[,1])), 4)
#0.8493

################################################################################
# Ermittlung des Trennparameters nach ROC-Kurve anhand der Trainingsdaten

#Darstellung der ROC-Kurve inkl. Trennparameter, Spezifität und Sensitivität
par(pty = "s")
pdf("roc_logreg.pdf")
roc(training$bluthochdruck, logreg$fitted.values, 
    plot=T, legacy.axes=T, percent=T, 
    xlab = "1-Spezifität", 
    ylab = "Sensitivität", 
    col = "midnightblue", 
    print.auc=T, print.auc.cex = 1.5, 
    print.thres = "best", print.thres.pattern.cex = 1.5,
    cex.axis = 1.5, cex.lab = 1.5)
dev.off()

roc_plot <- roc(training$bluthochdruck, logreg$fitted.values, plot = T, legacy.axes = T, 
                print.thres = "best",xlab = "1-Spezifität", 
                ylab = "Sensitivität",)

#Der Trennparameter
round(croc_log <- roc_plot$thresholds[
  which.max(roc_plot$sensitivities + roc_plot$specificities)], 4)
# 0.343

#die dazugehörige Sensitivität
round(roc_plot$sensitivities[which.max(roc_plot$sensitivities + roc_plot$specificities)], 4)
# 0.6643

#die dazugehörige Spezifität
round(roc_plot$specificities[which.max(roc_plot$sensitivities + roc_plot$specificities)], 4)
# 0.6538

#Ausgabe der Wahrheitstablle
pred_train <- ifelse(logreg$fitted.values > croc_log, 1, 0)
(tab_train <- table(pred_train, training$bluthochdruck))
# pred_train FALSE TRUE
#0  4945 1376
#1  2618 2723

#Die dazugehörige Gesamtfehlerrate
round(fehler_train <- fehler(croc_log, logreg$fitted.values, 
                             training$bluthochdruck), 4)
# 0.3425

################################################################################
#Anwendung des nach der Gesamtfehlerrate optimierten Trennparameter auf die Testdaten

#Vorhersage der Wahrscheinlichkeiten anhand der Testdaten
pdata <- predict.glm(logreg, newdata = test, type = "response")

#Optimierung nach Gesamtfehler
table(ifelse(pdata > copt_log, 1, 0), test$bluthochdruck)
#FALSE TRUE
#0  1643  603
#1   284  386

#Ermitteln der Gesamtfehlerrate
round(fehler(c = copt_log, y=test$bluthochdruck, pi =pdata), 4)
# 0.3042

# sensitivitat:
round(table(ifelse(pdata > copt_log, 1, 0), test$bluthochdruck)[2,2]/(sum(table(ifelse(pdata > croc_log, 1, 0), test$bluthochdruck)                                                                                    [,2])), 4)
# 0.3903

# spezifitat:
round(table(ifelse(pdata > copt_log, 1, 0), test$bluthochdruck)[1,1]/(sum(table(ifelse(pdata > croc_log, 1, 0), test$bluthochdruck)
                                                                                    [,1])), 4)
# 0.8526


#Optimierung nach ROC-Kurve
table(ifelse(pdata > croc_log, 1, 0), test$bluthochdruck)
#   FALSE TRUE
#0  1251  344
#1   676  645

#Gesamtfehlerrate
round(fehler(c = croc_log, y=test$bluthochdruck, pi =pdata), 4)
# 0.3498

# sensitivitat:
round(table(ifelse(pdata > croc_log, 1, 0), test$bluthochdruck)[2,2]/
        (sum(table(ifelse(pdata > croc_log, 1, 0), test$bluthochdruck)[,2])), 4)
# 0.6522

# spezifitat:
round(table(ifelse(pdata > croc_log, 1, 0), test$bluthochdruck)[1,1]/
        (sum(table(ifelse(pdata > croc_log, 1, 0), test$bluthochdruck)[,1])), 4)
# 0.6492

################################################################################
#Anwendung des Random Forest
################################################################################

#Vorgehen:
#1.Formatieren der Daten
#2.Ermittlung des optimalen Waldes (Anzahl der Bäume, Variablen pro Splitt und Tiefe des Waldes)
#3.Optimierung der Trennparameter nach Gesamtfehlerrate und ROC-Kurve


#1.Formatieren der Daten
#Die Variablen terminal, bundesland, befinden, stunde, monat und bluthochdruck als factor codieren
for(i in c("terminal","bundesland","befinden","stunde","monat","bluthochdruck")){
  training[,i] <- as.factor(training[,i])
  test[,i] <- as.factor(test[,i])
}

#2.Ermittlung des optimalen Waldes (Anzahl der Bäume, Variablen pro Splitt und Tiefe des Waldes)
#Ermitteln der optimalen Modellparameter über ein Gitter

#Array in dem die OOB-Fehlerraten gespeichert werden sollen 
opt_par <- array(0, dim=c(13, 4, 3),
                 dimnames = list(c("Tiefe 1", "Tiefe 2", "Tiefe 3", "Tiefe 4", "Tiefe 5", "Tiefe 6", "Tiefe 7", "Tiefe 8", "Tiefe 9", "Tiefe 10", "Tiefe 11", "Tiefe 12", "Tiefe 13"),
                                 c("250 Bäume", "500 Bäume", "750 Bäume", "1000 Bäume"),
                                 c("2 Variablen pro Splitt", "3 Variablen pro Splitt", "4 Variablen pro Splitt")))
opt_par

#Aufstellen der verschiedenen Wälder und speichern der OOB-Fehlerrate in angelegtem Array
for(i in 1:nrow(opt_par)) {
  for(j in 1:ncol(opt_par)) {
    for(k in 1:3) {
      temp.model <- ranger(bluthochdruck ~ ., data=training, mtry=(k+1), num.trees=j*250, 
                           max.depth =i, seed = 102021)
      opt_par[i,j,k] = temp.model$prediction.error
    }
  }
}
opt_par

#OOB-Fehlerraten für mtry=2
opt_par2 <- opt_par[,,1]
opt_par2
#Umwandlung zu Dataframe
opt_par2 <- adply(opt_par2, 1)
opt_par2[,-1]
xtable(opt_par2, digits=4)



#OOB-Fehlerraten für mtry=3
opt_par3 <- opt_par[,,2]
#Umwandlung zu Dataframe
opt_par3 <- adply(opt_par3, 1)
xtable(opt_par3, digits=4)

#OOB-Fehlerraten für mtry=4
opt_par4 <- opt_par[,,3]
opt_par4
#Umwandlung zu Dataframe
opt_par4 <- adply(opt_par4, 1)
xtable(opt_par4, digits=4)

#Ausgabe des minimalen OOB-fehlers und der dazugehörigen Modellparameter

#minimaler OOB-Fehlerrate
min(opt_par)
# 0.3017493

#maximaler OOB-Fehlerrate
max(opt_par)

#opimale Modellparameter anhand der Dimensionen
mod_para <- which(opt_par == min(opt_par), arr.ind = TRUE)
mod_para
#           dim1 dim2 dim3
#Tiefe 10   10    3    3
opt_par[2,4,1] # =min(opt_par)

#Anwenden des nach der OOB-Fehlers optimierten Random Forest
ranFor1 <- ranger(bluthochdruck ~ ., data=training, num.trees=mod_para[2]*250, mtry=mod_para[3]+1, max.depth = mod_para[1], probability = TRUE, seed = 102021)
ranFor1

#Ermitteln der vorhergesagten Wahrscheinlichkeiten für die Trainings- und Testdaten
train_prob <- predict(ranFor1, data = training)$predictions
train_prob
test_prob <- predict(ranFor1, data = test)$predictions
test_prob


################################################################################
#Ermitteln der Kennzahlen nach dem Mehrheitsvotum

#Für die Trainingsdaten:

#Die Wahrheitstabelle
table(ifelse(train_prob[,"TRUE"] > 0.5, 1, 0), training$bluthochdruck)
#   FALSE TRUE
#0  7043 2059
#1   520 2040

#Die Funktion fehler musste angepasst werden. Die abhängige Variable musste als factor in das Modell einfließen, dies hat jedoch in der Codierung verursacht, dass die Wert später mit einer 1/2 statt 0/1 einflossen.
#Daher habe ich die Funktion "fehler" angepasst und vom (as.numeric(bluthochdruck) wird noch die 1 subtrheíert. Dies bildet die Funktion fehler1 ab.
fehler1 <- function(c, pi, y){
  pred <- ifelse(pi > c, 1, 0)
  return(mean(pred!=(as.numeric(y)-1)))
}

#Gesamtfehler
round(fehler1(c = 0.5, y=training$bluthochdruck, pi =train_prob[,"TRUE"]), 4)
# 0.2211

# sensitivitat:
round(table(ifelse(train_prob[,"TRUE"] > 0.5, 1, 0), training$bluthochdruck)[2,2]/
        (sum(table(ifelse(train_prob[,"TRUE"] > 0.5, 1, 0), training$bluthochdruck)[,2])), 4)
# 0.4977

# spezifitat:
round(table(ifelse(train_prob[,"TRUE"] > 0.5, 1, 0), training$bluthochdruck)[1,1]/
        (sum(table(ifelse(train_prob[,"TRUE"] > 0.5, 1, 0), training$bluthochdruck)[,1])), 4)
# 0.9312

#Für die Testdaten

#Die Wahrheitstabelle
table(ifelse(test_prob[,"TRUE"] > 0.5, 1, 0), test$bluthochdruck)

#   FALSE TRUE
#0  1700  616
#1   227  373

#Gesamtfehler
round(fehler1(c = 0.5, y=test$bluthochdruck, pi =test_prob[,"TRUE"]), 4)
# 0.2891
# sensitivitat:
round(table(ifelse(test_prob[,"TRUE"] > 0.5, 1, 0), test$bluthochdruck)[2,2]/
        (sum(table(ifelse(test_prob[,"TRUE"] > 0.5, 1, 0), test$bluthochdruck)[,2])), 4)
# 0.3771

# spezifitat:
round(table(ifelse(test_prob[,"TRUE"] > 0.5, 1, 0), test$bluthochdruck)[1,1]/
        (sum(table(ifelse(test_prob[,"TRUE"] > 0.5, 1, 0), test$bluthochdruck)[,1])), 4)
# 0.8822
################################################################################
#3.Optimierung der Trennparameter nach Gesamtfehlerrate und ROC-Kurve
################################################################################
#Optimierung nach Gesamtfehlerrate und Ermittlung der Kennzahlen
#Für die Trainingsdaten:

#Ermittlung der Gesamtfehlerrate (nach gleichem Muster wie für die logistische Regression)
copt_log1 <- optimize(fehler1, lower = 0, upper = 1, pi = train_prob[,"TRUE"], y = training$bluthochdruck)
copt_log1
#Trennparameter
# 0.465663

#Gesamtfehlerrate
# 0.2214028

#minimaler Gesamtfehler
round(fehler1(copt_log <- copt_log1$minimum, train_prob[,"TRUE"], training$bluthochdruck), 4)
#0.2214

#Trennparameter
round(copt_log, 4)
# 0.4657

#Wahrheitstabelle mit nach Gesamtfehlerrate optimiertem Trennparameter
table(ifelse(train_prob[,"TRUE"] > copt_log, 1, 0), training$bluthochdruck)
#   FALSE TRUE
#0  6812 1831
#1   751 2268

#Gesamtfehler
round(fehler1(c = copt_log, y=training$bluthochdruck, pi =train_prob[,"TRUE"]), 4)
#0.2214

# sensitivitat:
round(table(ifelse(train_prob[,"TRUE"] > copt_log, 1, 0), training$bluthochdruck)[2,2]/
        (sum(table(ifelse(train_prob[,"TRUE"] > copt_log, 1, 0), training$bluthochdruck)[,2])), 4)
#0.5533

# spezifitat:
round(table(ifelse(train_prob[,"TRUE"] > copt_log, 1, 0), training$bluthochdruck)[1,1]/
        (sum(table(ifelse(train_prob[,"TRUE"] > copt_log, 1, 0), training$bluthochdruck)[,1])), 4)
#0.9007

#Für die Testdaten

#Wahrheitstablle für Testdaten mit nach Gesamtfehlerrate optimiertem Trennparameter
table(ifelse(test_prob[,"TRUE"] > copt_log, 1, 0), test$bluthochdruck)
#   FALSE TRUE
#0  1622  564
#1   305  425

# Gesamtfehlerrate:
round(fehler1(c = copt_log, y=test$bluthochdruck, pi =test_prob[,"TRUE"]), 4)
#0.298

# sensitivitat:
round(table(ifelse(test_prob[,"TRUE"] > copt_log, 1, 0), test$bluthochdruck)[2,2]/(sum(table(ifelse(test_prob[,"TRUE"] > copt_log, 1, 0), test$bluthochdruck)
                                                                                       [,2])), 4)
#0.4297

# spezifitat:
round(table(ifelse(test_prob[,"TRUE"] > copt_log, 1, 0), test$bluthochdruck)[1,1]/(sum(table(ifelse(test_prob[,"TRUE"] > copt_log, 1, 0), test$bluthochdruck)
                                                                                       [,1])), 4)
#0.8417

################################################################################
#Optimierung nach ROC-Kurve und Ermittlung der Kennzahlen


#Ermittlung des Trennparameter anhand der Trainingsdaten nach gleichem Schema wie für die logistische Regression:
par(pty = "s")
pdf("roc_logreg.pdf")
roc(training$bluthochdruck, train_prob[,"TRUE"], 
    plot=T, legacy.axes=T, percent=T, 
    xlab = "prozentuale Falsch-Positiv-Rate (1-Spezifität)", 
    ylab = "prozentuale Richtig-Positiv-Rate (Sensitivität)", 
    col = "midnightblue", 
    print.auc=T, print.auc.cex = 1.5, 
    print.thres = "best", print.thres.pattern.cex = 1.5,
    cex.axis = 1.5, cex.lab = 1.5)
dev.off()

roc_plot <- roc(training$bluthochdruck, train_prob[,"TRUE"], plot = T, legacy.axes = T, 
                print.thres = "best", 
                xlab = "1-Spezifität", 
                ylab = "Sensitivität")
#Trennparameter
round(croc_log <- roc_plot$thresholds[
  which.max(roc_plot$sensitivities + roc_plot$specificities)], 4)
#0.3622

#Sensitivität
round(roc_plot$sensitivities[which.max(roc_plot$sensitivities + roc_plot$specificities)], 4)
#0.738

#Spezifität
round(roc_plot$specificities[which.max(roc_plot$sensitivities + roc_plot$specificities)], 4)
#0.7641

#Wahrheitstabelle
pred_train <- ifelse(train_prob[,"TRUE"] > croc_log, 1, 0)
(tab_train <- table(pred_train, training$bluthochdruck))
#   FALSE TRUE
#0  5779 1074
#1  1784 3025

#Gesamtfehlerrate
round(fehler_train <- fehler1(croc_log, train_prob[,"TRUE"], 
                              training$bluthochdruck), 4)
#0.2451




#Ermittlung der Kennzahlen mit nach ROC-Kurve optimiertem Trennparameter für die Testdaten

#Wahrheitstabelle:
table(ifelse(test_prob[,"TRUE"] > croc_log, 1, 0), test$bluthochdruck)
#   FALSE TRUE
#0  1334  377
#1   593  612

#Gesamtfehler
round(fehler1(c = croc_log, y=test$bluthochdruck, pi =test_prob[,"TRUE"]), 4)
#0.3326

# sensitivitat:
round(table(ifelse(test_prob[,"TRUE"] > croc_log, 1, 0), test$bluthochdruck)[2,2]/
        (sum(table(ifelse(test_prob[,"TRUE"] > croc_log, 1, 0), test$bluthochdruck)[,2])), 4)
#0.6188

# spezifitat:
round(table(ifelse(test_prob[,"TRUE"] > croc_log, 1, 0), test$bluthochdruck)[1,1]/
        (sum(table(ifelse(test_prob[,"TRUE"] > croc_log, 1, 0), test$bluthochdruck)[,1])), 4)
#0.6923
rm(list = ls())



