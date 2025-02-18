# Lade benötigte Pakete
library(magrittr)
library(dplyr)

# Setze dein Arbeitsverzeichnis (anpassen, falls nötig)
setwd("/Users/claudia.weileder/Desktop/mfcurve_dev/mfcurve")

# Lade die Package- und Funktionsdateien
# Stelle sicher, dass sich diese Dateien im Verzeichnis "R/" befinden.
source("R/package.R")
source("R/preprocessing.R")
source("R/stat_testing.R")
source("R/plotting.R")

# -----------------------------
# Simulierter Datensatz
# -----------------------------
# Setze einen Seed für Reproduzierbarkeit
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),                     # Generiere Löhne (Normalverteilung)
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),  # Zufällige Zuweisung der Rassen
  south = sample(c("Yes", "No"), n, replace = TRUE),      # Zufällige Zuweisung, ob im Süden oder nicht
  union = sample(c("Yes", "No"), n, replace = TRUE)       # Zufällige Zuweisung, ob gewerkschaftlich organisiert
)

# -----------------------------
# Preprocessing
# -----------------------------
# Die Funktion mfcurve_preprocessing:
# - Entfernt fehlende Werte in den angegebenen Spalten.
# - Erstellt eine Gruppenvariable, indem die Levels der Faktoren kombiniert werden.
# - Berechnet Gruppenstatistiken (Mittelwert, Standardabweichung, Anzahl) und ordnet die Gruppen nach dem Mittelwert.
preprocessed_data <- mfcurve_preprocessing(
  data = df,
  outcome_var = "wage",
  factors = c("race", "south", "union")
)
cat("Preprocessed Data:\n")
print(head(preprocessed_data))  # Zeigt die ersten Zeilen der aufbereiteten Daten an

# -----------------------------
# Statistische Tests
# -----------------------------
# Die Funktion mfcurve_stat_testing führt einen t-Test durch, der den Gruppenmittelwert
# entweder gegen den Grand Mean (bei test = "mean") oder gegen Null (bei test = "zero") vergleicht.
# Hier wird "mean" verwendet, um die Gruppen gegen den Grand Mean zu testen.
stat_test_results <- mfcurve_stat_testing(
  data = preprocessed_data,
  test = "mean",   # Vergleich der Gruppenmittelwerte mit dem Grand Mean
  alpha = 0.05     # Signifikanzniveau von 5%
)
cat("\nStatistical Test Results:\n")
print(head(stat_test_results))  # Zeigt die ersten Zeilen der Testergebnisse an

# -----------------------------
# Interaktive Visualisierung
# -----------------------------
# Zwei Versionen des Plots werden erstellt:
#
# 1. plot1: Im "collapsed" Modus
#    - Das untere Panel (Faktor-Kombinationen) wird kompakt dargestellt.
#
# 2. plot2: Im "expanded" Modus mit fixierter oberer Y-Achse
#    - Das untere Panel zeigt eine ausführlichere Darstellung (Faktor und Level kombiniert).
#    - Mit upper_fixed_range = TRUE wird die Y-Achse im oberen Panel fixiert.
plot1 <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage",
  alpha = 0.05,
  showTitle = TRUE,
  mode = "collapsed",   # "collapsed": kompakte Darstellung im unteren Panel
  upper_fixed_range = FALSE,  # obere Y-Achse bleibt scrollbar
  color_scheme = "default"    # farbig
)

plot2 <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage",
  alpha = 0.05,
  showTitle = TRUE,
  mode = "expanded",         # "expanded": ausführliche Darstellung im unteren Panel
  upper_fixed_range = TRUE,   # obere Y-Achse fixiert (keine Scroll-/Drag-Funktion)
  color_scheme = "bw"         # schwarz-weiß Modus
)

# -----------------------------
# Anzeige der Plots
# -----------------------------
# In einer interaktiven Umgebung (z.B. RStudio) werden die Plotly-Objekte angezeigt.
print(plot1)  # Zeigt den Plot im "collapsed" Modus
print(plot2)  # Zeigt den Plot im "expanded" Modus mit fixierter oberer Y-Achse
