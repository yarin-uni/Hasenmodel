# Course: Process-based modeling in ecology - WS 24/25
# Model: Reduzierung einer Hasen-Population von Kanninchen durch Inzucht
# Author: Yarin Gora & Kirsten Flockenhaus
# Date: 2024-12-14

# Beschreibung ----

# Der R Code definiert eine Simulation für die Dynamik einer Hasen-Population mit möglichem Inzucht. 
# Dabei wird aufbauend auf die Ricker Funktion eine Funktion für für den Inzucht-Faktor implementiert. 
# Die Auswirkung der Inzucht wird, als Reduktion der Reproduktionsrate betrachtet. 

# Wie klein muss ein urbanes Areal sein, damit die Inzucht eine Population von Kaninchen
# reduziert?

# Parameter ----

# N: Populationsgroeße
# R: Reproduktionsrate
# K: carrying capacity
# I: Inzucht Faktor 
# t: Zeitschritte
# m: Mortalitaet

# Ricker Funktion ----

# Die Ricker Funktion kalkuliert die Populationsgröße 

ricker <- function(n, r, k,I) {
  n_new <- n * exp(r * I*(1 - n / k)) # Einbau des Inzucht-Faktors
  return(n_new)
}
rickerhase <- function(n, r, m, I) {
  n_new <- n + n * (r * I) - (n*m)
  return(n_new)
}
ricker(18.45997, 1.5, 100,7)


# Simulation Funktion fuer die Hasen-Population ----
simulation <- function(timesteps, n_initial, r, k, visualisation) {
  n <- vector(mode = "numeric", length = timesteps) # Vektor starten
  I <- vector(mode = "numeric", length = timesteps)
  n[1] <- n_initial
  I[1] <- 1 # Startwert für den Inzucht- Faktor
  # Schleife, mit der Ricker-Funktion über Zeitschritte und Berechnung des Inzest-Faktors
# while(any(n > 0) ==TRUE){
    for (t in 1:timesteps) {
      n[t + 1] <- ricker(n[t], r, k, I[t])
      I[t + 1] <- 1-(1/(2*n[t]))*exp(t) # Berechnung Inzest-Faktor mit Populationsgröße und Zeitschritte
      if(I[t +1]< 0)
      {I[t + 1] = I[t]}
      #if(n[t+1]<0)
      #n[t+ 1] = 0
      #break}
    }
    
#  }
  
  print(n) # Ausgabe des Vektors n für die Populationsgröße
  print(I) # Ausgabe vom I-Faktors
  # Visualisierung 
  if (visualisation == TRUE) {
    plot(n[1:t], 
         type = "l", xlab = "Zeitschritt", ylab = "HASEN", 
         main = "Hasen mit Inzucht 🐇", col.main = "black")
    
    # Hasen-Emoji als Punkt für die plots
    hasen <- "\U1F407" # 🐇
    text(n[1:t], labels = hasen, cex = 1, col = "brown")
  }
}
# Start der Simulation mit den vorgegebenden festen Werten ----
simulation(timesteps= 15, n_initial= 10, r= 1.5, k = 100 , visualisation = TRUE)
