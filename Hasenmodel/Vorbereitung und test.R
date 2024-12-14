# Course: Process-based modeling in ecology - WS 24/25
# Session: Implement the model
# Author: Yarin Gora & Kirsten Flockenhaus
# Date: 2024-12-14

# Description ----

# This R code defines a function for simulating population dynamics using
# two
# versions of the Ricker function, one deterministic and the other stochastic.
# Inzest nur auf growth rate; weniger kanninchen pro Kanninchen 

# Parameter ----

# N: current population size
# R: growth rate
# K: carrying capacity
# I: incest factor 
# t: Timestep
# m: mortalität
# Functions ----

# Ricker function
# The first function, "Ricker", calculates the population size at the next time
# step using a deterministic equation:

ricker <- function(n, r, k,I) {
  n_new <- n * exp(r * I*(1 - n / k))
  return(n_new)
}
rickerhase <- function(n, r, m, I) {
  n_new <- n + n * (r * I) - (n*m)
  return(n_new)
}
ricker(18.45997, 1.5, 100,7)


# Simulation function for local population dynamics ----
simulation <- function(timesteps, n_initial, r, k, visualisation) {
  n <- vector(mode = "numeric", length = timesteps) # Vektor starten
  I <- vector(mode = "numeric", length = timesteps)
  n[1] <- n_initial
  I[1] <- 1 # Startwert für die Carrying Capacity
  # Schleife, überprüft ob ein Wert im Vektor unter 10, dann die Ricker-Funktion anwenden 
  # und den k-Wert erhöhen
# while(any(n > 0) ==TRUE){
    for (t in 1:timesteps) {
      n[t + 1] <- ricker(n[t], r, k, I[t])
      I[t + 1] <- 1-(1/(2*n[t]))*exp(t)
      if(I[t +1]< 0)
      {I[t + 1] = I[t]}
      #if(n[t+1]<0)
      #n[t+ 1] = 0
      #break}
    }
    
#  }
  
  print(n) # Ausgabe des Vektors n
  print(I) # Ausgabe vom K-Wert
  # Visualization of output
  if (visualisation == TRUE) {
    plot(n[1:t], 
         type = "l", xlab = "Zeitschritt", ylab = "HASEN", 
         main = "Hasen mit Inzucht 🐇", col.main = "black")
    
    # Geist-Emoji as points for the plots
    hasen <- "\U1F407" # 🐇
    text(n[1:t], labels = hasen, cex = 1, col = "brown")
  }
}
# Start der Simulation mit den vorgegebenden festen Werten ----
simulation(timesteps= 15, n_initial= 10, r= 1.5, k = 100 , visualisation = TRUE)
