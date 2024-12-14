# Course: Process-based modeling in ecology - WS 24/25
# Session: Implement the model
# Author: Yarin Gora & Kirstenn Flockenhaus
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

# Functions ----

# Ricker function
# The first function, "Ricker", calculates the population size at the next time
# step using a deterministic equation:

ricker <- function(n, r, k,I) {
  n_new <- n * exp(r * I*(1 - n / k))
  return(n_new)
}

ricker(18.45997, 1.5, 100,7)


# Simulation function for local population dynamics ----
simulation <- function(timesteps, n_initial, r,k) {
  n <- vector(mode = "numeric", length = timesteps) # Vektor starten
  n[1] <- n_initial
  I <- 1 # Startwert für die Carrying Capacity
  # Schleife, überprüft ob ein Wert im Vektor unter 10, dann die Ricker-Funktion anwenden 
  # und den k-Wert erhöhen
#  while(any(n<n_initial) ==TRUE){
    for (t in 1:timesteps) {
      n[t + 1] <- ricker(n[t], r, k,I)
      I<-I-0.05
    }
    
#  }
  print(n) # Ausgabe des Vektors n
  print(I) # Ausgabe vom K-Wert
}

# Start der Simulation mit den vorgegebenden festen Werten ----
simulation(timesteps=150, n_initial=10, r=2,k=100)
