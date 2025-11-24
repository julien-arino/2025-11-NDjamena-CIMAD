## ----set-options,echo=FALSE,warning=FALSE,message=FALSE-----------------------
# Charger les bibliothèques requises
required_packages = c("adaptivetau",
                      "deSolve",
                      "dplyr",
                      "future",
                      "future.apply",
                      "GillespieSSA2",
                      "ggplot2", 
                      "knitr", 
                      "latex2exp",
                      "lattice",
                      "magick",
                      "readr", 
                      "tidyr",
                      "viridis")

for (p in required_packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    require(p, character.only = TRUE)
  }
}
# Options Knitr
opts_chunk$set(echo = TRUE, 
               warning = FALSE, 
               message = FALSE, 
               dev = c("pdf", "png"),
               fig.width = 6, 
               fig.height = 4, 
               fig.path = "FIGS/cours-03-",
               fig.keep = "high",
               fig.show = "hide")
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
options(knitr.table.format = "latex")
# Date pour la page de titre (si nécessaire)
yyyy = strsplit(as.character(Sys.Date()), "-")[[1]][1]
# L'environnement kframe peut entrer en conflit avec allowframebreaks, 
# donc nous le supprimons.
knit_hooks$set(document = function(x) {
  gsub('\\\\(begin|end)\\{kframe\\}', '', x)
})


## ----set-slide-background,echo=FALSE,results='asis'---------------------------
# Arrière-plan foncé ?
plot_blackBG = FALSE
if (plot_blackBG) {
  bg_colour = "black"
  fg_colour = "white"
  input_setup = "\\input{slides-setup-black-background-fr.tex}"
} else {
  bg_colour = "white"
  fg_colour = "black"
  fill_colour = "darkgreen"
  input_setup = "\\input{slides-setup-white-background-fr.tex}"
}
cat(input_setup)


## ----distrib_a_b,eval=TRUE,echo=FALSE,fig.width=8,fig.height=6,include=FALSE----
# Paramètres pour la distribution normale
mu <- 5      # moyenne (mode pour la distribution normale)
sigma <- 1.5 # écart-type
a <- 4       # limite gauche
b <- 7       # limite droite

# Créer les valeurs x pour le tracé
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

# Créer les valeurs x pour la zone ombrée
x_fill <- seq(a, b, length.out = 200)
y_fill <- dnorm(x_fill, mean = mu, sd = sigma)

# Tracer la distribution (pas de cadre, axes affichés)
plot(x, y, type = "l", lwd = 3, col = fg_colour,
     xlab = "t", ylab = "f(t)",
     axes = FALSE, frame.plot = FALSE)

# Ajouter la zone ombrée (de la courbe à y=0)
polygon(c(a, x_fill, b), c(0, y_fill, 0), 
         col = fill_colour, border = NA, density = 20)

# Ajouter une ligne horizontale à y=0
abline(h=0, col = fg_colour, lty = 1, lwd = 1)
# Ajouter des lignes verticales à a et b
abline(v = a, col = "red", lwd = 2, lty = 2)
abline(v = b, col = "red", lwd = 2, lty = 2)

# Ajouter les étiquettes
text(a, -0.02, "a", pos = 1, cex = 1.2, col = "red", font = 2)
text(b, -0.02, "b", pos = 1, cex = 1.2, col = "red", font = 2)


## ----distrib_minf_b,eval=TRUE,echo=FALSE,include=FALSE------------------------
# Paramètres pour la distribution normale
mu <- 5      # moyenne (mode pour la distribution normale)
sigma <- 1.5 # écart-type
a <- mu - 4*sigma       # limite gauche
b <- 7       # limite droite

# Créer les valeurs x pour le tracé
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

# Créer les valeurs x pour la zone ombrée
x_fill <- seq(a, b, length.out = 200)
y_fill <- dnorm(x_fill, mean = mu, sd = sigma)

# Tracer la distribution (pas de cadre, axes affichés)
plot(x, y, type = "l", lwd = 3, col = fg_colour,
     xlab = "t", ylab = "f(t)",
     axes = FALSE, frame.plot = FALSE)

# Ajouter la zone ombrée (de la courbe à y=0)
polygon(c(a, x_fill, b), c(0, y_fill, 0), 
         col = fill_colour, border = NA, density = 20)

# Ajouter une ligne horizontale à y=0
abline(h=0, col = fg_colour, lty = 1, lwd = 1)
# Ajouter des lignes verticales à b
abline(v = b, col = "red", lwd = 2, lty = 2)
# Ajouter les étiquettes
text(b, -0.02, "b", pos = 1, cex = 1.2, col = "red", font = 2)


## ----pdf-cdf-surv-normal,eval=TRUE,echo=FALSE,include=FALSE-------------------
# Paramètres pour la distribution normale
mu <- 5
sigma <- 1.5
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)

# Calculer les fonctions
pdf <- dnorm(x, mean = mu, sd = sigma)
cdf <- pnorm(x, mean = mu, sd = sigma)
surv <- 1 - cdf

# Tracer les trois fonctions
plot(x, pdf, type = "l", col = "blue", lwd = 2,
     ylim = c(0, 1), xlab = "t", ylab = "Value",
     main = "PD, CD and Survival functions",
     xaxt = "n")
lines(x, cdf, col = "darkgreen", lwd = 2, lty = 2)
lines(x, surv, col = "red", lwd = 2, lty = 3)

# Ajouter la légende
legend("right",
       legend = c("PDF", "CDF", "Survival"),
       col = c("blue", "darkgreen", "red"),
       lty = c(1, 2, 3),
       lwd = 2,
       cex = 0.9)


## ----pdf-cdf-surv-hazard-normal,eval=TRUE,echo=FALSE,include=FALSE------------
# Paramètres pour la distribution normale
mu <- 5
sigma <- 1.5
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)

# Calculer les fonctions
pdf <- dnorm(x, mean = mu, sd = sigma)
cdf <- pnorm(x, mean = mu, sd = sigma)
surv <- 1 - cdf

# Tracer les trois fonctions
plot(x, pdf, type = "l", col = "blue", lwd = 2,
     ylim = c(0, 1), xlab = "t", ylab = "Value",
     main = "PD, CD and Survival functions & Hazard rate",
     xaxt = "n")
lines(x, cdf, col = "darkgreen", lwd = 2, lty = 2)
lines(x, surv, col = "red", lwd = 2, lty = 3)
lines(x, pdf/surv, col = fg_colour, lty = 3, lwd = 1)

# Ajouter la légende
legend("right",
       legend = c("PDF", "CDF", "Survival", "Hazard"),
       col = c("blue", "darkgreen", "red", "black"),
       lty = c(1, 2, 3, 3),
       lwd = 2,
       cex = 0.9)


## ----pdf-cdf-surv-hazard-expon,eval=TRUE,echo=FALSE,include=FALSE-------------
# Paramètres pour la distribution exponentielle
lambda <- 0.8
x <- seq(0, 10, length.out = 1000)

# Calculer les fonctions
pdf <- dexp(x, rate = lambda)
cdf <- pexp(x, rate = lambda)
surv <- 1 - cdf

# Tracer les trois fonctions
plot(x, pdf, type = "l", col = "blue", lwd = 2,
     ylim = c(0, 1), xlab = "t", ylab = "Value",
     main = "PD, CD and Surv. functions & Hazard rate of exponential")
lines(x, cdf, col = "darkgreen", lwd = 2, lty = 2)
lines(x, surv, col = "red", lwd = 2, lty = 3)
lines(x, pdf/surv, col = fg_colour, lty = 3, lwd = 1)

# Ajouter la légende
legend("right",
       legend = c("PDF", "CDF", "Survival", "Hazard"),
       col = c("blue", "darkgreen", "red", "black"),
       lty = c(1, 2, 3, 3),
       lwd = 2,
       cex = 0.9)


## ----pdf-cdf-surv-hazard-gamma,eval=TRUE,echo=FALSE,include=FALSE-------------
# Paramètres pour la distribution gamma
shape <- 3       # paramètre de forme (k)
scale <- 2       # paramètre d'échelle (θ)
x <- seq(0, shape*scale + 4*scale, length.out = 1000)

# Calculer les fonctions
pdf    <- dgamma(x, shape = shape, scale = scale)
cdf    <- pgamma(x, shape = shape, scale = scale)
surv   <- 1 - cdf
hazard <- pdf / surv

# Tracer les quatre courbes
plot(x, pdf, type = "l", col = "blue",      lwd = 2,
     ylim = c(0, 1), xlab = "t", ylab = "Value",
     main = "PDF, CDF, Survival & Hazard of Gamma Distribution")
lines(x, cdf,    col = "darkgreen", lwd = 2, lty = 2)
lines(x, surv,   col = "red",       lwd = 2, lty = 3)
lines(x, hazard, col = fg_colour,   lwd = 1, lty = 3)

# Légende
legend("right",
       legend = c("PDF", "CDF", "Survival", "Hazard"),
       col    = c("blue","darkgreen","red","black"),
       lty    = c(1,2,3,3),
       lwd    = c(2,2,2,1),
       cex    = 0.9)


## ----prop_surviving_exp_80years,eval=TRUE,echo=FALSE,fig.width=8,fig.height=6,include=FALSE----
plot_blackBG = FALSE
if (plot_blackBG) {
  par(bg = 'black', fg = 'white') # set background to black, foreground white
  colour = "white"
} else {
  colour = "black"
}
t = 0:150
plot(t,exp(-1/80*t), lwd = 2, ylim = c(0,1), col = colour,
     xlab = "Age (years)", ylab = "Proportion of cohort surviving",
     type = "l")
abline(v = 80,
       col = "red", lwd = 2)
grid(lwd=2)


## ----prop_surviving_dirac_80years,eval=TRUE,echo=FALSE,fig.width=8,fig.height=6,include=FALSE----
plot_blackBG = FALSE
if (plot_blackBG) {
  par(bg = 'black', fg = 'white') # set background to black, foreground white
  colour = "white"
} else {
  colour = "black"
}
plot(0:80, rep(1, length(0:80)), lwd = 2, 
     xlim = c(0, 150), ylim = c(0,1), 
     col = colour,
     xlab = "Age (years)", 
     ylab = "Proportion of cohort surviving",
     type = "l")
lines(80:150, rep(0, length(80:150)), 
      lwd = 2, col = colour, type = "l")
abline(v = 80,
       col = "red", lwd = 2)
grid(lwd=2)


## ----prop_surviving_exp_80years_details,eval=TRUE,echo=FALSE,fig.width=8,fig.height=6,include=FALSE----
if (plot_blackBG) {
  par(bg = 'black', fg = 'white') # set background to black, foreground white
  colour = "white"
} else {
  colour = "black"
}
# Définir les paramètres
d_1 = 1/80 
y_1 = seq(0, 200, 0.1)
S_1 = 1-pexp(y_1, d_1)
d_2 = 1/4
y_2 = seq(0, 20, 0.05)
S_2 = 1-pexp(y_2, d_2)
# Trouver quelques emplacements
idx_50_years = max(which(y_1<=50))
idx_80_years = max(which(y_1<=80))
idx_100_years = min(which(y_1>=100))
idx_150_years = min(which(y_1>=150))
idx_1_days = max(which(y_2<=1))
idx_10_days = max(which(y_2<=10))
# Tracer
par(mfrow = c(1,2))
plot(y_2, S_2,
     type = "l", lwd = 3,
     xaxs = "i", yaxs = "i",
     ylim = c(0, 1.02),
     xlab = "t (days)", ylab = "S(t)")
abline(v = 4, lwd = 2, lty = 3, col = "red")
lines(x = c(1, 1), y = c(0, S_2[idx_1_days]),
      lty = 3, lwd = 1)
lines(x = c(0, 1), y = c(S_2[idx_1_days], S_2[idx_1_days]),
      lty = 3, lwd = 1)
lines(x = c(10, 10), y = c(0, S_2[idx_10_days]),
      lty = 3, lwd = 1)
lines(x = c(0, 10), y = c(S_2[idx_10_days], S_2[idx_10_days]),
      lty = 3, lwd = 1)
points(x = 1, y = S_2[idx_1_days], pch = 19)
points(x = 10, y = S_2[idx_10_days], pch = 19)
text(x = 1, y = S_2[idx_1_days], 
     labels = paste0(round(S_2[idx_1_days], 2)),
     pos = 4)
text(x = 10, y = S_2[idx_10_days], 
     labels = paste0(round(S_2[idx_10_days], 2)),
     pos = 3)
text(x = 4, y = 0.8, 
     labels = "Avg. 4 days", col = "red",
     pos = 4)

plot(y_1, S_1,
     type = "l", lwd = 3,
     xaxs = "i", yaxs = "i",
     ylim = c(0, 1.02),
     xlab = "t (years)", ylab = "S(t)")
abline(v = 80, lwd = 2, lty = 3, col = "red")
lines(x = c(50, 50), y = c(0, S_1[idx_50_years]),
      lty = 3, lwd = 1)
lines(x = c(0, 50), y = c(S_1[idx_50_years], S_1[idx_50_years]),
      lty = 3, lwd = 1)
lines(x = c(100, 100), y = c(0, S_1[idx_100_years]),
      lty = 3, lwd = 1)
lines(x = c(0, 100), y = c(S_1[idx_100_years], S_1[idx_100_years]),
      lty = 3, lwd = 1)
lines(x = c(150, 150), y = c(0, S_1[idx_150_years]),
      lty = 3, lwd = 1)
lines(x = c(0, 150), y = c(S_1[idx_150_years], S_1[idx_150_years]),
      lty = 3, lwd = 1)
points(x = 50, y = S_1[idx_50_years], pch = 19)
points(x = 100, y = S_1[idx_100_years], pch = 19)
points(x = 150, y = S_1[idx_150_years], pch = 19)
text(x = 50, y = S_1[idx_50_years], 
     labels = paste0(round(S_1[idx_50_years], 2)),
     pos = 4)
text(x = 100, y = S_1[idx_100_years], 
     labels = paste0(round(S_1[idx_100_years], 2)),
     pos = 3)
text(x = 150, y = S_1[idx_150_years], 
     labels = paste0(round(S_1[idx_150_years], 2)),
     pos = 3)
text(x = 80, y = 0.8, 
     labels = "Avg. 80 years", col = "red",
     pos = 4)


## ----error_Gamma,eval=TRUE,echo=TRUE------------------------------------------
error_Gamma <- function(theta,shape,t,d) {
  test_points <- dgamma(t, shape = shape, scale = theta)
  ls_error <- sum((d-test_points)^2)
  return(ls_error)
}


## ----optimise_gamma-----------------------------------------------------------
optimize_gamma <- function(t,d) {
  max_shape <- 10
  error_vector <- mat.or.vec(max_shape,1)
  scale_vector <- mat.or.vec(max_shape,1)
  for (i in 1:max_shape) {
    result_optim <- try(optim(par = 3,
                              fn = error_Gamma,
                              lower = 0,
                              method = "L-BFGS-B",
                              shape = i,
                              t = t,
                              d = d),
                        TRUE)
    if (!inherits(result_optim,"try-error")) {
      error_vector[i] <- result_optim$value
      scale_vector[i] <- result_optim$par
    } else {
      error_vector[i] <- NaN
      scale_vector[i] <- NaN
    }
  }
  result_optim <- data.frame(seq(1,max_shape),
                             scale_vector,
                             error_vector)
  colnames(result_optim) <- c("shape","scale","error")
  result_optim <- result_optim[complete.cases(result_optim),]
  return(result_optim)
}


## ----run_optim_Erlang---------------------------------------------------------
time_points <- seq(0,60)
data_points <- dgamma(time_points, shape = 1.57, 
                      scale = 6.53)
# Exécuter la minimisation
optim_fits <- optimize_gamma(time_points,data_points)
# Quelle est la meilleure Erlang pour ajuster les données
idx_best <- which.min(optim_fits$error)


## ----plot_results_optim_Erlang,eval=TRUE,echo=FALSE,fig.width=8,fig.height=6,include=FALSE----
time_points_plot <- seq(0,60,0.05)
found_points_plot <- 
  dgamma(time_points_plot,
         shape = optim_fits[idx_best,]$shape,
         scale = optim_fits[idx_best,]$scale)
max_y <- max(max(data_points),max(found_points_plot))
plot(time_points,data_points, ylim = c(0,max_y),
     xlab = "Days", ylab = "Frequency", col = "red",pch = 16)
lines(time_points_plot,found_points_plot,
      type="l",lwd=2,col="blue")
legend("topright", legend = c("Data","Best Erlang fit"),
       col=c("red","blue"),
       lwd = c(1,2), lty = c(NA,1), pch = c(16,NA))


## ----birth-death-setup--------------------------------------------------------
birth_death_CTMC = function(b = 0.01, d = 0.01) {
  t_0 = 0    # Initial time
  N_0 = 100  # Initial population

  # Vectors to store time and state. Initialise with initial condition.
  t = t_0
  N = N_0

  t_f = 1000  # Final time

  # Track the current time and state (could just check last entry in t
  # and N, but will take more operations)
  t_curr = t_0
  N_curr = N_0
  while (t_curr<=t_f) {
    xi_t = (b+d)*N_curr
    if (N_curr == 0) {
      break # Avoid error with rexp when xi_t = 0
    }
    tau_t = rexp(1, rate = xi_t)
    t_curr = t_curr+tau_t
    v = c(b*N_curr, xi_t)/xi_t
    zeta_t = runif(n = 1)
    pos = findInterval(zeta_t, v)+1
    switch(pos,
           { N_curr = N_curr+1},  # Birth
           { N_curr = N_curr-1}) # Death
    N = c(N, N_curr)
    t = c(t, t_curr)
  }
  plot(t, N, type = "l",
       xlab = "Time", ylab = "Population size",
       main = paste("Birth-death CTMC with b =", b, "and d =", d))
}


## ----CTMC_birth_death_b0_01_d0_01,include=FALSE-------------------------------
birth_death_CTMC()


## ----CTMC_birth_death_b0_01_d0_02,include=FALSE-------------------------------
birth_death_CTMC(b=0.01, d=0.02)


## ----birth-death-setup-2,include=FALSE----------------------------------------
birth_death_CTMC = function(b = 0.01, d = 0.01) {
  t_0 = 0    # Initial time
  N_0 = 100  # Initial population

  # Vectors to store time and state. Initialise with initial condition.
  t = t_0
  N = N_0

  t_f = 1000  # Final time

  # Track the current time and state (could just check last entry in t
  # and N, but will take more operations)
  t_curr = t_0
  N_curr = N_0
  while (t_curr<=t_f) {
    xi_t = (b+d)*N_curr
    if (N_curr == 0) {
      break # Avoid error with rexp when xi_t = 0
    }
    tau_t = rexp(1, rate = xi_t)
    t_curr = t_curr+tau_t
    v = c(b*N_curr, xi_t)/xi_t
    zeta_t = runif(n = 1)
    pos = findInterval(zeta_t, v)+1
    switch(pos,
           { N_curr = N_curr+1},  # Birth
           { N_curr = N_curr-1}) # Death
    N = c(N, N_curr)
    t = c(t, t_curr)
    if (t[length(t)]-t[(length(t)-1)] < 1e-8) {
      # If the time step is too small, stop the simulation
      message("Stopping simulation because time step is too small")
      break
    }
  }
  plot(t, N, type = "l",
       xlab = "Time", ylab = "Population size",
       main = paste("Birth-death CTMC with b =", b, "and d =", d))
  return(list(t = t, N = N))
}


## ----CTMC_birth_death_b0_03_d0_01,include=FALSE-------------------------------
results = birth_death_CTMC(b=0.03, d=0.01)
interevent_time = diff(results$t)


## ----plot_CTMC_birth_death_b0_03_d0_01_interevent,include=FALSE---------------
plot(interevent_time, type = "h",
     ylab = "Inter-event time",
     main = "Inter-event time for birth-death CTMC with b=0.03 and d=0.01")


## -----------------------------------------------------------------------------
tail(diff(results$t))


## ----sim-gillespie2-first-----------------------------------------------------
library(GillespieSSA2)
Pop <- 1000
I_0 <- 2
IC <- c(S = (Pop-I_0), I = I_0)
gamma = 1/3
# R0=beta/gamma*S0, donc beta=R0*gamma/S0
beta = as.numeric(1.5*gamma/IC["S"])
params <- c(gamma = gamma, beta = beta)
t_f = 100
reactions <- list(
  reaction("beta*S*I", c(S=-1,I=+1), "new_infection"),
  reaction("gamma*I", c(S=+1,I=-1), "recovery")
)
set.seed(NULL)


sol <- ssa(
  initial_state = IC,
  reactions = reactions,
  params = params,
  method = ssa_exact(),
  final_time = t_f,
)
plot(sol$time, sol$state[,"I"], type = "l",
     xlab = "Time (days)", ylab = "Number infectious")


## ----parallel-CTMC-plot, echo=FALSE, message=FALSE, warning=FALSE-------------
library(adaptivetau)
library(future.apply)
# Il est utile d'avoir les transitions, les taux et
# les noms définis dans une fonction
CTMC_SIS <- function() {
  # Définir les transitions pour adaptivetau
  transitions <- list(
    c(S = -1, I = +1),  # nouvelle_infection
    c(S = +1, I = -1)   # rétablissement
  )
  # Définir la fonction de taux
  rates <- function(x, params, t) {
    c(
      params[["beta"]] * x["S"] * x["I"],
      params[["gamma"]] * x["I"]
    )
  }
  event_names = c("new_infection", "recovery")
  return(list(transitions = transitions,
              rates = rates,
              event_names = event_names))
}

run_one_sim = function(CTMC, params) {
    IC <- c(S = (params$Pop-params$I_0),
            I = params$I_0)
    set.seed(NULL)
    sol <- ssa.exact(
        init.values = IC,
        transitions = CTMC$transitions,
        rateFunc = CTMC$rates,
        params = params,
        tf = params$t_f
    )
    # Interpolate result (just I will do)
    wanted_t =
      seq(from = 0, to = params$t_f, by = 0.01)
    interp_I = approx(x = sol[,"time"],
                      y = sol[,"I"],
                      xout = wanted_t)
    names(interp_I) = c("time", "I")
    sol$interp_I = interp_I
    # Retourner le résultat
    return(sol)
}

# Par défaut, utiliser tous les cœurs disponibles
plan(multisession)
## Pour utiliser moins de workers, en laissant un vide par
# instance
# plan(multisession, availableCores()-1)
## Pour exécuter séquentiellement
# plan(sequential)

# Configurer les paramètres ne nécessitant pas de calcul
params <- list(gamma = 1/3,
               Pop = 1000,
               I_0 = 2,
               R0 = 1.5,
               t_f = 100, nb_sims = 50)
IC <- c(S = (params$Pop-params$I_0),
        I = params$I_0)
# R0=beta/gamma*S0, donc beta=R0*gamma/S0
params =
  c(params,
    beta = as.numeric(params$R0*params$gamma /
                        IC["S"]))
# Exécuter la simulation
CTMC <- CTMC_SIS()
SIMS = future_lapply(
  X = 1:params$nb_sims,
  FUN =  function(x) run_one_sim(CTMC, params))

# Trouver la valeur y maximale pour le tracé
y_max = max(unlist(lapply(SIMS, function(x) max(x$interp_I$I))),
            na.rm = TRUE)
# Maintenant tracer
plot(SIMS[[1]]$interp_I$time,
     SIMS[[1]]$interp_I$I,
     type = "l", lwd = 0.5,
     xlab = "Time (days)",
     ylab = "Number infectious",
     ylim = c(0, y_max),
     main = paste("CTMC with R0 =", params$R0))
for (i in 2:length(SIMS)) {
  lines(SIMS[[i]]$interp_I$time,
        SIMS[[i]]$interp_I$I,
        type = "l", lwd = 0.5)
}


## ----parallel-CTMC-run-one-sim, message=FALSE, warning=FALSE------------------
run_one_sim = function(params) {
    IC <- c(S = (params$Pop-params$I_0), I = params$I_0)
    params_local <- c(gamma = params$gamma, beta = params$beta)
    reactions <- list(
        # propensity function effects name for reaction
        reaction("beta*S*I", c(S=-1,I=+1), "new_infection"),
        reaction("gamma*I", c(S=+1,I=-1), "recovery")
    )
    set.seed(NULL)
    sol <- ssa(
      initial_state = IC,
      reactions = reactions,
      params = params_local,
      method = ssa_exact(),
      final_time = params$t_f,
      log_firings = TRUE    # This way we keep track of events
    )
    # Interpolate result (just I will do)
    wanted_t = seq(from = 0, to = params$t_f, by = 0.01)
    sol$interp_I = approx(x = sol$time, y = sol$state[,"I"],
                          xout = wanted_t)
    names(sol$interp_I) = c("time", "I")
    # Return result
    return(sol)
}


## ----sim-birth-death-1,echo=FALSE---------------------------------------------
library(GillespieSSA2)
pop_desiree <- 1000
N0 = 1000
IC <- c(N = N0)
d = 1/(45 * 365.25)
b = pop_desiree * d
params <- c(b = b, d = d)
t_f = 100
reactions <- list(
  reaction("b", c(N=+1), "naissance"),
  reaction("d*N", c(N=-1), "deces")
)
set.seed(NULL)


sol <- ssa(
  initial_state = IC,
  reactions = reactions,
  params = params,
  method = ssa_exact(),
  final_time = t_f,
)
plot(sol$time, sol$state[,"N"], type = "l",
     xlab = "Temps (jours)", ylab = "Population")
abline(h = 1000, col = "darkred", lty = 2)


## ----sim-birth-death-2,echo=FALSE---------------------------------------------
pop_desiree <- 1000
N0 = 1000
IC <- c(N = N0)
d = 1/(45)
b = pop_desiree * d
params <- c(b = b, d = d)
t_f = 100
reactions <- list(
  reaction("b", c(N=+1), "naissance"),
  reaction("d*N", c(N=-1), "deces")
)
set.seed(NULL)


sol <- ssa(
  initial_state = IC,
  reactions = reactions,
  params = params,
  method = ssa_exact(),
  final_time = t_f,
)
plot(sol$time, sol$state[,"N"], type = "l",
     xlab = "Temps (jours)", ylab = "Population")
abline(h = 1000, col = "darkred", lty = 2)


## ----convert-Rnw-to-R, echo=FALSE, results='hide', warning=FALSE, message=FALSE----
# From https://stackoverflow.com/questions/36868287/purl-within-knit-duplicate-label-error
rmd_chunks_to_r_temp <- function(file){
  callr::r(function(file, temp){
    out_file = sprintf("../CODE/%s", gsub(".Rnw", ".R", file))
    knitr::purl(file, output = out_file, documentation = 1)
  }, args = list(file))
}
rmd_chunks_to_r_temp("cours-03-aspects-stochastiques.Rnw")

