#############
# Library
#############

#############
SIR <- function(S0, I0, R0, N, gamma, beta, h, tmax){
  time <- seq(0, tmax, h)
  S <- numeric(length = length(time))
  I <- numeric(length = length(time))
  R <- numeric(length = length(time))
  S[1] <- S0
  I[1] <- I0
  R[1] <- R0
  for (i in 2:length(time)) {
    S[i] <- S[i-1]+h*(-beta*I[i-1]*S[i-1]/N)
    I[i] <- I[i-1] + h*(beta*I[i-1]*S[i-1]/N - gamma*I[i-1])
    R[i] <- R[i-1] + h*(gamma*I[i-1])
  }
  data.frame('time' = time,
             'S' = S,
             'I' = I,
             'R' = R)
}
SIR(250, 10, 0, 300, 0.2, 0.5, 0.05, 25) -> wspolczynniki
plot(wspolczynniki$time, wspolczynniki$S, col = 'red', ylim = c(0,300), type = 'l')
lines(wspolczynniki$time, wspolczynniki$I, col = 'green')
lines(wspolczynniki$time, wspolczynniki$R, col = 'blue')

