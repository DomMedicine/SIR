library(shiny)

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

ui <- fluidPage(
  
  tags$head(
    tags$meta(name="author", content="Dominik Nowakowski"),
    tags$meta(name="creation_date", content="15/05/2022")
  ),

    titlePanel("SIR model"),

    sidebarLayout(
        sidebarPanel(
          numericInput(inputId = 'inputN',
                       label = "Liczebność populacji (N)",
                       value = 300,
                       min = 1, 
                       max = 500),
          numericInput(inputId = 'inputS',
                       label = "Podatni (S0)",
                       value = 250,
                       min = 1, 
                       max = 500),
          numericInput(inputId = 'inputI',
                       label = "Zainfekowani (I0)",
                       value = 10,
                       min = 1, 
                       max = 500),
          numericInput(inputId = 'inputR',
                       label = "Ozdrowieńcy lub zmarli (R0)",
                       value = 0,
                       min = 0, 
                       max = 500),
          sliderInput(inputId = 'step',
                      label = "Krok h",
                      min = 0.01,
                      max = 0.5,
                      value = 0.05),
          numericInput(inputId = 'gamma',
                       label = "Współczynnik wyzdrowień (gamma)",
                       value = 0.1,
                       min = 0.01,
                       max = 1),
          numericInput(inputId = 'beta',
                       label = "Współczynnik zachorowań (beta)",
                       value = 0.5,
                       min = 0.01,
                       max = 1),
          numericInput(inputId = 'timeMax',
                       label = "Maksymalny czas (t_max)",
                       value = 50,
                       min = 1,
                       max = 150)
        ),

        mainPanel(
           plotOutput("trajektorie"),
           plotOutput("stationarity")
        )
    )
)

server <- function(input, output) {
  
    temp <- reactive({
      SIR(input[['inputS']], 
          input[['inputI']], 
          input[['inputR']], 
          input[['inputN']], 
          input[['gamma']], 
          input[['beta']], 
          input[['step']],
          input[['timeMax']])
    })

    output$trajektorie <- renderPlot({
      # SIR(input[['inputS']], 
      #     input[['inputI']], 
      #     input[['inputR']], 
      #     input[['inputN']], 
      #     input[['gamma']], 
      #     input[['beta']], 
      #     input[['step']],
      #     input[['timeMax']]) -> wspolczynniki
      plot(temp()[['time']], 
           temp()[['S']], 
           col = 'red', 
           ylim = c(0,input[['inputN']]), 
           type = 'l', 
           xlab = "Czas", 
           ylab = "Liczebność")
      lines(temp()[['time']], 
            temp()[['I']], 
            col = 'green')
      lines(temp()[['time']], 
            temp()[['R']], 
            col = 'blue')
      legend("right", 
             legend=c("S", "I", "R"),
             col=c("red", "green", "blue"), lty=1:1, cex=0.8)
    })
    
      output$stationarity <- renderPlot({
        plot(temp()[['I']],
             temp()[['S']])
      })
}

shinyApp(ui = ui, server = server)
