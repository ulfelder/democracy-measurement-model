PD <- read.csv("data.out/democracy.scores.merged.csv", stringsAsFactors = FALSE)

plotPD <- function(countryname, startyear, endyear) {

  par(cex.axis = 0.9)
  
  slice <- subset(PD,
                  country == countryname & year >= startyear & year <= endyear,
                  select = c(country, year, auto.p, auto.lcl, auto.ucl, yrborn, yrdied))
  
  if (startyear < slice$yrborn[1]) {
    slice <- subset(slice, year >= yrborn)
  }
  
  if (slice$yrdied[1] < endyear) {
    slice <- subset(slice, year <= yrdied)
  }
  
  plot(x = 1:(endyear - startyear + 1), y = slice$auto.p,
       type = "l", lwd = 2, col = "cornflowerblue",
       xlim = c(1,endyear - startyear + 1), ylim = c(0,1),
       xlab = "", ylab = "", axes = FALSE,
       main = countryname)
  
  lines(x = 1:(endyear - startyear + 1), y = slice$auto.lcl,
        lwd = 1, lty = 2, col = "gray50")
  
  lines(x = 1:(endyear - startyear + 1), y = slice$auto.ucl,
        lwd = 1, lty = 2, col = "gray50")
  
  axis(1,
       at = if (endyear - startyear < 15) {
                     seq(1, endyear - startyear + 1, 3)
                } else {
                     seq(1, endyear - startyear + 1, 5)
                },
       labels = if (endyear - startyear < 15) {
                     seq(startyear, endyear, 3)
                } else {
                     seq(startyear, endyear, 5)
                },
       tick = FALSE, las = 2, pos = 0.025)
  
  axis(2, seq(0,1,0.25), tick = FALSE, las = 2)
  
  abline(h = seq(0,1,0.25), lwd = 0.5, col = "gray")

}

shinyServer(function(input, output) {
    
  output$lineplot <- renderPlot({
    
    plotPD(input$countryname, input$startyear, input$endyear)
    
  })
  
})