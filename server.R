library("shiny")
library("ggplot2")

shinyServer(function(input, output) {
  
  output$prob <- renderUI({
    minimo <- round(input$alfa * 0.05, 3)
    maximo <- round(input$alfa * 0.95, 3)
    inicial <- mean(minimo, maximo)
    salto <- round(input$alfa * 0.05, 3)
    
    sliderInput('prob', 'Probabilidad cola izquierda:', 
                min = minimo,
                max = maximo,
                value = inicial,
                step = salto)
  })
  
  output$graf <- renderPlot({
    
    xsup <- 1.1 * qchisq(0.999, input$gdl)
    ysup <- 1.1 * max(dchisq(seq(0.1, xsup, 0.1), input$gdl))
    
    topx <- 10 * ceiling(xsup / 10)
    topy <- 0.01 * ceiling(ysup / 0.01)
    
    coord.x1 <- c(0, seq(0, qchisq(input$prob, input$gdl), 0.05), qchisq(input$prob, input$gdl)) 
    coord.y1 <- c(0, dchisq(seq(0, qchisq(input$prob, input$gdl), 0.05), input$gdl), 0)
    
    coord.x2 <- c(qchisq(1 - input$alfa + input$prob, input$gdl),
                  seq(qchisq(1 - input$alfa + input$prob, input$gdl), xsup, 0.05), 50) 
    coord.y2 <- c(0, dchisq(seq(qchisq(1 - input$alfa + input$prob, input$gdl), xsup, 0.05), input$gdl), 0)
    
    par(mar = c(3.5, 3, 1, 1))
    plot(0, 0, type = 'n', xlim = c(0, xsup), ylim = c(0, ysup), axes = F,
         xlab = '', ylab = ''); box()
    
    # Eje x escalable
    if(topx <= 40){
      axis(1, seq(0, topx , 2.5))  
    }
    if(topx > 40 & topx <= 80){
      axis(1, seq(0, topx , 5))  
    }
    if((topx > 80) & (topx <= 160)){
      axis(1, seq(0, topx , 10))  
    }
    if(topx > 160 & topx <= 280){
      axis(1, seq(0, topx , 20))  
    }
    if(topx > 280){
      axis(1, seq(0, topx , 25))  
    }
      
    
    # Eje y escalable
    if(topy <= 0.035){
      axis(2, seq(0, topy, 0.005/2), las = 2)  
    }
    if(topy >= 0.035 & topy <= 0.05){
      axis(2, seq(0, topy, 0.005), las = 2)  
    } 
    if((topy > 0.05) & (topy <= 0.12)){
      axis(2, seq(0, topy, 0.01), las = 2)
    }
    if(topy > 0.12){
      axis(2, seq(0, topy, 0.02), las = 2)
    }
    
    polygon(coord.x1, coord.y1, col = 'steelblue2')
    polygon(coord.x2, coord.y2, col = 'steelblue2')
    curve(dchisq(x, input$gdl), add = T, lwd = 1.5)
    segments(0, 0, xsup, 0)
    text(qchisq(input$prob/2, input$gdl), ysup / 1.1, input$prob, pos = 2)
    text(qchisq(1 - input$alfa + input$prob/2, input$gdl), ysup / 1.1, input$alfa - input$prob, pos = 4)
  })
  
  output$quants <- renderTable({
    
    aux <- data.frame(Gamma1 = qchisq(input$prob, input$gdl),
                      Gamma2 = qchisq(1 - input$alfa + input$prob, input$gdl))
    aux$Longitud <- 1/aux$Gamma1 - 1/aux$Gamma2
    
    aux
  }, digits = 5)
  
})
