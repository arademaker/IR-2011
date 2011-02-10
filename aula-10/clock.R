
library(tcltk)
 
circulo <- function(t){
  y <- rbind(cos(t), sin(t)); return(t(y))
}
 
xinter <- seq(from = 0, to = 2*pi, by = 0.015)
 
plot(circulo(xinter) , type = "l", xlab = "", ylab = "")
grid()
text(0 , y =  1, labels = 12)
text(1 , y =  0, labels = 3)
text(0 , y = -1, labels = 6)
text(-1, y =  0, labels = 9)
 
theta <- pi/2   # posicao incial do ponteiro
 
plot.segment <- function() {
  theta <<- theta + 0.1047198
  segments(x0 = 0, y0 = 0, x1 = 0.95*cos(theta),
           y1 = 0.95*sin(theta), lwd = 3, col = "white")
  theta <<- theta - 0.1047198
  segments(x0 = 0, y0 = 0, x1 = 0.95*cos(theta),
           y1 = 0.95*sin(theta), lwd = 3, col = "blue")
          
  theta <<- theta - 0.1047198
  id <<- tcl("after", 1000, plot.segment)
}
 
id <<- tcl("after", 1000, plot.segment)

