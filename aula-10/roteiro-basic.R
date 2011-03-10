
#    A Beginner's Guide to R (2009)
#    Zuur, Ieno, Meesters.    Springer
#

Veg <- read.table(file="Vegetation2.txt", header =TRUE)


# ERRO param data
plot(BARESOIL, R, data = Veg)
plot(R ~ BARESOIL, data = Veg)


# Titulos e limites
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19))

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(min(Veg$BARESOIL, na.rm = TRUE), max(Veg$BARESOIL, na.rm=TRUE)),
     ylim = c(4, 19))


# Variando shape
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = 19)

# Variando shape em funcao de outra variavel
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Transect)

# ERRO! pch espera vetor de numeros!
Veg$fTransect <- factor(Veg$Transect)
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$fTransect)
     

# Criando variavel auxiliar
Veg$Time2 <- ifelse(Veg$Time <= 1974, 1, 16)

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Time2)

# ERRO! Variavel Time!
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Time)


# Variar COR 2 e 3
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     col = 2)

# Cores
x <- 1:8
plot(x,col=x)


Veg$Time2 <- ifelse(Veg$Time <= 1974, 15, 16)
Veg$Col2  <- ifelse(Veg$Time <= 1974, 3, 5)

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Time2,
     col = Veg$Col2)

# Tamanho
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = 16, cex = 1.5)

Veg$Cex2 <- ifelse(Veg$Time == 2002, 4, 1)

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = 16, col = Veg$Col2, cex = Veg$Transect / 4)


## Salvando arquivo
pdf(file="teste.pdf")
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = 16, cex = Veg$Transect / 4)
dev.off()



## Plot shapes options
x <- 1:25
y <- 1:5
z <- rep(1,5)

par(mar=c(2,0,1,2))
plot(z,y,pch=x[1:5],axes=FALSE,xlim=c(0,5),xlab="",ylab="")
text(z-0.3,y,x[1:5],cex=0.8)

z1 <- rep(2,5)
points(z1,y,pch=x[6:10])
text(z1-0.3,y,x[6:10],cex=0.8)

z2 <- rep(3,5)
points(z2,y,pch=x[11:15])
text(z2-0.3,y,x[11:15],cex=0.8)

z3 <- rep(4,5)
points(z3,y,pch=x[16:20])
text(z3-0.3,y,x[16:20],cex=0.8)

z4 <- rep(5,5)
points(z4,y,pch=x[21:25])
text(z4-0.3,y,x[21:25],cex=0.8)


# Acrescentar setas

plot(x = 1:6, y = c(10,20,23,16,18,25), type = "l", col = "green", ylab = expression(delta), xlab = "Obs")
arrows(2,12,4,15.7, col = "red")
text(2,11.5, expression(paste("descida estranha de ",delta, "!")))


# Graficos 3-D

x <- seq(-10,10,length=30)
y <- x
f <- function(x,y) {
  r <- sqrt(x^2 + y^2)
  10 * sin(r)/r
}
z <- outer(x, y, f)
z[is.na(z)] <- 1
persp(x,y,z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

