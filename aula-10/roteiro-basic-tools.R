
#    A Beginner's Guide to R (2009)
#    Zuur, Ieno, Meesters.    Springer
#

# Lendo dados
BFCases <- read.table(file="BirdfluCases.txt", header = TRUE,sep="\t")
                                                                      
names(BFCases)
str(BFCases)

# Soma linhas
Cases  <- rowSums(BFCases[,2:16])
names(Cases) <- BFCases[,1]

# Histogram
hist(iris$Petal.Width)

# Piechart
par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
pie(Cases , main = "Ordinary pie chart")
pie(Cases , col = gray(seq(0.4,1.0,length=6)), clockwise=TRUE, main = "Grey colours")
pie(Cases , col = rainbow(6),clockwise = TRUE, main="Rainbow colours")


# 3D Exploded Pie Chart
library(plotrix)
pie3D(Cases , labels = names(Cases ), explode = 0.1,
    main = "3D pie chart", labelcex=0.6)
      

# Salvando opcoes antes de alterar
op <- par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
pie(Cases , main = "Ordinary pie chart")
pie(Cases , col = gray(seq(0.4,1.0,length=6)),
    clockwise=TRUE, main = "Grey colours")
pie(Cases , col = rainbow(6),clockwise = TRUE,
    main = "Rainbow colours")
pie3D(Cases , labels = names(Cases ), explode = 0.1,
      main = "3D pie chart", labelcex=0.6)
par(op)

      
# Barplots

barplot(table(subset(iris, Petal.Width > 1)$Species))

BFDeaths <- read.table(file="BirdFluDeaths.txt", header = TRUE, sep="\t")

Deaths <- rowSums(BFDeaths[,2:16])
names(Deaths) <- BFDeaths[,1]
Deaths

Counts <- cbind(Cases, Deaths)
Counts

par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
barplot(Cases , main = "Bird flu cases")
barplot(Counts)
barplot(t(Counts), beside = TRUE)
barplot(t(Counts), col = gray(c(0.5,1)))


Benthic <- read.table(file = "RIKZ2.txt", header = TRUE, sep="\t")
Bent.M <- tapply(Benthic$Richness, INDEX=Benthic$Beach, FUN=mean)
Bent.sd <- tapply(Benthic$Richness, INDEX=Benthic$Beach, FUN=sd)
MSD<- cbind(Bent.M, Bent.sd)


# Cores, arrows 
bp <- barplot(Bent.M, xlab = "Beach",
	      ylab = "Richness", col = rainbow(9), ylim = c(0,20))
arrows(bp, Bent.M, bp, Bent.M + Bent.sd, lwd = 1.5,
     angle=90,length=0.1)
box()


# BoxPlot

Owls <- read.table(file = "Owls.txt", header= TRUE, sep="\t")
boxplot(Owls$NegPerChick, main = "Negotiation per chick")

par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
Owls$LogNeg <- log10(Owls$NegPerChick + 1)
boxplot(NegPerChick~SexParent, data = Owls)
boxplot(NegPerChick~FoodTreatment, data = Owls)
boxplot(NegPerChick~SexParent * FoodTreatment,data = Owls)
boxplot(NegPerChick~SexParent * FoodTreatment,
        names = c("F/Dep","M/Dep","F/Sat","M/Dep"),data = Owls)

par(mar = c(2,2,3,3))
boxplot(NegPerChick ~ Nest, data = Owls, axes = FALSE, ylim = c (-3.5, 9))
axis(2, at = c(0, 2, 4, 6, 8))
text(x = 1:27, y = -2, labels = levels(Owls$Nest),
     cex=0.75, srt=65)


# Boxplot for RIKZ data, retorno do boxplot

Benthic <- read.table(file = "RIKZ2.txt", header= TRUE, sep="\t")
Bentic.n <- tapply(Benthic$Richness, Benthic$Beach, FUN =length)
Bentic.n

bp <- boxplot(Richness ~ Beach, data = Benthic, col = "grey",
       xlab = "Beach", ylab = "Richness")

bpmid <- bp$stats[2, ] + (bp$stats[4,] - bp$stats[2,]) / 2
text(1:9, bpmid, Bentic.n, col = "white", font = 2)


Boar <-read.table("Deer.txt", header = TRUE, sep="\t")

par(mfrow=c(1,2))
dotchart(Boar$LCT, xlab="Length (cm)", ylab = "Observation number")
Isna <- is.na(Boar$Sex)
dotchart(Boar$LCT[!Isna], groups = factor(Boar$Sex[!Isna]),
         xlab = "Length (cm)", ylab = "Observation number grouped by sex")

dotchart(Owls$NegPerChick, xlab = "Negotiation per check", ylab = "Order of the data")


Benthic <- read.table(file = "RIKZ2.txt", header= TRUE,sep="\t")
Benthic$fBeach <- factor(Benthic$Beach)
par(mfrow=c(1,2))
dotchart(Benthic$Richness,groups=Benthic$fBeach,
   xlab="Richness", ylab = "Beach")

Bent.M<-tapply(Benthic$Richness,Benthic$Beach,FUN = mean)

dotchart(Benthic$Richness,groups=Benthic$fBeach,
         gdata = Bent.M, gpch=19, xlab="Richness", ylab="Beach")

legend("bottomright", c("values","mean"), pch=c(1,19), bg="white")


## Plot eh generico

methods(plot)

# plot
Benthic <- read.table(file = "RIKZ2.txt", header= TRUE,sep="\t")
Benthic$fBeach <- factor(Benthic$Beach)

# Versus
plot(Benthic$Richness ~ Benthic$fBeach)
plot(Benthic$Richness, Benthic$fBeach)

# Plot linear model e opcoes
par(mfrow = c(2,2), mar = c(4,4,2,2) +.5)
plot(y = Benthic$Richness, x = Benthic$NAP,
     xlab = "Mean high tide (m)", ylab = "Species richness",
     main = "Benthic data")
M0 <- lm(Richness ~ NAP, data = Benthic)
abline(M0)

plot(y = Benthic$Richness, x = Benthic$NAP,
     xlab = "Mean high tide (m)", ylab = "Species richness",
     xlim = c(-3, 3), ylim = c (0,20))

plot(y = Benthic$Richness, x = Benthic$NAP,
     type = "n", axes = FALSE,
     xlab = "Mean high tide", ylab = "Species richness")
points(y = Benthic$Richness, x = Benthic$NAP)

plot(y = Benthic$Richness, x = Benthic$NAP,
     type = "n", axes = FALSE,
     xlab = "Mean high tide", ylab = "Species richness",
     xlim = c(-1.75,2), ylim = c(0,20))
points(y = Benthic$Richness, x = Benthic$NAP)
axis(2, at = c(0, 10, 20), tcl = 1)
axis(1, at = c(-1.75,0,2), labels = c("Sea","Water line","Dunes") )



# Special character
Whales <- read.table(file="TeethNitrogen.txt", header= TRUE)
N.Moby <- Whales$X15N[Whales$Tooth == "Moby"]
Age.Moby <- Whales$Age[Whales$Tooth == "Moby"]
plot(x = Age.Moby, y = N.Moby, xlab = "Age",
     ylab = expression(paste(delta^{15}, "N")))
     

# Coplot
Benthic <- read.table(file = "RIKZ2.txt", header= TRUE, sep="\t")
coplot(Richness ~ NAP | as.factor(Beach), pch=19, data = Benthic)
coplot(Richness ~ NAP | grainsize, pch=19, data = Benthic)

panel.lm = function(x, y, ...) {
  tmp<-lm(y~x,na.action=na.omit)
  abline(tmp, lwd = 2)
  points(x,y, ...)}

coplot(Richness ~ NAP | as.factor(Beach), pch=19, span =1,
panel = panel.smooth, data=Benthic)



# combining types of plots

MyLayOut <- matrix(c(2,0,1,3), nrow = 2, ncol=2, byrow = TRUE)
MyLayOut
nf <- layout(mat = MyLayOut,widths = c(3, 1), heights = c(1, 3), respect = TRUE)
layout.show(nf)

xrange<-c(min(Benthic$NAP),max(Benthic$NAP))
yrange<-c(min(Benthic$Richness),max(Benthic$Richness))

par(mar=c(4,4,2,2))
plot(Benthic$NAP,Benthic$Richness,xlim=xrange,ylim=yrange, xlab = "NAP", ylab="Richness")

par(mar=c(0,3,1,1))
boxplot(Benthic$NAP,horizontal=TRUE,axes=FALSE, frame.plot=FALSE, ylim=xrange, space=0)

par(mar=c(3,0,1,1))
boxplot(Benthic$Richness,axes=FALSE,ylim=yrange,space=0,horiz=TRUE)
