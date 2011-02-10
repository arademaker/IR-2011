
library(lattice)

# Exames de quimica de alunos britanicos
data(Chem97, package = "mlmRev")

xtabs(~ score, data = Chem97)

tp1 <- histogram(~ gcsescore | factor(score), data = Chem97)
tp1
plot(tp1)
print(tp1)
summary(tp1)
class(tp1)


densityplot(~ gcsescore | factor(score), data = Chem97, plot.points = FALSE, ref = TRUE)
tp2 <- densityplot(~ gcsescore, group= score, data = Chem97, plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))
tp2

# mix plots
plot(tp1, split = c(1,1,1,2))
plot(tp2, split = c(1,2,1,2), newpage = FALSE)


data(Oats, package = "MEMSS")
tp1.oats <- xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")
tp1.oats

summary(tp1.oats)
dimnames(tp1.oats)
xtabs(~ Variety + Block, data = Oats)

dotplot(variety ~ yield | site, barley, layout = c(1,6), aspect = c(0.7), groups = year, auto.key = list(space = "right"))


barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), groups = Survived, stack = TRUE,
         layout = c(4,1), auto.key = list(title = "Survived", columns = 2))

# 3 classe mulhers e criancas nao foram poupadas
barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), groups = Survived, stack = TRUE,
         layout = c(4,1), auto.key = list(title = "Survived", columns = 2), scales = list(x = "free"))


Env <- read.table(file="RIKZENV.txt",header = TRUE)
Env$MyTime <- Env$Year+Env$dDay3/365

xyplot(SAL ~ MyTime | factor(Station), data = Env)

histogram( ~ SAL | Station, data = Env,
          subset = Area=="OS", layout = c(1,4),
          nint = 30, xlab = "Salinity", strip = FALSE,
          strip.left = TRUE, ylab="Frequencies")


xyplot(SAL ~ Month | Year, data = Env,
     type = c("p"), subset = (Station =="GROO"),
     xlim = c(0, 12), ylim = c(0, 30),pch = 19, span = 0.5,
     panel = function (...){
     panel.xyplot(...)
     panel.grid(..., h = -1, v = -1)
     panel.loess(...)})
