###################################################
### chunk number 1: 
###################################################
ISIT<-read.table("ISIT.txt",header=TRUE)
library(lattice)
p <- xyplot(Sources ~ SampleDepth | factor(Station), 
       data=ISIT,
       xlab="Sample Depth", ylab="Sources",
       strip = function(bg='white', ...) {
         strip.default(bg='white', ...)
       },
       panel = function(x, y) {
         panel.grid(h=-1, v= 2)
         I1<-order(x)
         llines(x[I1], y[I1],col=1)
       })
print(p)


###################################################
### chunk number 2:  eval=FALSE
###################################################
?boxplot
help(boxplot)


###################################################
### chunk number 3: 
###################################################
str(InsectSprays)


###################################################
### chunk number 4:  eval=FALSE
###################################################
?InsectSprays


###################################################
### chunk number 5:  eval=FALSE
###################################################
library(lattice)
install.packages("lattice")
update.packages()


###################################################
### chunk number 6:  eval=FALSE
###################################################
q()
q(save="no")
setwd(file="c:\\")
getwd()
rm(list = ls(all=TRUE))
citation()


