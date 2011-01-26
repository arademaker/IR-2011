###################################################
### chunk number 1: 
###################################################
options(width=60)


###################################################
### chunk number 2: 
###################################################
url <- "http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html?codigo=PETR4.SA&beginDay=1&beginMonth=1&beginYear=2008&endDay=9&endMonth=11&endYear=2010&size=200&page=1"


###################################################
### chunk number 3: 
###################################################
p <- unlist(strsplit(strsplit(strsplit(url, "?", fixed=TRUE)[[1]][2],"&")[[1]],"="))
p


###################################################
### chunk number 4: 
###################################################
p[1:length(p) %% 2 == 0]
p[1:length(p) %% 2 != 0]


###################################################
### chunk number 5: 
###################################################
base <- "http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html"
params <- c(codigo = "PETR4.SA",
            beginDay = 1,
            beginMonth = 1,
            beginYear = 2008,
            endDay = 9,
            endMonth = 11,
            endYear = 2010,
            size = 200,
            page = 1)


###################################################
### chunk number 6: 
###################################################
url <- paste(base,paste(apply(cbind(names(params),params), 1,
                              paste, collapse="="),
                        collapse="&"), 
             sep = "?")


###################################################
### chunk number 7:  eval=FALSE
###################################################
## a <- read.table(file("clipboard"), sep="\t", dec=",")


###################################################
### chunk number 8: 
###################################################
Squid <- read.table(file = "squidGSI.txt", header = TRUE)
str(Squid, vec.len = 1)
names(Squid)


###################################################
### chunk number 9: 
###################################################
Squid2 <- read.table(file = "squidGSI.txt", dec = ",", header = TRUE)
str(Squid2, vec.len = 1)


###################################################
### chunk number 10: 
###################################################
Squid3 <- read.table(file = "squidGSI1.txt", dec = ",", header = TRUE, sep = "\t")


###################################################
### chunk number 11: 
###################################################
Squid4 <- read.table("squidGSI2.txt", head=TRUE, sep="\t")
head(Squid4, 15)
str(Squid4)


###################################################
### chunk number 12: 
###################################################
Squid5 <- read.table("squidGSI2.txt", head=TRUE, sep="\t", na.strings = "-")
head(Squid5, 15)
str(Squid5)


###################################################
### chunk number 13: 
###################################################
dados1 <- read.table(unz("antarctic-birds.zip", "antarctic-birds.txt"))
dados2 <- read.table("antarctic-birds.txt.gz")


###################################################
### chunk number 14: 
###################################################
params <- c(key="0AmWelpCNYLymdDFkbGdjLWhTTWxUd2hndWM4d2VvR3c", hl="en", single="true", gid="0", output="csv")
url <- paste("http://spreadsheets.google.com/pub", 
             paste(apply(cbind(names(params), params), 1, paste, collapse="="), 
                   collapse="&"), sep="?")
d <- read.csv(url, header=FALSE)
str(d, vec.len = 1)


###################################################
### chunk number 15: 
###################################################
tmp <- Squid$Sex
head(tmp)
Sel <- Squid$Sex == 1
SquidF <- Squid[Sel,]
SquidM <- Squid[Squid$Sex == 2, ]


###################################################
### chunk number 16: 
###################################################
a <- Squid[Squid$Location == 1 & Squid$Year == 1, ]
b <- Squid[Squid$Sex == 1 & 
           (Squid$Location == 1 | Squid$Location == 2),]


###################################################
### chunk number 17: 
###################################################
SquidF <- Squid[Squid$Sex == 1, ]
nrow(SquidF) 
nrow(Squid)
SquidF1 <- SquidF[Squid$Location == 1, ]


###################################################
### chunk number 18: 
###################################################
a <- subset(Squid, Location == 1, c("Sex","Location","Sample"))
head(a)


###################################################
### chunk number 19: 
###################################################
Ord1 <- order(Squid$GSI)
tmp <- Squid[Ord1, ]
head(tmp)


###################################################
### chunk number 20: 
###################################################
tmp <- Squid[order(Squid$GSI), ]
head(tmp)


###################################################
### chunk number 21: 
###################################################
Squid$fLocation <- factor(Squid$Location)
Squid$fSex <- factor(Squid$Sex, levels = c(1,2), labels=c("M","F"))


###################################################
### chunk number 22: 
###################################################
boxplot(GSI ~ fSex, data = Squid)


###################################################
### chunk number 23: 
###################################################
M1 <- lm(GSI ~ fSex + fLocation, data = Squid)
summary(M1)


###################################################
### chunk number 24: 
###################################################
M2 <- lm(GSI ~ Sex + Location, data = Squid)
summary(M2)


###################################################
### chunk number 25: 
###################################################
Squid$fLocation <- factor(Squid$Location, levels = c(2, 3, 1, 4))
summary(Squid$fLocation)


