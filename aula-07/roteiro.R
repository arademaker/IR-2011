###################################################
### chunk number 1: 
###################################################
options(width=80)
options(continue=" ")


###################################################
### chunk number 2:  eval=FALSE
###################################################
## install.packages("SSOAP", repos = "http://www.omegahat.org/R", type = "source")


###################################################
### chunk number 3:  eval=FALSE
###################################################
## install.packages("SSOAP.tar.gz", repos = NULL, type = "source")


###################################################
### chunk number 4: 
###################################################
library(SSOAP)
library(XML)
library(RCurl)


###################################################
### chunk number 5: 
###################################################
wsdl <- getURL("https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl", 
               ssl.verifypeer = FALSE)


###################################################
### chunk number 6: 
###################################################
doc  <- xmlInternalTreeParse(wsdl)


###################################################
### chunk number 7: 
###################################################
bcbdef <- processWSDL(doc)
ff  <- genSOAPClientInterface(def = bcbdef)


###################################################
### chunk number 8: 
###################################################
codigos <- c(4606, 4607, 4608, 4609, 4610, 4611, 4612, 4613, 4614, 4615, 4616)
data.ini <- "01/01/1998"
data.fim <- "01/01/2011"


###################################################
### chunk number 9: recupera eval=FALSE
###################################################
## xmlstr <- ff@functions$getValoresSeriesXML(codigos, data.ini, data.fim, ssl.verifypeer = FALSE)
## doc <- xmlInternalTreeParse(xmlstr)


###################################################
### chunk number 10: processa eval=FALSE
###################################################
## series <- xpathApply(doc,"//SERIE", function(s) {
##   id <- xmlGetAttr(s, "ID")
##   s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
##   s1 <- t(s1)
##   dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
##   df <- as.data.frame(s1, stringsAsFactors=FALSE)
##   df$SERIE <- id
##   df
## })


###################################################
### chunk number 11: merge eval=FALSE
###################################################
## df <- Reduce(rbind, series)


###################################################
### chunk number 12: limpeza eval=FALSE
###################################################
## df$data  <- as.Date(sapply(strsplit(df$DATA,  "/"),
##                            function(x) paste(c(x[2:1], 1), collapse="-")), "%Y-%m-%d")
## df$valor <- as.numeric(df$VALOR)
## df$serie <- factor(df$SERIE)
## if(remove.old){
##   df$BLOQUEADO <- NULL
##   df$SERIE <- NULL
##   df$DATA <- NULL
##   df$VALOR <- NULL
## }


###################################################
### chunk number 13: 
###################################################
getSeries <- function(codigos, data.ini = "01/01/1998", data.fim = "01/01/2011", remove.old = TRUE) {
xmlstr <- ff@functions$getValoresSeriesXML(codigos, data.ini, data.fim, ssl.verifypeer = FALSE)
doc <- xmlInternalTreeParse(xmlstr)
series <- xpathApply(doc,"//SERIE", function(s) {
  id <- xmlGetAttr(s, "ID")
  s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
  s1 <- t(s1)
  dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
  df <- as.data.frame(s1, stringsAsFactors=FALSE)
  df$SERIE <- id
  df
})
df <- Reduce(rbind, series)
df$data  <- as.Date(sapply(strsplit(df$DATA,  "/"),
                           function(x) paste(c(x[2:1], 1), collapse="-")), "%Y-%m-%d")
df$valor <- as.numeric(df$VALOR)
df$serie <- factor(df$SERIE)
if(remove.old){
  df$BLOQUEADO <- NULL
  df$SERIE <- NULL
  df$DATA <- NULL
  df$VALOR <- NULL
}
df
}


###################################################
### chunk number 14: 
###################################################
df <- getSeries(codigos)


###################################################
### chunk number 15: 
###################################################
head(df)


###################################################
### chunk number 16: 
###################################################
tmp <- df
tmp$year <- cut(tmp$data, "year")


###################################################
### chunk number 17: 
###################################################
tmp <- aggregate(valor ~ year + serie, data = tmp, FUN = mean)
head(tmp)


###################################################
### chunk number 18: 
###################################################
library(sqldf)
colunas <- c(maximum = "max(valor)", minimum = "min(valor)", soma = "sum(valor)", serie = "serie")
mysql <- paste("select", 
               paste(apply(cbind(colunas, names(colunas)), 1, paste, collapse=" "), collapse = ", "), 
               "from df group by serie")
sqldf(mysql)


###################################################
### chunk number 19: 
###################################################
library(ggplot2)


###################################################
### chunk number 20: 
###################################################
p <- qplot(data, valor, data = df, colour = serie, geom = "line") + scale_x_date(format = "%Y") + opts(axis.text.x=theme_text(angle=90, hjust=0))


###################################################
### chunk number 21: 
###################################################
print(p)


###################################################
### chunk number 22: 
###################################################
p1 <- ggplot(df, aes(x = data, y = valor, group = serie, colour = serie)) +
  geom_point() + geom_line() + theme_bw() + opts(panel.grid.major = theme_blank()) +
  xlab(NULL) + ylab(NULL) + opts(axis.text.x=theme_text(angle=90, hjust=0))


###################################################
### chunk number 23: 
###################################################
print(p1)


