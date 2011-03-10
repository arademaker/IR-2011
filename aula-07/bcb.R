

## No windows pode ser mais complicado instalar os pacotes SSOAP e
## RCurl. Tem uma versão binária ro RCurl disponivel, vide
## http://cran.r-project.org/bin/windows/contrib/r-release/ReadMe

# O pacote SSOAP deve conseguir ser instalado via:
# > install.packages("SSOAP", repos = "http://www.omegahat.org/R", type = "source")


library(SSOAP)
library(XML)
library(RCurl)

## Recuperar arquivo WSDL
## certificado do BCB esta expirado. Por isso ssl false
library(RCurl)
wsdl <- getURL("https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl", ssl.verifypeer = FALSE)
doc  <- xmlInternalTreeParse(wsdl)

# Alternativa, baixar o arquivo WSDL manualmente, colocar no diretório
# do script e executar.
# > def <- processWSDL("FachadaWSSGS.wsdl")

def <- processWSDL(doc)
ff  <- genSOAPClientInterface(def = def)


getSeries <- function(codigos, data.ini = "01/01/1998", data.fim = "01/01/2011", remove.old = TRUE) {
  xmlstr <- ff@functions$getValoresSeriesXML(codigos, data.ini, data.fim, ssl.verifypeer = FALSE)
  doc <- xmlInternalTreeParse(xmlstr)

  teste <- xpathApply(doc,"//SERIE", function(s) {
    id <- xmlGetAttr(s, "ID")
    s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
    s1 <- t(s1)
    dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
    df <- as.data.frame(s1, stringsAsFactors=FALSE)
    df$SERIE <- id
    df
  })
  df <- Reduce(rbind, teste)
  
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


## Recuperando os dados
codigos <- c(4606, 4607, 4608, 4609, 4610, 4611, 4612, 4613, 4614, 4615, 4616)
df <- getSeries(codigos)



## Agregando dados

# Max valores por ano por serie
tmp <- df
tmp$year <- cut(df$data, "year")
tmp <- aggregate(valor ~ year + serie, data = tmp, FUN = mean)

# outra forma de usar os data.frames
library(sqldf)
sqldf("select serie, max(valor) maximum, min(valor) minimum, sum(valor) soma from df group by serie")


## Plot

library(ggplot2)
p <- qplot(data, valor, data = df, colour = serie, geom = "line")
p <- p + scale_x_date(format = "%Y")
p

ggplot(df, aes(x = data, y = valor, group = serie, colour = serie)) +
  geom_point() + geom_line()

last_plot() + theme_bw() + opts(panel.grid.major = theme_blank()) +
     xlab(NULL) + ylab(NULL)
