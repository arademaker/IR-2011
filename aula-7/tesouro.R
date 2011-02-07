
# Parametros
data  <- c("2010","12","01")
meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", 
           "out", "nov", "dez")

# Make urls
urls <- c(paste(c("http://www.tesouro.fazenda.gov.br/hp/downloads/resultado/",data[1], "/Tabelas_",
                  meses[as.numeric(data[2])],data[1],".xls"), collapse=""),
          paste("http://www.tesouro.fazenda.gov.br/hp/downloads/resultado/Tabela", c(1,2,3), ".xls", sep=""))

# Download files
sapply(urls, function(u) system(paste("curl", u, "-o", tail(strsplit(u, "/")[[1]],1)),
                                wait = TRUE, intern = FALSE, ignore.stderr = TRUE))


## Proc Excel?
# http://cran.r-project.org/doc/manuals/R-data.html#Reading-Excel-spreadsheets
# http://www.johndcook.com/r_excel_clipboard.html
# http://www.omegahat.org/RSXML/
# http://www.omegahat.org/SSOAP/
# http://www4.bcb.gov.br/pec/series/port/aviso.asp
# http://en.wikipedia.org/wiki/RExcel

