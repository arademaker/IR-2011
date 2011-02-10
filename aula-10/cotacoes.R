
## http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html?codigo=PETR4.SA
## 
## nele vc consegue selecionar de que dia e até quando vc quer os
## dados e as cotações...  o que eu não consegui foi ler isso no R e
## trabalhar com a tabela...pois ela fica muito grande, afinal cada
## dia tem várias cotações (trabalhar com anos, ficaria gigantesco)

library(XML)

getAcao <- function(acao, pages) {
  base <- "http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html"
  params <- c(codigo = acao,
              beginDay = 1,
              beginMonth = 1,
              beginYear = 2008,
              endDay = 9,
              endMonth = 11,
              endYear = 2010,
              size = 200,
              page = 1)

  data <- data.frame(data=0, cotacao=0, min=0,max=0,var=0,var.percent=0,volume=0)
  for(n in 1:pages){
    params["page"] <- n
    url <- paste(base,paste(apply(cbind(names(params),params), 1, paste, collapse="="),
                            collapse="&"), sep = "?")
    doc <- htmlParse(url)
    tableNodes <- getNodeSet(doc, "//table")
    tb <- readHTMLTable(tableNodes[[2]], stringsAsFactors=FALSE)
    names(tb) <- names(data)
    data <- rbind(data,tb)
  }
  data <- data[-c(1,grep("Data",data$data)),]
  data <- cbind(data[,c(1,7)],
                sapply(data[,-c(1,7)],
                       function (x) as.numeric(gsub(",", ".", x, fixed=TRUE))))
  data$volume <- as.numeric(gsub(".","",data$volume,fixed=TRUE))
  data$dia <- as.Date(data$data, "%d/%m/%Y")
  data$data <- NULL
  return(data)
}

vale <- getAcao("VALE5.SA",4)
petr <- getAcao("PETR4.SA",4)
