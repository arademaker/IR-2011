
library(RCurl)
library(XML)

url <- "http://www.sidra.ibge.gov.br/bda/tabela/protabl.asp"
query <- "c=1387&i=P&nome=on&notarodape=on&tab=1387&orc315=3&unit=0&pov=1&sec315=7169&OpcTipoNivt=1&opn1=2&nivt=0&opc315=2&orp=4&opv=1&pop=3&orv=2&poc315=2&sev=355&opp=1&qtu6=2&ascendente=on&sep=23008&sep=21157&sep=16377&sep=12879&orn=1&qtu7=9&pon=1&opn6=0&proc=1&qtu1=1&cabec=on&opn7=0&decm=99"

params <- unlist(strsplit(strsplit(query, "&")[[1]], "="))
params[1:length(params) %% 2 == 0]
params[1:length(params) %% 2 != 0]

doc <- htmlParse(paste(url,"?",query, sep=""))
tableNodes <- getNodeSet(doc, "//table")
tab <- readHTMLTable(tableNodes[[2]], stringsAsFactors=FALSE)

tb1 <- as.data.frame(sapply(tab[,-1], function(x) as.numeric(gsub(",", ".", x, fixed=TRUE))),
                     stringsAsFactors = FALSE)
tb2 <- cbind(tab$V1,tb1)
