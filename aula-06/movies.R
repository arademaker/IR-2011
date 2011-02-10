
## Exemplo adaptado do Cap 1 do livro:
##   Programming Collective Intelligence
##   http://oreilly.com/catalog/9780596529321
## Autor: Alexandre Rademaker

critics <- read.table("critics.text", sep="|", header=TRUE, stringsAsFactors = FALSE)

# Preparando os dados para visualizacao da dispersao do rank dados
# pelas pessoas aos dois filmes seguintes
movies <- c("Snakes on a Plane", "You, Me and Dupree")
a <- critics[critics$item == movies[1],]
b <- critics[critics$item == movies[2],]
temp <- merge(a,b, by = "person")

# Para plotar no grafico, a coluna fn tera apenas os primeiros nomes
# das pessoas
nomes <- temp[,c(1,3,5)]
nomes$fn <- sapply(strsplit(nomes$person, " "), "[", 1)

# O vetor v irá conter os nomes de pessoas que deram mesmo rank para
# ambos os filmes juntos, separador por "and"
v <- unique(nomes[,-c(1,4)])
v$names <- apply(v, 1, function(x) paste(nomes[nomes$rank.x == x[1] & nomes$rank.y == x[2],"fn"],
                                         collapse=" and "))

# Finalmente o plot
plot(temp$rank.x, temp$rank.y, xlim = c(0,6), ylim = c(0,6), xlab = movies[1], ylab = movies[2])
text(v$rank.x, v$rank.y, pos=3, labels = v$names)

# Removendo variveis temporarias
rm(a, b, temp, v, nomes, movies)


