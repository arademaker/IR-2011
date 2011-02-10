#    A Beginner's Guide to R (2009)
#    Zuur, Ieno, Meesters.    Springer
#
#    With modifications by Alexandre Rademaker
#    (alexandre.rademaker@fgv.br)


## Basic Calculator

5 + 49 - (8 * 5)
-5
12 / 4

31 %% 7
31 %/% 7


## Functions

# retorna a definição da função
q

# executa a função
q()

# retorna os argumentos, assinatura, da função
args(q)
str(q)

# passagem de argumentos por posicao ou nomes
q(save="no", runLast=TRUE)
q("no",,TRUE)



## Storage

# assign, <- versus relation (=)

Wing1 <- 59
Wing2 <- 55

# cuidado! a<-5
a < -5
a <- 5

# aritmetica com variaveis
2 * Wing1
Wing1 + Wing2
(Wing1 + Wing2)/5

# funcoes simples: sum, sqrt, mean
sqrt(Wing1)
S.win <- sum(Wingcrd)


# Retornando valor da atribuicao (; do MATLAB)
Sum.12 <- Wing1 + Wing2
(Sum.12 <- Wing1 + Wing2)


## Example

# taxa de 0.25% por 30 anos 
taxa.30 <- 1.0025^30
saldo.inicial <- 3000
saldo.final <- saldo.inicial * taxa.30
saldo.final


# VETORES e indexacao
Wingcrd <- c(59, 55, 53.5, 55, 52.5, 57.5, 53, 55)
Wingcrd[1]
Wingcrd[1:5]
Wingcrd[-2]
Wingcrd[-c(1,2)]

# Erro
Wingcrd[c(-1,2)]

# Alternativa
Wingcrd[-1][2]


# O operador ":"
options(width=40)
1:20

Tarsus <- c(22.3, 19.7, 20.8, 20.3, 20.8, 21.5, 20.6, 21.5)
Head <- c(31.2, 30.4, 30.6, 30.3, 30.3, 30.8, 32.5, NA)
Wt <- c(9.5, 13.8, 14.8, 15.2, 15.5, 15.6, 15.6, 15.7)

# nomes em Maiusculas ou Minusculas
sum(Head, na.rm = TRUE)
?head

str(Head)
str(head)


# aritmética com vetores
x <- c(1, 10,4)
x * 3
y <- x - 5
x^3
y^x

# reciclagem: casos
rep(1:10,each=2) %% 2:3
rep(1:10,each=2) %% 2:4


# funcoes rep e seq
BirdData <- c(Wingcrd, Tarsus, Head, Wt)
Id <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
        2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4)

rep(c(1, 2, 3, 4), each = 8)
rep(1:4, each = 8)
rep(c(1,2),c(3,4))

a <- seq(from = 1, to = 4, by = 1)
rep(a, each = 8)

VarNames <- c("Wingcrd", "Tarsus", "Head", "Wt")
Id <- rep(VarNames, each = 8)


# Valores especiais
alguns.pares <- NULL
alguns.pares[seq(2,20,2)] <- seq(2,20,2)
alguns.pares

x <- c(0,7,8)
x/x
1/x
x[0.4]


# Aproximações: vide slides.pdf
n <- 1:10
1.25 * (n * 0.8) - n
5/4 * (n * 4/5) - n


