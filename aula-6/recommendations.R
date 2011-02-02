
critics <- read.table("critics.text", sep="|", header=TRUE, stringsAsFactors = FALSE)

simDistance <- function(prefs, person1, person2) {
  a <- prefs[prefs$person == person1,]
  b <- prefs[prefs$person == person2,]
  data <- merge(a, b, by = "item")
  if(nrow(data) == 0)
    return(0)

  1/(1 + sqrt(sum((data$rank.x - data$rank.y)^2)))
}

simPearson1 <- function(prefs, person1, person2) {
  a <- prefs[prefs$person == person1,]
  b <- prefs[prefs$person == person2,]
  data <- merge(a, b, by = "item")
  if(nrow(data) == 0)
    return(0)

  x <- data$rank.x
  y <- data$rank.y
  n <- nrow(data)
  sum1   <- sum(x)
  sum2   <- sum(y)
  sum1Sq <- sum(x^2)
  sum2Sq <- sum(y^2)
  pSum   <- sum(x*y)
  
  num <- pSum - (sum1 * sum2/n)
  den <- sqrt((sum1Sq - sum1^2/n) * (sum2Sq - sum2^2/n))
  if( den == 0)
    return(0)
  
  return(num/den)
}


# Outra versao usando funcao do R, cor
simPearson2 <- function(prefs, person1, person2) {
  a <- prefs[prefs$person == person1,]
  b <- prefs[prefs$person == person2,]
  data <- merge(a, b, by = "item")
  cor(data$rank.x, data$rank.y, method = "pearson")
}
  

topMatches <- function(prefs, person, n = NA, similarity = simDistance) {
  data <- as.data.frame(table(prefs$person, prefs$person))
  data$Freq <- NULL
  names(data) <- c("person1","person2")
  data$distance <- apply(data, 1, function(x) similarity(prefs, x[1], x[2]))

  dummy <- subset(data, person2 != person1 & person1 == person)
  dummy <- dummy[order(dummy$distance, decreasing=TRUE),]
  if(!is.na(n))
    dummy[1:n,]
  else
    dummy
}

## Exercicio: O que faz a expressao abaixo?
## 
## subset(as.data.frame(table(unique(critics$person),rep("Toby", 7)),
##                      stringsAsFactors=FALSE), Var1 != Var2, select=c(1,2))


getRecommendation <- function(prefs, person, similarity = simDistance) {
  data <- prefs[prefs$person != person & prefs$rank > 0,]

  # data$similarity <- apply(data, 1, function(x) similarity(prefs, person, x[1]))
  dummy <- data.frame(person = unique(data$person), me = person)
  dummy$similarity <- apply(dummy, 1, function(x) similarity(prefs, person, x[1]))
  data <- merge(data, dummy)

  # rank weighted by similarity
  data$srank <- data$rank * data$similarity

  # alternative 1
  ## sum.srank <- aggregate(data$srank, by = list(movie = data$movie), sum)
  ## sum.sim <- aggregate(data$similarity, by = list(movie = data$movie), sum)
  ## dummy <- merge(sum.srank,sum.sim, by = "movie")
  ## dummy$rank <- dummy$x.x / dummy$x.y
  ## dummy[order(dummy$rank, decreasing=TRUE),c("movie","rank")]

  ## alternative 2
  ## dummy <- tapply(1:nrow(data), data["movie"],
  ##                 function(ind, data) sum(data[ind,"srank"])/sum(data[ind,"similarity"]), data = data)
  ## alternativa 3
  ## dummy <- sapply(split(data, data$movie),
  ##                 function(df) sum(df$srank) / sum(df$similarity))
  # d <- as.data.frame(dummy)
  # d$movies <- rownames(d)
  # rownames(d) <- NULL
  # d[order(d$d, decreasing=TRUE),]

  # alternativa 4
  ## library(reshape)
  ## mdata <- melt(data[,c("person","movie","similarity","srank")])
  ## d <- as.data.frame(cast(mdata, movie ~ variable, sum))
  ## d$rank <- d$srank / d$similarity
  ## d[order(d$rank, decreasing=TRUE),c("movie","rank")]

  d <- as.data.frame(as.table(by(data, data$item,
                                 function(df) sum(df$srank) / sum(df$similarity))),
                     responseName = "rank")
  d[order(d$rank, decreasing=TRUE),]
}

