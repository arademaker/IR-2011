
source("movielens.R")

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


# Outra versao usando funcao "cor" do R
simPearson2 <- function(prefs, person1, person2) {
  a <- prefs[prefs$person == person1,]
  b <- prefs[prefs$person == person2,]
  data <- merge(a, b, by = "item")
  cor(data$rank.x, data$rank.y, method = "pearson")
}


topMatches <- function(prefs, person, n = NA, similarity = simDistance) {
  others <- setdiff(unique(prefs$person), person)
  dummy <- sapply(others, function(x) similarity(prefs, person, x))
  if(!is.na(n)){
    n <- min(n, length(dummy))
    dummy[order(dummy, decreasing=TRUE)][1:n]
  }
  else
    dummy[order(dummy, decreasing=TRUE)]
}


### Exercicio: O que faz a expressao abaixo?
## 
## subset(as.data.frame(table(unique(critics$person),rep("Toby", 7)),
##                      stringsAsFactors=FALSE), Var1 != Var2, select=c(1,2))


getRecommendations <- function(prefs, person, similarity = simDistance) {
  haveseen <- prefs[prefs$person == person,"item"]
  data <- prefs[(! prefs$item %in% haveseen) & prefs$rank > 0,]

  # data$similarity <- sapply(data$person, function(x) similarity(prefs, person, x[1]))  
  dummy <- data.frame(person = unique(data$person), me = person)
  dummy$similarity <- apply(dummy, 1, function(x) similarity(prefs, x[1], x[2]))
  data <- merge(data, dummy)
  data$srank <- with(data, rank * similarity)
  
  ### alternativa 2
  ## dummy <- sapply(split(data, data$item),
  ##                 function(df) sum(df$srank) / sum(df$similarity))
  ## d <- as.data.frame(dummy)
  ## d$movies <- rownames(d)
  ## rownames(d) <- NULL
  ## d[order(d$d, decreasing=TRUE),]

  ### alternativa 3
  ## library(reshape)
  ## mdata <- melt(data[,c("person","item","similarity","srank")])
  ## d <- as.data.frame(cast(mdata, item ~ variable, sum))
  ## d$rank <- d$srank / d$similarity
  ## d[order(d$rank, decreasing=TRUE),c("movie","rank")]

  d <- as.data.frame(as.table(by(data, data$item,
                                 function(df) sum(df$srank) / sum(df$similarity))),
                     responseName = "rank")
  d[order(d$rank, decreasing=TRUE),]
}


### ITEM BASED
## Diff python implementation: names/data.frames versus dict

calculateSimilarItems <- function(prefs, n = 10, similarity = simDistance){
  names(prefs) <- c("person", "item", "rank")
  ranks <- Reduce(rbind, lapply(unique(prefs$person),
                                function(x) {
                                  tt <- topMatches(prefs, x, n = n, similarity = similarity)
                                  data.frame(item.x = x, item.y = names(tt), similarity = tt, row.names = NULL)
                                }))
  ranks
}


getRecommendedItems <- function(prefs, itemMatch, person){
  personRating <- prefs[prefs$person == person,]
  sims <- itemMatch[! itemMatch$item.y %in% personRating$item,]

  dummy <- merge(personRating, sims, by.x = "item", by.y = "item.x")
  dummy$srank <- with(dummy, rank * similarity)

  a <- aggregate(similarity ~ item.y, dummy, sum)
  b <- aggregate(srank ~ item.y, dummy, sum)
  predict <- merge(a,b)
  predict$ranks <-  with(predict, srank / similarity)
  predict[order(predict$ranks, decreasing=TRUE), c(1,4)]
}

