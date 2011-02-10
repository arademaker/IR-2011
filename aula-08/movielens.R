
movies <- read.table("ml-data/u.item", sep = "|", header = FALSE, stringsAsFactors = FALSE, quote="")
movies <- movies[,c(1,2)]
names(movies) <- c("movieid","movie")

rank   <- read.table("ml-data/u.data", sep = "\t", header = FALSE, stringsAsFactors = FALSE,
                     col.names = c("userid","movieid","rating","ts"))

critics <- merge(movies, rank, by = "movieid")
critics$movieid <- NULL
critics$ts <- NULL
names(critics) <- c("item","person","rank")


