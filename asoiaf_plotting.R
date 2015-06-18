ratings <- read.csv("ASOIAFRatingsForPlotting.csv")
ratings$Book = factor(ratings$Book,levels(ratings$Book)[c(4,1,5,3,2,6)])

filtered_ratings <- ratings[ratings[,"Book"] %in% c("AGOT", "ACOK", "ASOS", "BOIL"),c("Book", "Chapter", "Rating")]
filtered_ratings$ChapterNumber <- c(1:nrow(filtered_ratings))

library(ggplot2)

flow_plot <- ggplot(filtered_ratings, aes(x=ChapterNumber, y=Rating))

library(splines)
library(MASS)

flow_plot + geom_line() + stat_smooth(method="lm", formula = y ~ ns(x, 5))  + ggtitle("Chapter by Chapter Ratings")


filtered_ratings <- ratings[ratings[,"Book"] %in% c("AGOT", "ACOK", "ASOS", "AFFC", "ADWD"),c("Book", "Chapter", "Rating")]

book_plot <- ggplot(filtered_ratings, aes(x=Chapter, y=Rating, group=Book))
book_plot + geom_line(aes(colour=Book), size=0.8)

AGOT_ratings <- ratings[ratings[,"Book"] %in% c("AGOT"),c("Book", "Chapter", "Rating")]
ACOK_ratings <- ratings[ratings[,"Book"] %in% c("ACOK"),c("Book", "Chapter", "Rating")]
ASOS_ratings <- ratings[ratings[,"Book"] %in% c("ASOS"),c("Book", "Chapter", "Rating")]
AFFC_ratings <- ratings[ratings[,"Book"] %in% c("AFFC"),c("Book", "Chapter", "Rating")]
ADWD_ratings <- ratings[ratings[,"Book"] %in% c("ADWD"),c("Book", "Chapter", "Rating")]
BOIL_ratings <- ratings[ratings[,"Book"] %in% c("BOIL"),c("Book", "Chapter", "Rating")]

AGOT_plot <- ggplot(AGOT_ratings, aes(x=Chapter, y=Rating)) + geom_line(colour="blue", size=0.8) + ggtitle("AGOT")
ACOK_plot <- ggplot(ACOK_ratings, aes(x=Chapter, y=Rating)) + geom_line(colour="yellow", size=0.8) + ggtitle("ACOK")
ASOS_plot <- ggplot(ASOS_ratings, aes(x=Chapter, y=Rating)) + geom_line(colour="green", size=0.8) + ggtitle("ASOS")
AFFC_plot <- ggplot(AFFC_ratings, aes(x=Chapter, y=Rating)) + geom_line(colour="red", size=0.8) + ggtitle("AFFC")
ADWD_plot <- ggplot(ADWD_ratings, aes(x=Chapter, y=Rating)) + geom_line(colour="black", size=0.8) + ggtitle("ADWD")
BOIL_plot <- ggplot(BOIL_ratings, aes(x=Chapter, y=Rating)) + geom_line(colour="brown", size=0.8) + ggtitle("Boiled Leather")

multiplot(AGOT_plot, ACOK_plot, ASOS_plot, AFFC_plot, ADWD_plot, BOIL_plot, cols=2)


qplot(Book, Rating, data = ratings, geom = "boxplot", ylab="Chapter Ratings") + ggtitle("Distribution of chapter ratings by Book")

means2 <- ddply(ratings, .(Book), summarize,  rating=mean(Rating))
means1 <- ddply(ratings, .(Book), summarize,  rating=mean(Weighted_Sum)/mean(Votes))