ratings <- read.csv("ASOIAFRatingsForPlotting.csv")
ratings$Book = factor(ratings$Book,levels(ratings$Book)[c(4,1,5,3,2,6)])


reorder( RatingsByPOV$Character, c("Tyrion", "Jon", "Arya", "Daenerys", "Sansa", "Catelyn", "Bran", "Jaime", "Eddard", "Theon", "Davos", "Cersei", "Samwell", "Brienne", "Barristan", "Victarion", "Arianne", "Asha", "Quentyn", "Aeron", "Areo", "Connington", "Arys", "Melisandre", "Will", "Cressen", "Chett", "Merrett", "Pate", "Varamyr", "Kevan") )

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



# Plotting by POV

POVs <- read.csv("POVs.csv")
ratings <- read.csv("ASOIAFRatingsForPlotting.csv")
ratings$Book = factor(ratings$Book,levels(ratings$Book)[c(4,1,5,3,2,6)])
POVs$Book = factor(POVs$Book,levels(POVs$Book)[c(4,1,5,3,2,6)])

AvgRatings <- ratings[ratings[,"Book"] %in% c("AGOT", "ACOK", "ASOS", "BOIL"),c("Book", "Chapter", "RefID", "Rating")]
AvgRatings$ChronoOrder <- as.numeric(AvgRatings$Book)*1000 + AvgRatings$Chapter


RatingsByPOV <- merge(AvgRatings, POVs, by = "RefID")

# ratings$Book = factor(ratings$Book,levels(ratings$Book)[c(4,1,5,3,2,6)])

MultiChapterPOVs <- c("Tyrion", "Jon", "Arya", "Daenerys", "Sansa", "Catelyn", "Bran", "Jaime", "Eddard", "Theon", "Davos", "Cersei", "Samwell", "Brienne", "Barristan", "Victarion", "Arianne", "Asha", "Quentyn", "Aeron", "Areo")
RatingsTop10POVs <- RatingsByPOV[RatingsByPOV[,"Character"] %in% MultiChapterPOVs[c(1:10)],]
RatingsTop21POVs <- RatingsByPOV[RatingsByPOV[,"Character"] %in% MultiChapterPOVs,]

POVStats <- ddply(RatingsByPOV, .(Character), summarize,  MeanRating=mean(Rating),  MedianRating=median(Rating), MinRating=min(Rating), MaxRating=max(Rating))

qplot(Character, Rating, data = RatingsTop21POVs, geom = "boxplot", ylab="Chapter Ratings") + ggtitle("Distribution of chapter ratings by POV")

Sansa_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Sansa"),c("ChronoOrder", "Character", "Rating")]
Sansa_ratings$ChronoOrder <- rank(Sansa_ratings$ChronoOrder)
Sansa_plot <- ggplot(Sansa_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="blue", size=0.8) + ggtitle("Sansa")

Bran_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Bran"),c("ChronoOrder", "Character", "Rating")]
Bran_ratings$ChronoOrder <- rank(Bran_ratings$ChronoOrder)
Bran_plot <- ggplot(Bran_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="green", size=0.8) + ggtitle("Bran")

Arya_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Arya"),c("ChronoOrder", "Character", "Rating")]
Arya_ratings$ChronoOrder <- rank(Arya_ratings$ChronoOrder)
Arya_plot <- ggplot(Arya_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="red", size=0.8) + ggtitle("Arya")

Jon_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Jon"),c("ChronoOrder", "Character", "Rating")]
Jon_ratings$ChronoOrder <- rank(Jon_ratings$ChronoOrder)
Jon_plot <- ggplot(Jon_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="blue", size=0.8) + ggtitle("Jon")


multiplot(Jon_plot, Arya_plot, Sansa_plot, Bran_plot, cols=2)

StarkSisters <- rbind(Sansa_ratings, Arya_ratings)
StarkSisters_plot <- ggplot(StarkSisters, aes(x=ChronoOrder, y=Rating, group=Character)) + geom_line(aes(colour=Character), size=0.8) + ggtitle("Stark Sisters")


Tyrion_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Tyrion"),c("ChronoOrder", "Character", "Rating")]
Tyrion_ratings$ChronoOrder <- rank(Tyrion_ratings$ChronoOrder)
Tyrion_plot <- ggplot(Tyrion_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="red", size=0.8) + ggtitle("Tyrion")

Jaime_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Jaime"),c("ChronoOrder", "Character", "Rating")]
Jaime_ratings$ChronoOrder <- rank(Jaime_ratings$ChronoOrder)
Jaime_plot <- ggplot(Jaime_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="red", size=0.8) + ggtitle("Jaime")

Cersei_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Cersei"),c("ChronoOrder", "Character", "Rating")]
Cersei_ratings$ChronoOrder <- rank(Cersei_ratings$ChronoOrder)
Cersei_plot <- ggplot(Cersei_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="red", size=0.8) + ggtitle("Cersei")

Daenerys_ratings <- RatingsByPOV[RatingsByPOV[,"Character"] %in% c("Daenerys"),c("ChronoOrder", "Character", "Rating")]
Daenerys_ratings$ChronoOrder <- rank(Daenerys_ratings$ChronoOrder)
Daenerys_plot <- ggplot(Daenerys_ratings, aes(x=ChronoOrder, y=Rating)) + geom_line(colour="black", size=0.8) + ggtitle("Daenerys")

multiplot(Tyrion_plot, Jon_plot, Daenerys_plot, cols=2)

multiplot(Tyrion_plot, Jaime_plot, Cersei_plot, cols=2)