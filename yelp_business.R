library(ggplot2)
library(dplyr)

yelp_business <- read.csv("C:/Users/JJQ/Documents/data/yelp_business.csv", stringsAsFactors = FALSE, header = TRUE)
str(yelp_business)
head(yelp_business)
View(head(yelp_business))

summary(is.na(yelp_business))

n_restaurants <- yelp_business %>% group_by(state) %>% count(state) 
head(n_restaurants)
tail(n_restaurants)
n_restaurants <- n_restaurants[-c(1:5), ]
top_20 <- arrange(n_restaurants, desc(n))
head(top_20, 20)

g <- ggplot(head(top_20, 20), aes(x = as.factor(state), y = n) + geom_bar(stat = "identity") + xlab("States") + ylab("Number of Restaurant") + ggtitle("Number of Restaurant in different states (Top 20)")
g

g1 <- g + theme(axis.text.x = element_text(size = 10, angle = 45))
g1
          
          
          
g2 <- ggplot(head(top_20, 20), aes(x = reorder(as.factor(state), -n), y = n)) + geom_bar(stat = "identity") + xlab("States") + ylab("Number of Restaurant") + ggtitle("Number of Restaurant in different states (Top 20)")

g2

p <- ggplot(yelp_business, aes(x = as.factor(stars), y = review_count)) + geom_bar(stat = "identity", fill = "blue") + xlab("Stars") + ylab("Number of Reviews")
p


cor(yelp_business$stars, yelp_business$review_count)

fit <- lm(review_count ~ stars, yelp_business)
fit

