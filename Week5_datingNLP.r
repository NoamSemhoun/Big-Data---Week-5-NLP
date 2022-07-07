library(stringr)
library(dplyr)
library(ggplot2)
library(mosaic)
library(xtable)
library(gridExtra)
library(stopwords)
library(quanteda)
library(bigmemory)
library(rpart)
library(rpart.plot)
library(caret)
library(factoextra)
library(ggfortify)

# Set the rounding to 2 digits round
options(digits=2)


okcupid_profiles <- read.csv( file.path('okcupid_profiles.csv' ), header=TRUE, stringsAsFactors=FALSE)
n <- nrow(okcupid_profiles)

str(okcupid_profiles)

pdf( file.path('Week5_datingNLP.pdf') )

#filtering the height between 55 and 80 inches
okcupid_profiles.subset <- filter(okcupid_profiles, height>=55 & height <=80)

# Histograms of the users heights split by gender (f, m)
histogram( ~ height | sex, width=1, layout=c(1,2), xlab="Height (inches)",
           data=okcupid_profiles.subset)

# The distributions of gender and diet
op <- par(mfrow=c(1, 2))
barplot(table(okcupid_profiles$sex)/n, xlab="sex", ylab="proportion")
barplot(table(okcupid_profiles$diet)/n, xlab="diet", ylab="proportion")

# Joint distribution of gender and sexual diet
addmargins ( xtabs(~ diet + sex, data=okcupid_profiles) )
addmargins ( tally(diet ~ sex, data=okcupid_profiles, format='proportion') )

#joining the similar diets into one
#ex: (vegetarian, mostly vegetarian, strictly vegetarian) => vegetarian
okcupid_profiles$Diet <- sapply( okcupid_profiles$diet, function(kind_of_diet) last( strsplit( kind_of_diet, "\\s", perl = T)[[1]] ) )

sex.by.diet <- tally(~ sex + Diet, data=okcupid_profiles)
sex.by.diet
mosaicplot(sex.by.diet, main="Gender VS Diet", las=1)

essays <- select(okcupid_profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse=" ")

html_characters <- c( "<a", "class=.ilink.", "\n", "\\n", "<br ?/>", "/>", "/>\n<br" )
stop.words <-  c( "a", "am", "an", "and", "as", "at", "are", "be", "but", "can", "do", "for", "have", "i'm", "if", "in", "is", "it", "like", "love", "my", "of", "on", "or", "so", "that", "the", "to", "with", "you", "i" )

html_characters.pat <- paste0( "(", paste(html_characters, collapse = "|"), ")" )
html_characters.pat
stop.words.pat <- paste0( "\\b(", paste(stop.words, collapse = "|"), ")\\b" )
stop.words.pat
essays <- str_replace_all(essays, html_characters.pat, " ")
essays <- str_replace_all(essays, stop.words.pat, " ")



okcupid_profiles$has.book <- str_detect(essays, "book")
tally(has.book ~ sex, okcupid_profiles, format='proportion')

queries <- c("travel", "food", "wine", "beer", "football", "art")
output <- data.frame(word=queries, female=rep(0, length(queries)), male=rep(0, length(queries)))
for(i in 1:length(queries)) {
  query <- queries[i]
  has.query <- str_detect(essays, query)
  results <- table(has.query, okcupid_profiles$sex)
  output[i, 2:3] <- results[2, ] / colSums(results)
}
print(xtable(output, digits=c(0, 0, 3, 3),
             caption ="Proportions of every gender that using 'word' in essays.",
             label = "tab:word_use"), include.rownames=FALSE)

# Co-occurrence of 'travel' and 'wine'.
okcupid_profiles$has.wine <- str_detect(essays, "wine")
okcupid_profiles$has.travel <- str_detect(essays, "travel")
travel.vs.wine <- tally(~has.travel + has.wine, data=okcupid_profiles)
mosaicplot(travel.vs.wine, xlab="travel", ylab="wine", main = "has Travel VS has Wine")


# one-column plots
par(op)

# get the length of the texts
okcupid_profiles$TextLength <- nchar(essays)
summary(okcupid_profiles$TextLength)

# Visualize distribution with ggplot2, adding segmentation for gender
ggplot(okcupid_profiles, aes(x = TextLength, fill = sex)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  xlim(20, 6000) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Lenght of Text by gender")

#plot to the pdf file
qplot(1:10, 1:10, geom = "blank") + theme_bw() +
  theme(line = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title = element_text(face = "bold")
  ) +
  annotation_custom(grob = tableGrob(output), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(title = "", subtitle = "Proportions of every gender using word in essays", caption = "")

okcupid_profiles$has.football <- str_detect(essays, "football")
results <- tally( ~ has.football + sex, data=okcupid_profiles)
prop.test(x=results[1, ], n=colSums(results), alternative="two.sided")

male.words <- subset(essays, okcupid_profiles$sex == "m") %>%
  str_split(" ") %>%
  unlist() %>%
  table() %>%
  sort(decreasing=TRUE) %>%
  names()
female.words <- subset(essays, okcupid_profiles$sex == "f") %>%
  str_split(" ") %>%
  unlist() %>%
  table() %>%
  sort(decreasing=TRUE) %>%
  names()


# Top 20 female words
print( female.words[1:20] )
# Top 20 male words:
print( male.words[1:20] )


# Tokenize the essay texts
all.tokens <- tokens(essays, what = "word",
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_hyphens = TRUE)



# Lower the tokens.
all.tokens <- tokens_tolower(all.tokens)

# remove the stop words from tokens
all.tokens <- tokens_select(all.tokens, stopwords(),
                            selection = "remove")


# stemming the tokens.
all.tokens <- tokens_wordstem(all.tokens, language = "english")


# bag of words
dfm <- dfm(all.tokens, tolower = FALSE)
dfm <- dfm_select(dfm, names(topfeatures(dfm, n = 2000)))

#we can not run it on the actual size so we lowered it down
dfm <- dfm[1:2000,1:2000]

tf <- dfm / rowSums(dfm)
tf[is.na(tf)] <- 0

N <- nrow(dfm)

idf <- log(N/colSums(dfm))
tf.idf <- t(t(tf)*idf)

tf.idf <- as.matrix(tf.idf)

# Setup a data frame with labels.
tf.idf <- cbind(Label = okcupid_profiles$sex[1:dim(tf.idf)[1]], data.frame(tf.idf))

#train the cv model
model <- train(
  Label ~ . ,
  method = "rpart",
  data = tf.idf,
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

mytree <- model$finalModel

pred <- predict(mytree, type="class")
table(pred, tf.idf$Label)

dev.off()

pdf( file.path('Week5_datingNLP.pdf') ,width = 20, height = 20)

#plot the tree to the pdf file
rpart.plot(mytree)


df.names <- data.frame(val = mytree$variable.importance)
df.names <- subset(df.names, val > 1)

names.to.remove <- c(row.names(df.names), "Label")

# Clean the names of column.
tf.idf <- tf.idf[, !colnames(tf.idf) %in% c(names.to.remove)]

tf.idf.PCA <- prcomp(tf.idf)

#pca for k = 2
k <- 2
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("The clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("The PCAs for K = ", k), collapse = " "))

#pca for k = 3
k <- 3
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("The clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("The PCAs for K = ", k), collapse = " "))

#pca for k = 4
k <- 4
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("The clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("The PCAs for K = ", k), collapse = " "))

#pca for k = 10
k <- 10
clusters <- kmeans(tf.idf.PCA$x[,1:2], k)
fviz_cluster(clusters, data =  tf.idf.PCA$x[,1:2],geom = "point",ellipse.type = "convex", ggtheme = theme_bw() , main = paste(c("The clusters for K = ", k), collapse = " "))
plot(tf.idf.PCA, main = paste(c("The PCAs for K = ", k), collapse = " "))

dev.off()

train.model <- mytree
train.dfm <- dfm
cluster.pca <- tf.idf.PCA

save(file = 'Week5_datingNLP.rdata', train.model, train.dfm, cluster.pca)