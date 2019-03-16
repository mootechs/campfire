setwd("C:/Users/Jon/Documents/UC Davis/Winter 2019/STA 141B/project")
#setwd("~/project")
txt <- read.csv("cleaned_lemma.csv")
txt$cleaned <- gsub(pattern = "\\[|\\]", "", txt$cleaned)

library(tm)
corpus <- Corpus(VectorSource(txt$cleaned))
dtm <- DocumentTermMatrix(corpus)

library("ldatuning")

rowTotals <- apply(dtm, 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]  

result <- FindTopicsNumber(
  dtm.new,
  topics = seq(from = 20, to = 150, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 20L,
  verbose = TRUE
)

png('topics_2.png', width = 1280, height = 960, res = 120)
result <- readRDS("tuning.RDS")
FindTopicsNumber_plot(result)
dev.off()


txt_only <- txt$cleaned[which(txt$cleaned != "")]
write.table(x = txt_only, "cleaned_text_only.txt")

