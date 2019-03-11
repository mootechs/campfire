library(RJSONIO)
library(LDAvis)
library(rJava)
library(mallet)
library(parallel)


#https://github.com/mlinegar/litMagModelling/blob/master/R/makeLDAvis.R

#' Easy interface to LDAvis, only requires a trained topic.model as input
#' @import rJava
#' @param topic.model Mallet-object a trained topic.model from MALLET
#' @param outDir character directory to store html/js/json files
#' @param openBrowser logical should R open a browser to create visualizations?
#' @param asGist logical should the vis be uploaded as a gist? Requires gistr
#' @param ... arguments passed onto gistr::gist_create
#' @return html/js/json files of a LDAvis visualization
#' @export

saveTables <- function(topic.model, outDir = tempfile(), ...){
	# Make JSON
	phi <- mallet::mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)
	theta <- mallet::mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE)
	doc.length <- rowSums(mallet::mallet.doc.topics(topic.model, smoothed = FALSE, normalized = FALSE))
	word.freqs <- mallet::mallet.word.freqs(topic.model)
	vocab <- topic.model$getVocabulary()
	json <- list(
	phi = phi, theta = theta, doc.length = doc.length, vocab = vocab,
	term.frequency = droplevels(word.freqs)$term.freq)
	json <- toJSON(json)
	write(json, paste0(outDir, "/vis.JSON"))
}

# Obtained from the RMallet src
load.mallet.instances <- function(filename) {
  rJava::J("cc.mallet.types.InstanceList")$load(rJava::.jnew("java/io/File", filename))
}

# the number of cores to be used
var_num_cores <- 6
	

# Setup
setwd("~/project/lda_model")

instance.list.name <- paste0(getwd(), "/topics.mallet")
instance.list <- load.mallet.instances(instance.list.name)

# https://www.rdocumentation.org/packages/mallet/versions/1.0/topics/MalletLDA
# Create a topic trainer object.
mallet.model <- mallet::MalletLDA(num.topics = 40,
                                 alpha.sum = 5,
                                 beta = 0.01)

# Load our documents.
mallet.model$loadDocuments(instance.list)


output_dir <- paste0(getwd(), "/output")
saveTables(topic.model = mallet.model, 
           outDir = output_dir)
