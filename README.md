# Camp Fire Project

## Overview

STA 141B final project repository. The file report is:

* STA 141B Project Report.pdf

Note: To open the visualization, you must either use Mozilla Firefox or 
run Google Chrome with ```--allow-file-access-from-files``` parameter.

## Team Members

Christina Chang, Troy Lui, Jonathan Quach

## Contents

### Directories

* LDAvis - Directory containing the interactive visualization for 
the LDA topic model. Open index.html to view the visualization.
* LDAscripts - Contains scripts used to build the LDA model
	
	- lda_tuning.R - script used to make plot to find the number of topics
	- mallet_to_LDAvis.R - converts a mallet instance list to LDAvis 
	visualization
	
* data - Contains data used for this project. Format is a pickle
file with extension .txt. Must have the package pickle installed
to load these dataframes.

	- all_df.txt - all data combined into one data frame
	- all_nodup.txt - all data in one data frame with duplicate text and titles removed
	- articles_gn.txt - data frame containing Google News data
	- paradise_merge.txt - data frame containing local news data
	- tweets_with_id.txt - data frame containing tweets data with 
	unique ID numbers assigned to each Tweet

### Notebooks

* Paradise_STA141B.ipynb - Notebook containing code for scraping
Local (Paradise) News and Frequency Plots and Word Clouds
* Twitter.ipynb - Notebook containing code for using Twitter API
and Frequency Plots and Word Clouds
* googlenews.ipynb - Notebook containing code for scraping
National (Google) News and Frequency Plots and Word Clouds
* clustering_hierarchical.ipynb - contains code for constructing
the hierarchical clustering dendrogram
* clustering_kmeans.ipynb - contains code for constructing
the K-means clustering plot

