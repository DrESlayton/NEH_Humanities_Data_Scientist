#### Script for text mining of NEH XML files #####

#### Instaling packages ######
# For adding in data and cleaning the corpus
install.packages("XML")
install.packages("tm")
install.packages("SnowballC")
install.packages("dplyr")


# For visulizing the data 
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("ggplot2")

library(XML)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(dplyr)


#### Load XML File #####

# Set working directory 
setwd("~/Desktop/NEH/NEH_GrantProducts")

# Check you are in the correct working directory 
getwd()

# Set the path to XML file
OpenAccessItems <- "NEH_OpenAccessItems.xml"

# Reading in the XML file
OpenAccessItems_content <- xmlParse(OpenAccessItems)

#### Getting to the XML text #### 

# Extracting text from the XML content
text <- sapply(xmlChildren(OpenAccessItems_content), function(x) xmlValue(x))

# Converting extracted text to character vector
text <- unlist(text)

# Creating Corpus for text mining
corpus <- Corpus(VectorSource(text))

# Delve into the 'corpus' 
inspect(corpus)

###### Corpus Cleaning #### 

#Text Preprocessing: Transformations are done via the tm_map() 
# function which applies (maps) a function to all elements 
# of the corpus

# Clean the corpus of white space
corpus_clean <- tm_map(corpus, stripWhitespace)

# Set characters to lower case
corpus_clean <- tm_map(corpus_clean, tolower)

# Remove 'Stop Words': Stop words are common words in a language that are typically filtered out or ignored when processing natural language text or conducting searches. These words are considered to be of little value in text analysis because they occur frequently and don't carry significant meaning on their own. Examples of stop words in English include "the," "and," "in," "of," "is," "at," "on," and "it."

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))

# Stemming the values in the corpus 
# Stemming is a natural language processing technique that is used to reduce words to their base form, also known as the root form. 

corpus_clean <- tm_map(corpus_clean, stemDocument)

###### Term Document Matrix ####
# A common approach in text mining is to create a term-document matrix from a corpus.
# A document-term matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents. 

dtm <- DocumentTermMatrix(corpus_clean)
inspect(dtm)

### Identifying the frequency of terms ###

# Identify the terms that apear more than 5 times 
findFreqTerms(dtm, 5)

# Identify the terms that apear more than 10 times
findFreqTerms(dtm, 10)

# Identify the assocations between concepts in the corpus
findAssocs(dtm, c("open"), c(0.2))

# Find the frequesncy of terms in the corpus
findFreqTerms(dtm, lowfreq = 0, highfreq = Inf)

### Parsing XML data to transfer to CSV #####

#Add XML file to transition to CSV
setwd("~/Desktop/NEH/NEH_Grants1990s")
Grants1990s <- "NEH_Grants1990s.xml"

# Parses an XML or HTML file or string containing XML/HTML content, and generates an R structure representing the XML/HTML tree.
Grants1990s <- xmlTreeParse("NEH_Grants1990s.xml")

# Extract the data from the parsed xml
records <- xmlToList(Grants1990s)

# Convert the list of records to a data frame
data_frame <- do.call(rbind, records)


# Write to a CSV file 
write.csv(data_frame, "Grants1990s.csv", row.names = FALSE)

######## Cleaning the data and creating new CSV ####

# Recognized issue in that some elements of the column for 'Institution' are bleeding over into 'OrganizationType'.
# As such, we need to re-parse the data and make sure that each variable is initilized to ensure that there is no bleed in the data.

# Re-parsing the XML file
xml_data <- xmlTreeParse("NEH_Grants1990s.xml", useInternalNodes = TRUE)

# Initialize empty vectors for each variable to store the extracted data from the XML in nodes
appNumber <- character()
applicantType <- character()
institution <- character()
organizationType <- character()
instCity <- character()
instState <- character()
instPostalCode <- character()
instCountry <- character()
congressionalDistrict <- character()
latitude <- character()
longitude <- character()
councilDate <- character()
yearAwarded <- character()
projectTitle <- character()
program <- character()
division <- character()
approvedOutright <- character()
approvedMatching <- character()
awardOutright <- character()
awardMatching <- character()
originalAmount <- character()
supplementAmount <- character()
beginGrant <- character()
endGrant <- character()
projectDesc <- character()
toSupport <- character()
primaryDiscipline <- character()
supplementCount <- character()
participantCount <- character()

# Once the data has been set into nodes and tied to type, we can extract data from XML nodes to populate variables.
# To do this, we need to use a for loop, or the process of running functions to set data nodes for each separate grant entry in turn 
# The loop is used to extract data for each variable through accessing the corresponding XML elements and attributes to then append the extracted data to respective vectors. 

records <- getNodeSet(xml_data, "//Grant")
for (record_node in records) {
  appNumber <- c(appNumber, xmlGetAttr(record_node, "AppNumber"))
  applicantType <- c(applicantType, xmlValue(record_node[["ApplicantType"]]))
  institution <- c(institution, xmlValue(record_node[["Institution"]]))
  organizationType <- c(organizationType, xmlValue(record_node[["OrganizationType"]]))
  instCity <- c(instCity, xmlValue(record_node[["InstCity"]]))
  instState <- c(instState, xmlValue(record_node[["InstState"]]))
  instPostalCode <- c(instPostalCode, xmlValue(record_node[["InstPostalCode"]]))
  instCountry <- c(instCountry, xmlValue(record_node[["InstCountry"]]))
  congressionalDistrict <- c(congressionalDistrict, xmlValue(record_node[["CongressionalDistrict"]]))
  latitude <- c(latitude, xmlValue(record_node[["Latitude"]]))
  longitude <- c(longitude, xmlValue(record_node[["Longitude"]]))
  councilDate <- c(councilDate, xmlValue(record_node[["CouncilDate"]]))
  yearAwarded <- c(yearAwarded, xmlValue(record_node[["YearAwarded"]]))
  projectTitle <- c(projectTitle, xmlValue(record_node[["ProjectTitle"]]))
  program <- c(program, xmlValue(record_node[["Program"]]))
  division <- c(division, xmlValue(record_node[["Division"]]))
  approvedOutright <- c(approvedOutright, xmlValue(record_node[["ApprovedOutright"]]))
  approvedMatching <- c(approvedMatching, xmlValue(record_node[["ApprovedMatching"]]))
  awardOutright <- c(awardOutright, xmlValue(record_node[["AwardOutright"]]))
  awardMatching <- c(awardMatching, xmlValue(record_node[["AwardMatching"]]))
  originalAmount <- c(originalAmount, xmlValue(record_node[["OriginalAmount"]]))
  supplementAmount <- c(supplementAmount, xmlValue(record_node[["SupplementAmount"]]))
  beginGrant <- c(beginGrant, xmlValue(record_node[["BeginGrant"]]))
  endGrant <- c(endGrant, xmlValue(record_node[["EndGrant"]]))
  projectDesc <- c(projectDesc, xmlValue(record_node[["ProjectDesc"]]))
  toSupport <- c(toSupport, xmlValue(record_node[["ToSupport"]]))
  primaryDiscipline <- c(primaryDiscipline, xmlValue(record_node[["PrimaryDiscipline"]]))
  supplementCount <- c(supplementCount, xmlValue(record_node[["SupplementCount"]]))
  participantCount <- c(participantCount, xmlValue(record_node[["ParticipantCount"]]))
}

# It is then possible to create a data frame that will ensure we have columns corresponding to each variable
data_frame <- data.frame(
  AppNumber = appNumber,
  ApplicantType = applicantType,
  Institution = institution,
  OrganizationType = organizationType,
  InstCity = instCity,
  InstState = instState,
  InstPostalCode = instPostalCode,
  InstCountry = instCountry,
  CongressionalDistrict = congressionalDistrict,
  Latitude = latitude,
  Longitude = longitude,
  CouncilDate = councilDate,
  YearAwarded = yearAwarded,
  ProjectTitle = projectTitle,
  Program = program,
  Division = division,
  ApprovedOutright = approvedOutright,
  ApprovedMatching = approvedMatching,
  AwardOutright = awardOutright,
  AwardMatching = awardMatching,
  OriginalAmount = originalAmount,
  SupplementAmount = supplementAmount,
  BeginGrant = beginGrant,
  EndGrant = endGrant,
  ProjectDesc = projectDesc,
  ToSupport = toSupport,
  PrimaryDiscipline = primaryDiscipline,
  SupplementCount = supplementCount,
  ParticipantCount = participantCount
)

# Then write to CSV to create the CSV
write.csv(data_frame, "Grants1990s_a.csv", row.names = FALSE)


# Additional cleaning of the above created CSV was conducted in Open Refine 

# This includes splitting postal code column, to better reflect the range of post code values provided (either full or partial)

# Additionally, the CouncilDate column was split into CouncilDateYear, CouncilDateMonth, CouncilDateDay to be able to compare between years and months of assignment

# This process was then repeated for NEH_Grants2020s_Flat and NEH_Grants2010s. 

###### Merging CSVs  #####

# To merge our three new working CSV files, first read all CSVs in, ensuring you are loading from the correct working directory 
setwd("~/Desktop/NEH/NEH_Grants2020s_Flat")
data1 <- read.csv("Grants2020s.csv")
setwd("~/Desktop/NEH/NEH_Grants2010s")
data2 <- read.csv("Grants2010s.csv")

# As our column values and varible types are consistent between 2010s and 2020s data, we will start by comparing those datasets in a merge
# We can merge the data frames vertically
merged_data <- rbind(data1, data2)

# We can then write this merged data to a new CSV in our broader NEH working directory 
setwd("~/Desktop/NEH")
write.csv(data_frame, "Grants2010s_2020s.csv", row.names = FALSE)


### Getting into the text as a CSV ####

# Now that we have a workable CSV file, we can re-run tm() functions on this version of the text to compare results

# First make sure CSV is read into R workspace
Grants2010s2020 <- read.csv("Grants2010s_2020s.csv")

# We can now repeat the process of creating a corpus we explored using the XML data, as referenced above. 

# However, in this case as we are now working with tabular or vector data, we need to specify the column on which we will be running anlysis 
# For this project, that will be the column related to project description

Corpus2010s2020s <- Corpus(VectorSource(Grants2010s2020$ProjectDesc))

# We can again delve into the corpus
inspect(Corpus2010s2020s)

###### Corpus cleaning of merged data ####

## Clean the corpus of white space
Corpus2010s2020s_clean <- tm_map(Corpus2010s2020s, stripWhitespace)

# Set characters to lower case
Corpus2010s2020s_clean <- tm_map(Corpus2010s2020s_clean, tolower)

# Remove 'Stop Words': Stop words are common words in a language that are typically filtered out or ignored when processing natural language text or conducting searches. These words are considered to be of little value in text analysis because they occur frequently and don't carry significant meaning on their own. Examples of stop words in English include "the," "and," "in," "of," "is," "at," "on," and "it."
Corpus2010s2020s_clean <- tm_map(Corpus2010s2020s_clean, removeWords, stopwords("english"))

# Stemming the values in the corpus 
# Stemming is a natural language processing technique that is used to reduce words to their base form, also known as the root form. 

Corpus2010s2020s_clean <- tm_map(Corpus2010s2020s_clean, stemDocument)

###### Term Document Matrix for merged data ####
# A common approach in text mining is to create a term-document matrix from a corpus.
# A document-term matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents. 

dtm2010s2020s <- DocumentTermMatrix(Corpus2010s2020s_clean)
inspect(dtm2010s2020s)

# We can then export this output as a character vector 

Grants2010s2020s_ChV <- capture.output(inspect(dtm2010s2020s))

# Then we can output the data into our general NEH folder
Grants2010s2020s_ChV_file <- "Grants2010s2020s_ChV_output.txt"
writeLines(Grants2010s2020s_ChV, con = Grants2010s2020s_ChV_file)


# 





### Visualizing the data #####

##### Create a plot of term frequency from the Grants2010s2020s Corpus ######
term_freq <- rowSums(as.matrix(dtm2010s2020s))

# Create a data frame with the term frequencies
term_freq_df <- data.frame(Term = names(term_freq), Frequency = term_freq)

# Create a bar plot with term labels on the x-axis using ggplot2
bar_plot <- ggplot(data = term_freq_df) +
  geom_bar(aes(x = reorder(Term, -Frequency), y = Frequency), stat = "identity", fill = "skyblue") +
  labs(title = "Term Frequency", x = "Term", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as a large-size JPG image
ggsave("term_frequency_plot.jpg", plot = bar_plot, width = 18, height = 12, units = "in")

# As the graph with all of the terms in the matrix is so large, it becomes important to select out key terms.

# We can do this by both selecting the top 20 terms in the data set, but also looking for key termonology important to our theme

# To select the top 20 terms for visualization 
term_freq <- rowSums(as.matrix(dtm2010s2020s))
term_freq_df <- data.frame(Term = names(term_freq), Frequency = term_freq)
# Set the number of top terms to display
top_n_terms <- 20
# Filter the data frame to keep only the top terms
term_freq_df <- term_freq_df %>%
  top_n(top_n_terms, wt = Frequency)

# Create a bar plot with term labels on the x-axis using ggplot2
bar_plot <- ggplot(data = term_freq_df) +
  geom_bar(aes(x = reorder(Term, -Frequency), y = Frequency), stat = "identity", fill = "skyblue") +
  labs(title = "Top Terms by Frequency", x = "Term", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("term_frequency_plot_Top_20a.jpg", plot = bar_plot, width = 12, height = 8, units = "in")


# We can also choose to show only specific terms from the corpus which may help to fit our anlysis on theme

# To identify a theme, we can explore the counts of specific words as they apear in the corpus
term_freq <- colSums(as.matrix(dtm2010s2020s))
print(term_freq)

# To find specific terms
words_to_count <- c("open", "public", "access", "share", "community")

word_counts <- numeric(length(words_to_count))

# Loop through the list of words and count each one
for (i in 1:length(words_to_count)) {
  word <- words_to_count[i]
  
  # Check if the word exists in the DTM's column names
  if (word %in% colnames(dtm2010s2020s)) {
    # Get the column index corresponding to the specified word
    col_index <- which(colnames(dtm2010s2020s) == word)
    
    # Calculate the count of the specified word by summing the values in that column
    word_count <- sum(dtm2010s2020s[, col_index])
    
    word_counts[i] <- word_count
  } else {
    word_counts[i] <- 0  # Set count to 0 if the word is not found
  }
}

# Display the counts
for (i in 1:length(words_to_count)) {
  cat("Count of", words_to_count[i], ":", word_counts[i], "\n")
}


# It can also be helpful to see those terms in context of the preceding and following words

words_to_find_context <- c("open", "public", "share", "community")

for (doc_index in 1:length(Corpus2010s2020s_clean)) {
  # Get the text content of the document
  doc_text <- as.character(Corpus2010s2020s_clean[[doc_index]])
  
  # Split the document into words
  words_in_doc <- unlist(strsplit(doc_text, " "))
  
  # Loop through the words in the document
  for (word_to_find in words_to_find_context) {
    if (word_to_find %in% words_in_doc) {
      # Find all occurrences of the target word
      word_indices <- which(words_in_doc == word_to_find)
      
      for (word_index in word_indices) {
        # Find the neighboring words (context) of the target word
        preceding_word <- ifelse(word_index > 1, words_in_doc[word_index - 1], NA)
        following_word <- ifelse(word_index < length(words_in_doc), words_in_doc[word_index + 1], NA)
        
        cat("Document", doc_index, ":")
        cat("Target Word:", word_to_find, "\n")
        cat("Preceding Word:", preceding_word, "\n")
        cat("Following Word:", following_word, "\n\n")
      }
    }
  }
}

## After finding words in context, we can make a visulization of the counts of our chosen terms

selected_words <- c("open", "public", "share")

# Initialize an empty data frame to store word counts
word_count_data <- data.frame(Word = character(0), Count = numeric(0))

# Loop through the selected words and count their occurrences
for (word in selected_words) {
  # Check if the word exists in the DTM's column names
  if (word %in% colnames(dtm2010s2020s)) {
    # Get the column index corresponding to the specified word
    col_index <- which(colnames(dtm2010s2020s) == word)
    
    # Calculate the count of the specified word by summing the values in that column
    word_count <- sum(dtm2010s2020s[, col_index])
    
    # Add the word and its count to the data frame
    word_count_data <- rbind(word_count_data, data.frame(Word = word, Count = word_count))
  }
}

# Create a bar chart
bar_chart <- ggplot(data = word_count_data, aes(x = Word, y = Count)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Word Counts for Selected Words", x = "Word", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Display the bar chart
print(bar_chart)


##### Create a wordcloud from the Grants2010s2020s Corpus #####

wordcloud(Corpus2010s2020s_clean, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))



