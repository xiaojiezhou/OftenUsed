<a id="table-of-contents"></a>
## My NLP R Functions - Table of Contents
* [replace.words: Replace words listed in col 1 by words in col 2](#replace-words)
* [corpus2sent: Break corpus to sentence corpus](#corpus2sent)
* [text2sent: Break text to sentences and merge back with other data ](#text2sent)
* [Terms co-occurrance freq](#cooccur)
* [Create tdm with ngrams](#ngram)
* [Some handy function](#handy)
    - [spacehold] (#spaceholder)


    
<div id='replace-words'/>   
### -- Replacing words in ListA by words in ListB --

    # -- replace.words() replaces all words in dict[,1] with  words in dict[,2]
    # -- It does not replace the word if it is a only partial match
    # -- Dict is a data.frame with two columns
    
    # -- Read in lookup table ---
    require(XLConnect)
    wb = loadWorkbook("C:\\Users\\zhou.x\\Desktop\\Other\\DataScience\\TextAnalytics\\R\\ReplacementWords.xlsx", create = TRUE)
    Dict = readWorksheet(wb, sheet = "sheet1", startCol = 1, endCol = 2, header=TRUE)
    Dict[is.na(Dict[,2]),2] = ""
    head(Dict)

    # -- my messy text ---
    mytext <- c("I cant move",
                'I can\'t move',
                'amd',
                'camd',
                'in the place  ')
    mytext

    # -- replace.words function
    replace.words <- function( txt, dict){
        dict = apply(dict, c(1,2), trimws)
        
        for(x in 1:nrow(dict))
            txt <- gsub(paste0('\\<', dict[x,1], '\\>'), dict[x,2], txt)
        return(txt)
    }

    replace.words(mytext, Dict)
    
[(back to top)](#table-of-contents)

<div id='corpus2sent'/> 
### -- functions to break corpus to sentence corpus
    #Load Packages
    require(tm)
    require(NLP)
    require(openNLP)
    
    ## Convert text to sentences
    convert_text_to_sentences <- function(text, lang = "en") {
      # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
      sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)

      # Convert text to class String from package NLP
      text <- as.String(text)

      # Sentence boundaries in text
      sentence.boundaries <- annotate(text, sentence_token_annotator)

      # Extract sentences
      sentences <- text[sentence.boundaries]

      # return sentences
      return(sentences)
    }
    
    ## Reshape the corpus
    reshape_corpus <- function(current.corpus, FUN, ...) {
      # Extract the text from each document in the corpus and put into a list
      text <- lapply(current.corpus, "content")

      # Basically convert the text
      docs <- lapply(text, FUN, ...)
      docs <- as.vector(unlist(docs))

      # Create a new corpus structure and return it
      new.corpus <- Corpus(VectorSource(docs))
      return(new.corpus)
    }
    
    ## Put all together
    dat <- data.frame(doc1 = "Doctor Who is a British science fiction television programme produced by the BBC. The programme depicts the adventures of a Time Lord—a time travelling, humanoid alien known as the Doctor. He explores the universe in his TARDIS (acronym: Time and Relative Dimension in Space), a sentient time-travelling space ship. Its exterior appears as a blue British police box, a common sight in Britain in 1963, when the series first aired. Along with a succession of companions, the Doctor faces a variety of foes while working to save civilisations, help ordinary people, and right wrongs.",
                      doc2 = "The show has received recognition from critics and the public as one of the finest British television programmes, winning the 2006 British Academy Television Award for Best Drama Series and five consecutive (2005–10) awards at the National Television Awards during Russell T Davies's tenure as Executive Producer.[3][4] In 2011, Matt Smith became the first Doctor to be nominated for a BAFTA Television Award for Best Actor. In 2013, the Peabody Awards honoured Doctor Who with an Institutional Peabody \"for evolving with technology and the times like nothing else in the known television universe.\"[5]",
                      doc3 = "The programme is listed in Guinness World Records as the longest-running science fiction television show in the world[6] and as the \"most successful\" science fiction series of all time—based on its over-all broadcast ratings, DVD and book sales, and iTunes traffic.[7] During its original run, it was recognised for its imaginative stories, creative low-budget special effects, and pioneering use of electronic music (originally produced by the BBC Radiophonic Workshop).",
                      stringsAsFactors = FALSE)

    current.corpus <- Corpus(VectorSource(dat))  # A corpus with 3 text documents

    ## reshape the corpus into sentences (modify this function if you want to keep meta data)
    reshape_corpus(current.corpus, convert_text_to_sentences)   # A corpus with 10 text documents

[(back to top)](#table-of-contents)


<div id='text2sent'/> 
### -- break text to sentences in a data frame and merge back with other data
    ## Load Packages
    require(tm)
    require(NLP)
    require(openNLP)
    library(sqldf)
    library(dplyr)
    library(reshape2)
    
    ## Convert text to sentences
    convert_text_to_sentences <- function(txt, lang = "en") {
        # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
        sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
        
        # Convert text to class String from package NLP
        txt <- as.String(txt)
        
        # Sentence boundaries in text
        sentence.boundaries <- annotate(txt, sentence_token_annotator)
        
        # Extract sentences
        sentences <- txt[sentence.boundaries]
        
        # return sentences
        return(sentences)
    }

 
    ## create a data frame
    dat <- data.frame(Doc = c("Doctor Who is a British science fiction television programme produced by the BBC. The programme depicts the adventures of a Time Lord—a time travelling, humanoid alien known as the Doctor. He explores the universe in his TARDIS (acronym: Time and Relative Dimension in Space), a sentient time-travelling space ship. Its exterior appears as a blue British police box, a common sight in Britain in 1963, when the series first aired. Along with a succession of companions, the Doctor faces a variety of foes while working to save civilisations, help ordinary people, and right wrongs.",
                        "The show has received recognition from critics and the public as one of the finest British television programmes, winning the 2006 British Academy Television Award for Best Drama Series and five consecutive (2005–10) awards at the National Television Awards during Russell T Davies's tenure as Executive Producer.[3][4] In 2011, Matt Smith became the first Doctor to be nominated for a BAFTA Television Award for Best Actor. In 2013, the Peabody Awards honoured Doctor Who with an Institutional Peabody \"for evolving with technology and the times like nothing else in the known television universe.\"[5]",
                        "The programme is listed in Guinness World Records as the longest-running science fiction television show in the world[6] and as the \"most successful\" science fiction series of all time—based on its over-all broadcast ratings, DVD and book sales, and iTunes traffic.[7] During its original run, it was recognised for its imaginative stories, creative low-budget special effects, and pioneering use of electronic music (originally produced by the BBC Radiophonic Workshop)."),
                      othr = c('a','b', 'c'),
                      stringsAsFactors = FALSE)

    ## convert text to sentences
    sent <- lapply(dat$Doc, convert_text_to_sentences)
    sent = melt(sent) #unlist with list names


    ## Merge sentences back with original data
    dat$ID=1:length(dat$Doc)
    newdat = sqldf("select a.value as sent, b.* from sent as a left join  dat as b where  a.L1=b.ID")

[(back to top)](#table-of-contents)

<div id='cooccur'/> 
### --- Takes term document matrix and calculate term co-occurrance frequency
    #--- Set up data ---#   
    dat <- read.table(text="T1 T2 T3 T4  
    1 1 0 0   
    1 1 0 1  
    1 1 1 1  
    0 0 1 1  
    0 0 0 0 ", header=T)

    nTerms = dim(dat)[2]

    #--- terms co-occurrance freq function  ---#   
    dtm2cooccur <- function(dtm){
        From=list()
        To=list()
        Ct=list()
        nTerms = dim(dtm)[2]
        
        nedges=0
        for (i in 1:(nTerms-1)) {
            for (j in (i+1):nTerms ) {
                tmp = dtm[,i]%*%dtm[,j]
                if(tmp>0){
                    nedges = nedges+1
                    From = c(From, colnames[i])
                    To = c(To, colnames[j])
                    Ct =c(Ct, tmp)
                }
            } 
        }
        g=NULL
        g$From= as.matrix(From)
        g$To = as.matrix(To)
        g$Ct = as.matrix(Ct)
        return(as.data.frame(g))
    }

    #--- Call the function ---#
    aa = dtm2cooccur(dat)
    aa[aa$Ct>2,] # co-occur terms with high frequency

[(back to top)](#table-of-contents)

<div id='ngram'/>
### ---  Create ngrams
    library("RWeka")
    library("tm")


    docs <- c("This is a text.", "This is another one.", "This is fine")
    c2 <- VCorpus(VectorSource(docs))

    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    tdm <- TermDocumentMatrix(c2, control = list(tokenize = BigramTokenizer))

    inspect(tdm[1:5, 1:2])
[(back to top)](#table-of-contents)


<div id='handy'/> 


### -- Some handy functions  
    chartr("iXs", "why", x)     #Translate characters: i -> w, X -> h, s -> y  
    chartr("a-cX", "D-Fw", x)   #a->D, b->E, c->F, x->w  
    tolower(x)  
    toupper(x)

    # --- Small cap --- #
    .simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
    }
    .simpleCap("the quick red fox jumps over the lazy brown dog")
[(back to top)](#table-of-contents)   






### --Other
program='C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe'
junk<-aspell(as.factor("interface"),program=program)


install.packages("Aspell", repos = "http://www.omegahat.org/R", version='3.0')

##### check with Hongxiang about SVD 

td = tempfile()
dir.create(td)
write( c("dog", "cat", "mouse"), file=paste(td, "D1", sep="/"))
write( c("hamster", "mouse", "sushi"), file=paste(td, "D2", sep="/"))
write( c("dog", "monster", "monster"), file=paste(td, "D3", sep="/"))
write( c("dog", "mouse", "dog"), file=paste(td, "D4", sep="/"))



library(tm)
myCorpus <- c("Quick response", 
              "respond quick and was very helpful",
              "A quick reply",
              "The situation was resolved quick and efficient.",
              "fast response",
              "I am happy with how quick it was handled.",
              "Understand my query.",
              "Understand exactly what I needed !!",
              "Understand my complaint right from the start and did not attempt to argue",
              "Very positive and understand",
              "Very understand",
              "Very understand and accommodate.",
              "response quick")
corpus <- Corpus(VectorSource(myCorpus))
myMatrix = TermDocumentMatrix(corpus)

myMatrix = as.matrix(myMatrix)


### R spell check ###

# check_spelling <- function(text) {
# Create a file with on each line one of the words we want to check
text <- gsub("[,.]", "", text)
text <- strsplit(text, " ", fixed=TRUE)[[1]]
filename <- tempfile()
writeLines(text, con = filename);
# Check spelling of file using aspell
result <- aspell(filename, "Rd")
# Extract list of suggestions from result
suggestions <- result$Suggestions
names(suggestions) <- result$Original
unlink(filename)
suggestions
}

text <- "I am text mining a large database to create indicator variables which indicate the occurence of certain phrases in a comments field of an observation. The comments were entered by technicians, so the terms used are always consistent. "
check_spelling(text)


$occurence
[1] "occurrence"   "occurrences"  "occurrence's"



install.packages("KernSmooth")
library(KernSmooth)


library(koRpus)
tagged.text <- treetag("C:\\Treetagger\\INSTALL.txt", treetagger="manual",
                       lang="en", TT.options=c(path="C:\\treetagger", preset="en"))


tagged.results <- treetag(c("run", "ran", "running"), treetagger="manual", format="obj",
                          TT.tknz=FALSE , lang="en",
                          TT.options=list(path="C:/TreeTagger", preset="en"))
tagged.results@TT.res


agged.results <- treetag(c("run", "ran", "running"),  format="obj",
                         TT.tknz=FALSE , lang="en",
                         treetagger="C:/treetagger/cmd/tree-tagger-english")

agged.results <- treetag(c("run", "ran", "running"), treetagger="manual", 
                            lang="en",
                            TT.options=list(path="C:/Users/zhou.x/Desktop/Other/Data Science/TextAnalytics/TreeTagger/bin/", 
                            preset="en"))


library(wordnet)
setDict(pathData = "C:/Program Files (x86)/WordNet/2.1/dict/")

filter <- getTermFilter("ExactMatchFilter", "hot", TRUE)
terms <- getIndexTerms("ADJECTIVE", 1, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "!")
sapply(synsets, getWord)

sapply(related, getWord)

synonyms("what", "NOUN")
synonyms("corrupt", "VERB")
synonyms("CAR", "NOUN")


