library(XLConnect)
library(tm)


wb = loadWorkbook("B:\\XiaojieZhou\\2014\\GCR Survery\\From Others\\Global English CX Results to Jun 1.xlsx")
cmm = readWorksheet(wb, sheet = "report1433191502074", header = TRUE,startCol=13,endCol=13)
# cmm = readWorksheet(wb, sheet = "report1433191502074", header = TRUE,startCol=13,endCol=13, stringsAsFactors=False)

cmm=cmm[!is.na( cmm$CX.Service.Comments),1]

#--- Add replace words according to a dictionary
#--- Replace didn't by 'did not', etc...



#--- replace '/' with 'space' and '...' with '.'
cmm = gsub("[^[:alnum:] ]",' ', cmm)


Comments = head(cmm, n=20L)
Comments=cmm


txt<-VectorSource(Comments)
txt.corpus<-Corpus(txt)
txt.corpus<-tm_map(txt.corpus,tolower)
# txt.corpus<-tm_map(txt.corpus,removePunctuation)
txt.corpus<-tm_map(txt.corpus,removeNumbers)
#txt.corpus<-tm_map(txt.corpus,removeWords,stopwords("english"))
txt.corpus <- tm_map(txt.corpus, PlainTextDocument)
tdm<- TermDocumentMatrix(txt.corpus)
temp <- inspect(tdm)
FreqMat <- data.frame(terms = rownames(temp), freq = rowSums(temp))
row.names(FreqMat) <- NULL
FreqMat<-FreqMat[order(FreqMat$freq,decreasing=T),]
FreqMat$terms=gsub("(<)[a-zA-Z0-9\\+]*(>)",'',FreqMat$terms)



terms<-as.character(FreqMat$terms)
freq<-FreqMat$freq
terms2test<- terms[FreqMat$freq<3]
terms_right<- terms[FreqMat$freq>=10]
class(terms2test)
class(terms_right)

###obtain the wrong words and dictionary suggestion
# program='C:\\Users\\shi.h.4\\AppData\\Roaming\\SepllCheckers\\Aspell\\bin\\aspell.exe'

program='C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe'

result<-aspell(as.factor(terms2test),program=program)
suggestion<-sapply(result$Suggestions, function(x) unlist(x)[1])
non.null.list <- lapply(suggestion,function(x)ifelse(is.null(x), 'NA', x))
suggestion<-unlist(non.null.list)
WrongTerms<-as.data.frame(cbind(result$Original,suggestion))
WrongTerms<-WrongTerms[!WrongTerms$V1== tolower(as.character(WrongTerms$suggestion)),]

DisMat<-adist(terms_right,WrongTerms$V1)
min_dist_index<-apply(DisMat,2, function(x) which(x==min(x))[1] )
min_dist<-apply(DisMat,2, function(x) x=min(x))
CorpusReco<-tolower(as.character(terms_right[min_dist_index]))

df2<-cbind(WrongTerms, CorpusReco ,min_dist)
names(df2) = c("Misspelling", "DictReco", "CorpusReco", "Dist2Corpus")
head(df2, n=12)

df2$CombinedReco=as.character(df2$DictReco)
df2$CombinedReco[df2$Dist2Corpus ==1]=as.character(df2$CorpusReco[df2$Dist2Corpus ==1])


df2$ConsrvReco= (as.character(df2$DictReco) == as.character(df2$CorpusReco))


write.csv(df2, "C:\\Users\\zhou.x\\Desktop\\all corrected.csv")

