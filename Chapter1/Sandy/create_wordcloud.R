#' Create WordCloud
#' @examples
#' create_wordcloud(DTMInput = dtm_PreCalm, sparceFactor = 0.999,
#'              OutFolder = "/home/marcela/Coding/EWE-reporting/Sandy/Outputs", 
#'              OutFile = "/WordcloudSandyPreCalm.png", background = "Black", ncolors = 8,
#'              palette = "Blues")
#' 
create_wordcloud = function(DTMInput, sparceFactor = 0.9, OutFolder, OutFile, 
                        background = "transparent", ncolors = 6, palette = "Blues"){
  DTMInput <- removeSparseTerms(DTMInput, sparceFactor)
  # define tdm as matrix
  matrix_Input = as.matrix(DTMInput)
  # get word counts in decreasing order
  word_freqs = sort(colSums(matrix_Input), decreasing=TRUE)
  # create a data frame with words and their frequencies
  df.Input = data.frame(word=names(word_freqs), freq=word_freqs)

  # Creating wordcloud
  png(filename = paste(OutFolder,OutFile, sep = ""), width=3, height=3, 
      units="in", res=300, bg = background) 
  # bg can be "Black"
  set.seed(1234)
  wordcloud(df.Input$word, df.Input$freq, scale=c(3,.8), max.words=30, 
          random.order=FALSE, rot.per=.15, 
          colors=brewer.pal(ncolors, palette), font = 1, family = "serif") 
  # change brewer.pal from "Dark2" to Blues if want just play with intensity
  dev.off()
}



