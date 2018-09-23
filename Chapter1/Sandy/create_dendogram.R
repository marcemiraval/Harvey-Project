#' Create Dendogram
#' @examples
#' create_dendogram(DTMInput = dtm_PreCalm, sparceFactor = 0.999,
#'              OutFolder = "/home/marcela/Coding/EWE-reporting/Sandy/Outputs", 
#'              OutFile = "/WordcloudSandyPreCalm.png", background = "Black", ncolors = 8,
#'              palette = "Blues")
#' 
create_dendogram = function(DTMInput, sparceFactor = 0.9, nclusters =3, OutFolder,
                            OutFile, palette = "Blues"){
  DTMInput <- removeSparseTerms(DTMInput, sparceFactor)
  matrix_Input = as.matrix(DTMInput) #Defining TermDocumentMatrix as matrix
  word_freqs = sort(colSums(matrix_Input), decreasing=TRUE)
  
  cos_dist <- cosine(matrix_Input) # calculate cosine metric
  cos_dist <- as.dist(1- cos_dist) # convert to dissimilarity distances
  hc <-hclust(cos_dist)

  # prepare dendogram
  dendro <- dendro_data(hc, type="rectangle") # convert for ggplot
  cutForCluster    <- cutree(hc, k= nclusters)
  clust.df <- data.frame(label=names(cutForCluster), cluster=factor(cutForCluster), 
                               freq=word_freqs)
  # dendr[["labels"]] has the labels, merge with clust.df based on label column
  dendro[["labels"]] <- merge(dendro[["labels"]], clust.df, by="label")
  
  # plot the dendrogram; note use of color=cluster in geom_text(...)
  Dendogram <- ggplot() + 
    geom_segment(data=segment(dendro), aes(x=x, y=y, xend=xend, yend=yend),
                 size = 0.4, colour = " Dark gray") +
    geom_text(data=label(dendro), aes(x, y, label=label, hjust=0, color=cluster), 
              size=7) + coord_flip() + scale_y_reverse(expand=c(0.5, 0)) +
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill=NULL),
          panel.grid=element_blank())+
    scale_color_manual(values = brewer.pal(nclusters, palette),
                       guide = 'none')
  Dendogram <- Dendogram + theme_void()

  ggsave(filename = OutFile, plot = Dendogram, path = OutFolder, width=5, 
         height=10, units="in", dpi = 300, bg = "White")
  # colors defined by http://tools.medialab.sciences-po.fr/iwanthue/
}



