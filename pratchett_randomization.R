# parameters

# remove this line if you want different random results each time you run. Warning: will be unreproducible. 
set.seed(413)
pagecounts <- c(390, 355, 399)
booknames <- c("lost continent", "guards guards", "unseen")
breaks <- c(.25, .5, .75, 1)
# number of pages to sample from each book
samplesize=12



takeSample<-function(booklength, breaks, samplesize) {
  lastpage <- 0
  bookpages<-numeric(0)
  for(ppbreak in breaks) {
    firstpage <- lastpage + 1
    lastpage <- round(booklength * ppbreak)
    print(paste0("Checking pages ", 
                 firstpage, "-", lastpage,
                 " of '", bookname, "'"))
    bookpages <- c(bookpages, sample(x=firstpage:lastpage, size=round(samplesize/length(breaks)), replace=F))
  }
  return(bookpages)
}

booksamples<-list()
for(i in 1:length(pagecounts)) {
  booklength <- pagecounts[i]
  bookname <- booknames[i]
  booksamples[[bookname]]<-sample(takeSample(booklength, breaks, samplesize))
}
print(booksamples)
