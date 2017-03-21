library(data.table)
set.seed(413)
booknames <- c("lost continent", "guards guards", "unseen")
bookcounts <- list(
  c(1,4,3,2,5,4,6,5,1,2,4,3),
  c(2,5,6,10,2,4,3,9,5,1,2,3),
  c(1,2,3,4,5,6,7,8,9,10,11,12)
)
nbooks<-length(booknames)
results <- data.table(name=as.character(character()), mean=as.numeric(numeric()), sd=as.numeric(numeric()), lower=as.numeric(numeric()), upper=as.numeric(numeric()))
# number of pages to sample from each book
samplesize=12

getCI <- function(m, sd, p) {
  err <- sd * pnorm(1.96)
  ci <- c(m-sd, m+sd)
  return(ci)
}

jokeDescriptives <- function(countlist, namelist) {
  for(i in 1:length(countlist)) {
    name<-booknames[i]
    jokes<-countlist[[i]]
    mj<-mean(jokes)
    sj<-sd(jokes)
    nj<-length(jokes)
    semj<-sj/sqrt(nj)
    cij<-getCI(mj, semj, .80)
    results<-rbind(results, list(name, mj, sj, cij[1], cij[2]))
  }
  return(results)
}

results<-jokeDescriptives(bookcounts, booknames)
results
