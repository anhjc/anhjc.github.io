# anhjc.github.io

HW8: LDAvis/README.md • Hsiang-Ju/LDAvis

R package for interactive topic model visualization.

![LDAvis icon](https://github.com/anhjc/anhjc.github.io/blob/master/pic1.jpg)


### Installing the package

* Stable version on CRAN:

```s
install.packages("LDAvis")
```

* Development version on GitHub (with [devtools](http://cran.r-project.org/web/packages/devtools/index.html)):

```s
devtools::install_github("cpsievert/LDAvis")
```
### Pre-processing
In summary, we:

Tokenise the data into unigrams</br>
Remove stop words from the dataset</br>
Transform the unigrams to vector form by using the bag of words model</br>

* passing Full Text to variable review
```s
review<-precorpus$reviewText 
```

* clean corpus
```s
stop_words <- c(stop_words, "said", "the", "also", "say", "just", "like","for", 
                "us", "can", "may", "now", "year", "according", "mr")
review <- gsub("'", "", review) # remove apostrophes
review <- gsub("[[:punct:]]", " ", review)  # replace punctuation with space
review <- gsub("[[:cntrl:]]", " ", review)  # replace control characters with space
review <- gsub("^[[:space:]]+", "", review) # remove whitespace at beginning of documents
review <- gsub("[[:space:]]+$", "", review) # remove whitespace at end of documents
review <- gsub("[^a-zA-Z -]", " ", review) # allows only letters
review <- tolower(review)  # force to lowercase

review <- review[review != ""]
```

* tokenize on space and output as a list
```s
doc.list <- strsplit(review, "[[:space:]]+")
```

* compute the table of terms
```s
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
```

* remove terms that are stop words or occur fewer than 5 times
```s
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
term.table <- term.table[names(term.table) != ""]
vocab <- names(term.table)
```

* put the documents into the format required by the lda package
```s
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
```

* Compute some statistics related to the data set
```s
D <- length(documents)  # number of documents (1)
W <- length(vocab)  # number of terms in the vocab (1741)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (56196)
term.frequency <- as.integer(term.table) 
```

* MCMC and model tuning parameters
```s
K <- 15  #no. of topic
G <- 3000 #run 3000 times
alpha <- 0.02 
eta <- 0.02
```

* Fit the model
```s
library(lda) 
set.seed(357) 
t1 <- Sys.time()  #
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()  #
## display runtime
t2 - t1           #


theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

review_for_LDA <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
```


### Sharing a Visualization

* Dataset: Amazon Reviews (Amazon Instant Video category) 
[https://anhjc.github.io/#topic=1&lambda=1&term=]
(https://anhjc.github.io/#topic=1&lambda=1&term=)



### Running LDA on the Data
A 15 topics model
<table>
   <th>Topic #</th>
   <th>Topic-Words Distribution</th>
   <tr>
      <td>1</td>
      <td>people, live, time,things, place characters, sense, work</td>
   </tr>
   <tr>
      <td>2</td>
      <td>pretty, boring, good, bad</td>
   </tr>
   <tr>
      <td>3</td>
      <td>love, season, actor, drama, action, excellent, joy, fun</td>
   </tr>
   <tr>
      <td>4</td>
      <td>horror, story, plot, special</td>
   </tr>
   <tr>
      <td>5</td>
      <td>season, show, episode, series, final, ending</td>
   </tr>
   <tr>
      <td>6</td>
      <td>young, play, family, wife, daughter, husband, relationship</td>
   </tr>
   <tr>
      <td>7</td>
      <td>amazon, prime, enjoy, buy</td>
   </tr>
   <tr>
      <td>8</td>
      <td>funny, comedy, laugh, jokes, loud</td>
   </tr>
   <tr>
      <td>9</td>
      <td>police, murder, crimes, drama</td>
   </tr>
   <tr>
      <td>10</td>
      <td>love, history, acting, american, war, excellent</td>
   </tr>
   <tr>
      <td>11</td>
      <td>fun,show, people, love, good</td>
   </tr>
   <tr>
      <td>12</td>
      <td>series, fi, sci, science</td>
   </tr>
   <tr>
      <td>13</td>
      <td>vampire, spy, love, shuck secret, killer, relationship</td>
   </tr>
   <tr>
      <td>14</td>
      <td>kids, children, family, young, son, cute</td>
   </tr>
   <tr>
      <td>15</td>
      <td>dvd, sherlock, watson, musical, music</td>
   </tr>
</table>

### Analysing the generated topics
Topic #2:
<b>pretty, boring, good, bad</b></br>
From the above distribution, we can infer that the topic described by these words is most likely something relating to people's comments about a movie. Words like pretty, boring, and good all refer to attributes of comment words. 

Topic #6:
<b>young, play, family, wife, daughter, husband, relationship</b></br>
From the above distribution, we can infer that the topic described by these words is most likely something relating to family movies. Words like family, wife anddaughter all refer to attributes of family words.
