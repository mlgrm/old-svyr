#' create a persistent list of pronounceable bubble-babble style words
#'
#' babble returns a vector of n words of at least syllables syllables that are
#' always the same for a given seed value.  the state of the random number
#' generator is preserved.

babble <- function(n,syllables=1,seed=getOption("babble.seed",1)){
  rstate <- .Random.seed
  vowels <- c("a","e","i","o","u","y")
  consonants <- letters[!(letters %in% vowels)]
  nwords <- function(s)length(consonants)^(s+1)*length(vowels)^s
  if(n>nwords(syllables))
    return(c(babble(nwords(syllables)),
             babble(n-nwords(syllables),syllables+1)))
  set.seed(seed)
  ind <- sample(nwords(syllables),n)
  .Random.seed <<- rstate

  i <- ind %% length(consonants)
  word <- consonants[i+1]
  ind <- (ind-i)/length(consonants)

  for(s in 1:syllables){
    i <- ind %% length(vowels)
    word <- paste0(word,vowels[i+1])
    ind <- (ind-i)/length(vowels)

    i <- ind %% length(consonants)
    word <- paste0(word,consonants[i+1])
    ind <- (ind-i)/length(consonants)
  }
  word
}
