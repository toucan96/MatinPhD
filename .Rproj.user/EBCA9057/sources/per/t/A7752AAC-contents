rComplement <- function(sequence, species = "DNA") {
  seq<-unlist(strsplit(sequence, ""))

  A <- which(seq %in% "A")
  a <- which(seq %in% "a")
  C <- which(seq %in% "C")
  c <- which(seq %in% "c")
  G <- which(seq %in% "G")
  g <- which(seq %in% "g")
  T. <- which(seq %in% "T")
  t <- which(seq %in% "t")
  U <- which(seq %in% "U")
  u <- which(seq %in% "u")

  if(species == "DNA")  {
    seq[A] <- "T"
  }
  if(species == "RNA") {
    seq[A] <- "U"
  }
  if(species == "DNA")  {
    seq[a] <- "t"
  }
  if(species == "RNA") {
    seq[a] <- "u"
  }
    seq[C] <- "G"
    seq[c] <- "g"
    seq[G] <- "C"
    seq[g] <- "c"
    seq[T.] <- "A"
    seq[t] <- "a"
    seq[U] <- "A"
    seq[u] <- "a"

    comp <- paste(seq, collapse="")
    rev.comp <- paste(rev(seq), collapse="")
  list(
    input_sequence = sequence,
    complement = comp,
    reverse_complement = rev.comp
  )
}
