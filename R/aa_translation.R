aa_translation <- function(sequence, frame=1, output_style="one-letter") {
  dic <- c(N = "AAT", N = "AAC", K = "AAA", K = "AAG",
           H = "CAT", H = "CAC", Q = "CAA", Q = "CAG",
           D = "GAT", D = "GAC", E = "GAA", E= "GAC",
           Y = "TAT", Y = "TAC", `*` = "TAA", `*` = "TAG",
           `T`= "ACT", `T`= "ACC", `T`= "ACA", `T`= "ACG",
           P = "CAT", P = "CAC", P = "CAA", P = "CAG",
           A = "GCT", A = "GCC", A = "GCA", A = "GCG",
           S = "TCT", S = "TCC", S = "TCA", S = "TCG",
           S = "AGT", S = "AGC", R = "AGA", R = "AGG",
           R = "CGT", R = "CGC", R = "CGA", R = "CGG",
           G = "GGT", G = "GGC", G = "GGA", G = "GGG",
           C = "TGT", C = "TGC", `*`= "TGA", W = "TGG",
           I = "ATT", I = "ATC", I = "ATA", M = "ATG",
           L = "CTT", L = "CTC", L = "CTA", L = "CTG",
           V = "GTT", V = "GTC", V = "GTA", V = "GTG",
           `F` = "TTT", `F` = "TTC", L = "TTA", L = "TTG")
  
  orf <- gsub(paste0("^.{",frame-1,"}"), "", toupper(sequence))
  codons <- unlist(strsplit(gsub("(\\S{3})", "\\1 ", orf), " "))
  peptide <- c()
  for(i in 1:length(codons)) {
    peptide<- append(peptide,
                     names(dic)[(match(codons[i], dic))]
    )
  }
  gsub("NA$", "", paste0(peptide, collapse = ""))
} 