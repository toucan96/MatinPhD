aa_translation <- function(sequence, frame=1, output_style="oneletter") {
  dic <- data.frame(
    codon= c("AAT","AAC","AAA","AAG","CAT","CAC","CAA","CAG",
             "GAT","GAC","GAA","GAC","TAT","TAC","TAA","TAG",
             "ACT","ACC","ACA","ACG","CAT","CAC","CAA","CAG",
             "GCT","GCC","GCA","GCG","TCT","TCC","TCA","TCG",
             "AGT","AGC","AGA","AGG","CGT","CGC","CGA","CGG",
             "GGT","GGC","GGA","GGG","TGT","TGC","TGA","TGG",
             "ATT","ATC","ATA","ATG","CTT","CTC","CTA","CTG",
             "GTT","GTC","GTA","GTG","TTT","TTC","TTA","TTG"),
    oneletter=c("N","N","K","K","H","H","Q","Q","D","D","E","E","Y","Y","*","*",
                "T","T","T","T","P","P","P","P","A","A","A","A","S","S","S","S",
                "S","S","R","R","R","R","R","R","G","G","G","G","C","C","*","W",
                "I","I","I","M","L","L","L","L","V","V","V","V","F","F","L","L"),
    threeletter=c("Asn","Asn","Lys","Lys","His","His","Gln","Gln","Asp","Asp","Glu","Glu","Tyr","Tyr","Stp","Stp",
                  "Thr","Thr","Thr","Thr","Pro","Pro","Pro","Pro","Ala","Ala","Ala","Ala","Ser","Ser","Ser","Ser",
                  "Ser","Ser","Arg","Arg","Arg","Arg","Arg","Arg","Gly","Gly","Gly","Gly","Cys","Cys","Stp","Trp",
                  "Iso","Iso","Iso","Met","Leu","Leu","Leu","Leu","Val","Val","Val","Val","Phe","Phe","Leu","Leu")
  )
  
  orf <- gsub(paste0("^.{",frame-1,"}"), "", toupper(sequence))
  codons <- unlist(strsplit(gsub("(\\S{3})", "\\1 ", orf), " "))
  peptide <- c()
  for(i in 1:length(codons)) {
    peptide<- append(peptide, dic[dic$codon==codons[i], output_style])
  }
  peptide
  gsub("NA$", "", paste0(peptide, collapse = ""))
} 
