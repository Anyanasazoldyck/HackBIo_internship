#Stage Zeo task is as follows 
"Write a simple R script for printing the names, affiliation, 
and the name of the genes you love most, and the name of the organism bearing the gene.
The final output should be something like, “Hi, my name is Adewale Ogunleye, a researcher at
the University of Tübingen. My favorite gene is lexA in Escherichia coli.”"




print( "Hi, my name is Adewale Ogunleye, a researcher at the University of Tübingen. My favorite gene is lexA in Escherichia coli.")
  

IntroduceYourselfe <- function(name="", affiliation="", favorite_gene="", organism_bearing_that_gene="" ){
  name <-
  affiliation <-
  favorite_gene <-
  organism_bearing_that_gene <-
  
    print(sprintf(
      "Hi, my name is %s, a %s. My favorite gene is %s in %s.",
      name, affiliation, favorite_gene, organism_bearing_that_gene))
  
  
}



name <- "anya"
affiliation <- "Pharmacist"
favorite_gene <-"MYC"
organism_bearing_that_gene <- "hs"


IntroduceYourselfe(name,affiliation ,favorite_gene,organism_bearing_that_gene)

