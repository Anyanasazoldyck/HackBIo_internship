#Stage 0

#task 1
"For the GC% calculation,
make it robust enough to handle nucleotide sequences written in upper and lower case. I.e., 
GCATTTA and gcaTTTA should both return 25%."

GC_percent_counter <- function(read){
  
  #Set a gc_count 
  gc_count <- 0
  
  #loop to get the GC count
  read <- strsplit(read,split = "") [[1]]
  for (base in read){
    if (base=='G'|base=='g'|base=='c'|base=='C'){
        gc_count <- gc_count+1
      }
    GC_percent <- (gc_count/ length(read))*100
  }
  #return
  return(GC_percent)
  
}



#task 2
"Above are the molecular weights of proteins.

Write a function that returns the molecular weight of any protein in KiloDalton
Let the function accept your name as an input by default
if your name or any input contains a non-protein character by default (such as B), return 0 for that value."

reference <- read.csv("Book1.csv")


calculate_protein_weight_kDa <- function(protein,reference){
  
  
  protein_wt <- 0
  
  if (grepl("-", protein)) {
    
    protein_string <- strsplit(protein, split = "-")[[1]]
    
    for (aa in protein_string) {
      aa_wt <- reference$Weight_inDA[reference$three_letter_Code == aa]
      protein_wt <- protein_wt + aa_wt
    }
    
  } else {
    
    protein_string <- strsplit(protein, split = "")[[1]]
    
    for (aa in protein_string) {
      aa_wt <- reference$Weight_inDA[reference$one_letter_Code == aa ]
      protein_wt <- protein_wt + aa_wt
    }
  }
  
  #wt in KD
  protein_wt <- protein_wt/1000
  return(protein_wt)
  
}


