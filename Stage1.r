#Stage 0
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



