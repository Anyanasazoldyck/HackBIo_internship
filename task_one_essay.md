
## Steps in creating calculate_protien_weight_kDa function
The first step in calculating the total weight of protein is the naming format. 
to solve this isue, I creates an `if/else` statment to handle One_letter_code and Three_letter_code naming formats seperatedly. 
The second step is using a `for loop` to automate adding up the weight of each amino acid in the protien name
to a variable called `protein_wt`.
last step is unit transformation to KDa. 
