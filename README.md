# HackBIo_internship
# stage 1 assay 

## Steps in creating calculate_protien_weight_kDa function
The first step in calculating the total weight of protein is the naming format. 
To solve this issue, I created an `if/else` statement to handle One_letter_code and Three_letter_code naming formats separately. 
The second step is using a `for loop` to automate adding up the weight of each amino acid in the protein name
to a variable called `protein_wt`.
last step is unit transformation to KDa.


# stage 2 tutorial
## Exploring microbacterial sensetivity test. 
Data downloaded from ("https://raw.githubusercontent.com/HackBio-Internship/2025_project_collection/refs/heads/main/bacteria.csv").
This is a csv file containing microbacterial testing result 