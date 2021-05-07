##########
# Reference, sequential and simultaneous optimization method toy examples (3 groups)
##########
library(mgcv)

# input: matrix of shifts; reference: which group will not be shifted
# output: matrix of shifts with one column of zeros
standardize_shifts <- function(matrix, reference = 3) {
  for (i in 1:nrow(matrix)) {
    if (matrix[i,reference]!=0) {
      matrix[i,] <- matrix[i,]-matrix[i,reference]
    }
  }
  return(matrix)
}

### simultaneous optimization (finding all local maxima)
# output: list where first item is matrix of shifts, second item corresponding Gini sums (in increasing order)
all_maxima <- function(beta1, beta2, beta3, reference = 3) {
  # all c1, c2 corresponding to local maxima (unordered)
  c_combs <- potential_maxima(beta1, beta2, beta3)
  
  # calculate Gini sum for each potential maximum
  Ginis <- rep(0, dim(c_combs)[1])
  for (i in 1:dim(c_combs)[1]) {
    Ginis[i] <- Gini_sum(beta1 = beta3, 
                         beta2 = beta1, 
                         beta3 = beta2, 
                         c2 = c_combs[i,1], c3 = c_combs[i,2])
  }
  # ordered Gini sums
  Ginis_ord <- Ginis[order(Ginis, decreasing=TRUE)]
  
  # corresponding shifts c1, c2 (default: c3 = 0)
  shifts_ord <- c_combs[order(Ginis, decreasing=TRUE),]
  
  shifts_all <- cbind(shifts_ord, 0)
  shifts_all <- standardize_shifts(shifts_all, reference = reference)
  shifts_all_Ginis <- cbind(shifts_all, Ginis_ord)
  return(list("shifts"=shifts_all, "Ginis"=Ginis_ord))
}

### reference
# output: matrix of shifts, each row corresponding to reference 1, 2, 3 respectively
reference_method_maxima <- function(beta1, beta2, beta3, reference = 3) {
  betas <- cbind(beta1, beta2, beta3)
  shifts_ref <- matrix(0, nrow=3, ncol=3)
  for (i in 1:3) {
    for (j in 1:3) {
      if (i==j) {
        shifts_ref[i,j] <- 0
      } else {
        shifts_ref[i,j] <- calculate_shift(betas[,i], betas[,j])
      }
    }
  }
 # standardize the shifts, s.t. c_reference = 0
  shifts_ref <- standardize_shifts(shifts_ref, reference = reference)
  return(shifts_ref)
}

### sequential
# output: matrix of shifts for the sequential methods
sequential_method_maxima <- function(beta1, beta2, beta3, reference = 3) {
  shifts_seq <- matrix(0, nrow=6, ncol=3)
  for (i in 1:3) {
    pairs <- combn(c(1,2,3), 2)
    Ms <- c(3, 2, 1)
    pair <- pairs[,i]
    k <- pair[1]
    l <- pair[2]
    m <- Ms[i]
    first_shift <- calculate_shift(betas[,k], betas[,l])
    
    shifts_seq[(2*i-1):(2*i), k] <- c(0,0)
    shifts_seq[(2*i-1):(2*i), l] <- rep(first_shift, 2)
    
    shift_1 <- calculate_shift(betas[,k], betas[,m])
    shift_2 <- calculate_shift(betas[,l]+first_shift, betas[,m])
    shifts_seq[2*i-1, m] <- shift_1
    shifts_seq[2*i, m] <- shift_2  
  }
  shifts_seq <- standardize_shifts(shifts_seq, reference = reference)
  return(shifts_seq)
}

# Number maxima for reference and sequential method
match_shifts <- function(shifts_matrix, shifts_all) {
  ranks <- rep(0, nrow(shifts_matrix))
  for (i in 1:nrow(shifts_all)) {
    ranks_temp <- i*as.numeric(apply(shifts_matrix, 1, function(x) identical(x, shifts_all[i,])))
    ranks <- ranks + ranks_temp
  }
  return(ranks)
}

### Function outputting the three desired dataframes
all_methods_maxima <- function(beta1, beta2, beta3, reference = 3) {
  betas <<- cbind(beta1, beta2, beta3)
  all <- all_maxima(beta1 = beta1, 
                    beta2 = beta2, 
                    beta3 = beta3, 
                    reference = reference)
  shifts_all <- all[[1]]
  Ginis_ord <- all[[2]]
  
  shifts_ref <- reference_method_maxima(beta1 = beta1, 
                                        beta2 = beta2, 
                                        beta3 = beta3, 
                                        reference = reference)
  shifts_seq <- sequential_method_maxima(beta1 = beta1, 
                                         beta2 = beta2, 
                                         beta3 = beta3, 
                                         reference = reference)
  
  ranks_seq <- match_shifts(shifts_seq, shifts_all)
  ranks_ref <- match_shifts(shifts_ref, shifts_all)
  
  # create dataframes
  colNames_ref <- c("Rank", "Reference", "c1", "c2", "c3", "Gini_Sum")
  df_ref <- data.frame(ranks_ref, c(1:3), shifts_ref, Ginis_ord[ranks_ref])
  colnames(df_ref) <- colNames_ref
  
  colNames_seq <- c("Rank", "Pair", "Reference", "c1", "c2", "c3", "Gini_Sum")
  pairs <- c(rep("(1,2)", 2), rep("(1,3)", 2), rep("(2,3)", 2))
  refs <- c(1, 2, 1, 3, 2, 3)
  df_seq <- data.frame(ranks_seq, pairs, refs, shifts_seq, Ginis_ord[ranks_seq])
  colnames(df_seq) <- colNames_seq
  
  colNames_all <- c("Rank", "c1", "c2", "c3", "Gini_Sum")
  df_all <- data.frame(c(1:nrow(shifts_all)), shifts_all, Ginis_ord )
  colnames(df_all) <- colNames_all
  rownames(df_all) <- NULL
  
  return(list("Reference" = df_ref, "Sequential" = df_seq, "All" = df_all))
}


### Example
# set beta coefficients
#beta1 <- c(1, 2, 3, 4, 4, 3, 2, 1)
#beta2 <- c(1, 2, 3, 5, 5, 4, 3, 2)
#beta3 <- c(2, 3, 4, 5, 5, 3, 2, 1)
#beta4 <- c(3, 1, 2, 1, 3, 1, 5, 5)
#betas <- cbind(beta1, beta2, beta3)
#save(betas,file="betas.Rda")
#write.csv(betas, file = "data/betas.csv")

# Plot coefficients
#plot_betas(test, shifts = c(0,0,0), type="b", cex.main=1.5, 
#           title="Original coefficients", cex.axis=1.5, cex.lab=1.5, cex.legend=1.5)

# Find all maxima and corresponding shifts for each method
#all_methods_maxima(beta1, beta2, beta3, reference = 1)
