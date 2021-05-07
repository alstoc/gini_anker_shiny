## Demonstration of the importance of reference group

library(ineq)
library(xtable)

# function that calculates shift c (optimization of Gini between 2 groups)
calculate_shift <- function(beta_ref, beta_foc) {
  c_grid1 <- - (beta_foc - beta_ref)
  c_grid2 <- combn(c_grid1,2,FUN = sum)/2
  
  names(c_grid1) <- NULL
  names(c_grid2) <- NULL
  
  sparseGrid <- c(c_grid1,c_grid2)
  c_grid <- unique(sort(sparseGrid))
  
  newbeta_foc <- lapply(c_grid,function(c_grid) beta_foc+c_grid )
  betas_grid <- lapply(newbeta_foc, function(newbeta_foc) {
    cbind(beta1=beta_ref,newbeta2=newbeta_foc,dist=beta_ref-newbeta_foc)
    }
    )
  ind <- which.max(unlist(lapply(
    betas_grid,
    function(x) return(ifelse(test = all(x[,3]==0)
                              ,yes = 0,
                              no = Gini(abs(x[,3])))))))
  shift <- c_grid[ind]
  return(shift)
}

plot_shifted_betas <- function(betas) {
  shift_12 <- calculate_shift(betas[,1], betas[,2])
  shift_13 <- calculate_shift(betas[,1], betas[,3])
  shift_21 <- -shift_12
  shift_23 <- calculate_shift(betas[,2], betas[,3])
  shift_31 <- -shift_13
  shift_32 <- -shift_23

  par(mfrow=c(2,2))
  plot_betas(betas[,1], betas[,2], betas[,3], title="Original coefficients")
  plot_betas(betas[,1], betas[,2]+shift_12, betas[,3]+shift_13, 
             title="Reference group: 1")
  plot_betas(betas[,1]+shift_21, betas[,2], betas[,3]+shift_23, 
             title="Reference group: 2")
  plot_betas(betas[,1]+shift_31, betas[,2]+shift_32, betas[,3], 
             title="Reference group: 3")
}

# calculation of Gini between each pair (could be written better)
calculate_Ginis <- function(betas, reference=1) {
  if (reference==1) {
    shift2 <- calculate_shift(betas[,1], betas[,2])
    shift3 <- calculate_shift(betas[,1], betas[,3])
    gini_12 <- Gini(abs(betas[,1]-(betas[,2]+shift2)))
    gini_13 <- Gini(abs(betas[,1]-(betas[,3]+shift3)))
    gini_23 <- Gini(abs(betas[,2]+shift2-(betas[,3]+shift3)))
    return(c(gini_12, gini_13, gini_23))
  }
  if (reference==2) {
    shift1 <- calculate_shift(betas[,2], betas[,1])
    shift3 <- calculate_shift(betas[,2], betas[,3])
    gini_21 <- Gini(abs(betas[,2]-(betas[,1]+shift1)))
    gini_23 <- Gini(abs(betas[,2]-(betas[,3]+shift3)))
    gini_13 <- Gini(abs(betas[,1]+shift1-(betas[,3]+shift3)))
    return(c(gini_21, gini_13, gini_23))
  }
  if (reference==3) {
    shift1 <- calculate_shift(betas[,3], betas[,1])
    shift2 <- calculate_shift(betas[,3], betas[,2])
    gini_31 <- Gini(abs(betas[,3]-(betas[,1]+shift1)))
    gini_32 <- Gini(abs(betas[,3]-(betas[,2]+shift2)))
    gini_12 <- Gini(abs(betas[,1]+shift1-(betas[,2]+shift2)))
    return(c(gini_12, gini_31, gini_32))
  }
}

create_table <- function(betas) {
  ginis_table <- rbind(calculate_Ginis(betas, reference=1), calculate_Ginis(betas, reference=2),
                       calculate_Ginis(betas, reference=3))
  ginis_table[is.na(ginis_table)] <- 0
  ginis_df <- data.frame(one=ginis_table[,1], two=ginis_table[,2], three=ginis_table[,3], 
                         sum=rowSums(ginis_table))
  rownames(ginis_df) <- c("ref1", "ref2", "ref3")
  colnames(ginis_df) <- c("1vs2", "1vs3", "2vs3", "Ginis_sum")
  return(ginis_df)
}

###########################
# Examples #
# 1
#beta1 <- c(1, 2, 3, 4, 4, 3, 2, 1)
#beta2 <- c(1, 2, 5, 4, 4, 3, 2, 1)
#beta3 <- c(1, 2, 3, 4, 4, 3, 2, 1)


