library(ineq)
library(tidyr)
library(dplyr)

Gini_index <- function(beta1, beta2) {
  if (sum(beta1 == beta2) == length(beta1)) {
    return(0)
  } else {
    Gini(abs(beta1-beta2))
  }
}

reset_betas <- function(example=4) {
  if (example==1){
    # 1
    beta1 <- c(1, 2, 3, 4, 4, 3, 2, 1)
    beta2 <- c(1, 2, 5, 4, 4, 3, 2, 1)
    beta3 <- c(1, 2, 3, 4, 4, 3, 2, 1)
  }
  if (example==2) {
    # 2
    beta1 <- c(1, 2, 3, 4, 4, 3, 2, 1)
    beta2 <- c(1, 2, 5, 4, 4, 3, 2, 1)
    beta3 <- c(1, 2, 3, 4, 4, 5, 2, 1)
  }
  
  if (example==3) {
    # 3
    beta1 <- c(1, 2, 3, 4, 4, 3, 2, 1)
    beta2 <- c(1, 2, 5, 4, 4, 3, 2, 1)
    beta3 <- c(1, 2, 1, 4, 4, 3, 2, 1)
  }
  if (example == 4) {
    # 4
    beta1 <- c(1, 2, 3, 4, 4, 3, 2, 1)
    beta2 <- c(1, 2, 3, 5, 5, 4, 3, 2)
    beta3 <- c(2, 3, 4, 5, 5, 3, 2, 1)
  }
  return(cbind(beta1, beta2, beta3))
}


Gini_sum <- function(beta1, beta2, beta3, c2, c3) {
  G12 <- Gini(abs(beta1-(beta2+c2)))
  G13 <- Gini(abs(beta1-(beta3+c3)))
  G23 <- Gini(abs(beta2+c2-(beta3+c3)))
  return(G12 + G13 + G23)
}

# function that plots original beta coefficinets
# betas is a matrix of coefficients with columns as groups and rows as items
plot_betas <- function(betas, shifts = c(0,0,0), point_types=c(1, 4, 2), 
                       point_colors=c(1, 2, 3), type="b", title="",
                       cex=1, cex.main = 1.5, cex.axis=1.5, cex.lab=1.5,
                       cex.legend=1) {
  shifted_betas <- t(apply(betas, 1, function(x) x + shifts))
  for (i in 1:3) {
    if (i==1) {
      plot(shifted_betas[,i], 
           xlim = c(1,8), 
           ylim = c(min(shifted_betas), max(shifted_betas)), 
           pch=point_types[i], 
           type=type, 
           col=point_colors[i],
           ylab="Difficulty", 
           xlab="Item", cex=cex, 
           main=title, 
           cex.main=cex.main,
           cex.axis=cex.axis, 
           cex.lab=cex.lab)
    }
    else {
      lines(shifted_betas[,i], 
            pch=point_types[i], 
            type=type, 
            col=point_colors[i])
    }
  }
  legend("topright", 
         legend=c(1, 2, 3), 
         pch=point_types, 
         col=point_colors, 
         cex=cex.legend)
}

# Same as plot_betas but with ggplot2 (Alain Stocker)
ggplot_betas <- function(betas, 
                         shifts = NA, 
                         title = NULL) {
  
  shifted_betas <- t(apply(betas, 1, function(x) x + unlist(shifts))) %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), 
                 names_to = "variable", 
                 values_to = "difficulty") %>% 
    mutate(item = rep(1:nrow(betas), each = ncol(betas)))
  
  shifted_betas$variable <- shifted_betas$variable %>% 
    as.factor()
  
  shifted_betas$item <- shifted_betas$item %>% 
    as.factor()
  
  beta_graph <- 
    ggplot(shifted_betas, aes(x = item, 
                              y = difficulty,
                              group = variable, 
                              colour = variable,
                              shape = variable,
                              linetype = variable)) +
    geom_line(size = 1.1) +
    geom_point(size = 3) +
    labs(
      title = title,
      x = "Item",
      y = "Difficulty",
      shape = "Beta",
      linetype = "Beta",
      colour = "Beta"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 26),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom")
  
  return(beta_graph)

}

calculate_surface <- function(beta1, beta2, beta3, 
                              reference=1, j.length=100, example_no=NA) {
  if (!(is.na(example_no))) {
    betas <- reset_betas(example_no)
    beta1 <- betas[,1]
    beta2 <- betas[,2]
    beta3 <- betas[,3]
  }
  if (reference == 1) {
    beta1 <- beta1
    beta2 <- beta2
    beta3 <- beta3
  }
  if (reference == 2) {
    b1 <- beta1
    b2 <- beta2
    b3 <- beta3
    beta1 <- b2
    beta2 <- b1
  }
  if (reference == 3) {
    b1 <- beta1
    b2 <- beta2
    b3 <- beta3
    beta2 <- b1
    beta3 <- b2
    beta1 <- b3
  }
  
  c_grid1 <- seq(from = (min(beta1)-max(beta2)),
                 to = max(beta1) - min(beta2), length.out=j.length)
  c_grid2 <- seq(from = (min(beta1)-max(beta3)),
                 to = max(beta1) - min(beta3), length.out=j.length)
  
  grid <- as.matrix(expand.grid(c_grid1, c_grid2))
  
  surf <- apply(grid, 1, function(x) Gini_sum(beta1, beta2, beta3, x[1], x[2]))
  return(list(cbind(c_grid1, c_grid2), surf))
}

## plots
plot3d_Gini <- function(beta1, beta2, beta3, reference=1, j.length=100, title="", 
                        xlim=c(-4,4), ylim=c(-4,4), example_no=NA) {
  if (reference == 1) {
    xlab="c2"
    ylab="c3"
  }
  if (reference == 2) {
    xlab="c1"
    ylab="c3"
  }
  if (reference == 3) {
    xlab="c1"
    ylab="c2"
  }
  
  surf_list <- calculate_surface(beta1,  beta2, beta3, reference=reference, 
                                 j.length=j.length, example_no=example_no)
  c_grid1 <- surf_list[[1]][,1]
  c_grid2 <- surf_list[[1]][,2]
  surf <- surf_list[[2]]
  
  grid <- as.matrix(expand.grid(c_grid1, c_grid2))
  z <- matrix(surf, nrow=j.length, ncol=j.length)
  pmat <- persp(x=c_grid1, y=c_grid2,z=z, theta = 25, phi = 25, xlab=xlab, ylab=ylab,
                zlab="Gini sum", ticktype = "detailed", main=title, xlim=xlim, ylim=ylim)
  ind <- which.max(surf)
  points(trans3d(grid[ind,1], grid[ind,2], surf[ind], pmat), pch=16, col="red")
}

get_maximum_coordinates <- function(beta1, beta2, beta3, 
                                    reference=1, j.length=100, 
                                    points_no=3, example_no=NA) {
  surf_list <- calculate_surface(beta1,  beta2, beta3, reference=reference, 
                                 j.length=j.length, example_no=example_no)
  c_grid1 <- surf_list[[1]][,1]
  c_grid2 <- surf_list[[1]][,2]
  surf <- surf_list[[2]]
  
  grid <- as.matrix(expand.grid(c_grid1, c_grid2))
  ind <- which.max(surf)
  ord_index <- order(surf, decreasing=TRUE)[1:points_no]
  first_three <- data.frame(grid[ord_index,1], grid[ord_index,2], surf[ord_index])
  return(first_three)
}


potential_maxima <- function(beta1, beta2, beta3) {
  a_12 <- unique(beta1 - beta2)
  a_13 <- unique(beta1 - beta3)
  a_23 <- unique(beta2 - beta3)
  
  c1 <- -a_13
  c2 <- sapply(a_12, function(x) x + c1)
  options1 <- NULL
  for (i in 1:length(c1)) {
    if (is.null(dim(c2))) {
      for (j in 1:length(c2)) {
        comb <- c(c1[i], c2[j])
        options1 <- rbind(options1, comb)
      }
    } else {
      for (j in 1:dim(c2)[2]) {
        comb <- c(c1[i], c2[i,j])
        options1 <- rbind(options1, comb)
      }
    }
  }
  
  c2 <- -a_23
  c1 <- sapply(a_12, function(x) c2 - x)
  options2 <- NULL
  for (i in 1:length(c2)) {
    if (is.null(dim(c1))) {
      for (j in 1:length(c1)) {
        comb <- c(c1[j], c2[i])
        options2 <- rbind(options2, comb)
      }
    } else {
      for (j in 1:dim(c1)[2]) {
        comb <- c(c1[i,j], c2[i])
        options2 <- rbind(options2, comb)
      }
    }
  }
  
  c1 <- -a_13
  c2 <- -a_23
  options3 <- NULL
  for (i in 1:length(c1)) {
    for (j in 1:length(c2)) {
      comb <- c(c1[i], c2[j])
      options3 <- rbind(options3, comb)
    }
  }
  combs <- uniquecombs(rbind(options1, options2, options3))
  return(combs)
}
