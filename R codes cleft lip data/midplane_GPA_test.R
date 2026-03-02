############################################
#Check on the midplane estimated by GPA
#and compare it with the clinical midplane
require(shapes)
#############################################
#1. Find the bilaterally symmetric mean shape############################
mu_GPA <- GPA(Con_fir_o_ref, tol = 10^(-8))
##############################################
#2. Use OPA to register each subject to this mean######################
#2.1 Cleft##################################
#2.1.1 First frame###########################
Cle_OPA_muGPA_fr1 <- array(0, dim = c(24, 3, 13))
for (subject in 1:13) {
  Cle_OPA_muGPA_fr1[, , subject] <- procOPA(mu_GPA$mean, 
              as.matrix(cleft_data[[subject]][[1]]))$Bhat
  
  Cle_OPA_muGPA_fr1[, , subject] <- 
    Cle_OPA_muGPA_fr1[, , subject]/sqrt(sum(Cle_OPA_muGPA_fr1[, , subject]^2))*
    centroid.size(C %*% as.matrix(cleft_data[[subject]][[1]]))
}
#2.1.2 Middle frame###########################
Cle_OPA_muGPA_mid <- array(0, dim = c(24, 3, 13))
for (subject in 1:13) {
  Cle_OPA_muGPA_mid[, , subject] <- procOPA(mu_GPA$mean, 
              as.matrix(cleft_data[[subject]][[2]]))$Bhat
  
  Cle_OPA_muGPA_mid[, , subject] <- 
    Cle_OPA_muGPA_mid[, , subject]/sqrt(sum(Cle_OPA_muGPA_mid[, , subject]^2))*
    centroid.size(C %*% as.matrix(cleft_data[[subject]][[2]]))
}
#2.1.3 Last frame###########################
Cle_OPA_muGPA_la <- array(0, dim = c(24, 3, 13))
for (subject in 1:13) {
  Cle_OPA_muGPA_la[, , subject] <- procOPA(mu_GPA$mean, 
              as.matrix(cleft_data[[subject]][[3]]))$Bhat
  
  Cle_OPA_muGPA_la[, , subject] <- 
    Cle_OPA_muGPA_la[, , subject]/sqrt(sum(Cle_OPA_muGPA_la[, , subject]^2))*
    centroid.size(C %*% as.matrix(cleft_data[[subject]][[3]]))
}
#2.2 Control##################################
#2.2.1 First frame###########################
Con_OPA_muGPA_fr1 <- array(0, dim = c(24, 3, 12))
for (subject in 1:12) {
  Con_OPA_muGPA_fr1[, , subject] <- procOPA(mu_GPA$mean, 
              as.matrix(control_data[[subject]][[1]]))$Bhat
  
  Con_OPA_muGPA_fr1[, , subject] <- 
    Con_OPA_muGPA_fr1[, , subject]/sqrt(sum(Con_OPA_muGPA_fr1[, , subject]^2))*
    centroid.size(C %*% as.matrix(control_data[[subject]][[1]]))
}
#2.2.2 Middle frame###########################
Con_OPA_muGPA_mid <- array(0, dim = c(24, 3, 12))
for (subject in 1:12) {
  Con_OPA_muGPA_mid[, , subject] <- procOPA(mu_GPA$mean, 
              as.matrix(control_data[[subject]][[2]]))$Bhat
  
  Con_OPA_muGPA_mid[, , subject] <- 
    Con_OPA_muGPA_mid[, , subject]/sqrt(sum(Con_OPA_muGPA_mid[, , subject]^2))*
    centroid.size(C %*% as.matrix(control_data[[subject]][[2]]))
}
#2.2.3 Last frame###########################
Con_OPA_muGPA_la <- array(0, dim = c(24, 3, 12))
for (subject in 1:12) {
  Con_OPA_muGPA_la[, , subject] <- procOPA(mu_GPA$mean, 
              as.matrix(control_data[[subject]][[3]]))$Bhat
  
  Con_OPA_muGPA_la[, , subject] <- 
    Con_OPA_muGPA_la[, , subject]/sqrt(sum(Con_OPA_muGPA_la[, , subject]^2))*
    centroid.size(C %*% as.matrix(control_data[[subject]][[3]]))
}
##############################################
#3. Compute asymmetry scores w.r.t GPA midplane##################
#3.1 Cleft########################################
#3.1.1 First frame################################
AS_GPAmidplane_cle_fir <- c()
for (subject in 1:13) {
  Sx <- Cle_OPA_muGPA_fr1[c(1:6, 20:24), 1, subject] + 
    Cle_OPA_muGPA_fr1[c(13:8, 18:14), 1, subject]
  Dy <- Cle_OPA_muGPA_fr1[c(1:6, 20:24), 2, subject] - 
    Cle_OPA_muGPA_fr1[c(13:8, 18:14), 2, subject]
  Dz <- Cle_OPA_muGPA_fr1[c(1:6, 20:24), 3, subject] - 
    Cle_OPA_muGPA_fr1[c(13:8, 18:14), 3, subject]
  AS_GPAmidplane_cle_fir[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs(Cle_OPA_muGPA_fr1[7, 1, subject]) + 
    abs(Cle_OPA_muGPA_fr1[19, 1, subject])
}
#3.1.2 Middle frame################################
AS_GPAmidplane_cle_mid <- c()
for (subject in 1:13) {
  Sx <- Cle_OPA_muGPA_mid[c(1:6, 20:24), 1, subject] + 
    Cle_OPA_muGPA_mid[c(13:8, 18:14), 1, subject]
  Dy <- Cle_OPA_muGPA_mid[c(1:6, 20:24), 2, subject] - 
    Cle_OPA_muGPA_mid[c(13:8, 18:14), 2, subject]
  Dz <- Cle_OPA_muGPA_mid[c(1:6, 20:24), 3, subject] - 
    Cle_OPA_muGPA_mid[c(13:8, 18:14), 3, subject]
  AS_GPAmidplane_cle_mid[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs(Cle_OPA_muGPA_mid[7, 1, subject]) + 
    abs(Cle_OPA_muGPA_mid[19, 1, subject])
}
#3.1.3 Last frame################################
AS_GPAmidplane_cle_la <- c()
for (subject in 1:13) {
  Sx <- Cle_OPA_muGPA_la[c(1:6, 20:24), 1, subject] + 
    Cle_OPA_muGPA_la[c(13:8, 18:14), 1, subject]
  Dy <- Cle_OPA_muGPA_la[c(1:6, 20:24), 2, subject] - 
    Cle_OPA_muGPA_la[c(13:8, 18:14), 2, subject]
  Dz <- Cle_OPA_muGPA_la[c(1:6, 20:24), 3, subject] - 
    Cle_OPA_muGPA_la[c(13:8, 18:14), 3, subject]
  AS_GPAmidplane_cle_la[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs(Cle_OPA_muGPA_la[7, 1, subject]) + 
    abs(Cle_OPA_muGPA_la[19, 1, subject])
}
#3.2 Control########################################
#3.2.1 First frame################################
AS_GPAmidplane_con_fir <- c()
for (subject in 1:12) {
  Sx <- Con_OPA_muGPA_fr1[c(1:6, 20:24), 1, subject] + 
    Con_OPA_muGPA_fr1[c(13:8, 18:14), 1, subject]
  Dy <- Con_OPA_muGPA_fr1[c(1:6, 20:24), 2, subject] - 
    Con_OPA_muGPA_fr1[c(13:8, 18:14), 2, subject]
  Dz <- Con_OPA_muGPA_fr1[c(1:6, 20:24), 3, subject] - 
    Con_OPA_muGPA_fr1[c(13:8, 18:14), 3, subject]
  AS_GPAmidplane_con_fir[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs(Con_OPA_muGPA_fr1[7, 1, subject]) + 
    abs(Con_OPA_muGPA_fr1[19, 1, subject])
}
#3.2.2 Middle frame################################
AS_GPAmidplane_con_mid <- c()
for (subject in 1:12) {
  Sx <- Con_OPA_muGPA_mid[c(1:6, 20:24), 1, subject] + 
    Con_OPA_muGPA_mid[c(13:8, 18:14), 1, subject]
  Dy <- Con_OPA_muGPA_mid[c(1:6, 20:24), 2, subject] - 
    Con_OPA_muGPA_mid[c(13:8, 18:14), 2, subject]
  Dz <- Con_OPA_muGPA_mid[c(1:6, 20:24), 3, subject] - 
    Con_OPA_muGPA_mid[c(13:8, 18:14), 3, subject]
  AS_GPAmidplane_con_mid[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs(Con_OPA_muGPA_mid[7, 1, subject]) + 
    abs(Con_OPA_muGPA_mid[19, 1, subject])
}
#3.2.3 Last frame################################
AS_GPAmidplane_con_la <- c()
for (subject in 1:12) {
  Sx <- Con_OPA_muGPA_la[c(1:6, 20:24), 1, subject] + 
    Con_OPA_muGPA_la[c(13:8, 18:14), 1, subject]
  Dy <- Con_OPA_muGPA_la[c(1:6, 20:24), 2, subject] - 
    Con_OPA_muGPA_la[c(13:8, 18:14), 2, subject]
  Dz <- Con_OPA_muGPA_la[c(1:6, 20:24), 3, subject] - 
    Con_OPA_muGPA_la[c(13:8, 18:14), 3, subject]
  AS_GPAmidplane_con_la[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs(Con_OPA_muGPA_la[7, 1, subject]) + 
    abs(Con_OPA_muGPA_la[19, 1, subject])
}
#4. Compute asymmetry scores w.r.t clinical midplane##############
#4.1 Cleft########################################
#4.1.1 First frame################################
AS_clinmidplane_cle_fir <- c()
for (subject in 1:13) {
  Sx <- (C %*% as.matrix(cleft_data[[subject]][[1]]))[c(1:6, 20:24), 1] + 
    (C %*% as.matrix(cleft_data[[subject]][[1]]))[c(13:8, 18:14), 1]
  Dy <- (C %*% as.matrix(cleft_data[[subject]][[1]]))[c(1:6, 20:24), 2] - 
    (C %*% as.matrix(cleft_data[[subject]][[1]]))[c(13:8, 18:14), 2]
  Dz <- (C %*% as.matrix(cleft_data[[subject]][[1]]))[c(1:6, 20:24), 3] - 
    (C %*% as.matrix(cleft_data[[subject]][[1]]))[c(13:8, 18:14), 3]
  AS_clinmidplane_cle_fir[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs((C %*% as.matrix(cleft_data[[subject]][[1]]))[7, 1]) + 
    abs((C %*% as.matrix(cleft_data[[subject]][[1]]))[19, 1])
}
#4.1.2 Middle frame################################
AS_clinmidplane_cle_mid <- c()
for (subject in 1:13) {
  Sx <- (C %*% as.matrix(cleft_data[[subject]][[2]]))[c(1:6, 20:24), 1] + 
    (C %*% as.matrix(cleft_data[[subject]][[2]]))[c(13:8, 18:14), 1]
  Dy <- (C %*% as.matrix(cleft_data[[subject]][[2]]))[c(1:6, 20:24), 2] - 
    (C %*% as.matrix(cleft_data[[subject]][[2]]))[c(13:8, 18:14), 2]
  Dz <- (C %*% as.matrix(cleft_data[[subject]][[2]]))[c(1:6, 20:24), 3] - 
    (C %*% as.matrix(cleft_data[[subject]][[2]]))[c(13:8, 18:14), 3]
  AS_clinmidplane_cle_mid[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs((C %*% as.matrix(cleft_data[[subject]][[2]]))[7, 1]) + 
    abs((C %*% as.matrix(cleft_data[[subject]][[2]]))[19, 1])
}
#4.1.3 Last frame################################
AS_clinmidplane_cle_la <- c()
for (subject in 1:13) {
  Sx <- (C %*% as.matrix(cleft_data[[subject]][[3]]))[c(1:6, 20:24), 1] + 
    (C %*% as.matrix(cleft_data[[subject]][[3]]))[c(13:8, 18:14), 1]
  Dy <- (C %*% as.matrix(cleft_data[[subject]][[3]]))[c(1:6, 20:24), 2] - 
    (C %*% as.matrix(cleft_data[[subject]][[3]]))[c(13:8, 18:14), 2]
  Dz <- (C %*% as.matrix(cleft_data[[subject]][[3]]))[c(1:6, 20:24), 3] - 
    (C %*% as.matrix(cleft_data[[subject]][[3]]))[c(13:8, 18:14), 3]
  AS_clinmidplane_cle_la[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs((C %*% as.matrix(cleft_data[[subject]][[3]]))[7, 1]) + 
    abs((C %*% as.matrix(cleft_data[[subject]][[3]]))[19, 1])
}
#4.2 Control########################################
#4.2.1 First frame################################
AS_clinmidplane_con_fir <- c()
for (subject in 1:12) {
  Sx <- (C %*% as.matrix(control_data[[subject]][[1]]))[c(1:6, 20:24), 1] + 
    (C %*% as.matrix(control_data[[subject]][[1]]))[c(13:8, 18:14), 1]
  Dy <- (C %*% as.matrix(control_data[[subject]][[1]]))[c(1:6, 20:24), 2] - 
    (C %*% as.matrix(control_data[[subject]][[1]]))[c(13:8, 18:14), 2]
  Dz <- (C %*% as.matrix(control_data[[subject]][[1]]))[c(1:6, 20:24), 3] - 
    (C %*% as.matrix(control_data[[subject]][[1]]))[c(13:8, 18:14), 3]
  AS_clinmidplane_con_fir[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs((C %*% as.matrix(control_data[[subject]][[1]]))[7, 1]) + 
    abs((C %*% as.matrix(control_data[[subject]][[1]]))[19, 1])
}
#4.2.2 Middle frame################################
AS_clinmidplane_con_mid <- c()
for (subject in 1:12) {
  Sx <- (C %*% as.matrix(control_data[[subject]][[2]]))[c(1:6, 20:24), 1] + 
    (C %*% as.matrix(control_data[[subject]][[2]]))[c(13:8, 18:14), 1]
  Dy <- (C %*% as.matrix(control_data[[subject]][[2]]))[c(1:6, 20:24), 2] - 
    (C %*% as.matrix(control_data[[subject]][[2]]))[c(13:8, 18:14), 2]
  Dz <- (C %*% as.matrix(control_data[[subject]][[2]]))[c(1:6, 20:24), 3] - 
    (C %*% as.matrix(control_data[[subject]][[2]]))[c(13:8, 18:14), 3]
  AS_clinmidplane_con_mid[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs((C %*% as.matrix(control_data[[subject]][[2]]))[7, 1]) + 
    abs((C %*% as.matrix(control_data[[subject]][[2]]))[19, 1])
}
#4.2.3 Last frame################################
AS_clinmidplane_con_la <- c()
for (subject in 1:12) {
  Sx <- (C %*% as.matrix(control_data[[subject]][[3]]))[c(1:6, 20:24), 1] + 
    (C %*% as.matrix(control_data[[subject]][[3]]))[c(13:8, 18:14), 1]
  Dy <- (C %*% as.matrix(control_data[[subject]][[3]]))[c(1:6, 20:24), 2] - 
    (C %*% as.matrix(control_data[[subject]][[3]]))[c(13:8, 18:14), 2]
  Dz <- (C %*% as.matrix(control_data[[subject]][[3]]))[c(1:6, 20:24), 3] - 
    (C %*% as.matrix(control_data[[subject]][[3]]))[c(13:8, 18:14), 3]
  AS_clinmidplane_con_la[subject] <- sum(sqrt(Sx^2 + Dy^2 + Dz^2)) +
    abs((C %*% as.matrix(control_data[[subject]][[3]]))[7, 1]) + 
    abs((C %*% as.matrix(control_data[[subject]][[3]]))[19, 1])
}
#################################################
#5. Elementary features#############################
#5.1 GPA midplane##############################
#5.1.1 Cleft################################
#5.1.1.1 First frame###############################
d_cle_GPAmidplane_fir <- matrix(0, nrow = 13, ncol = 35)
for (subject in 1:13) {
  diff_matrix <- Cle_OPA_muGPA_fr1[, , subject] - 
    (Cle_OPA_muGPA_fr1[, , subject] %*% H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_cle_GPAmidplane_fir[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_cle_GPAmidplane_fir[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_cle_GPAmidplane_fir[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.1.1.2 Middle frame###############################
d_cle_GPAmidplane_mid <- matrix(0, nrow = 13, ncol = 35)
for (subject in 1:13) {
  diff_matrix <- Cle_OPA_muGPA_mid[, , subject] - 
    (Cle_OPA_muGPA_mid[, , subject] %*% H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_cle_GPAmidplane_mid[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_cle_GPAmidplane_mid[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_cle_GPAmidplane_mid[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.1.1.3 Last frame###############################
d_cle_GPAmidplane_la <- matrix(0, nrow = 13, ncol = 35)
for (subject in 1:13) {
  diff_matrix <- Cle_OPA_muGPA_la[, , subject] - 
    (Cle_OPA_muGPA_la[, , subject] %*% H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_cle_GPAmidplane_la[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_cle_GPAmidplane_la[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_cle_GPAmidplane_la[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.1.2 Control################################
#5.1.2.1 First frame###############################
d_con_GPAmidplane_fir <- matrix(0, nrow = 12, ncol = 35)
for (subject in 1:12) {
  diff_matrix <- Con_OPA_muGPA_fr1[, , subject] - 
    (Con_OPA_muGPA_fr1[, , subject] %*% H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_con_GPAmidplane_fir[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_con_GPAmidplane_fir[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_con_GPAmidplane_fir[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.1.2.2 Middle frame###############################
d_con_GPAmidplane_mid <- matrix(0, nrow = 12, ncol = 35)
for (subject in 1:12) {
  diff_matrix <- Con_OPA_muGPA_mid[, , subject] - 
    (Con_OPA_muGPA_mid[, , subject] %*% H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_con_GPAmidplane_mid[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_con_GPAmidplane_mid[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_con_GPAmidplane_mid[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.1.2.3 Last frame###############################
d_con_GPAmidplane_la <- matrix(0, nrow = 12, ncol = 35)
for (subject in 1:12) {
  diff_matrix <- Con_OPA_muGPA_la[, , subject] - 
    (Con_OPA_muGPA_la[, , subject] %*% H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_con_GPAmidplane_la[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_con_GPAmidplane_la[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_con_GPAmidplane_la[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.2 Clinical midplane##############################
#5.2.1 Cleft################################
#5.2.1.1 First frame###############################
d_cle_clinmidplane_fir <- matrix(0, nrow = 13, ncol = 35)
for (subject in 1:13) {
  diff_matrix <- (as.matrix(cleft_data[[subject]][[1]])) - 
    ((as.matrix(cleft_data[[subject]][[1]])) %*% 
       H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_cle_clinmidplane_fir[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_cle_clinmidplane_fir[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_cle_clinmidplane_fir[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.2.1.2 Middle frame###############################
d_cle_clinmidplane_mid <- matrix(0, nrow = 13, ncol = 35)
for (subject in 1:13) {
  diff_matrix <- (as.matrix(cleft_data[[subject]][[2]])) - 
    ((as.matrix(cleft_data[[subject]][[2]])) %*% 
       H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_cle_clinmidplane_mid[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_cle_clinmidplane_mid[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_cle_clinmidplane_mid[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.2.1.3 Last frame###############################
d_cle_clinmidplane_la <- matrix(0, nrow = 13, ncol = 35)
for (subject in 1:13) {
  diff_matrix <- (as.matrix(cleft_data[[subject]][[3]])) - 
    ((as.matrix(cleft_data[[subject]][[3]])) %*% 
       H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_cle_clinmidplane_la[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_cle_clinmidplane_la[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_cle_clinmidplane_la[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.2.2 Control################################
#5.2.2.1 First frame###############################
d_con_clinmidplane_fir <- matrix(0, nrow = 12, ncol = 35)
for (subject in 1:12) {
  diff_matrix <- (C %*% as.matrix(control_data[[subject]][[1]])) - 
    ((C %*% as.matrix(control_data[[subject]][[1]])) %*% 
       H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_con_clinmidplane_fir[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_con_clinmidplane_fir[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_con_clinmidplane_fir[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.2.2.2 Middle frame###############################
d_con_clinmidplane_mid <- matrix(0, nrow = 12, ncol = 35)
for (subject in 1:12) {
  diff_matrix <- (C %*% as.matrix(control_data[[subject]][[2]])) - 
    ((C %*% as.matrix(control_data[[subject]][[2]])) %*% 
       H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_con_clinmidplane_mid[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_con_clinmidplane_mid[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_con_clinmidplane_mid[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
#5.2.2.3 Last frame###############################
d_con_clinmidplane_la <- matrix(0, nrow = 12, ncol = 35)
for (subject in 1:12) {
  diff_matrix <- (C %*% as.matrix(control_data[[subject]][[3]])) - 
    ((C %*% as.matrix(control_data[[subject]][[3]])) %*% 
       H)[c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
  
  d_con_clinmidplane_la[subject, 1:18] <- 
    matrix(diff_matrix[1:6, ], nrow = 1)
  
  d_con_clinmidplane_la[subject, 19:33] <- 
    matrix(diff_matrix[20:24, ], nrow = 1)
  
  d_con_clinmidplane_la[subject, 34:35] <- c(diff_matrix[7, 1],
          diff_matrix[19, 1])
}
###################################################
#6. L1 and L2 statistics#############################
#6.1 L1 statistics##################################
#6.1.1 GPA midplane##################################
#6.1.1.1 Cleft######################################
cle_L1_GPAmidplane_fir <- rowSums(abs(d_cle_GPAmidplane_fir))
cle_L1_GPAmidplane_mid <- rowSums(abs(d_cle_GPAmidplane_mid))
cle_L1_GPAmidplane_la <- rowSums(abs(d_cle_GPAmidplane_la))
#6.1.1.2 Control######################################
con_L1_GPAmidplane_fir <- rowSums(abs(d_con_GPAmidplane_fir))
con_L1_GPAmidplane_mid <- rowSums(abs(d_con_GPAmidplane_mid))
con_L1_GPAmidplane_la <- rowSums(abs(d_con_GPAmidplane_la))
#6.1.2 Clinical midplane##################################
#6.1.2.1 Cleft######################################
cle_L1_clinmidplane_fir <- rowSums(abs(d_cle_clinmidplane_fir))
cle_L1_clinmidplane_mid <- rowSums(abs(d_cle_clinmidplane_mid))
cle_L1_clinmidplane_la <- rowSums(abs(d_cle_clinmidplane_la))
#6.1.2.2 Control######################################
con_L1_clinmidplane_fir <- rowSums(abs(d_con_clinmidplane_fir))
con_L1_clinmidplane_mid <- rowSums(abs(d_con_clinmidplane_mid))
con_L1_clinmidplane_la <- rowSums(abs(d_con_clinmidplane_la))
#6.2 L2 statistics##################################
#6.2.1 GPA midplane##################################
#6.2.1.1 Cleft######################################
cle_L2_GPAmidplane_fir <- rowSums(abs(d_cle_GPAmidplane_fir)^2)
cle_L2_GPAmidplane_mid <- rowSums(abs(d_cle_GPAmidplane_mid)^2)
cle_L2_GPAmidplane_la <- rowSums(abs(d_cle_GPAmidplane_la)^2)
#6.2.1.2 Control######################################
con_L2_GPAmidplane_fir <- rowSums(abs(d_con_GPAmidplane_fir)^2)
con_L2_GPAmidplane_mid <- rowSums(abs(d_con_GPAmidplane_mid)^2)
con_L2_GPAmidplane_la <- rowSums(abs(d_con_GPAmidplane_la)^2)
#6.2.2 Clinical midplane##################################
#6.2.2.1 Cleft######################################
cle_L2_clinmidplane_fir <- rowSums(abs(d_cle_clinmidplane_fir)^2)
cle_L2_clinmidplane_mid <- rowSums(abs(d_cle_clinmidplane_mid)^2)
cle_L2_clinmidplane_la <- rowSums(abs(d_cle_clinmidplane_la)^2)
#6.2.2.2 Control######################################
con_L2_clinmidplane_fir <- rowSums(abs(d_con_clinmidplane_fir)^2)
con_L2_clinmidplane_mid <- rowSums(abs(d_con_clinmidplane_mid)^2)
con_L2_clinmidplane_la <- rowSums(abs(d_con_clinmidplane_la)^2)
##############################################
#7. Some plots################################
plot(cle_L2_clinmidplane_fir, ylim = c(0, max(cle_L2_clinmidplane_fir)),
     main = 'First frame, score u', pch = 19, xlab = 'time')
points(cle_L2_GPAmidplane_fir)

plot(cle_L2_clinmidplane_mid, ylim = c(0, max(cle_L2_clinmidplane_mid)),
     main = 'Middle frame, score u', pch = 19, xlab = 'time')
points(cle_L2_GPAmidplane_mid)

plot(cle_L2_clinmidplane_la, ylim = c(0, max(cle_L2_clinmidplane_la)),
     main = 'Last frame, score u', pch = 19, xlab = 'time')
points(cle_L2_GPAmidplane_la)


#C711
plot(as.matrix(cleft_data[[11]][[1]]), asp = 1, xlim = c(-15, 15),
     xlab = 'x', ylab = 'y', main = 'Clinic registration')
points(as.matrix(cleft_data[[11]][[1]]) %*% H, pch = 19)

plot(Cle_OPA_muGPA_fr1[, , 11], asp = 1, xlim = c(-15, 15),
     xlab = 'x', ylab = 'y', main = 'Procrustes registration')
points(Cle_OPA_muGPA_fr1[, , 11] %*% H, pch = 19)

#C709
plot(as.matrix(cleft_data[[9]][[1]]), asp = 1, xlim = c(-15, 15),
     xlab = 'x', ylab = 'y', main = 'Clinic registration')
points(as.matrix(cleft_data[[9]][[1]]) %*% H, pch = 19)

plot(Cle_OPA_muGPA_fr1[, , 9], asp = 1, xlim = c(-15, 15),
     xlab = 'x', ylab = 'y', main = 'Procrustes registration')
points(Cle_OPA_muGPA_fr1[, , 9] %*% H, pch = 19)

for (subject in 1:13) {
  plot(as.matrix(cleft_data[[subject]][[1]]), asp = 1, xlim = c(-20, 20))
  points(as.matrix(cleft_data[[subject]][[1]]) %*% H, pch = 19)
  
  plot(Cle_OPA_muGPA_fr1[, , subject], asp = 1, xlim = c(-20, 20),
       main = subject)
  points(Cle_OPA_muGPA_fr1[, , subject] %*% H, pch = 19)
}
#C711, C710, C709, C707, C705











