########################################
#Codes for performing analyses on the cleft lip data
#Load required packages
require(rio)
require(MASS)
##########################################
#0. Read in data and store them into a list#######################
#0.1 Cleft data##################
cleft_data <- list()

cleft_data$'DS2' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_DS2.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'GJ' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_GJ.xlsx',
                               col_names = c('X', 'Y', 'Z'))

cleft_data$'GSW' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_GSW.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'IM' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_IM.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'IRF' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_IRF.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'JE' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_JE.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'JF' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_JF.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'JS' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_JS.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'JT' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_JT.xlsx',
                                col_names = c('X', 'Y', 'Z'))

cleft_data$'KF' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_KF.xlsx',
                               col_names = c('X', 'Y', 'Z'))

cleft_data$'LJ' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_LJ.xlsx',
                               col_names = c('X', 'Y', 'Z'))

cleft_data$'LS' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_LS.xlsx',
                               col_names = c('X', 'Y', 'Z'))

cleft_data$'SS' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Cleft Data/Kanti_Cleft_Lips_SS.xlsx',
                               col_names = c('X', 'Y', 'Z'))
#0.2 Control data###########################
control_data <- list()

control_data$'con_002' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_002.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_007' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_007.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_017' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_017.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_025' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_025.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_028' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_028.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_041' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_041.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_044' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_044.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_062' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_062.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_070' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_070.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_085' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_085.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_092' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_092.xlsx',
              col_names = c('X', 'Y', 'Z'))

control_data$'con_117' <- 
  import_list('C:/Users/mmxw/Desktop/R/BV Data/Control Data/Kanti_Control_Lips_117.xlsx',
              col_names = c('X', 'Y', 'Z'))
#################################################
#1. Asymmetry features defined in (2.1)-(2.3)######################
#1.1 Cleft data#############################
#First frame
d_cle_fir <- matrix(0, nrow = 13, ncol = 35)
for (i in 1:13) {
  Sx <- (cleft_data[[i]][[1]][c(1:6, 20:24), 'X']) + 
    (cleft_data[[i]][[1]][c(13:8, 18:14), 'X'])
  Dy <- (cleft_data[[i]][[1]][c(1:6, 20:24), 'Y']) - 
    (cleft_data[[i]][[1]][c(13:8, 18:14), 'Y'])
  Dz <- (cleft_data[[i]][[1]][c(1:6, 20:24), 'Z']) - 
    (cleft_data[[i]][[1]][c(13:8, 18:14), 'Z'])
  d_cle_fir[i, seq(1, 33, 3)] <- Sx
  d_cle_fir[i, seq(2, 33, 3)] <- Dy
  d_cle_fir[i, seq(3, 33, 3)] <- Dz
  d_cle_fir[i, 34:35] <- c(cleft_data[[i]][[1]][7, 1], 
                           cleft_data[[i]][[1]][19, 1])
}

#Middle frame
d_cle_mid <- matrix(0, nrow = 13, ncol = 35)
for (i in 1:13) {
  Sx <- (cleft_data[[i]][[2]][c(1:6, 20:24), 'X']) + 
    (cleft_data[[i]][[2]][c(13:8, 18:14), 'X'])
  Dy <- (cleft_data[[i]][[2]][c(1:6, 20:24), 'Y']) - 
    (cleft_data[[i]][[2]][c(13:8, 18:14), 'Y'])
  Dz <- (cleft_data[[i]][[2]][c(1:6, 20:24), 'Z']) - 
    (cleft_data[[i]][[2]][c(13:8, 18:14), 'Z'])
  d_cle_mid[i, seq(1, 33, 3)] <- Sx
  d_cle_mid[i, seq(2, 33, 3)] <- Dy
  d_cle_mid[i, seq(3, 33, 3)] <- Dz
  d_cle_mid[i, 34:35] <- c(cleft_data[[i]][[2]][7, 1], 
                           cleft_data[[i]][[2]][19, 1])
}

#Last frame
d_cle_la <- matrix(0, nrow = 13, ncol = 35)
for (i in 1:13) {
  Sx <- (cleft_data[[i]][[3]][c(1:6, 20:24), 'X']) + 
    (cleft_data[[i]][[3]][c(13:8, 18:14), 'X'])
  Dy <- (cleft_data[[i]][[3]][c(1:6, 20:24), 'Y']) - 
    (cleft_data[[i]][[3]][c(13:8, 18:14), 'Y'])
  Dz <- (cleft_data[[i]][[3]][c(1:6, 20:24), 'Z']) - 
    (cleft_data[[i]][[3]][c(13:8, 18:14), 'Z'])
  d_cle_la[i, seq(1, 33, 3)] <- Sx
  d_cle_la[i, seq(2, 33, 3)] <- Dy
  d_cle_la[i, seq(3, 33, 3)] <- Dz
  d_cle_la[i, 34:35] <- c(cleft_data[[i]][[3]][7, 1], 
                           cleft_data[[i]][[3]][19, 1])
}

#1.2 Control data#############################
#First frame
d_con_fir <- matrix(0, nrow = 12, ncol = 35)
for (i in 1:12) {
  Sx <- (control_data[[i]][[1]][c(1:6, 20:24), 'X']) + 
    (control_data[[i]][[1]][c(13:8, 18:14), 'X'])
  Dy <- (control_data[[i]][[1]][c(1:6, 20:24), 'Y']) - 
    (control_data[[i]][[1]][c(13:8, 18:14), 'Y'])
  Dz <- (control_data[[i]][[1]][c(1:6, 20:24), 'Z']) - 
    (control_data[[i]][[1]][c(13:8, 18:14), 'Z'])
  d_con_fir[i, seq(1, 33, 3)] <- Sx
  d_con_fir[i, seq(2, 33, 3)] <- Dy
  d_con_fir[i, seq(3, 33, 3)] <- Dz
  d_con_fir[i, 34:35] <- c(control_data[[i]][[1]][7, 1], 
                    control_data[[i]][[1]][19, 1])
}

#Middle frame
d_con_mid <- matrix(0, nrow = 12, ncol = 35)
for (i in 1:12) {
  Sx <- (control_data[[i]][[2]][c(1:6, 20:24), 'X']) + 
    (control_data[[i]][[2]][c(13:8, 18:14), 'X'])
  Dy <- (control_data[[i]][[2]][c(1:6, 20:24), 'Y']) - 
    (control_data[[i]][[2]][c(13:8, 18:14), 'Y'])
  Dz <- (control_data[[i]][[2]][c(1:6, 20:24), 'Z']) - 
    (control_data[[i]][[2]][c(13:8, 18:14), 'Z'])
  d_con_mid[i, seq(1, 33, 3)] <- Sx
  d_con_mid[i, seq(2, 33, 3)] <- Dy
  d_con_mid[i, seq(3, 33, 3)] <- Dz
  d_con_mid[i, 34:35] <- c(control_data[[i]][[2]][7, 1], 
                           control_data[[i]][[2]][19, 1])
}

#Last frame
d_con_la <- matrix(0, nrow = 12, ncol = 35)
for (i in 1:12) {
  Sx <- (control_data[[i]][[3]][c(1:6, 20:24), 'X']) + 
    (control_data[[i]][[3]][c(13:8, 18:14), 'X'])
  Dy <- (control_data[[i]][[3]][c(1:6, 20:24), 'Y']) - 
    (control_data[[i]][[3]][c(13:8, 18:14), 'Y'])
  Dz <- (control_data[[i]][[3]][c(1:6, 20:24), 'Z']) - 
    (control_data[[i]][[3]][c(13:8, 18:14), 'Z'])
  d_con_la[i, seq(1, 33, 3)] <- Sx
  d_con_la[i, seq(2, 33, 3)] <- Dy
  d_con_la[i, seq(3, 33, 3)] <- Dz
  d_con_la[i, 34:35] <- c(control_data[[i]][[3]][7, 1], 
                           control_data[[i]][[3]][19, 1])
}

#####################################################
#2. Composite asymmetry scores###########################
#2.1 \phi^*_{L_1} in (3.9)###############################
#2.1.1 Cleft##################################
phi_star_L1_cle <- matrix(0, nrow = 13, ncol = 3)
for (i in 1:13) {
  for (j in 1:3) {
    Sx <- (cleft_data[[i]][[j]][c(1:6, 20:24), 'X']) + 
      (cleft_data[[i]][[j]][c(13:8, 18:14), 'X'])
    Dy <- (cleft_data[[i]][[j]][c(1:6, 20:24), 'Y']) - 
      (cleft_data[[i]][[j]][c(13:8, 18:14), 'Y'])
    Dz <- (cleft_data[[i]][[j]][c(1:6, 20:24), 'Z']) - 
      (cleft_data[[i]][[j]][c(13:8, 18:14), 'Z'])
    phi_star_L1_cle[i, j] <- 
      sum(c(sqrt(Sx^2 + Dy^2 + Dz^2), 
  abs(cleft_data[[i]][[j]][7, 1]), abs(cleft_data[[i]][[j]][19, 1])))
  }
}
#2.1.2 Control##################################
phi_star_L1_con <- matrix(0, nrow = 12, ncol = 3)
for (i in 1:12) {
  for (j in 1:3) {
    Sx <- (control_data[[i]][[j]][c(1:6, 20:24), 'X']) + 
      (control_data[[i]][[j]][c(13:8, 18:14), 'X'])
    Dy <- (control_data[[i]][[j]][c(1:6, 20:24), 'Y']) - 
      (control_data[[i]][[j]][c(13:8, 18:14), 'Y'])
    Dz <- (control_data[[i]][[j]][c(1:6, 20:24), 'Z']) - 
      (control_data[[i]][[j]][c(13:8, 18:14), 'Z'])
    phi_star_L1_con[i, j] <- 
      sum(c(sqrt(Sx^2 + Dy^2 + Dz^2), 
  abs(control_data[[i]][[j]][7, 1]), abs(control_data[[i]][[j]][19, 1])))
  }
}

#2.2 \phi_{L_1} in (3.6)################################
#2.2.1 Cleft##################################
phi_L1_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L1_cle[, 1] <- rowSums(abs(d_cle_fir))
phi_L1_cle[, 2] <- rowSums(abs(d_cle_mid))
phi_L1_cle[, 3] <- rowSums(abs(d_cle_la))
#2.2.2 Control##################################
phi_L1_con <- matrix(0, nrow = 12, ncol = 3)
phi_L1_con[, 1] <- rowSums(abs(d_con_fir))
phi_L1_con[, 2] <- rowSums(abs(d_con_mid))
phi_L1_con[, 3] <- rowSums(abs(d_con_la))
#2.3 \phi_{L_2} in (3.7)###############################
#2.3.1 Cleft##################################
phi_L2_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L2_cle[, 1] <- rowSums(d_cle_fir^2)
phi_L2_cle[, 2] <- rowSums(d_cle_mid^2)
phi_L2_cle[, 3] <- rowSums(d_cle_la^2)
#2.3.2 Control##################################
phi_L2_con <- matrix(0, nrow = 12, ncol = 3)
phi_L2_con[, 1] <- rowSums(d_con_fir^2)
phi_L2_con[, 2] <- rowSums(d_con_mid^2)
phi_L2_con[, 3] <- rowSums(d_con_la^2)
#2.4 Weighted composite scores#################
#2.4.1 Compute weights##########################
pair_lm_ind <- c(13:8, 18:14, 7, 19)
lm_ind <- c(1:6, 20:24, 7, 19)
#2.4.1.1 Compute symmetric mean shapes##################
H <- diag(3) - 2*matrix(c(1, 0, 0), nrow = 3, ncol = 1) %*% 
  matrix(c(1, 0, 0), nrow = 1, ncol = 3)

#First frame
Con_fir_ref <- array(0, dim = c(24, 3, 12))
for (i in 1:12) {
  Con_fir_ref[, , i] <- as.matrix(control_data[[i]][[1]]) %*% H
}

Con_fir_o_ref <- array(NA, dim = c(24, 3, 24))
for (i in 1:12) {
  Con_fir_o_ref[, , i] <- as.matrix(control_data[[i]][[1]])
}

for (i in 1:12) {
  Con_fir_o_ref[, , 12 + i] <- 
    Con_fir_ref[, , i][c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
}

mu_sym_fir <- apply(Con_fir_o_ref, c(1, 2), mean)

#Middle frame
Con_mid_ref <- array(0, dim = c(24, 3, 12))
for (i in 1:12) {
  Con_mid_ref[, , i] <- as.matrix(control_data[[i]][[2]]) %*% H
}

Con_mid_o_ref <- array(NA, dim = c(24, 3, 24))
for (i in 1:12) {
  Con_mid_o_ref[, , i] <- as.matrix(control_data[[i]][[2]])
}

for (i in 1:12) {
  Con_mid_o_ref[, , 12 + i] <- 
    Con_mid_ref[, , i][c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
}

mu_sym_mid <- apply(Con_mid_o_ref, c(1, 2), mean)

#Last frame
Con_la_ref <- array(0, dim = c(24, 3, 12))
for (i in 1:12) {
  Con_la_ref[, , i] <- as.matrix(control_data[[i]][[3]]) %*% H
}

Con_la_o_ref <- array(NA, dim = c(24, 3, 24))
for (i in 1:12) {
  Con_la_o_ref[, , i] <- as.matrix(control_data[[i]][[3]])
}

for (i in 1:12) {
  Con_la_o_ref[, , 12 + i] <- 
    Con_la_ref[, , i][c(13:8, 7, 6:1, 24:20, 19, 18:14), ]
}

mu_sym_la <- apply(Con_la_o_ref, c(1, 2), mean)
#2.4.1.2 First frame###########################
weight_fir <- c()
for (i in 1:11) {
  weight_fir[(3*(i-1)+1):(3*i)] <- 
    rep(sqrt(sum((mu_sym_fir[lm_ind[i], ] - 
      mu_sym_fir[pair_lm_ind[i], ])^2)), 3)
}

weight_fir[34:35] <- 1
#2.4.1.3 Middle frame##########################
weight_mid <- c()
for (i in 1:11) {
  weight_mid[(3*(i-1)+1):(3*i)] <- 
    rep(sqrt(sum((mu_sym_mid[lm_ind[i], ] - 
        mu_sym_mid[pair_lm_ind[i], ])^2)), 3)
}

weight_mid[34:35] <- 1
#2.4.1.4 Last frame###########################
weight_la <- c()
for (i in 1:11) {
  weight_la[(3*(i-1)+1):(3*i)] <- 
    rep(sqrt(sum((mu_sym_la[lm_ind[i], ] - 
  mu_sym_la[pair_lm_ind[i], ])^2)), 3)
}

weight_la[34:35] <- 1
#2.4.2 \phi_{L_1}###############################
#2.4.2.1 Cleft##################################
phi_L1_cle_scale <- matrix(0, nrow = 13, ncol = 3)
phi_L1_cle_scale[, 1] <- 
  rowSums(t(apply(abs(d_cle_fir), 1, 
                function(a) {a/weight_fir})))
phi_L1_cle_scale[, 2] <- 
  rowSums(t(apply(abs(d_cle_mid), 1, 
                  function(a) {a/weight_mid})))
phi_L1_cle_scale[, 3] <- 
  rowSums(t(apply(abs(d_cle_la), 1, 
                  function(a) {a/weight_la})))
#2.4.2.2 Control#################################
phi_L1_con_scale <- matrix(0, nrow = 12, ncol = 3)
phi_L1_con_scale[, 1] <- 
  rowSums(t(apply(abs(d_con_fir), 1, 
              function(a) {a/weight_fir})))
phi_L1_con_scale[, 2] <- 
  rowSums(t(apply(abs(d_con_mid), 1, 
                  function(a) {a/weight_mid})))
phi_L1_con_scale[, 3] <- 
  rowSums(t(apply(abs(d_con_la), 1, 
                  function(a) {a/weight_la})))
#2.4.3 \phi_{L_2}###############################
#2.4.3.1 Cleft##################################
phi_L2_cle_scale <- matrix(0, nrow = 13, ncol = 3)
phi_L2_cle_scale[, 1] <- 
  rowSums(t(apply(abs(d_cle_fir), 1, 
                  function(a) {a/weight_fir}))^2)
phi_L2_cle_scale[, 2] <- 
  rowSums(t(apply(abs(d_cle_mid), 1, 
                  function(a) {a/weight_mid}))^2)
phi_L2_cle_scale[, 3] <- 
  rowSums(t(apply(abs(d_cle_la), 1, 
                  function(a) {a/weight_la}))^2)
#2.4.3.2 Control#################################
phi_L2_con_scale <- matrix(0, nrow = 12, ncol = 3)
phi_L2_con_scale[, 1] <- 
  rowSums(t(apply(abs(d_con_fir), 1, 
                  function(a) {a/weight_fir}))^2)
phi_L2_con_scale[, 2] <- 
  rowSums(t(apply(abs(d_con_mid), 1, 
                  function(a) {a/weight_mid}))^2)
phi_L2_con_scale[, 3] <- 
  rowSums(t(apply(abs(d_con_la), 1, 
                  function(a) {a/weight_la}))^2)
#################################################
#3. Univariate tests############################
#3.1 t-test################################
#3.1.1 equal variance t-test########################
t.test(phi_star_L1_con[, 1], phi_star_L1_cle[, 1], alternative = 'less',
       var.equal = T)

t.test(phi_star_L1_con[, 2], phi_star_L1_cle[, 2], alternative = 'less',
       var.equal = T)

t.test(phi_star_L1_con[, 3], phi_star_L1_cle[, 3], alternative = 'less',
       var.equal = T)
#3.1.2 Welch t-test#####################################
t.test(phi_star_L1_con[, 1], phi_star_L1_cle[, 1], alternative = 'less')

t.test(phi_star_L1_con[, 2], phi_star_L1_cle[, 2], alternative = 'less')

t.test(phi_star_L1_con[, 3], phi_star_L1_cle[, 3], alternative = 'less')
#3.2 Mann-Whitney U test###################################
#3.2.1 \phi_{L_1}###########################################
wilcox.test(phi_L1_con[, 1], phi_L1_cle[, 1], alternative = 'less')

wilcox.test(phi_L1_con[, 2], phi_L1_cle[, 2], alternative = 'less')

wilcox.test(phi_L1_con[, 3], phi_L1_cle[, 3], alternative = 'less')
#3.2.2 \phi_{L_2}###########################################
wilcox.test(phi_L2_con[, 1], phi_L2_cle[, 1], alternative = 'less')

wilcox.test(phi_L2_con[, 2], phi_L2_cle[, 2], alternative = 'less')

wilcox.test(phi_L2_con[, 3], phi_L2_cle[, 3], alternative = 'less')
#3.2.3 Weighted \phi_{L_1}###########################################
wilcox.test(phi_L1_con_scale[, 1], phi_L1_cle_scale[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_con_scale[, 2], phi_L1_cle_scale[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_con_scale[, 3], phi_L1_cle_scale[, 3], 
            alternative = 'less')
#3.2.4 Weighted \phi_{L_2}###########################################
wilcox.test(phi_L2_con_scale[, 1], phi_L2_cle_scale[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_con_scale[, 2], phi_L2_cle_scale[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_con_scale[, 3], phi_L2_cle_scale[, 3], 
            alternative = 'less')
###################################################
#4. Feature selection#######################################
#4.1 Compute t-values##############################
tval_fir <- c()
for (i in 1:35) {
  tval_fir[i] <- t.test(abs(d_cle_fir)[, i], abs(d_con_fir)[, i],
                        alternative = 'great')$statistic
}

tval_mid <- c()
for (i in 1:35) {
  tval_mid[i] <- t.test(abs(d_cle_mid)[, i], abs(d_con_mid)[, i],
                        alternative = 'great')$statistic
}

tval_la <- c()
for (i in 1:35) {
  tval_la[i] <- t.test(abs(d_cle_la)[, i], abs(d_con_la)[, i],
                       alternative = 'great')$statistic
}
#4.2 Combine the data##########################
clecon_fir <- rbind(abs(d_cle_fir), abs(d_con_fir))

clecon_mid <- rbind(abs(d_cle_mid), abs(d_con_mid))

clecon_la <- rbind(abs(d_cle_la), abs(d_con_la))

#4.3 Bootstrap####################################
#4.3.1 First frame################################
tboot_fir <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = T)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_fir[cleft_id, ]
  control_boot <- clecon_fir[control_id, ]
  
  t_va <- c()
  for (j in 1:35) {
    t_va[j] <- t.test(abs(cleft_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  tboot_fir[i] <- max(t_va)
}
which(tval_fir > quantile(tboot_fir, 0.95))
#4.3.2 Middle frame################################
tboot_mid <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = T)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_mid[cleft_id, ]
  control_boot <- clecon_mid[control_id, ]
  
  t_va <- c()
  for (j in 1:35) {
    t_va[j] <- t.test(abs(cleft_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  tboot_mid[i] <- max(t_va)
}
which(tval_mid > quantile(tboot_mid, 0.95))
#4.3.3 Last frame################################
tboot_la <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = T)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_la[cleft_id, ]
  control_boot <- clecon_la[control_id, ]
  
  t_va <- c()
  for (j in 1:35) {
    t_va[j] <- t.test(abs(cleft_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  tboot_la[i] <- max(t_va)
}
which(tval_la > quantile(tboot_la, 0.95))
#4.4 Test with reduced features#####################
#4.4.1 Central pair and solos########################
#4.4.1.1 \phi_{L_1}###############################
phi_L1_lm67819_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L1_lm67819_cle[, 1] <- rowSums(abs(d_cle_fir[, c(16:18, 34, 35)]))
phi_L1_lm67819_cle[, 2] <- rowSums(abs(d_cle_mid[, c(16:18, 34, 35)]))
phi_L1_lm67819_cle[, 3] <- rowSums(abs(d_cle_la[, c(16:18, 34, 35)]))

phi_L1_lm67819_con <- matrix(0, nrow = 12, ncol = 3)
phi_L1_lm67819_con[, 1] <- rowSums(abs(d_con_fir[, c(16:18, 34, 35)]))
phi_L1_lm67819_con[, 2] <- rowSums(abs(d_con_mid[, c(16:18, 34, 35)]))
phi_L1_lm67819_con[, 3] <- rowSums(abs(d_con_la[, c(16:18, 34, 35)]))

wilcox.test(phi_L1_lm67819_con[, 1], phi_L1_lm67819_cle[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm67819_con[, 2], phi_L1_lm67819_cle[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm67819_con[, 3], phi_L1_lm67819_cle[, 3], 
            alternative = 'less')
#4.4.1.2 \phi_{L_2}###############################
phi_L2_lm67819_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L2_lm67819_cle[, 1] <- rowSums(d_cle_fir[, c(16:18, 34, 35)]^2)
phi_L2_lm67819_cle[, 2] <- rowSums(d_cle_mid[, c(16:18, 34, 35)]^2)
phi_L2_lm67819_cle[, 3] <- rowSums(d_cle_la[, c(16:18, 34, 35)]^2)

phi_L2_lm67819_con <- matrix(0, nrow = 12, ncol = 3)
phi_L2_lm67819_con[, 1] <- rowSums(d_con_fir[, c(16:18, 34, 35)]^2)
phi_L2_lm67819_con[, 2] <- rowSums(d_con_mid[, c(16:18, 34, 35)]^2)
phi_L2_lm67819_con[, 3] <- rowSums(d_con_la[, c(16:18, 34, 35)]^2)

wilcox.test(phi_L2_lm67819_con[, 1], phi_L2_lm67819_cle[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm67819_con[, 2], phi_L2_lm67819_cle[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm67819_con[, 3], phi_L2_lm67819_cle[, 3], 
            alternative = 'less')
#4.4.2 Corner pair and solos########################
#4.4.2.1 \phi_{L_1}###############################
phi_L1_lm171319_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L1_lm171319_cle[, 1] <- rowSums(abs(d_cle_fir[, c(1:3, 34, 35)]))
phi_L1_lm171319_cle[, 2] <- rowSums(abs(d_cle_mid[, c(1:3, 34, 35)]))
phi_L1_lm171319_cle[, 3] <- rowSums(abs(d_cle_la[, c(1:3, 34, 35)]))

phi_L1_lm171319_con <- matrix(0, nrow = 12, ncol = 3)
phi_L1_lm171319_con[, 1] <- rowSums(abs(d_con_fir[, c(1:3, 34, 35)]))
phi_L1_lm171319_con[, 2] <- rowSums(abs(d_con_mid[, c(1:3, 34, 35)]))
phi_L1_lm171319_con[, 3] <- rowSums(abs(d_con_la[, c(1:3, 34, 35)]))

wilcox.test(phi_L1_lm171319_con[, 1], phi_L1_lm171319_cle[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm171319_con[, 2], phi_L1_lm171319_cle[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm171319_con[, 3], phi_L1_lm171319_cle[, 3], 
            alternative = 'less')
#4.4.2.2 \phi_{L_2}###############################
phi_L2_lm171319_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L2_lm171319_cle[, 1] <- rowSums(d_cle_fir[, c(1:3, 34, 35)]^2)
phi_L2_lm171319_cle[, 2] <- rowSums(d_cle_mid[, c(1:3, 34, 35)]^2)
phi_L2_lm171319_cle[, 3] <- rowSums(d_cle_la[, c(1:3, 34, 35)]^2)

phi_L2_lm171319_con <- matrix(0, nrow = 12, ncol = 3)
phi_L2_lm171319_con[, 1] <- rowSums(d_con_fir[, c(1:3, 34, 35)]^2)
phi_L2_lm171319_con[, 2] <- rowSums(d_con_mid[, c(1:3, 34, 35)]^2)
phi_L2_lm171319_con[, 3] <- rowSums(d_con_la[, c(1:3, 34, 35)]^2)

wilcox.test(phi_L2_lm171319_con[, 1], phi_L2_lm171319_cle[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm171319_con[, 2], phi_L2_lm171319_cle[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm171319_con[, 3], phi_L2_lm171319_cle[, 3], 
            alternative = 'less')
#4.4.3 Corner, central pairs and solos#################
#4.4.3.1 \phi_{L_1}###############################
phi_L1_lm16781319_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L1_lm16781319_cle[, 1] <- rowSums(abs(d_cle_fir[, c(1:3, 16:18, 34, 35)]))
phi_L1_lm16781319_cle[, 2] <- rowSums(abs(d_cle_mid[, c(1:3, 16:18, 34, 35)]))
phi_L1_lm16781319_cle[, 3] <- rowSums(abs(d_cle_la[, c(1:3, 16:18, 34, 35)]))

phi_L1_lm16781319_con <- matrix(0, nrow = 12, ncol = 3)
phi_L1_lm16781319_con[, 1] <- rowSums(abs(d_con_fir[, c(1:3, 16:18, 34, 35)]))
phi_L1_lm16781319_con[, 2] <- rowSums(abs(d_con_mid[, c(1:3, 16:18, 34, 35)]))
phi_L1_lm16781319_con[, 3] <- rowSums(abs(d_con_la[, c(1:3, 16:18, 34, 35)]))

wilcox.test(phi_L1_lm16781319_con[, 1], phi_L1_lm16781319_cle[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm16781319_con[, 2], phi_L1_lm16781319_cle[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm16781319_con[, 3], phi_L1_lm16781319_cle[, 3], 
            alternative = 'less')
#4.4.3.2 \phi_{L_2}###############################
phi_L2_lm16781319_cle <- matrix(0, nrow = 13, ncol = 3)
phi_L2_lm16781319_cle[, 1] <- rowSums(d_cle_fir[, c(1:3, 16:18, 34, 35)]^2)
phi_L2_lm16781319_cle[, 2] <- rowSums(d_cle_mid[, c(1:3, 16:18, 34, 35)]^2)
phi_L2_lm16781319_cle[, 3] <- rowSums(d_cle_la[, c(1:3, 16:18, 34, 35)]^2)

phi_L2_lm16781319_con <- matrix(0, nrow = 12, ncol = 3)
phi_L2_lm16781319_con[, 1] <- rowSums(d_con_fir[, c(1:3, 16:18, 34, 35)]^2)
phi_L2_lm16781319_con[, 2] <- rowSums(d_con_mid[, c(1:3, 16:18, 34, 35)]^2)
phi_L2_lm16781319_con[, 3] <- rowSums(d_con_la[, c(1:3, 16:18, 34, 35)]^2)

wilcox.test(phi_L2_lm16781319_con[, 1], phi_L2_lm16781319_cle[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm16781319_con[, 2], phi_L2_lm16781319_cle[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm16781319_con[, 3], phi_L2_lm16781319_cle[, 3], 
            alternative = 'less')
##################################################
#5. Meta-analysis#################################
#5.1 Fisher's method############################
#5.1.1 Obtain separate p-values####################
p_wilcoxon_fir <- apply(clecon_fir, 2, function(a) {
  wilcox.test(abs(a[14:25]), abs(a[1:13]), alternative = 'less')$p.value
})

p_wilcoxon_mid <- apply(clecon_mid, 2, function(a) {
  wilcox.test(abs(a[14:25]), abs(a[1:13]), alternative = 'less')$p.value
})

p_wilcoxon_la <- apply(clecon_la, 2, function(a) {
  wilcox.test(abs(a[14:25]), abs(a[1:13]), alternative = 'less')$p.value
})
#5.1.2 Overall p-values##############################
1 - pchisq(-2*sum(log(p_wilcoxon_fir)), df = 70)

1 - pchisq(-2*sum(log(p_wilcoxon_mid)), df = 70)

1 - pchisq(-2*sum(log(p_wilcoxon_la)), df = 70)
#5.2 Pearson's method#################################
#5.2.1 First frame###################################
p_pearson_fir <- c()
p_rand_wilcox <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_fir[cleft_id, ]
  control_boot <- clecon_fir[control_id, ]
  
  p_rand_wilcox <- apply(rbind(cleft_boot, control_boot), 2, 
                             function(a) {
    wilcox.test(abs(a[14:25]), abs(a[1:13]), 
                alternative = 'less')$p.value
  })
  
  p_pearson_fir[i] <- max(-2*sum(log(p_rand_wilcox)),
                              -2*sum(log(1-p_rand_wilcox)))
}
mean(p_pearson_fir >= -2*sum(log(p_wilcoxon_fir)))
#5.2.2 Middle frame##########################
p_pearson_mid <- c()
p_rand_wilcox <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_mid[cleft_id, ]
  control_boot <- clecon_mid[control_id, ]
  
  p_rand_wilcox <- apply(rbind(cleft_boot, control_boot), 2, 
                         function(a) {
                           wilcox.test(abs(a[14:25]), abs(a[1:13]), 
                                       alternative = 'less')$p.value
                         })
  
  p_pearson_mid[i] <- max(-2*sum(log(p_rand_wilcox)),
                          -2*sum(log(1-p_rand_wilcox)))
}
mean(p_pearson_mid >= -2*sum(log(p_wilcoxon_mid)))
#5.2.3 Last frame##########################
p_pearson_la <- c()
p_rand_wilcox <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_la[cleft_id, ]
  control_boot <- clecon_la[control_id, ]
  
  p_rand_wilcox <- apply(rbind(cleft_boot, control_boot), 2, 
                         function(a) {
        wilcox.test(abs(a[14:25]), abs(a[1:13]), 
                                       alternative = 'less')$p.value
                         })
  
  p_pearson_la[i] <- max(-2*sum(log(p_rand_wilcox)),
                          -2*sum(log(1-p_rand_wilcox)))
}
mean(p_pearson_la >= -2*sum(log(p_wilcoxon_la)))
##########################################
#6. Hotelling's T^2 test########################
#6.1 First frame#############################
S_fir <- (12*cov(abs(d_cle_fir))+11*cov(abs(d_con_fir)))/(12+11)
T2_fir <- t(colMeans(abs(d_cle_fir))-colMeans(abs(d_con_fir))) %*% 
  ginv(S_fir*(1/13+1/12)) %*% 
  (colMeans(abs(d_cle_fir))-colMeans(abs(d_con_fir)))

T2_fir_rand <- c()
for (i in 1:10000) {
  #Sample with replacement
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- abs(clecon_fir)[cleft_id, ]
  control_boot <- abs(clecon_fir)[control_id, ]
  #Form test statistic
  S_boot <- (12*cov(cleft_boot)+11*cov(control_boot))/(12+11)
  T2_fir_rand[i] <- t(colMeans(cleft_boot)-colMeans(control_boot)) %*% 
    ginv(S_boot*(1/13+1/12)) %*% 
    (colMeans(cleft_boot)-colMeans(control_boot))
}
mean(T2_fir_rand >= T2_fir[1, 1])

#6.2 Middle frame###############################
S_mid <- (12*cov(abs(d_cle_mid))+11*cov(abs(d_con_mid)))/(12+11)
T2_mid <- t(colMeans(abs(d_cle_mid))-colMeans(abs(d_con_mid))) %*% 
  ginv(S_mid*(1/13+1/12)) %*% 
  (colMeans(abs(d_cle_mid))-colMeans(abs(d_con_mid)))

T2_mid_rand <- c()
for (i in 1:10000) {
  #Sample with replacement
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- abs(clecon_mid)[cleft_id, ]
  control_boot <- abs(clecon_mid)[control_id, ]
  #Form test statistic
  S_boot <- (12*cov(cleft_boot)+11*cov(control_boot))/(12+11)
  T2_mid_rand[i] <- t(colMeans(cleft_boot)-colMeans(control_boot)) %*% 
    ginv(S_boot*(1/13+1/12)) %*% 
    (colMeans(cleft_boot)-colMeans(control_boot))
}
mean(T2_mid_rand >= T2_mid[1, 1])

#6.3 Last frame##################################
S_la <- (12*cov(abs(d_cle_la))+11*cov(abs(d_con_la)))/(12+11)
T2_la <- t(colMeans(abs(d_cle_la))-colMeans(abs(d_con_la))) %*% 
  ginv(S_la*(1/13+1/12)) %*% 
  (colMeans(abs(d_cle_la))-colMeans(abs(d_con_la)))

T2_la_rand <- c()
for (i in 1:10000) {
  #Sample with replacement
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- abs(clecon_la)[cleft_id, ]
  control_boot <- abs(clecon_la)[control_id, ]
  #Form test statistic
  S_boot <- (12*cov(cleft_boot)+11*cov(control_boot))/(12+11)
  T2_la_rand[i] <- t(colMeans(cleft_boot)-colMeans(control_boot)) %*% 
    ginv(S_boot*(1/13+1/12)) %*% 
    (colMeans(cleft_boot)-colMeans(control_boot))
}
mean(T2_la_rand >= T2_la[1, 1])
###################################################
#7. Dot plots######################################
#7.1 Three composite scores########################
#7.1.1 First frame#############################
plot(phi_star_L1_cle[, 1], rep(5, 13), ylim = c(-0.5, 5.5), 
     pch = 19, xlim = c(0, 50),
     xlab = 'asymmetry measurements', ylab = 'first frame', yaxt = 'n')
axis(2, at = c(0, 1, 2, 3, 4, 5), 
     labels = c('con', 'cle', 'con', 'cle', 'con', 'cle'))
points(phi_star_L1_con[, 1], rep(4, 12), pch = 19)
points(phi_L1_cle[, 1]/2, rep(3, 13), pch = 2)
points(phi_L1_con[, 1]/2, rep(2, 12), pch = 2)
points(phi_L2_cle[, 1]/2, rep(1, 13), pch = 4)
points(phi_L2_con[, 1]/2, rep(0, 12), pch = 4)
#7.1.2 Middle frame############################
plot(phi_star_L1_cle[, 2], rep(5, 13), ylim = c(-0.5, 5.5), 
     pch = 19, xlim = c(0, 80),
     xlab = 'asymmetry measurements', ylab = 'middle frame', yaxt = 'n')
axis(2, at = c(0, 1, 2, 3, 4, 5), 
     labels = c('con', 'cle', 'con', 'cle', 'con', 'cle'))
points(phi_star_L1_con[, 2], rep(4, 12), pch = 19)
points(phi_L1_cle[, 2], rep(3, 13), pch = 2)
points(phi_L1_con[, 2], rep(2, 12), pch = 2)
points(phi_L2_cle[, 2]/3, rep(1, 13), pch = 4)
points(phi_L2_con[, 2]/3, rep(0, 12), pch = 4)
#7.1.3 Last frame############################
plot(phi_star_L1_cle[, 3], rep(5, 13), ylim = c(-0.5, 5.5), 
     pch = 19, xlim = c(0, 100),
     xlab = 'asymmetry measurements', ylab = 'last frame', yaxt = 'n')
axis(2, at = c(0, 1, 2, 3, 4, 5), 
     labels = c('con', 'cle', 'con', 'cle', 'con', 'cle'))
points(phi_star_L1_con[, 3], rep(4, 12), pch = 19)
points(phi_L1_cle[, 3], rep(3, 13), pch = 2)
points(phi_L1_con[, 3], rep(2, 12), pch = 2)
points(phi_L2_cle[, 3]/3, rep(1, 13), pch = 4)
points(phi_L2_con[, 3]/3, rep(0, 12), pch = 4)
#7.2 Weighted \phi_{L_1} and \phi_{L_2}########################
#7.2.1 First frame####################################
plot(phi_L1_cle_scale[, 1], rep(3, 13), ylim = c(-0.5, 3.5), pch = 19,
     xlim = c(0, 19),
     xlab = 'Weighted L1 and L2 statistics', ylab = 'first frame', 
     yaxt = 'n')
axis(2, at = c(0, 1, 2, 3), labels = c('con', 'cle', 'con', 'cle'))
points(phi_L1_con_scale[, 1], rep(2, 12), pch = 19)
points(phi_L2_cle_scale[, 1], rep(1, 13), pch = 2)
points(phi_L2_con_scale[, 1], rep(0, 12), pch = 2)
#7.2.2 Middle frame####################################
plot(phi_L1_cle_scale[, 2], rep(3, 13), ylim = c(-0.5, 3.5), pch = 19,
     xlim = c(0, 22),
     xlab = 'Weighted L1 and L2 statistics', ylab = 'middle frame', 
     yaxt = 'n')
axis(2, at = c(0, 1, 2, 3), labels = c('con', 'cle', 'con', 'cle'))
points(phi_L1_con_scale[, 2], rep(2, 12), pch = 19)
points(phi_L2_cle_scale[, 2], rep(1, 13), pch = 2)
points(phi_L2_con_scale[, 2], rep(0, 12), pch = 2)
#7.2.3 Last frame####################################
plot(phi_L1_cle_scale[, 3], rep(3, 13), ylim = c(-0.5, 3.5), pch = 19,
     xlim = c(0, 30),
     xlab = 'Weighted L1 and L2 statistics', ylab = 'last frame', 
     yaxt = 'n')
axis(2, at = c(0, 1, 2, 3), labels = c('con', 'cle', 'con', 'cle'))
points(phi_L1_con_scale[, 3], rep(2, 12), pch = 19)
points(phi_L2_cle_scale[, 3], rep(1, 13), pch = 2)
points(phi_L2_con_scale[, 3], rep(0, 12), pch = 2)





