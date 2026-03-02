########################################
#Codes for performing analyses on the cleft lip data
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
#0.3 Plot of lip configuration######################
plot(control_data[[1]][[1]][, 1], control_data[[1]][[1]][, 2], asp = 1,
     xlab = 'x', ylab = 'y', 
     main = 'Landmarks for Cleft Data 2', pch = 19,
     xlim = c(-15, 15), ylim = c(-26, -12))
lines(control_data[[1]][[1]][c(1:24, 1), 1], 
      control_data[[1]][[1]][c(1:24, 1), 2], lwd = 2)
text(control_data[[1]][[1]][, 1], 
     c(control_data[[1]][[1]][1:13, 2] + 1,
       control_data[[1]][[1]][14:24, 2] - 1),
     1:24)
#################################################
#1. Asymmetry features defined in (2.3)-(2.5)######################
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
#2. Composite score defined in (2.9)###########################
#2.1 Cleft##################################
u_L2_cle <- matrix(0, nrow = 13, ncol = 3)
u_L2_cle[, 1] <- rowSums(d_cle_fir^2)
u_L2_cle[, 2] <- rowSums(d_cle_mid^2)
u_L2_cle[, 3] <- rowSums(d_cle_la^2)
#2.2 Control##################################
u_L2_con <- matrix(0, nrow = 12, ncol = 3)
u_L2_con[, 1] <- rowSums(d_con_fir^2)
u_L2_con[, 2] <- rowSums(d_con_mid^2)
u_L2_con[, 3] <- rowSums(d_con_la^2)
###########################################
#3. Composite score test############################
wilcox.test(u_L2_con[, 1], u_L2_cle[, 1], alternative = 'less')

wilcox.test(u_L2_con[, 2], u_L2_cle[, 2], alternative = 'less')

wilcox.test(u_L2_con[, 3], u_L2_cle[, 3], alternative = 'less')
###################################################
#4. UIT approach#######################################
#4.1 Compute T_{Cj}##############################
Uval_fir <- c()
for (i in 1:35) {
  Uval_fir[i] <- wilcox.test(abs(d_cle_fir)[, i], abs(d_con_fir)[, i],
                        alternative = 'great')$statistic
}

Uval_mid <- c()
for (i in 1:35) {
  Uval_mid[i] <- wilcox.test(abs(d_cle_mid)[, i], abs(d_con_mid)[, i],
                             alternative = 'great')$statistic
}

Uval_la <- c()
for (i in 1:35) {
  Uval_la[i] <- wilcox.test(abs(d_cle_la)[, i], abs(d_con_la)[, i],
                             alternative = 'great')$statistic
}
#4.2 Combine the data##########################
clecon_fir <- rbind(abs(d_cle_fir), abs(d_con_fir))

clecon_mid <- rbind(abs(d_cle_mid), abs(d_con_mid))

clecon_la <- rbind(abs(d_cle_la), abs(d_con_la))
#4.3 Permutationt test####################################
#4.3.1 First frame################################
Uboot_fir <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_fir[cleft_id, ]
  control_boot <- clecon_fir[control_id, ]
  
  U_va <- c()
  for (j in 1:35) {
    U_va[j] <- wilcox.test(abs(cleft_boot)[, j], abs(control_boot)[, j],
                           alternative = 'great')$statistic
  }
  Uboot_fir[i] <- max(U_va)
}
which(Uval_fir > quantile(Uboot_fir, 0.95))
mean(Uboot_fir >= max(Uval_fir))
#4.3.2 Middle frame################################
Uboot_mid <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_mid[cleft_id, ]
  control_boot <- clecon_mid[control_id, ]
  
  U_va <- c()
  for (j in 1:35) {
    U_va[j] <- wilcox.test(abs(cleft_boot)[, j], abs(control_boot)[, j],
                           alternative = 'great')$statistic
  }
  Uboot_mid[i] <- max(U_va)
}
which(Uval_mid > quantile(Uboot_mid, 0.95))
mean(Uboot_mid >= max(Uval_mid))
#4.3.3 Last frame################################
Uboot_la <- c()
for (i in 1:10000) {
  id <- sample(1:25, 25, replace = F)
  cleft_id <- id[1:13]
  control_id <- id[14:25]
  cleft_boot <- clecon_la[cleft_id, ]
  control_boot <- clecon_la[control_id, ]
  
  U_va <- c()
  for (j in 1:35) {
    U_va[j] <- wilcox.test(abs(cleft_boot)[, j], abs(control_boot)[, j],
                           alternative = 'great')$statistic
  }
  Uboot_la[i] <- max(U_va)
}
which(Uval_la > quantile(Uboot_la, 0.95))
mean(Uboot_la >= max(Uval_la))
############################################
#5. Dot plots################################
plot(u_L2_cle[, 1], rep(5, 13), ylim = c(-2, 5.5), 
     pch = 19, xlim = range(rbind(u_L2_cle, u_L2_con)),
     xlab = 'Composite asymmetry score', ylab = 'Time frames', 
     yaxt = 'n', cex = 1.3)
axis(2, at = c(-1.5, -1, 1.5, 2, 4.5, 5), 
     labels = c('last', 'frame', 'middle', 'frame', 'first', 'frame'))
points(u_L2_con[, 1], rep(4.5, 12), pch = 1, cex = 1.3)
points(u_L2_cle[, 2], rep(2, 13), pch = 19, cex = 1.3)
points(u_L2_con[, 2], rep(1.5, 12), pch = 1, cex = 1.3)
points(u_L2_cle[, 3], rep(-1, 13), pch = 19, cex = 1.3)
points(u_L2_con[, 3], rep(-1.5, 12), pch = 1, cex = 1.3)
legend('topright', pch = c(19, 1), legend = c('cleft', 'control'))





































