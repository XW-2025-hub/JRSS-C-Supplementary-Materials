########################################
#Codes for performing analyses on the jaw data and
#109 control subjects from the cleft lip data.
#We focus on the 10 landmarks at the lip.
##########################################
#Centering matrix
Cen <- diag(10) - (1/10)*matrix(1, nrow = 10, ncol = 10)
#0. Read in the data for control subjects from cleft lip data############
con_fr1_Data1 <- read.csv("C:/Users/mmxw/Desktop/R/Data1/con_fr1.csv")

#middle frame
con_fr7_Data1 <- read.csv("C:/Users/mmxw/Desktop/R/Data1/con_fr7.csv")

#last frame
con_fr13_Data1 <- read.csv("C:/Users/mmxw/Desktop/R/Data1/con_fr13.csv")
#0.1 Convert them into arrays########
#first frame
con_fir_arr <- array(0, dim = c(10, 3, 109))
for (i in 1:109) {
  con_fir_arr[, , i] <- Cen %*%
    matrix(unlist(con_fr1_Data1[i, ]), nrow = 10)
}

#middle frame
con_mid_arr <- array(0, dim = c(10, 3, 109))
for (i in 1:109) {
  con_mid_arr[, , i] <- Cen %*%
    matrix(unlist(con_fr7_Data1[i, ]), nrow = 10)
}

#last frame
con_la_arr <- array(0, dim = c(10, 3, 109))
for (i in 1:109) {
  con_la_arr[, , i] <- Cen %*%
    matrix(unlist(con_fr13_Data1[i, ]), nrow = 10)
}
#0.2 Centre and scale the orthognathic surgery data################
#0.2.1 Pre-surgery#################################
#First frame
jawpre_arr_cen_fir <- array(0, dim = c(10, 3, 22))
for (i in 1:22) {
  jawpre_arr_cen_fir[, , i] <- Cen %*% 
    jawpre_std_arr_fir[5:14, , i]/10
}

#Middle frame
jawpre_arr_cen_mid <- array(0, dim = c(10, 3, 22))
for (i in 1:22) {
  jawpre_arr_cen_mid[, , i] <- Cen %*% 
    jawpre_std_arr_mid[5:14, , i]/10
}

#Last frame
jawpre_arr_cen_la <- array(0, dim = c(10, 3, 22))
for (i in 1:22) {
  jawpre_arr_cen_la[, , i] <- Cen %*% 
    jawpre_std_arr_la[5:14, , i]/10
}
#0.2.2 Post-surgery######################
jawpost_arr_cen_fir <- array(0, dim = c(10, 3, 22))
for (i in 1:22) {
  jawpost_arr_cen_fir[, , i] <- Cen %*% 
    jawpost_std_arr_fir[5:14, , i]/10
}

jawpost_arr_cen_mid <- array(0, dim = c(10, 3, 22))
for (i in 1:22) {
  jawpost_arr_cen_mid[, , i] <- Cen %*% 
    jawpost_std_arr_mid[5:14, , i]/10
}

jawpost_arr_cen_la <- array(0, dim = c(10, 3, 22))
for (i in 1:22) {
  jawpost_arr_cen_la[, , i] <- Cen %*% 
    jawpost_std_arr_la[5:14, , i]/10
}
###########################################
#1. Asymmetry features###################
#1.1 Control##############################
d_con_fir <- matrix(0, nrow = 109, ncol = 14)
for (i in 1:109) {
  dif_matrix <- con_fir_arr[, , i] - 
    (con_fir_arr[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_con_fir[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
                      dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}

#Middle frame
d_con_mid <- matrix(0, nrow = 109, ncol = 14)
for (i in 1:109) {
  dif_matrix <- con_mid_arr[, , i] - 
    (con_mid_arr[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_con_mid[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
                      dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}

#Last frame
d_con_la <- matrix(0, nrow = 109, ncol = 14)
for (i in 1:109) {
  dif_matrix <- con_la_arr[, , i] - 
    (con_la_arr[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_con_la[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
                     dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}
#1.2 Pre-surgery data########################
d_pre_fir_lip <- matrix(0, nrow = 22, ncol = 14)
for (i in 1:22) {
  dif_matrix <- jawpre_arr_cen_fir[, , i] - 
    (jawpre_arr_cen_fir[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_pre_fir_lip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
          dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}

#Middle frame
d_pre_mid_lip <- matrix(0, nrow = 22, ncol = 14)
for (i in 1:22) {
  dif_matrix <- jawpre_arr_cen_mid[, , i] - 
    (jawpre_arr_cen_mid[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_pre_mid_lip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
                         dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}

#Last frame
d_pre_la_lip <- matrix(0, nrow = 22, ncol = 14)
for (i in 1:22) {
  dif_matrix <- jawpre_arr_cen_la[, , i] - 
    (jawpre_arr_cen_la[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_pre_la_lip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
                        dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}
#1.3 Post-surgery###########################
d_post_fir_lip <- matrix(0, nrow = 22, ncol = 14)
for (i in 1:22) {
  dif_matrix <- jawpost_arr_cen_fir[, , i] - 
    (jawpost_arr_cen_fir[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_post_fir_lip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
          dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}

#Middle frame
d_post_mid_lip <- matrix(0, nrow = 22, ncol = 14)
for (i in 1:22) {
  dif_matrix <- jawpost_arr_cen_mid[, , i] - 
    (jawpost_arr_cen_mid[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_post_mid_lip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
                         dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}

#Last frame
d_post_la_lip <- matrix(0, nrow = 22, ncol = 14)
for (i in 1:22) {
  dif_matrix <- jawpost_arr_cen_la[, , i] - 
    (jawpost_arr_cen_la[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  d_post_la_lip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8), ]), nrow = 1),
                        dif_matrix[4, 1]/2, dif_matrix[9, 1]/2)
}
##########################################
#2. Composite asymmetry scores##################
#2.1 \phi^*_{L_1} in (3.9)######################
#2.1.1 Control subjects#######################
phistar_L1_con_lip <- matrix(0, nrow = 109, ncol = 3)
#First frame
for (i in 1:109) {
  dif_matrix <- con_fir_arr[, , i] - 
    (con_fir_arr[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_con_lip[i, 1] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}

#Middle frame
for (i in 1:109) {
  dif_matrix <- con_mid_arr[, , i] - 
    (con_mid_arr[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_con_lip[i, 2] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}

#Last frame
for (i in 1:109) {
  dif_matrix <- con_la_arr[, , i] - 
    (con_la_arr[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_con_lip[i, 3] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}
#2.1.2 Pre-surgery data#######################
phistar_L1_pre_lip <- matrix(0, nrow = 22, ncol = 3)
#First frame
for (i in 1:22) {
  dif_matrix <- jawpre_arr_cen_fir[, , i] - 
    (jawpre_arr_cen_fir[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_pre_lip[i, 1] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}

#Middle frame
for (i in 1:22) {
  dif_matrix <- jawpre_arr_cen_mid[, , i] - 
    (jawpre_arr_cen_mid[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_pre_lip[i, 2] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}

#Last frame
for (i in 1:22) {
  dif_matrix <- jawpre_arr_cen_la[, , i] - 
    (jawpre_arr_cen_la[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_pre_lip[i, 3] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}
#2.1.3 Post-surgery data#######################
phistar_L1_post_lip <- matrix(0, nrow = 22, ncol = 3)
#First frame
for (i in 1:22) {
  dif_matrix <- jawpost_arr_cen_fir[, , i] - 
    (jawpost_arr_cen_fir[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_post_lip[i, 1] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
          abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}

#Middle frame
for (i in 1:22) {
  dif_matrix <- jawpost_arr_cen_mid[, , i] - 
    (jawpost_arr_cen_mid[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_post_lip[i, 2] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
          abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}

#Last frame
for (i in 1:22) {
  dif_matrix <- jawpost_arr_cen_la[, , i] - 
    (jawpost_arr_cen_la[, , i] %*% H)[c(7:5, 4, 3:1, 10, 9, 8), ]
  phistar_L1_post_lip[i, 3] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8), ])^2)),
          abs(dif_matrix[4, 1])/2, abs(dif_matrix[9, 1])/2))
}
#2.2 \phi_{L_1} in (3.6)###############################
#2.2.1 Control#############################
phi_L1_con <- matrix(0, nrow = 109, ncol = 3)
phi_L1_con[, 1] <- rowSums(abs(d_con_fir))
phi_L1_con[, 2] <- rowSums(abs(d_con_mid))
phi_L1_con[, 3] <- rowSums(abs(d_con_la))
#2.2.2 Pre-surgery#############################
phi_L1_pre_lip <- matrix(0, nrow = 22, ncol = 3)
phi_L1_pre_lip[, 1] <- rowSums(abs(d_pre_fir_lip))
phi_L1_pre_lip[, 2] <- rowSums(abs(d_pre_mid_lip))
phi_L1_pre_lip[, 3] <- rowSums(abs(d_pre_la_lip))
#2.2.3 Post-surgery#############################
phi_L1_post_lip <- matrix(0, nrow = 22, ncol = 3)
phi_L1_post_lip[, 1] <- rowSums(abs(d_post_fir_lip))
phi_L1_post_lip[, 2] <- rowSums(abs(d_post_mid_lip))
phi_L1_post_lip[, 3] <- rowSums(abs(d_post_la_lip))
#2.3 \phi_{L_2} in (3.7)#########################
#2.3.1 Control############################
phi_L2_con <- matrix(0, nrow = 109, ncol = 3)
phi_L2_con[, 1] <- rowSums(abs(d_con_fir)^2)
phi_L2_con[, 2] <- rowSums(abs(d_con_mid)^2)
phi_L2_con[, 3] <- rowSums(abs(d_con_la)^2)
#2.3.2 Pre-surgery#############################
phi_L2_pre_lip <- matrix(0, nrow = 22, ncol = 3)
phi_L2_pre_lip[, 1] <- rowSums(abs(d_pre_fir_lip)^2)
phi_L2_pre_lip[, 2] <- rowSums(abs(d_pre_mid_lip)^2)
phi_L2_pre_lip[, 3] <- rowSums(abs(d_pre_la_lip)^2)
#2.3.3 Post-surgery#############################
phi_L2_post_lip <- matrix(0, nrow = 22, ncol = 3)
phi_L2_post_lip[, 1] <- rowSums(abs(d_post_fir_lip)^2)
phi_L2_post_lip[, 2] <- rowSums(abs(d_post_mid_lip)^2)
phi_L2_post_lip[, 3] <- rowSums(abs(d_post_la_lip)^2)
#2.4 weighted \phi_{L_1} in (3.6)#################
#2.4.1 Control#############################
phi_L1_con_wei <- matrix(0, nrow = 109, ncol = 3)

phi_L1_con_wei[, 1] <- rowSums(abs(d_con_fir[, 1:12])) + 
         2*abs(d_con_fir[, 13]) + 2*abs(d_con_fir[, 14])

phi_L1_con_wei[, 2] <- rowSums(abs(d_con_mid[, 1:12])) + 
         2*abs(d_con_mid[, 13]) + 2*abs(d_con_mid[, 14])

phi_L1_con_wei[, 3] <- rowSums(abs(d_con_la[, 1:12])) + 
         2*abs(d_con_la[, 13]) + 2*abs(d_con_la[, 14])
#2.4.2 Pre-surgery#############################
phi_L1_pre_wei <- matrix(0, nrow = 22, ncol = 3)

phi_L1_pre_wei[, 1] <- rowSums(abs(d_pre_fir_lip[, 1:12])) + 
  2*abs(d_pre_fir_lip[, 13]) + 2*abs(d_pre_fir_lip[, 14])

phi_L1_pre_wei[, 2] <- rowSums(abs(d_pre_mid_lip[, 1:12])) + 
  2*abs(d_pre_mid_lip[, 13]) + 2*abs(d_pre_mid_lip[, 14])

phi_L1_pre_wei[, 3] <- rowSums(abs(d_pre_la_lip[, 1:12])) + 
  2*abs(d_pre_la_lip[, 13]) + 2*abs(d_pre_la_lip[, 14])
#2.4.3 Post-surgery#############################
phi_L1_post_wei <- matrix(0, nrow = 22, ncol = 3)

phi_L1_post_wei[, 1] <- rowSums(abs(d_post_fir_lip[, 1:12])) + 
  2*abs(d_post_fir_lip[, 13]) + 2*abs(d_post_fir_lip[, 14])

phi_L1_post_wei[, 2] <- rowSums(abs(d_post_mid_lip[, 1:12])) + 
  2*abs(d_post_mid_lip[, 13]) + 2*abs(d_post_mid_lip[, 14])

phi_L1_post_wei[, 3] <- rowSums(abs(d_post_la_lip[, 1:12])) + 
  2*abs(d_post_la_lip[, 13]) + 2*abs(d_post_la_lip[, 14])
#2.5 weighted \phi_{L_2} in (3.7)#################
#2.5.1 Control#############################
phi_L2_con_wei <- matrix(0, nrow = 109, ncol = 3)

phi_L2_con_wei[, 1] <- rowSums(abs(d_con_fir[, 1:12])^2) + 
  (2*abs(d_con_fir[, 13]))^2 + (2*abs(d_con_fir[, 14]))^2

phi_L2_con_wei[, 2] <- rowSums(abs(d_con_mid[, 1:12])^2) + 
  (2*abs(d_con_mid[, 13]))^2 + (2*abs(d_con_mid[, 14]))^2

phi_L2_con_wei[, 3] <- rowSums(abs(d_con_la[, 1:12])^2) + 
  (2*abs(d_con_la[, 13]))^2 + (2*abs(d_con_la[, 14]))^2
#2.5.2 Pre-surgery#############################
phi_L2_pre_wei <- matrix(0, nrow = 22, ncol = 3)

phi_L2_pre_wei[, 1] <- rowSums(abs(d_pre_fir_lip[, 1:12])^2) + 
  (2*abs(d_pre_fir_lip[, 13]))^2 + (2*abs(d_pre_fir_lip[, 14]))^2

phi_L2_pre_wei[, 2] <- rowSums(abs(d_pre_mid_lip[, 1:12])^2) + 
  (2*abs(d_pre_mid_lip[, 13]))^2 + (2*abs(d_pre_mid_lip[, 14]))^2

phi_L2_pre_wei[, 3] <- rowSums(abs(d_pre_la_lip[, 1:12])^2) + 
  (2*abs(d_pre_la_lip[, 13]))^2 + (2*abs(d_pre_la_lip[, 14]))^2
#2.5.3 Post-surgery#############################
phi_L2_post_wei <- matrix(0, nrow = 22, ncol = 3)

phi_L2_post_wei[, 1] <- rowSums(abs(d_post_fir_lip[, 1:12])^2) + 
  (2*abs(d_post_fir_lip[, 13]))^2 + (2*abs(d_post_fir_lip[, 14]))^2

phi_L2_post_wei[, 2] <- rowSums(abs(d_post_mid_lip[, 1:12])^2) + 
  (2*abs(d_post_mid_lip[, 13]))^2 + (2*abs(d_post_mid_lip[, 14]))^2

phi_L2_post_wei[, 3] <- rowSums(abs(d_post_la_lip[, 1:12])^2) + 
  (2*abs(d_post_la_lip[, 13]))^2 + (2*abs(d_post_la_lip[, 14]))^2
##################################################
#3. Univariate tests#############################
#3.1 t-test################################
#Unweighted##############################
#3.1.1 Pre-surgery data and control###################
t.test(phistar_L1_con_lip[, 1], phistar_L1_pre_lip[, 1],
       alternative = 'less')

t.test(phistar_L1_con_lip[, 2], phistar_L1_pre_lip[, 2],
       alternative = 'less')

t.test(phistar_L1_con_lip[, 3], phistar_L1_pre_lip[, 3],
       alternative = 'less')
#3.1.2 Post-surgery data and control###################
t.test(phistar_L1_con_lip[, 1], phistar_L1_post_lip[, 1],
       alternative = 'less')

t.test(phistar_L1_con_lip[, 2], phistar_L1_post_lip[, 2],
       alternative = 'less')

t.test(phistar_L1_con_lip[, 3], phistar_L1_post_lip[, 3],
       alternative = 'less')
#3.2 Mann-Whitney U test############################
#3.2.1 \phi_{L_1}##########################
#3.2.1.1 Pre-surgery data and control###################
wilcox.test(phi_L1_con[, 1], phi_L1_pre_lip[, 1],
            alternative = 'less')

wilcox.test(phi_L1_con[, 2], phi_L1_pre_lip[, 2],
            alternative = 'less')

wilcox.test(phi_L1_con[, 3], phi_L1_pre_lip[, 3],
            alternative = 'less')
#3.2.1.2 Post-surgery data and control###################
wilcox.test(phi_L1_con[, 1], phi_L1_post_lip[, 1],
            alternative = 'less')

wilcox.test(phi_L1_con[, 2], phi_L1_post_lip[, 2],
            alternative = 'less')

wilcox.test(phi_L1_con[, 3], phi_L1_post_lip[, 3],
            alternative = 'less')
#3.2.2  \phi_{L_2}#############################
#3.2.2.1 Pre-surgery data and control###################
wilcox.test(phi_L2_con[, 1], phi_L2_pre_lip[, 1],
            alternative = 'less')

wilcox.test(phi_L2_con[, 2], phi_L2_pre_lip[, 2],
            alternative = 'less')

wilcox.test(phi_L2_con[, 3], phi_L2_pre_lip[, 3],
            alternative = 'less')
#3.2.2.2 Post-surgery data and control###################
wilcox.test(phi_L2_con[, 1], phi_L2_post_lip[, 1],
            alternative = 'less')

wilcox.test(phi_L2_con[, 2], phi_L2_post_lip[, 2],
            alternative = 'less')

wilcox.test(phi_L2_con[, 3], phi_L2_post_lip[, 3],
            alternative = 'less')
#3.2.3 weighted \phi_{L_1}#############################
#3.2.3.1 Pre-surgery data and control###################
wilcox.test(phi_L1_con_wei[, 1], phi_L1_pre_wei[, 1],
            alternative = 'less')

wilcox.test(phi_L1_con_wei[, 2], phi_L1_pre_wei[, 2],
            alternative = 'less')

wilcox.test(phi_L1_con_wei[, 3], phi_L1_pre_wei[, 3],
            alternative = 'less')
#3.2.3.2 Post-surgery data and control###################
wilcox.test(phi_L1_con_wei[, 1], phi_L1_post_wei[, 1],
            alternative = 'less')

wilcox.test(phi_L1_con_wei[, 2], phi_L1_post_wei[, 2],
            alternative = 'less')

wilcox.test(phi_L1_con_wei[, 3], phi_L1_post_wei[, 3],
            alternative = 'less')
#3.2.4 weighted \phi_{L_2}#############################
#3.2.4.1 Pre-surgery data and control###################
wilcox.test(phi_L2_con_wei[, 1], phi_L2_pre_wei[, 1],
            alternative = 'less')

wilcox.test(phi_L2_con_wei[, 2], phi_L2_pre_wei[, 2],
            alternative = 'less')

wilcox.test(phi_L2_con_wei[, 3], phi_L2_pre_wei[, 3],
            alternative = 'less')
#3.2.4.2 Post-surgery data and control###################
wilcox.test(phi_L2_con_wei[, 1], phi_L2_post_wei[, 1],
            alternative = 'less')

wilcox.test(phi_L2_con_wei[, 2], phi_L2_post_wei[, 2],
            alternative = 'less')

wilcox.test(phi_L2_con_wei[, 3], phi_L2_post_wei[, 3],
            alternative = 'less')
##########################################
#4. Feature selection##########################
#4.1 Compute t-values######################
#4.1.1 Pre-surgery and control#####################
#First frame
tval_precon_fir <- c()
for (i in 1:14) {
  tval_precon_fir[i] <- t.test(abs(d_pre_fir_lip[, i]), 
   abs(d_con_fir[, i]), alternative = 'great')$statistic
}

#Middle frame
tval_precon_mid <- c()
for (i in 1:14) {
  tval_precon_mid[i] <- t.test(abs(d_pre_mid_lip[, i]), 
   abs(d_con_mid[, i]), alternative = 'great')$statistic
}

#Last frame
tval_precon_la <- c()
for (i in 1:14) {
  tval_precon_la[i] <- t.test(abs(d_pre_la_lip[, i]), 
   abs(d_con_la[, i]), alternative = 'great')$statistic
}
#4.1.2 Post-surgery data and control########################
tval_postcon_fir <- c()
for (i in 1:14) {
  tval_postcon_fir[i] <- t.test(abs(d_post_fir_lip[, i]), 
   abs(d_con_fir[, i]), alternative = 'great')$statistic
}

#Middle frame
tval_postcon_mid <- c()
for (i in 1:14) {
  tval_postcon_mid[i] <- t.test(abs(d_post_mid_lip[, i]), 
   abs(d_con_mid[, i]), alternative = 'great')$statistic
}

#Last frame
tval_postcon_la <- c()
for (i in 1:14) {
  tval_postcon_la[i] <- t.test(abs(d_post_la_lip[, i]), 
   abs(d_con_la[, i]), alternative = 'great')$statistic
}
#4.2 Bootstrap####################################
#4.2.1 Pre-surgery and control#####################
#First frame
t_boot_precon_fir <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  pre_id <- id[1:22]
  control_id <- id[23:131]
  pre_boot <- (rbind(d_pre_fir_lip, d_con_fir))[pre_id, ]
  control_boot <- (rbind(d_pre_fir_lip, d_con_fir))[control_id, ]
  
  t_va <- c()
  for (j in 1:14) {
    t_va[j] <- t.test(abs(pre_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  t_boot_precon_fir[i] <- t_va[which.max(t_va)]
}

which(tval_precon_fir > quantile(t_boot_precon_fir, 0.95))

#Middle frame
t_boot_precon_mid <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  pre_id <- id[1:22]
  control_id <- id[23:131]
  pre_boot <- (rbind(d_pre_mid_lip, d_con_mid))[pre_id, ]
  control_boot <- (rbind(d_pre_mid_lip, d_con_mid))[control_id, ]
  
  t_va <- c()
  for (j in 1:14) {
    t_va[j] <- t.test(abs(pre_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  t_boot_precon_mid[i] <- t_va[which.max(t_va)]
}

which(tval_precon_mid > quantile(t_boot_precon_mid, 0.95))

#Last frame
t_boot_precon_la <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  pre_id <- id[1:22]
  control_id <- id[23:131]
  pre_boot <- (rbind(d_pre_la_lip, d_con_la))[pre_id, ]
  control_boot <- (rbind(d_pre_la_lip, d_con_la))[control_id, ]
  
  t_va <- c()
  for (j in 1:14) {
    t_va[j] <- t.test(abs(pre_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  t_boot_precon_la[i] <- t_va[which.max(t_va)]
}

which(tval_precon_la > quantile(t_boot_precon_la, 0.95))
#4.2.2 Post-surgery and control#####################
#First frame
t_boot_postcon_fir <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  pre_id <- id[1:22]
  control_id <- id[23:131]
  pre_boot <- (rbind(d_post_fir_lip, d_con_fir))[pre_id, ]
  control_boot <- (rbind(d_post_fir_lip, d_con_fir))[control_id, ]
  
  t_va <- c()
  for (j in 1:14) {
    t_va[j] <- t.test(abs(pre_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  t_boot_postcon_fir[i] <- t_va[which.max(t_va)]
}

which(tval_postcon_fir > quantile(t_boot_postcon_fir, 0.95))

#Middle frame
t_boot_postcon_mid <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  pre_id <- id[1:22]
  control_id <- id[23:131]
  pre_boot <- (rbind(d_post_mid_lip, d_con_mid))[pre_id, ]
  control_boot <- (rbind(d_post_mid_lip, d_con_mid))[control_id, ]
  
  t_va <- c()
  for (j in 1:14) {
    t_va[j] <- t.test(abs(pre_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  t_boot_postcon_mid[i] <- t_va[which.max(t_va)]
}

which(tval_postcon_mid > quantile(t_boot_postcon_mid, 0.95))

#Last frame
t_boot_postcon_la <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  pre_id <- id[1:22]
  control_id <- id[23:131]
  pre_boot <- (rbind(d_post_la_lip, d_con_la))[pre_id, ]
  control_boot <- (rbind(d_post_la_lip, d_con_la))[control_id, ]
  
  t_va <- c()
  for (j in 1:14) {
    t_va[j] <- t.test(abs(pre_boot)[, j], abs(control_boot)[, j],
                      alternative = 'great')$statistic
  }
  t_boot_postcon_la[i] <- t_va[which.max(t_va)]
}

which(tval_postcon_la > quantile(t_boot_postcon_la, 0.95))
#4.3 Test with reduced features#####################
#4.3.1 Central pair and solos########################
#4.3.1.1 \phi_{L_1}###############################
#Control
phi_L1_lm3459_con <- matrix(0, nrow = 109, ncol = 3)
phi_L1_lm3459_con[, 1] <- rowSums(abs(d_con_fir[, c(7:9)])) +
  2*abs(d_con_fir[, 13]) + 2*abs(d_con_fir[, 14])

phi_L1_lm3459_con[, 2] <- rowSums(abs(d_con_mid[, c(7:9)])) +
  2*abs(d_con_mid[, 13]) + 2*abs(d_con_mid[, 14])

phi_L1_lm3459_con[, 3] <- rowSums(abs(d_con_la[, c(7:9)])) +
  2*abs(d_con_la[, 13]) + 2*abs(d_con_la[, 14])

#Pre
phi_L1_lm3459_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L1_lm3459_pre[, 1] <- rowSums(abs(d_pre_fir_lip[, c(7:9)])) +
  2*abs(d_pre_fir_lip[, 13]) + 2*abs(d_pre_fir_lip[, 14])

phi_L1_lm3459_pre[, 2] <- rowSums(abs(d_pre_mid_lip[, c(7:9)])) +
  2*abs(d_pre_mid_lip[, 13]) + 2*abs(d_pre_mid_lip[, 14])

phi_L1_lm3459_pre[, 3] <- rowSums(abs(d_pre_la_lip[, c(7:9)])) +
  2*abs(d_pre_la_lip[, 13]) + 2*abs(d_pre_la_lip[, 14])

#Post
phi_L1_lm3459_post <- matrix(0, nrow = 22, ncol = 3)
phi_L1_lm3459_post[, 1] <- rowSums(abs(d_post_fir_lip[, c(7:9)])) +
  2*abs(d_post_fir_lip[, 13]) + 2*abs(d_post_fir_lip[, 14])
  
phi_L1_lm3459_post[, 2] <- rowSums(abs(d_post_mid_lip[, c(7:9)])) +
  2*abs(d_post_mid_lip[, 13]) + 2*abs(d_post_mid_lip[, 14])

phi_L1_lm3459_post[, 3] <- rowSums(abs(d_post_la_lip[, c(7:9)])) +
  2*abs(d_post_la_lip[, 13]) + 2*abs(d_post_la_lip[, 14])

#Pre
wilcox.test(phi_L1_lm3459_con[, 1], phi_L1_lm3459_pre[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm3459_con[, 2], phi_L1_lm3459_pre[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm3459_con[, 3], phi_L1_lm3459_pre[, 3], 
            alternative = 'less')

#Post
wilcox.test(phi_L1_lm3459_con[, 1], phi_L1_lm3459_post[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm3459_con[, 2], phi_L1_lm3459_post[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm3459_con[, 3], phi_L1_lm3459_post[, 3], 
            alternative = 'less')
#4.3.1.2 \phi_{L_2}###############################
#Control
phi_L2_lm3459_con <- matrix(0, nrow = 109, ncol = 3)
phi_L2_lm3459_con[, 1] <- rowSums(abs(d_con_fir[, c(7:9)])^2) +
  (2*abs(d_con_fir[, 13]))^2 + (2*abs(d_con_fir[, 14]))^2

phi_L2_lm3459_con[, 2] <- rowSums(abs(d_con_mid[, c(7:9)])^2) +
  (2*abs(d_con_mid[, 13]))^2 + (2*abs(d_con_mid[, 14]))^2

phi_L2_lm3459_con[, 3] <- rowSums(abs(d_con_la[, c(7:9)])^2) +
  (2*abs(d_con_la[, 13]))^2 + (2*abs(d_con_la[, 14]))^2

#Pre
phi_L2_lm3459_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L2_lm3459_pre[, 1] <- rowSums(abs(d_pre_fir_lip[, c(7:9)])^2) +
  (2*abs(d_pre_fir_lip[, 13]))^2 + (2*abs(d_pre_fir_lip[, 14]))^2

phi_L2_lm3459_pre[, 2] <- rowSums(abs(d_pre_mid_lip[, c(7:9)])^2) +
  (2*abs(d_pre_mid_lip[, 13]))^2 + (2*abs(d_pre_mid_lip[, 14]))^2

phi_L2_lm3459_pre[, 3] <- rowSums(abs(d_pre_la_lip[, c(7:9)])^2) +
  (2*abs(d_pre_la_lip[, 13]))^2 + (2*abs(d_pre_la_lip[, 14]))^2

#Post
phi_L2_lm3459_post <- matrix(0, nrow = 22, ncol = 3)
phi_L2_lm3459_post[, 1] <- rowSums(abs(d_post_fir_lip[, c(7:9)])^2) +
  (2*abs(d_post_fir_lip[, 13]))^2 + (2*abs(d_post_fir_lip[, 14]))^2
  
phi_L2_lm3459_post[, 2] <- rowSums(abs(d_post_mid_lip[, c(7:9)])^2) +
  (2*abs(d_post_mid_lip[, 13]))^2 + (2*abs(d_post_mid_lip[, 14]))^2

phi_L2_lm3459_post[, 3] <- rowSums(abs(d_post_la_lip[, c(7:9)])^2) +
  (2*abs(d_post_la_lip[, 13]))^2 + (2*abs(d_post_la_lip[, 14]))^2

#Pre
wilcox.test(phi_L2_lm3459_con[, 1], phi_L2_lm3459_pre[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm3459_con[, 2], phi_L2_lm3459_pre[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm3459_con[, 3], phi_L2_lm3459_pre[, 3], 
            alternative = 'less')

#Post
wilcox.test(phi_L2_lm3459_con[, 1], phi_L2_lm3459_post[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm3459_con[, 2], phi_L2_lm3459_post[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm3459_con[, 3], phi_L2_lm3459_post[, 3], 
            alternative = 'less')
#4.3.2 Central pair and corner pair########################
#4.3.2.1 \phi_{L_1}###############################
#Control
phi_L1_lm1357_con <- matrix(0, nrow = 109, ncol = 3)
phi_L1_lm1357_con[, 1] <- rowSums(abs(d_con_fir[, c(1:3, 7:9)]))
phi_L1_lm1357_con[, 2] <- rowSums(abs(d_con_mid[, c(1:3, 7:9)]))
phi_L1_lm1357_con[, 3] <- rowSums(abs(d_con_la[, c(1:3, 7:9)]))

#Pre
phi_L1_lm1357_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L1_lm1357_pre[, 1] <- rowSums(abs(d_pre_fir_lip[, c(1:3, 7:9)]))
phi_L1_lm1357_pre[, 2] <- rowSums(abs(d_pre_mid_lip[, c(1:3, 7:9)]))
phi_L1_lm1357_pre[, 3] <- rowSums(abs(d_pre_la_lip[, c(1:3, 7:9)]))

#Post
phi_L1_lm1357_post <- matrix(0, nrow = 22, ncol = 3)
phi_L1_lm1357_post[, 1] <- rowSums(abs(d_post_fir_lip[, c(1:3, 7:9)]))
phi_L1_lm1357_post[, 2] <- rowSums(abs(d_post_mid_lip[, c(1:3, 7:9)]))
phi_L1_lm1357_post[, 3] <- rowSums(abs(d_post_la_lip[, c(1:3, 7:9)]))

#Pre
wilcox.test(phi_L1_lm1357_con[, 1], phi_L1_lm1357_pre[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm1357_con[, 2], phi_L1_lm1357_pre[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm1357_con[, 3], phi_L1_lm1357_pre[, 3], 
            alternative = 'less')

#Post
wilcox.test(phi_L1_lm1357_con[, 1], phi_L1_lm1357_post[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm1357_con[, 2], phi_L1_lm1357_post[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm1357_con[, 3], phi_L1_lm1357_post[, 3], 
            alternative = 'less')
#4.3.2.2 \phi_{L_2}###############################
#Control
phi_L2_lm1357_con <- matrix(0, nrow = 109, ncol = 3)
phi_L2_lm1357_con[, 1] <- rowSums(abs(d_con_fir[, c(1:3, 7:9)])^2)
phi_L2_lm1357_con[, 2] <- rowSums(abs(d_con_mid[, c(1:4, 7:9)])^2)
phi_L2_lm1357_con[, 3] <- rowSums(abs(d_con_la[, c(1:3, 7:9)])^2)

#Pre
phi_L2_lm1357_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L2_lm1357_pre[, 1] <- rowSums(abs(d_pre_fir_lip[, c(1:3, 7:9)])^2)
phi_L2_lm1357_pre[, 2] <- rowSums(abs(d_pre_mid_lip[, c(1:3, 7:9)])^2)
phi_L2_lm1357_pre[, 3] <- rowSums(abs(d_pre_la_lip[, c(1:3, 7:9)])^2)

#Post
phi_L2_lm1357_post <- matrix(0, nrow = 22, ncol = 3)
phi_L2_lm1357_post[, 1] <- rowSums(abs(d_post_fir_lip[, c(1:3, 7:9)])^2)
phi_L2_lm1357_post[, 2] <- rowSums(abs(d_post_mid_lip[, c(1:3, 7:9)])^2)
phi_L2_lm1357_post[, 3] <- rowSums(abs(d_post_la_lip[, c(1:3, 7:9)])^2)

#Pre
wilcox.test(phi_L2_lm1357_con[, 1], phi_L2_lm1357_pre[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm1357_con[, 2], phi_L2_lm1357_pre[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm1357_con[, 3], phi_L2_lm1357_pre[, 3], 
            alternative = 'less')

#Post
wilcox.test(phi_L2_lm1357_con[, 1], phi_L2_lm1357_post[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm1357_con[, 2], phi_L2_lm1357_post[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm1357_con[, 3], phi_L2_lm1357_post[, 3], 
            alternative = 'less')
#4.3.3 Central pair, corner pair and solos########################
#4.3.3.1 \phi_{L_1}###############################
#Control
phi_L1_lm134579_con <- matrix(0, nrow = 109, ncol = 3)
phi_L1_lm134579_con[, 1] <- rowSums(abs(d_con_fir[, c(1:3, 7:9)])) +
  2*abs(d_con_fir[, 13]) + 2*abs(d_con_fir[, 14])

phi_L1_lm134579_con[, 2] <- rowSums(abs(d_con_mid[, c(1:3, 7:9)])) +
  2*abs(d_con_mid[, 13]) + 2*abs(d_con_mid[, 14])

phi_L1_lm134579_con[, 3] <- rowSums(abs(d_con_la[, c(1:3, 7:9)])) +
  2*abs(d_con_la[, 13]) + 2*abs(d_con_la[, 14])

#Pre
phi_L1_lm134579_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L1_lm134579_pre[, 1] <- rowSums(abs(d_pre_fir_lip[, c(1:3, 7:9)])) +
  2*abs(d_pre_fir_lip[, 13]) + 2*abs(d_pre_fir_lip[, 14])

phi_L1_lm134579_pre[, 2] <- rowSums(abs(d_pre_mid_lip[, c(1:3, 7:9)])) +
  2*abs(d_pre_mid_lip[, 13]) + 2*abs(d_pre_mid_lip[, 14])

phi_L1_lm134579_pre[, 3] <- rowSums(abs(d_pre_la_lip[, c(1:3, 7:9)])) +
  2*abs(d_pre_la_lip[, 13]) + 2*abs(d_pre_la_lip[, 14])

#Post
phi_L1_lm134579_post <- matrix(0, nrow = 22, ncol = 3)
phi_L1_lm134579_post[, 1] <- rowSums(abs(d_post_fir_lip[, c(1:3, 7:9)])) +
  2*abs(d_post_fir_lip[, 13]) + 2*abs(d_post_fir_lip[, 14])
  
phi_L1_lm134579_post[, 2] <- rowSums(abs(d_post_mid_lip[, c(1:3, 7:9)])) +
  2*abs(d_post_mid_lip[, 13]) + 2*abs(d_post_mid_lip[, 14])

phi_L1_lm134579_post[, 3] <- rowSums(abs(d_post_la_lip[, c(1:3, 7:9)])) +
  2*abs(d_post_la_lip[, 13]) + 2*abs(d_post_la_lip[, 14])

#Pre
wilcox.test(phi_L1_lm134579_con[, 1], phi_L1_lm134579_pre[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm134579_con[, 2], phi_L1_lm134579_pre[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm134579_con[, 3], phi_L1_lm134579_pre[, 3], 
            alternative = 'less')

#Post
wilcox.test(phi_L1_lm134579_con[, 1], phi_L1_lm134579_post[, 1], 
            alternative = 'less')

wilcox.test(phi_L1_lm134579_con[, 2], phi_L1_lm134579_post[, 2], 
            alternative = 'less')

wilcox.test(phi_L1_lm134579_con[, 3], phi_L1_lm134579_post[, 3], 
            alternative = 'less')
#4.3.3.2 \phi_{L_2}###############################
#Control
phi_L2_lm134579_con <- matrix(0, nrow = 109, ncol = 3)
phi_L2_lm134579_con[, 1] <- rowSums(abs(d_con_fir[, c(1:3, 7:9)])^2) +
  (2*abs(d_con_fir[, 13]))^2 + (2*abs(d_con_fir[, 14]))^2

phi_L2_lm134579_con[, 2] <- rowSums(abs(d_con_mid[, c(1:3, 7:9)])^2) +
  (2*abs(d_con_mid[, 13]))^2 + (2*abs(d_con_mid[, 14]))^2

phi_L2_lm134579_con[, 3] <- rowSums(abs(d_con_la[, c(1:3, 7:9)])^2) +
  (2*abs(d_con_la[, 13]))^2 + (2*abs(d_con_la[, 14]))^2

#Pre
phi_L2_lm134579_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L2_lm134579_pre[, 1] <- rowSums(abs(d_pre_fir_lip[, c(1:3, 7:9)])^2) +
  (2*abs(d_pre_fir_lip[, 13]))^2 + (2*abs(d_pre_fir_lip[, 14]))^2

phi_L2_lm134579_pre[, 2] <- rowSums(abs(d_pre_mid_lip[, c(1:3, 7:9)])^2) +
  (2*abs(d_pre_mid_lip[, 13]))^2 + (2*abs(d_pre_mid_lip[, 14]))^2

phi_L2_lm134579_pre[, 3] <- rowSums(abs(d_pre_la_lip[, c(1:3, 7:9)])^2) +
  (2*abs(d_pre_la_lip[, 13]))^2 + (2*abs(d_pre_la_lip[, 14]))^2

#Post
phi_L2_lm134579_post <- matrix(0, nrow = 22, ncol = 3)
phi_L2_lm134579_post[, 1] <- rowSums(abs(d_post_fir_lip[, c(1:3, 7:9)])^2) +
  (2*abs(d_post_fir_lip[, 13]))^2 + (2*abs(d_post_fir_lip[, 14]))^2
  
phi_L2_lm134579_post[, 2] <- rowSums(abs(d_post_mid_lip[, c(1:3, 7:9)])^2) +
  (2*abs(d_post_mid_lip[, 13]))^2 + (2*abs(d_post_mid_lip[, 14]))^2

phi_L2_lm134579_post[, 3] <- rowSums(abs(d_post_la_lip[, c(1:3, 7:9)])^2) +
  (2*abs(d_post_la_lip[, 13]))^2 + (2*abs(d_post_la_lip[, 14]))^2

#Pre
wilcox.test(phi_L2_lm134579_con[, 1], phi_L2_lm134579_pre[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm134579_con[, 2], phi_L2_lm134579_pre[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm134579_con[, 3], phi_L2_lm134579_pre[, 3], 
            alternative = 'less')

#Post
wilcox.test(phi_L2_lm134579_con[, 1], phi_L2_lm134579_post[, 1], 
            alternative = 'less')

wilcox.test(phi_L2_lm134579_con[, 2], phi_L2_lm134579_post[, 2], 
            alternative = 'less')

wilcox.test(phi_L2_lm134579_con[, 3], phi_L2_lm134579_post[, 3], 
            alternative = 'less')
##########################################
#5. Meta-analysis#################################
#5.1 Separate p-values###########################
#5.1.1 Pre-surgery and control#####################
p_wilcox_precon_fir <- apply(rbind(abs(d_con_fir), abs(d_pre_fir_lip)),
        2, function(a) {
  wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
      })

p_wilcox_precon_mid <- apply(rbind(abs(d_con_mid), abs(d_pre_mid_lip)),
        2, function(a) {
  wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
      })

p_wilcox_precon_la <- apply(rbind(abs(d_con_la), abs(d_pre_la_lip)),
        2, function(a) {
  wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
      })
#5.1.2 Post-surgery and control###################
p_wilcox_postcon_fir <- apply(rbind(abs(d_con_fir), abs(d_post_fir_lip)),
        2, function(a) {
  wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
      })

p_wilcox_postcon_mid <- apply(rbind(abs(d_con_mid), abs(d_post_mid_lip)),
        2, function(a) {
  wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
      })

p_wilcox_postcon_la <- apply(rbind(abs(d_con_la), abs(d_post_la_lip)),
        2, function(a) {
  wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
      })
#5.2 Fisher's method##########################
#5.2.1 Pre-surgery and control######################
1 - pchisq(-2*sum(log(p_wilcox_precon_fir)), df = 28)

1 - pchisq(-2*sum(log(p_wilcox_precon_mid)), df = 28)

1 - pchisq(-2*sum(log(p_wilcox_precon_la)), df = 28)
#5.2.2 Post-surgery and control######################
1 - pchisq(-2*sum(log(p_wilcox_postcon_fir)), df = 28)

1 - pchisq(-2*sum(log(p_wilcox_postcon_mid)), df = 28)

1 - pchisq(-2*sum(log(p_wilcox_postcon_la)), df = 28)
#5.3 Pearson's method##############################
#5.3.1 Pre-surgery and control###################
#First frame
p_pearson_precon_fir <- c()
p_rand_wilcox_fea <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  control_id <- id[1:109]
  pre_id <- id[110:131]
  control_boot <- 
    rbind(abs(d_con_fir), abs(d_pre_fir_lip))[control_id, ]
  pre_boot <- 
    rbind(abs(d_con_fir), abs(d_pre_fir_lip))[pre_id, ]
  
  p_rand_wilcox_fea <- apply(rbind(control_boot, pre_boot), 2, 
                             function(a) {
    wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
  })
  
  p_pearson_precon_fir[i] <- max(-2*sum(log(p_rand_wilcox_fea)),
                              -2*sum(log(1-p_rand_wilcox_fea)))
}
mean(p_pearson_precon_fir >= -2*sum(log(p_wilcox_precon_fir)))

#Middle frame
p_pearson_precon_mid <- c()
p_rand_wilcox_fea <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  control_id <- id[1:109]
  pre_id <- id[110:131]
  control_boot <- 
    rbind(abs(d_con_mid), abs(d_pre_mid_lip))[control_id, ]
  pre_boot <- 
    rbind(abs(d_con_mid), abs(d_pre_mid_lip))[pre_id, ]
  
  p_rand_wilcox_fea <- apply(rbind(control_boot, pre_boot), 2, 
                             function(a) {
    wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
  })
  
  p_pearson_precon_mid[i] <- max(-2*sum(log(p_rand_wilcox_fea)),
                              -2*sum(log(1-p_rand_wilcox_fea)))
}
mean(p_pearson_precon_mid >= -2*sum(log(p_wilcox_precon_mid)))

#Last frame
p_pearson_precon_la <- c()
p_rand_wilcox_fea <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  control_id <- id[1:109]
  pre_id <- id[110:131]
  control_boot <- 
    rbind(abs(d_con_la), abs(d_pre_la_lip))[control_id, ]
  pre_boot <- 
    rbind(abs(d_con_la), abs(d_pre_la_lip))[pre_id, ]
  
  p_rand_wilcox_fea <- apply(rbind(control_boot, pre_boot), 2, 
                             function(a) {
    wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
  })
  
  p_pearson_precon_la[i] <- max(-2*sum(log(p_rand_wilcox_fea)),
                              -2*sum(log(1-p_rand_wilcox_fea)))
}
mean(p_pearson_precon_la >= -2*sum(log(p_wilcox_precon_la)))
#5.3.2 Post-surgery and control###################
#First frame
p_pearson_postcon_fir <- c()
p_rand_wilcox_fea <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  control_id <- id[1:109]
  pre_id <- id[110:131]
  control_boot <- 
    rbind(abs(d_con_fir), abs(d_post_fir_lip))[control_id, ]
  pre_boot <- 
    rbind(abs(d_con_fir), abs(d_post_fir_lip))[pre_id, ]
  
  p_rand_wilcox_fea <- apply(rbind(control_boot, pre_boot), 2, 
                             function(a) {
    wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
  })
  
  p_pearson_postcon_fir[i] <- max(-2*sum(log(p_rand_wilcox_fea)),
                              -2*sum(log(1-p_rand_wilcox_fea)))
}
mean(p_pearson_postcon_fir >= -2*sum(log(p_wilcox_postcon_fir)))

#Middle frame
p_pearson_postcon_mid <- c()
p_rand_wilcox_fea <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  control_id <- id[1:109]
  pre_id <- id[110:131]
  control_boot <- 
    rbind(abs(d_con_mid), abs(d_post_mid_lip))[control_id, ]
  pre_boot <- 
    rbind(abs(d_con_mid), abs(d_post_mid_lip))[pre_id, ]
  
  p_rand_wilcox_fea <- apply(rbind(control_boot, pre_boot), 2, 
                             function(a) {
    wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
  })
  
  p_pearson_postcon_mid[i] <- max(-2*sum(log(p_rand_wilcox_fea)),
                              -2*sum(log(1-p_rand_wilcox_fea)))
}
mean(p_pearson_postcon_mid >= -2*sum(log(p_wilcox_postcon_mid)))

#Last frame
p_pearson_postcon_la <- c()
p_rand_wilcox_fea <- c()
for (i in 1:10000) {
  id <- sample(1:131, 131, replace = F)
  control_id <- id[1:109]
  pre_id <- id[110:131]
  control_boot <- 
    rbind(abs(d_con_la), abs(d_post_la_lip))[control_id, ]
  pre_boot <- 
    rbind(abs(d_con_la), abs(d_post_la_lip))[pre_id, ]
  
  p_rand_wilcox_fea <- apply(rbind(control_boot, pre_boot), 2, 
                             function(a) {
    wilcox.test(a[1:109], a[110:131], alternative = 'less')$p.value
  })
  
  p_pearson_postcon_la[i] <- max(-2*sum(log(p_rand_wilcox_fea)),
                              -2*sum(log(1-p_rand_wilcox_fea)))
}
mean(p_pearson_postcon_la >= -2*sum(log(p_wilcox_postcon_la)))
















