########################################
#Codes for performing analyses on the jaw data
#We focus on the 7 landmarks at the upper lip
#Load required packages
require(readxl)
##########################################
#0. Read in data############################
#0.1 Pre-surgery data####################
jaw_pre <- list()

jaw_pre$'1_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_1_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'9_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_9_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'12_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_12_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'18_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_18_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'24_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_24_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'25_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_25_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'26_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_26_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'27_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_27_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'29_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_29_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'30_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_30_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'2_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_2_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'3_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_3_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'4_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_4_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'11_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_11_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'14_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_14_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'19_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_19_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'20_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_20_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'22_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_22_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'23_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_23_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'28_F_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_28_F_Pre.xls", 
             col_names = FALSE)

jaw_pre$'32_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_32_M_Pre.xls", 
             col_names = FALSE)

jaw_pre$'33_M_Pre' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_33_M_Pre.xls", 
             col_names = FALSE)
#0.2 Post-surgery data################################
jaw_post <- list()

jaw_post$'1_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_1_F_Post.xls", 
             col_names = FALSE)

jaw_post$'9_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_9_M_Post.xls", 
             col_names = FALSE)

jaw_post$'12_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_12_F_Post.xls", 
             col_names = FALSE)

jaw_post$'18_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_18_M_Post.xls", 
             col_names = FALSE)

jaw_post$'24_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_24_M_Post.xls", 
             col_names = FALSE)

jaw_post$'25_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_25_F_Post.xls", 
             col_names = FALSE)

jaw_post$'26_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_26_F_Post.xls", 
             col_names = FALSE)

jaw_post$'27_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_27_M_Post.xls", 
             col_names = FALSE)

jaw_post$'29_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_29_M_Post.xls", 
             col_names = FALSE)

jaw_post$'30_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper and lower/Subject_30_M_Post.xls", 
             col_names = FALSE)

jaw_post$'2_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_2_F_Post.xls", 
             col_names = FALSE)

jaw_post$'3_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_3_F_Post.xls", 
             col_names = FALSE)

jaw_post$'4_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_4_M_Post.xls", 
             col_names = FALSE)

jaw_post$'11_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_11_M_Post.xls", 
             col_names = FALSE)

jaw_post$'14_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_14_F_Post.xls", 
             col_names = FALSE)

jaw_post$'19_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_19_M_Post.xls", 
             col_names = FALSE)

jaw_post$'20_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_20_M_Post.xls", 
             col_names = FALSE)

jaw_post$'22_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_22_M_Post.xls", 
             col_names = FALSE)

jaw_post$'23_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_23_M_Post.xls", 
             col_names = FALSE)

jaw_post$'28_F_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_28_F_Post.xls", 
             col_names = FALSE)

jaw_post$'32_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_32_M_Post.xls", 
             col_names = FALSE)

jaw_post$'33_M_Post' <- 
  read_excel("C:/Users/mmxw/Desktop/R/BV_jaw_data/Orthognathic data/jaw data csv/Upper only/Subject_33_M_Post.xls", 
             col_names = FALSE)
#1. Pre-process the data###########################
#1.1 Standardize the frames#########################
jaw_pre_std <- list()
for (i in 1:22) {
  fr <- dim(jaw_pre[[i]])[1]
  time_ind <- round(quantile(1:fr, prob = c(0, seq(1/12, 1, 1/12))))
  
  jaw_pre_std[[i]] <- jaw_pre[[i]][time_ind, ]
}

jaw_post_std <- list()
for (i in 1:22) {
  fr <- dim(jaw_post[[i]])[1]
  time_ind <- round(quantile(1:fr, prob = c(0, seq(1/12, 1, 1/12))))
  
  jaw_post_std[[i]] <- jaw_post[[i]][time_ind, ]
}
jaw_post_std[[13]] <- jaw_post_std[[13]][, 1:60]
#1.2 Change the data type to array###########################
#1.2.1 Pre-surgery data###############################
#First frame
jawpre_std_arr_fir <- array(0, dim = c(20, 3, 22))
for (i in 1:22) {
  jawpre_std_arr_fir[, , i] <- t(matrix(unlist(
    jaw_pre_std[[i]][1, ]), nrow = 3))
}

#Middle frame
jawpre_std_arr_mid <- array(0, dim = c(20, 3, 22))
for (i in 1:22) {
  jawpre_std_arr_mid[, , i] <- t(matrix(unlist(
    jaw_pre_std[[i]][7, ]), nrow = 3))
}

#Last frame
jawpre_std_arr_la <- array(0, dim = c(20, 3, 22))
for (i in 1:22) {
  jawpre_std_arr_la[, , i] <- t(matrix(unlist(
    jaw_pre_std[[i]][13, ]), nrow = 3))
}
#1.2.2 Post-surgery data################################
#First frame
jawpost_std_arr_fir <- array(0, dim = c(20, 3, 22))
for (i in 1:22) {
  jawpost_std_arr_fir[, , i] <- t(matrix(unlist(
    jaw_post_std[[i]][1, ]), nrow = 3))
}

#Middle frame
jawpost_std_arr_mid <- array(0, dim = c(20, 3, 22))
for (i in 1:22) {
  jawpost_std_arr_mid[, , i] <- t(matrix(unlist(
    jaw_post_std[[i]][7, ]), nrow = 3))
}

#Last frame
jawpost_std_arr_la <- array(0, dim = c(20, 3, 22))
for (i in 1:22) {
  jawpost_std_arr_la[, , i] <- t(matrix(unlist(
    jaw_post_std[[i]][13, ]), nrow = 3))
}
#2. Asymmetry features defined in (2.1)-(2.3)#################
#House hold matrix
H <- diag(3) - 2*matrix(c(1, 0, 0), ncol = 1) %*% 
  t(matrix(c(1, 0, 0), ncol = 1))
#2.1 Pre-surgery data#######################
#First frame
d_pre_fir_uplip <- matrix(0, nrow = 22, ncol = 18)
for (i in 1:22) {
  dif_matrix <- jawpre_std_arr_fir[c(5:11, 15:20), , i] - 
    (jawpre_std_arr_fir[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  d_pre_fir_uplip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8, 12), ]),
  nrow = 1), dif_matrix[4, 1]/2, dif_matrix[10, 1]/2, 
  dif_matrix[11, 1]/2)
}

#Middle frame
d_pre_mid_uplip <- matrix(0, nrow = 22, ncol = 18)
for (i in 1:22) {
  dif_matrix <- jawpre_std_arr_mid[c(5:11, 15:20), , i] - 
    (jawpre_std_arr_mid[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  d_pre_mid_uplip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8, 12), ]),
  nrow = 1), dif_matrix[4, 1]/2, dif_matrix[10, 1]/2, 
  dif_matrix[11, 1]/2)
}

#Last frame
d_pre_la_uplip <- matrix(0, nrow = 22, ncol = 18)
for (i in 1:22) {
  dif_matrix <- jawpre_std_arr_la[c(5:11, 15:20), , i] - 
    (jawpre_std_arr_la[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  d_pre_la_uplip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8, 12), ]),
  nrow = 1), dif_matrix[4, 1]/2, dif_matrix[10, 1]/2, 
  dif_matrix[11, 1]/2)
}
#2.2 Post-surgery data###############################
#First frame
d_post_fir_uplip <- matrix(0, nrow = 22, ncol = 18)
for (i in 1:22) {
  dif_matrix <- jawpost_std_arr_fir[c(5:11, 15:20), , i] - 
    (jawpost_std_arr_fir[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  d_post_fir_uplip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8, 12), ]),
  nrow = 1), dif_matrix[4, 1]/2, dif_matrix[10, 1]/2, 
  dif_matrix[11, 1]/2)
}

#Middle frame
d_post_mid_uplip <- matrix(0, nrow = 22, ncol = 18)
for (i in 1:22) {
  dif_matrix <- jawpost_std_arr_mid[c(5:11, 15:20), , i] - 
    (jawpost_std_arr_mid[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  d_post_mid_uplip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8, 12), ]),
  nrow = 1), dif_matrix[4, 1]/2, dif_matrix[10, 1]/2, 
  dif_matrix[11, 1]/2)
}

#Last frame
d_post_la_uplip <- matrix(0, nrow = 22, ncol = 18)
for (i in 1:22) {
  dif_matrix <- jawpost_std_arr_la[c(5:11, 15:20), , i] - 
    (jawpost_std_arr_la[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  d_post_la_uplip[i, ] <- c(matrix(t(dif_matrix[c(1:3, 8, 12), ]),
  nrow = 1), dif_matrix[4, 1]/2, dif_matrix[10, 1]/2, 
  dif_matrix[11, 1]/2)
}
#3. Composite asymmetry scores################################
#3.1 \phi^*_{L_1} in (3.9)#####################################
#3.1.1 Pre-surgery data#############################
phistar_L1_pre_uplip <- matrix(0, nrow = 22, ncol = 3)
#First frame
for (i in 1:22) {
  dif_matrix <- jawpre_std_arr_fir[c(5:11, 15:20), , i] - 
    (jawpre_std_arr_fir[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  phistar_L1_pre_uplip[i, 1] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8, 12), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[10, 1])/2, 
      abs(dif_matrix[11, 1])/2))
}

#Middle frame
for (i in 1:22) {
  dif_matrix <- jawpre_std_arr_mid[c(5:11, 15:20), , i] - 
    (jawpre_std_arr_mid[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  phistar_L1_pre_uplip[i, 2] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8, 12), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[10, 1])/2, 
      abs(dif_matrix[11, 1])/2))
}

#Last frame
for (i in 1:22) {
  dif_matrix <- jawpre_std_arr_la[c(5:11, 15:20), , i] - 
    (jawpre_std_arr_la[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  phistar_L1_pre_uplip[i, 3] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8, 12), ])^2)),
      abs(dif_matrix[4, 1])/2, abs(dif_matrix[10, 1])/2, 
      abs(dif_matrix[11, 1])/2))
}
#3.1.2 Post-surgery data############################
phistar_L1_post_uplip <- matrix(0, nrow = 22, ncol = 3)
#First frame
for (i in 1:22) {
  dif_matrix <- jawpost_std_arr_fir[c(5:11, 15:20), , i] - 
    (jawpost_std_arr_fir[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  phistar_L1_post_uplip[i, 1] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8, 12), ])^2)),
          abs(dif_matrix[4, 1])/2, abs(dif_matrix[10, 1])/2, 
          abs(dif_matrix[11, 1])/2))
}

#Middle frame
for (i in 1:22) {
  dif_matrix <- jawpost_std_arr_mid[c(5:11, 15:20), , i] - 
    (jawpost_std_arr_mid[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  phistar_L1_post_uplip[i, 2] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8, 12), ])^2)),
          abs(dif_matrix[4, 1])/2, abs(dif_matrix[10, 1])/2, 
          abs(dif_matrix[11, 1])/2))
}

#Last frame
for (i in 1:22) {
  dif_matrix <- jawpost_std_arr_la[c(5:11, 15:20), , i] - 
    (jawpost_std_arr_la[c(5:11, 15:20), , i] %*% H)[c(7:5, 4, 3:1, 
                        9, 8, 10, 11, 13, 12), ]
  phistar_L1_post_uplip[i, 3] <- 
    sum(c(sqrt(rowSums((dif_matrix[c(1:3, 8, 12), ])^2)),
          abs(dif_matrix[4, 1])/2, abs(dif_matrix[10, 1])/2, 
          abs(dif_matrix[11, 1])/2))
}
#3.2 \phi_{L_1} in (3.6)#########################
#3.2.1 Pre-surgery data##########################
phi_L1_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L1_pre[, 1] <- rowSums(abs(d_pre_fir_uplip))
phi_L1_pre[, 2] <- rowSums(abs(d_pre_mid_uplip))
phi_L1_pre[, 3] <- rowSums(abs(d_pre_la_uplip))
#3.2.2 Post-surgery data##########################
phi_L1_post <- matrix(0, nrow = 22, ncol = 3)
phi_L1_post[, 1] <- rowSums(abs(d_post_fir_uplip))
phi_L1_post[, 2] <- rowSums(abs(d_post_mid_uplip))
phi_L1_post[, 3] <- rowSums(abs(d_post_la_uplip))
#3.3 \phi_{L_2} in (3.7)###############################
#3.3.1 Pre-surgery##################################
phi_L2_pre <- matrix(0, nrow = 22, ncol = 3)
phi_L2_pre[, 1] <- rowSums(d_pre_fir_uplip^2)
phi_L2_pre[, 2] <- rowSums(d_pre_mid_uplip^2)
phi_L2_pre[, 3] <- rowSums(d_pre_la_uplip^2)
#3.3.2 Post-surgery##################################
phi_L2_post <- matrix(0, nrow = 22, ncol = 3)
phi_L2_post[, 1] <- rowSums(d_post_fir_uplip^2)
phi_L2_post[, 2] <- rowSums(d_post_mid_uplip^2)
phi_L2_post[, 3] <- rowSums(d_post_la_uplip^2)
#4. Univariate tests############################
#4.1 Paired t-test###############################
t.test(phistar_L1_pre_uplip[, 1], phistar_L1_post_uplip[, 1], 
       paired = T, alternative = 'less')

t.test(phistar_L1_pre_uplip[, 2], phistar_L1_post_uplip[, 2], 
       paired = T, alternative = 'less')

t.test(phistar_L1_pre_uplip[, 3], phistar_L1_post_uplip[, 3], 
       paired = T, alternative = 'less')
#4.2 Wilcoxon signed rank test####################
#4.2.1 \phi_{L_1}##########################
wilcox.test(phi_L1_pre[, 1], phi_L1_post[, 1], paired = T, 
            alternative = 'less')

wilcox.test(phi_L1_pre[, 2], phi_L1_post[, 2], paired = T, 
            alternative = 'less')

wilcox.test(phi_L1_pre[, 3], phi_L1_post[, 3], paired = T, 
            alternative = 'less')
#4.2.2 \phi_{L_2}##########################
wilcox.test(phi_L2_pre[, 1], phi_L2_post[, 1], paired = T, 
            alternative = 'less')

wilcox.test(phi_L2_pre[, 2], phi_L2_post[, 2], paired = T, 
            alternative = 'less')

wilcox.test(phi_L2_pre[, 3], phi_L2_post[, 3], paired = T, 
            alternative = 'less')
#5. Feature selection########################
#5.1 Compute t-values##########################
#First frame
tval_uplip_fir <- c()
for (i in 1:18) {
  tval_uplip_fir[i] <- t.test(abs(d_post_fir_uplip[, i]), 
                            abs(d_pre_fir_uplip[, i]),
                        paired = T, alternative = 'great')$statistic
}

#Middle frame
tval_uplip_mid <- c()
for (i in 1:18) {
  tval_uplip_mid[i] <- t.test(abs(d_post_mid_uplip[, i]), 
                        abs(d_pre_mid_uplip[, i]),
                        paired = T, alternative = 'great')$statistic
}

#Last frame
tval_uplip_la <- c()
for (i in 1:18) {
  tval_uplip_la[i] <- t.test(abs(d_post_la_uplip[, i]), 
                             abs(d_pre_la_uplip[, i]),
                       paired = T, alternative = 'great')$statistic
}
#5.2 Permutation test#####################
#First frame
tval_uplip_boot_fir <- c()
for (i in 1:10000) {
  
  t_va <- c()
  for (j in 1:18) {
    id <- sample(c(-1, 1), 22, replace = T)
    
    sample_boot <- (abs(d_post_fir_uplip[, j]) - 
                      abs(d_pre_fir_uplip[, j]))*id
    
    t_va[j] <- t.test(sample_boot, alternative = 'great')$statistic
  }
  tval_uplip_boot_fir[i] <- max(t_va)
}

which(tval_uplip_fir > quantile(tval_uplip_boot_fir, 0.75))
which(tval_uplip_fir > quantile(tval_uplip_boot_fir, 0.80))
which(tval_uplip_fir > quantile(tval_uplip_boot_fir, 0.85))
which(tval_uplip_fir > quantile(tval_uplip_boot_fir, 0.90))

#Middle frame
tval_uplip_boot_mid <- c()
for (i in 1:10000) {
  
  t_va <- c()
  for (j in 1:18) {
    id <- sample(c(-1, 1), 22, replace = T)
    
    sample_boot <- (abs(d_post_mid_uplip[, j]) - 
                      abs(d_pre_mid_uplip[, j]))*id
    
    t_va[j] <- t.test(sample_boot, alternative = 'great')$statistic
  }
  tval_uplip_boot_mid[i] <- max(t_va)
}

which(tval_uplip_mid > quantile(tval_uplip_boot_mid, 0.75))
which(tval_uplip_mid > quantile(tval_uplip_boot_mid, 0.80))
which(tval_uplip_mid > quantile(tval_uplip_boot_mid, 0.85))
which(tval_uplip_mid > quantile(tval_uplip_boot_mid, 0.90))

#Last frame
tval_uplip_boot_la <- c()
for (i in 1:10000) {
  
  t_va <- c()
  for (j in 1:18) {
    id <- sample(c(-1, 1), 22, replace = T)
    
    sample_boot <- (abs(d_post_la_uplip[, j]) - 
                      abs(d_pre_la_uplip[, j]))*id
    
    t_va[j] <- t.test(sample_boot, alternative = 'great')$statistic
  }
  tval_uplip_boot_la[i] <- max(t_va)
}

which(tval_uplip_la > quantile(tval_uplip_boot_la, 0.75))
which(tval_uplip_la > quantile(tval_uplip_boot_la, 0.80))
which(tval_uplip_la > quantile(tval_uplip_boot_la, 0.85))
which(tval_uplip_la > quantile(tval_uplip_boot_la, 0.90))
#6. Meta-analysis###########################
#6.1 Obtain separate p-values#################
#First frame
p_wilcox_uplip_fir <- apply(
  rbind(abs(d_pre_fir_uplip), abs(d_post_fir_uplip)), 
  2, function(a) {wilcox.test(a[1:22], a[23:44], paired = T, 
    alternative = 'less')$p.value})

#Middle frame
p_wilcox_uplip_mid <- apply(
  rbind(abs(d_pre_mid_uplip), abs(d_post_mid_uplip)), 
  2, function(a) {wilcox.test(a[1:22], a[23:44], paired = T, 
    alternative = 'less')$p.value})

#Last frame
p_wilcox_uplip_la <- apply(
  rbind(abs(d_pre_la_uplip), abs(d_post_la_uplip)), 
  2, function(a) {wilcox.test(a[1:22], a[23:44], paired = T, 
    alternative = 'less')$p.value})
#6.2 Fisher's method####################
1 - pchisq(-2*sum(log(p_wilcox_uplip_fir)), df = 36)

1 - pchisq(-2*sum(log(p_wilcox_uplip_mid)), df = 36)

1 - pchisq(-2*sum(log(p_wilcox_uplip_la)), df = 36)
#6.3 Pearson's method###################
#First frame
p_pearson_fir <- c()
for (i in 1:10000) {
  id <- sample(c(-1, 1), 22, replace = T)
  
  prepost_boot <- (abs(d_post_fir_uplip) - abs(d_pre_fir_uplip))
  for (j in 1:22) {
    prepost_boot[j, ] <- (abs(d_post_fir_uplip) - 
            abs(d_pre_fir_uplip))[j, ] * id[j]
  }
  
  p_wil_boot <- apply(prepost_boot, 2, function(a) {
    wilcox.test(a, alternative = 'great')$p.value
  })
  
  p_pearson_fir[i] <- max(-2*sum(log(p_wil_boot)),
                                  -2*sum(log(1-p_wil_boot)))
}
mean(p_pearson_fir >= -2*sum(log(p_wilcox_uplip_fir)))

#Middle frame
p_pearson_mid <- c()
for (i in 1:10000) {
  id <- sample(c(-1, 1), 22, replace = T)
  
  prepost_boot <- (abs(d_post_mid_uplip) - abs(d_pre_mid_uplip))
  for (j in 1:22) {
    prepost_boot[j, ] <- (abs(d_post_mid_uplip) - 
                            abs(d_pre_mid_uplip))[j, ] * id[j]
  }
  
  p_wil_boot <- apply(prepost_boot, 2, function(a) {
    wilcox.test(a, alternative = 'great')$p.value
  })
  
  p_pearson_mid[i] <- max(-2*sum(log(p_wil_boot)),
                          -2*sum(log(1-p_wil_boot)))
}
mean(p_pearson_mid >= -2*sum(log(p_wilcox_uplip_mid)))

#Last frame
p_pearson_la <- c()
for (i in 1:10000) {
  id <- sample(c(-1, 1), 22, replace = T)
  
  prepost_boot <- (abs(d_post_la_uplip) - abs(d_pre_la_uplip))
  for (j in 1:22) {
    prepost_boot[j, ] <- (abs(d_post_la_uplip) - 
                            abs(d_pre_la_uplip))[j, ] * id[j]
  }
  
  p_wil_boot <- apply(prepost_boot, 2, function(a) {
    wilcox.test(a, alternative = 'great')$p.value
  })
  
  p_pearson_la[i] <- max(-2*sum(log(p_wil_boot)),
                          -2*sum(log(1-p_wil_boot)))
}
mean(p_pearson_la >= -2*sum(log(p_wilcox_uplip_la)))



