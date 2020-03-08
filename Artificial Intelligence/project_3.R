library(Diagnostics)

learn <- function(hist){
  
  c <- list()
  
  historical_data = hist
  
  # p(Pn,Te,VTB,TB,Sm,LC,Br,XR,Dy) = p(Dy|Pn,Te,VTB,TB,Sm,LC,Br,XR) . p(XR|Pn,Te,VTB,TB,Sm,LC,Br) . p(Br|Pn,Te,VTB,TB,Sm,LC) . p(LC|Pn,Te,VTB,TB,Sm) . p(Sm|Pn,Te,VTB,TB) . p(TB|Pn,Te,VTB) . p(VTB|Pn,Te) . p(Te|Pn) . p(Pn)
  # Our Bayesian Network will look like: 
  # p(Pn,Te,VTB,TB,Sm,LC,Br,XR,Dy) = p(Dy|LC,Br) . p(XR|Pn,TB,LC) . p(Br|Sm) . p(LC|Sm) . p(Sm) . p(TB|VTB) . p(VTB) . p(Te|Pn) . p(Pn)
  
  fit_pn = matrix(0, nrow = 1, ncol = 2)
  fit_te_given_pn = matrix(0, nrow = 2, ncol = 3)
  fit_vtb = matrix(0, nrow = 1, ncol = 2)
  fit_tb_given_vtb = matrix(0, nrow = 2, ncol = 3)
  fit_sm = matrix(0, nrow = 1, ncol = 2)
  fit_lc_given_sm = matrix(0, nrow = 2, ncol = 3)
  fit_br_given_sm = matrix(0, nrow = 2, ncol = 3)
  fit_xr_given_pn_tb_lc = matrix(0, nrow = 8, ncol = 5)
  fit_dy_given_lc_br = matrix(0, nrow = 4, ncol = 4)
  mean_sd_val = matrix(0, nrow = 2, ncol = 2)
  
  # Fit Variable or Node "Pneumonia" of our Bayesian Network to our historical data.
  fit_pn[1] = length(which(historical_data[,1] == 0)) / length(historical_data[,1])
  fit_pn[2] = length(which(historical_data[,1] == 1)) / length(historical_data[,1])
  
  # Fit conditional normal distribution of variable "Temparature" of our Bayesian Network to our historical data.
  indices_where_pn_is_0 = which(historical_data[,1] == 0)
  indices_where_pn_is_1 = which(historical_data[,1] == 1)
  
  te_vector_for_pn_0 = vector()
  te_vector_for_pn_1 = vector()
  
  for(i in 1:length(indices_where_pn_is_0)){
    te_vector_for_pn_0 = c(te_vector_for_pn_0, historical_data[indices_where_pn_is_0[i],2])
  }
  
  for(i in 1:length(indices_where_pn_is_1)){
    te_vector_for_pn_1 = c(te_vector_for_pn_1, historical_data[indices_where_pn_is_1[i],2])
  }
  
  mean_of_te_vector_for_pn_0 = mean(te_vector_for_pn_0)
  sd_of_te_vector_for_pn_0 =  sd(te_vector_for_pn_0)
  mean_sd_val[1,1] = mean_of_te_vector_for_pn_0
  mean_sd_val[2,1] = sd_of_te_vector_for_pn_0
  normalize_value_of_te_vector_for_pn_0 = dnorm(te_vector_for_pn_0,mean_of_te_vector_for_pn_0,sd_of_te_vector_for_pn_0)
  
  mean_of_te_vector_for_pn_1 = mean(te_vector_for_pn_1)
  sd_of_te_vector_for_pn_1 =  sd(te_vector_for_pn_1)
  mean_sd_val[1,2] = mean_of_te_vector_for_pn_1
  mean_sd_val[2,2] = sd_of_te_vector_for_pn_1
  normalize_value_of_te_vector_for_pn_1 = dnorm(te_vector_for_pn_1,mean_of_te_vector_for_pn_1,sd_of_te_vector_for_pn_1)
  
  fit_te_given_pn[1,1] = 0
  fit_te_given_pn[1,2] = length(which(normalize_value_of_te_vector_for_pn_0 < 0.5)) / length(normalize_value_of_te_vector_for_pn_0)
  fit_te_given_pn[1,3] = length(which(normalize_value_of_te_vector_for_pn_0 >= 0.5)) / length(normalize_value_of_te_vector_for_pn_0)
  
  fit_te_given_pn[2,1] = 1
  fit_te_given_pn[2,2] = length(which(normalize_value_of_te_vector_for_pn_1 < 0.5)) / length(normalize_value_of_te_vector_for_pn_1)
  fit_te_given_pn[2,3] = length(which(normalize_value_of_te_vector_for_pn_1 >= 0.5)) / length(normalize_value_of_te_vector_for_pn_1)
  #print(fit_te_given_pn)
  
  # Fit Variable or Node "Visited TB Spot" of our Bayesian Network to our historical data.
  fit_vtb[1] = length(which(historical_data[,3] == 0)) / length(historical_data[,3])
  fit_vtb[2] = length(which(historical_data[,3] == 1)) / length(historical_data[,3])
  #print(fit_vtb)
  
  # Fit conditional distribution of variable "TB" of our Bayesian Network to our historical data.
  indices_where_vtb_is_0 = which(historical_data[,3] == 0)
  indices_where_vtb_is_1 = which(historical_data[,3] == 1)
  
  tb_vector_for_vtb_0 = vector()
  tb_vector_for_vtb_1 = vector()
  
  for(i in 1:length(indices_where_vtb_is_0)){
    tb_vector_for_vtb_0 = c(tb_vector_for_vtb_0, historical_data[indices_where_vtb_is_0[i],4])
  }
  
  for(i in 1:length(indices_where_vtb_is_1)){
    tb_vector_for_vtb_1 = c(tb_vector_for_vtb_1, historical_data[indices_where_vtb_is_1[i],4])
  }
  
  fit_tb_given_vtb[1,1] = 0
  fit_tb_given_vtb[1,2] = length(which(tb_vector_for_vtb_0 == 0)) / length(tb_vector_for_vtb_0)
  fit_tb_given_vtb[1,3] = length(which(tb_vector_for_vtb_0 == 1)) / length(tb_vector_for_vtb_0)
  
  fit_tb_given_vtb[2,1] = 1
  fit_tb_given_vtb[2,2] = length(which(tb_vector_for_vtb_1 == 0)) / length(tb_vector_for_vtb_1)
  fit_tb_given_vtb[2,3] = length(which(tb_vector_for_vtb_1 == 1)) / length(tb_vector_for_vtb_1)
  #print(fit_tb_given_vtb)
  
  # Fit Variable or Node "Pneumonia" of our Bayesian Network to our historical data.
  fit_sm[1] = length(which(historical_data[,5] == 0)) / length(historical_data[,5])
  fit_sm[2] = length(which(historical_data[,5] == 1)) / length(historical_data[,5])
  #print(fit_sm)
  
  # Fit conditional distribution of variable "LC" of our Bayesian Network to our historical data.
  indices_where_sm_is_0 = which(historical_data[,5] == 0)
  indices_where_sm_is_1 = which(historical_data[,5] == 1)
  
  lc_vector_for_sm_0 = vector()
  lc_vector_for_sm_1 = vector()
  
  for(i in 1:length(indices_where_sm_is_0)){
    lc_vector_for_sm_0 = c(lc_vector_for_sm_0, historical_data[indices_where_sm_is_0[i],6])
  }
  
  for(i in 1:length(indices_where_sm_is_1)){
    lc_vector_for_sm_1 = c(lc_vector_for_sm_1, historical_data[indices_where_sm_is_1[i],6])
  }
  
  fit_lc_given_sm[1,1] = 0
  fit_lc_given_sm[1,2] = length(which(lc_vector_for_sm_0 == 0)) / length(lc_vector_for_sm_0)
  fit_lc_given_sm[1,3] = length(which(lc_vector_for_sm_0 == 1)) / length(lc_vector_for_sm_0)
  
  fit_lc_given_sm[2,1] = 1
  fit_lc_given_sm[2,2] = length(which(lc_vector_for_sm_1 == 0)) / length(lc_vector_for_sm_1)
  fit_lc_given_sm[2,3] = length(which(lc_vector_for_sm_1 == 1)) / length(lc_vector_for_sm_1)
  #print(fit_lc_given_sm)
  
  # Fit conditional distribution of variable "BR" of our Bayesian Network to our historical data.
  br_vector_for_sm_0 = vector()
  br_vector_for_sm_1 = vector()
  
  for(i in 1:length(indices_where_sm_is_0)){
    br_vector_for_sm_0 = c(br_vector_for_sm_0, historical_data[indices_where_sm_is_0[i],7])
  }
  
  for(i in 1:length(indices_where_sm_is_1)){
    br_vector_for_sm_1 = c(br_vector_for_sm_1, historical_data[indices_where_sm_is_1[i],7])
  }
  
  fit_br_given_sm[1,1] = 0
  fit_br_given_sm[1,2] = length(which(br_vector_for_sm_0 == 0)) / length(br_vector_for_sm_0)
  fit_br_given_sm[1,3] = length(which(br_vector_for_sm_0 == 1)) / length(br_vector_for_sm_0)
  
  fit_br_given_sm[2,1] = 1
  fit_br_given_sm[2,2] = length(which(br_vector_for_sm_1 == 0)) / length(br_vector_for_sm_1)
  fit_br_given_sm[2,3] = length(which(br_vector_for_sm_1 == 1)) / length(br_vector_for_sm_1)
  #print(fit_br_given_sm)
  
  # Fit conditional distribution of variable "XR" of our Bayesian Network to our historical data.
  indices_where_pn_tb_lc_is_0 = which(historical_data[,1] == 0 & historical_data[,4] == 0 & historical_data[,6] == 0)
  indices_where_pn_tb_0_lc_is_1 = which(historical_data[,1] == 0 & historical_data[,4] == 0 & historical_data[,6] == 1)
  indices_where_pn_0_tb_1_lc_is_0 = which(historical_data[,1] == 0 & historical_data[,4] == 1 & historical_data[,6] == 0)
  indices_where_pn_0_tb_1_lc_is_1 = which(historical_data[,1] == 0 & historical_data[,4] == 1 & historical_data[,6] == 1)
  indices_where_pn_1_tb_0_lc_is_0 = which(historical_data[,1] == 1 & historical_data[,4] == 0 & historical_data[,6] == 0)
  indices_where_pn_1_tb_0_lc_is_1 = which(historical_data[,1] == 1 & historical_data[,4] == 0 & historical_data[,6] == 1)
  indices_where_pn_1_tb_1_lc_is_0 = which(historical_data[,1] == 1 & historical_data[,4] == 1 & historical_data[,6] == 0)
  indices_where_pn_tb_lc_is_1 = which(historical_data[,1] == 1 & historical_data[,4] == 1 & historical_data[,6] == 1)
  
  xr_vector_for_pn_tb_lc_is_0 = vector()
  xr_vector_for_pn_tb_0_lc_is_1 = vector()
  xr_vector_for_pn_0_tb_1_lc_is_0  = vector()
  xr_vector_for_pn_0_tb_1_lc_is_1 = vector()
  xr_vector_for_pn_1_tb_0_lc_is_0 = vector()
  xr_vector_for_pn_1_tb_0_lc_is_1 = vector()
  xr_vector_for_pn_1_tb_1_lc_is_0 = vector()
  xr_vector_for_pn_tb_lc_is_1 = vector()
  
  for(i in 1:length(indices_where_pn_tb_lc_is_0)){
    xr_vector_for_pn_tb_lc_is_0 = c(xr_vector_for_pn_tb_lc_is_0, historical_data[indices_where_pn_tb_lc_is_0[i],8])
  }
  
  for(i in 1:length(indices_where_pn_tb_0_lc_is_1)){
    xr_vector_for_pn_tb_0_lc_is_1 = c(xr_vector_for_pn_tb_0_lc_is_1, historical_data[indices_where_pn_tb_0_lc_is_1[i],8])
  }
  
  for(i in 1:length(indices_where_pn_0_tb_1_lc_is_0)){
    xr_vector_for_pn_0_tb_1_lc_is_0 = c(xr_vector_for_pn_0_tb_1_lc_is_0, historical_data[indices_where_pn_0_tb_1_lc_is_0[i],8])
  }
  
  for(i in 1:length(indices_where_pn_0_tb_1_lc_is_1)){
    xr_vector_for_pn_0_tb_1_lc_is_1 = c(xr_vector_for_pn_0_tb_1_lc_is_1, historical_data[indices_where_pn_0_tb_1_lc_is_1[i],8])
  }
  
  for(i in 1:length(indices_where_pn_1_tb_0_lc_is_0)){
    xr_vector_for_pn_1_tb_0_lc_is_0 = c(xr_vector_for_pn_1_tb_0_lc_is_0, historical_data[indices_where_pn_1_tb_0_lc_is_0[i],8])
  }
  
  for(i in 1:length(indices_where_pn_1_tb_0_lc_is_1)){
    xr_vector_for_pn_1_tb_0_lc_is_1 = c(xr_vector_for_pn_1_tb_0_lc_is_1, historical_data[indices_where_pn_1_tb_0_lc_is_1[i],8])
  }
  
  for(i in 1:length(indices_where_pn_1_tb_1_lc_is_0)){
    xr_vector_for_pn_1_tb_1_lc_is_0 = c(xr_vector_for_pn_1_tb_1_lc_is_0, historical_data[indices_where_pn_1_tb_1_lc_is_0[i],8])
  }
  
  for(i in 1:length(indices_where_pn_tb_lc_is_1)){
    xr_vector_for_pn_tb_lc_is_1 = c(xr_vector_for_pn_tb_lc_is_1, historical_data[indices_where_pn_tb_lc_is_1[i],8])
  }
  
  fit_xr_given_pn_tb_lc[1,1] = 0
  fit_xr_given_pn_tb_lc[1,2] = 0
  fit_xr_given_pn_tb_lc[1,3] = 0
  fit_xr_given_pn_tb_lc[1,4] = length(which(xr_vector_for_pn_tb_lc_is_0 == 0)) / length(xr_vector_for_pn_tb_lc_is_0)
  fit_xr_given_pn_tb_lc[1,5] = length(which(xr_vector_for_pn_tb_lc_is_0 == 1)) / length(xr_vector_for_pn_tb_lc_is_0)
  
  fit_xr_given_pn_tb_lc[2,1] = 0
  fit_xr_given_pn_tb_lc[2,2] = 0
  fit_xr_given_pn_tb_lc[2,3] = 1
  fit_xr_given_pn_tb_lc[2,4] = length(which(xr_vector_for_pn_tb_0_lc_is_1 == 0)) / length(xr_vector_for_pn_tb_0_lc_is_1)
  fit_xr_given_pn_tb_lc[2,5] = length(which(xr_vector_for_pn_tb_0_lc_is_1 == 1)) / length(xr_vector_for_pn_tb_0_lc_is_1)
  
  fit_xr_given_pn_tb_lc[3,1] = 0
  fit_xr_given_pn_tb_lc[3,2] = 1
  fit_xr_given_pn_tb_lc[3,3] = 0
  fit_xr_given_pn_tb_lc[3,4] = length(which(xr_vector_for_pn_0_tb_1_lc_is_0 == 0)) / length(xr_vector_for_pn_0_tb_1_lc_is_0)
  fit_xr_given_pn_tb_lc[3,5] = length(which(xr_vector_for_pn_0_tb_1_lc_is_0 == 1)) / length(xr_vector_for_pn_0_tb_1_lc_is_0)
  
  fit_xr_given_pn_tb_lc[4,1] = 0
  fit_xr_given_pn_tb_lc[4,2] = 1
  fit_xr_given_pn_tb_lc[4,3] = 1
  fit_xr_given_pn_tb_lc[4,4] = length(which(xr_vector_for_pn_0_tb_1_lc_is_1 == 0)) / length(xr_vector_for_pn_0_tb_1_lc_is_1)
  fit_xr_given_pn_tb_lc[4,5] = length(which(xr_vector_for_pn_0_tb_1_lc_is_1 == 1)) / length(xr_vector_for_pn_0_tb_1_lc_is_1)
  
  fit_xr_given_pn_tb_lc[5,1] = 1
  fit_xr_given_pn_tb_lc[5,2] = 0
  fit_xr_given_pn_tb_lc[5,3] = 0
  fit_xr_given_pn_tb_lc[5,4] = length(which(xr_vector_for_pn_1_tb_0_lc_is_0 == 0)) / length(xr_vector_for_pn_1_tb_0_lc_is_0)
  fit_xr_given_pn_tb_lc[5,5] = length(which(xr_vector_for_pn_1_tb_0_lc_is_0 == 1)) / length(xr_vector_for_pn_1_tb_0_lc_is_0)
  
  fit_xr_given_pn_tb_lc[6,1] = 1
  fit_xr_given_pn_tb_lc[6,2] = 0
  fit_xr_given_pn_tb_lc[6,3] = 1
  fit_xr_given_pn_tb_lc[6,4] = length(which(xr_vector_for_pn_1_tb_0_lc_is_1 == 0)) / length(xr_vector_for_pn_1_tb_0_lc_is_1)
  fit_xr_given_pn_tb_lc[6,5] = length(which(xr_vector_for_pn_1_tb_0_lc_is_1 == 1)) / length(xr_vector_for_pn_1_tb_0_lc_is_1)
  
  fit_xr_given_pn_tb_lc[7,1] = 1
  fit_xr_given_pn_tb_lc[7,2] = 1
  fit_xr_given_pn_tb_lc[7,3] = 0
  fit_xr_given_pn_tb_lc[7,4] = length(which(xr_vector_for_pn_1_tb_1_lc_is_0 == 0)) / length(xr_vector_for_pn_1_tb_1_lc_is_0)
  fit_xr_given_pn_tb_lc[7,5] = length(which(xr_vector_for_pn_1_tb_1_lc_is_0 == 1)) / length(xr_vector_for_pn_1_tb_1_lc_is_0)
  
  fit_xr_given_pn_tb_lc[8,1] = 1
  fit_xr_given_pn_tb_lc[8,2] = 1
  fit_xr_given_pn_tb_lc[8,3] = 1
  fit_xr_given_pn_tb_lc[8,4] = length(which(xr_vector_for_pn_tb_lc_is_1 == 0)) / length(xr_vector_for_pn_tb_lc_is_1)
  fit_xr_given_pn_tb_lc[8,5] = length(which(xr_vector_for_pn_tb_lc_is_1 == 1)) / length(xr_vector_for_pn_tb_lc_is_1)
  
  #print(fit_xr_given_pn_tb_lc)
  
  # Fit conditional distribution of variable "DY" of our Bayesian Network to our historical data.
  indices_where_lc_br_is_0 = which(historical_data[,6] == 0 & historical_data[,7] == 0)
  indices_where_lc_0_br_is_1 = which(historical_data[,6] == 0 & historical_data[,7] == 1)
  indices_where_lc_1_br_is_0 = which(historical_data[,6] == 1 & historical_data[,7] == 0)
  indices_where_lc_br_is_1 = which(historical_data[,6] == 1 & historical_data[,7] == 1)
  
  dy_vector_for_lc_br_is_0 = vector()
  dy_vector_for_lc_0_br_is_1 = vector()
  dy_vector_for_lc_1_br_is_0  = vector()
  dy_vector_for_lc_br_is_1 = vector()
  
  for(i in 1:length(indices_where_lc_br_is_0)){
    dy_vector_for_lc_br_is_0 = c(dy_vector_for_lc_br_is_0, historical_data[indices_where_lc_br_is_0[i],9])
  }
  
  for(i in 1:length(indices_where_lc_0_br_is_1)){
    dy_vector_for_lc_0_br_is_1 = c(dy_vector_for_lc_0_br_is_1, historical_data[indices_where_lc_0_br_is_1[i],9])
  }
  
  for(i in 1:length(indices_where_lc_1_br_is_0)){
    dy_vector_for_lc_1_br_is_0 = c(dy_vector_for_lc_1_br_is_0, historical_data[indices_where_lc_1_br_is_0[i],9])
  }
  
  for(i in 1:length(indices_where_lc_br_is_1)){
    dy_vector_for_lc_br_is_1 = c(dy_vector_for_lc_br_is_1, historical_data[indices_where_lc_br_is_1[i],9])
  }
  
  fit_dy_given_lc_br[1,1] = 0
  fit_dy_given_lc_br[1,2] = 0
  fit_dy_given_lc_br[1,3] = length(which(dy_vector_for_lc_br_is_0 == 0)) / length(dy_vector_for_lc_br_is_0)
  fit_dy_given_lc_br[1,4] = length(which(dy_vector_for_lc_br_is_0 == 1)) / length(dy_vector_for_lc_br_is_0)
  
  fit_dy_given_lc_br[2,1] = 0
  fit_dy_given_lc_br[2,2] = 1
  fit_dy_given_lc_br[2,3] = length(which(dy_vector_for_lc_0_br_is_1 == 0)) / length(dy_vector_for_lc_0_br_is_1)
  fit_dy_given_lc_br[2,4] = length(which(dy_vector_for_lc_0_br_is_1 == 1)) / length(dy_vector_for_lc_0_br_is_1)
  
  fit_dy_given_lc_br[3,1] = 1
  fit_dy_given_lc_br[3,2] = 0
  fit_dy_given_lc_br[3,3] = length(which(dy_vector_for_lc_1_br_is_0 == 0)) / length(dy_vector_for_lc_1_br_is_0)
  fit_dy_given_lc_br[3,4] = length(which(dy_vector_for_lc_1_br_is_0 == 1)) / length(dy_vector_for_lc_1_br_is_0)
  
  fit_dy_given_lc_br[4,1] = 1
  fit_dy_given_lc_br[4,2] = 1
  fit_dy_given_lc_br[4,3] = length(which(dy_vector_for_lc_br_is_1 == 0)) / length(dy_vector_for_lc_br_is_1)
  fit_dy_given_lc_br[4,4] = length(which(dy_vector_for_lc_br_is_1 == 1)) / length(dy_vector_for_lc_br_is_1)
  #print(fit_dy_given_lc_br)
  
  c[[1]] <- fit_pn
  c[[2]] <- fit_te_given_pn
  c[[3]] <- fit_vtb
  c[[4]] <- fit_tb_given_vtb
  c[[5]] <- fit_sm
  c[[6]] <- fit_lc_given_sm
  c[[7]] <- fit_br_given_sm
  c[[8]] <- fit_xr_given_pn_tb_lc
  c[[9]] <- fit_dy_given_lc_br
  c[[10]] <- mean_sd_val
  
  return(c)
}

revert_binary <- function(x){
  
  if(x == 0){
    x = 1
  }else{
    x = 0
  }
  return(x)
  
}

calculateProbability <- function(network,case,mean_val,sd_val){
  pn = 0
  te = 0
  vtb = 0
  tb = 0
  sm = 0
  lc = 0
  br = 0
  xr = 0
  dy = 0
  
  pn = case[1]
  if(pn[,1] == 0){
    pn = network[[1]][1]
    
    te = case[2]
    #mean_val = network[[10]][1,1]
    #sd_val = network[[10]][2,1]
    normalize_value_of_te_for_pn_1 = dnorm(te[,1],mean_val,sd_val)
    
    if(normalize_value_of_te_for_pn_1 < 0.5){
      te = network[[2]][1,2]
    }else{
      te = network[[2]][1,3]
    }
    
  }else{
    pn = network[[1]][2]
    
    te = case[2]
    #mean_val = network[[10]][1,2]
    #sd_val = network[[10]][2,2]
    normalize_value_of_te_for_pn_1 = dnorm(te[,1],mean_val,sd_val)
    
    if(normalize_value_of_te_for_pn_1 < 0.5){
      te = network[[2]][2,2]
    }else{
      te = network[[2]][2,3]
    }
  }
  vtb = case[3]
  if(vtb[,1] == 0){
    vtb = network[[3]][1]
    
    tb = case[4]
    
    if(tb[,1] == 0){
      tb = network[[4]][1,2]
    }else{
      tb = network[[4]][1,3]
    }
    
  }else{
    vtb = network[[3]][2]
    
    tb = case[4]
    
    if(tb[,1] == 0){
      tb = network[[4]][2,2]
    }else{
      tb = network[[4]][2,3]
    }
  }
  
  sm = case[5]
  if(sm[,1] == 0){
    sm = network[[5]][1]
    
    lc = case[6]
    br = case[7]
    
    if(lc[,1] == 0){
      lc = network[[6]][1,2]
    }else{
      lc = network[[6]][1,3]
    }
    
    if(br[,1] == 0){
      br = network[[7]][1,2]
    }else{
      br = network[[7]][1,3]
    }
    
  }else{
    sm = network[[5]][2]
    
    lc = case[6]
    br = case[7]
    
    if(lc[,1] == 0){
      lc = network[[6]][2,2]
    }else{
      lc = network[[6]][2,3]
    }
    
    if(br[,1] == 0){
      br = network[[7]][1,2]
    }else{
      br = network[[7]][1,3]
    }
  }
  
  xr = case[8]
  if(xr[,1] == 0){
    if(pn == 0 && tb == 0 && lc == 0){
      xr = network[[8]][1,4]
    }else if(pn == 0 && tb == 0 && lc == 1){
      xr = network[[8]][2,4]
    }else if(pn == 0 && tb == 1 && lc == 0){
      xr = network[[8]][3,4]
    }else if(pn == 0 && tb == 1 && lc == 1){
      xr = network[[8]][4,4]
    }else if(pn == 1 && tb == 0 && lc == 1){
      xr = network[[8]][5,4]
    }else if(pn == 1 && tb == 0 && lc == 1){
      xr = network[[8]][6,4]
    }else if(pn == 1 && tb == 1 && lc == 0){
      xr = network[[8]][7,4]
    }else {
      xr = network[[8]][8,4]
    }
  }else{
    if(pn == 0 && tb == 0 && lc == 0){
      xr = network[[8]][1,5]
    }else if(pn == 0 && tb == 0 && lc == 1){
      xr = network[[8]][2,5]
    }else if(pn == 0 && tb == 1 && lc == 0){
      xr = network[[8]][3,5]
    }else if(pn == 0 && tb == 1 && lc == 1){
      xr = network[[8]][4,5]
    }else if(pn == 1 && tb == 0 && lc == 1){
      xr = network[[8]][5,5]
    }else if(pn == 1 && tb == 0 && lc == 1){
      xr = network[[8]][6,5]
    }else if(pn == 1 && tb == 1 && lc == 0){
      xr = network[[8]][7,5]
    }else {
      xr = network[[8]][8,5]
    }
  }
  
  dy = case[9]
  if(dy[,1] == 0){
    if(lc == 0 && br == 0){
      dy = network[[9]][1,3]
    }else if (lc == 0 && br == 1){
      dy = network[[9]][2,3]
    }else if (lc == 1 && br == 0){
      dy = network[[9]][3,3]
    }else {
      dy = network[[9]][4,3]
    }
  }else{
    if(lc == 0 && br == 0){
      dy = network[[9]][1,4]
    }else if (lc == 0 && br == 1){
      dy = network[[9]][2,4]
    }else if (lc == 1 && br == 0){
      dy = network[[9]][3,4]
    }else {
      dy = network[[9]][4,4]
    }
  }
  
  probability = pn*te*vtb*tb*sm*lc*br*xr*dy
  return(probability)
}

diagnose <- function(input_network, cases){
  network = input_network
  output_matrix = matrix(0, nrow = 10, ncol = 4)
  mean_val = mean(cases[,2])
  sd_val = sd(cases[,2])
  for(k in 1:length(cases[,1])){
    samples = matrix(0, nrow = 1000, ncol = 9)
    burn_samples = 100
    total_samples = length(samples[,1]) + burn_samples
    row = 0
    indices_of_unknown_variables = which(is.na(cases[k,]))
    for(j in 1:total_samples){
      
      for(i in 1:length(indices_of_unknown_variables)){
        random_binary <- rbinom(1, 1, 0.5)
        cases[k,indices_of_unknown_variables[i]] = random_binary
      }
      
      for(i in 1:length(indices_of_unknown_variables)){
        probability_old = calculateProbability(network,cases[k,],mean_val,sd_val)
        
        cases[k,indices_of_unknown_variables[i]] = revert_binary(cases[k,indices_of_unknown_variables[i]])
        probability_new = calculateProbability(network,cases[k,],mean_val,sd_val)
        if(probability_new > probability_old){
          
        }else{
          if(probability_old <= 0){
            probability_old = 1
          }
          benchmark = probability_new / probability_old
          generated_random_number = runif(1, min=0, max=1)
          
          if(benchmark < generated_random_number){
            
          }else{
            cases[k,indices_of_unknown_variables[i]] = random_binary
          }
        }
      }
      
      if(j > 100){
        row = row + 1
        samples[row,1] = cases[k,1]
        samples[row,2] = cases[k,2]
        samples[row,3] = cases[k,3]
        samples[row,4] = cases[k,4]
        samples[row,5] = cases[k,5]
        samples[row,6] = cases[k,6]
        samples[row,7] = cases[k,7]
        samples[row,8] = cases[k,8]
        samples[row,9] = cases[k,9]
      }
      
    }
    sample_data_frame <- data.frame(
      Pn = c(samples[,1]),
      Te = c(samples[,2]),
      VTB = c(samples[,3]),
      TB = c(samples[,4]),
      Sm = c(samples[,5]),
      LC = c(samples[,6]),
      Br = c(samples[,7]),
      XR = c(samples[,8]),
      Dy = c(samples[,9])
    )
    output_network = learn(sample_data_frame)
    if(cases[k,1] == 0 & cases[k,4] == 0 & cases[k,6] == 0 & cases[k,7] == 0){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2]
      }else{
        output_matrix[k,1] = 0
      }
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else if (cases[k,1] == 0 & cases[k,4] == 0 & cases[k,6] == 0 & cases[k,7] == 1){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2] 
      }else{
        output_matrix[k,1] = 0
      }
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }else if (cases[k,1] == 0 & cases[k,4] == 0 & cases[k,6] == 1 & cases[k,7] == 0){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2]
      }else{
        output_matrix[k,1] = 0
      }
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else if (cases[k,1] == 0 & cases[k,4] == 0 & cases[k,6] == 1 & cases[k,7] == 1){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2]
      }else{
        output_matrix[k,1] = 0
      }
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }else if (cases[k,1] == 0 & cases[k,4] == 1 & cases[k,6] == 0 & cases[k,7] == 0){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2]
      }else{
        output_matrix[k,1] = 0
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else if (cases[k,1] == 0 & cases[k,4] == 1 & cases[k,6] == 0 & cases[k,7] == 1){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2]
      }else{
        output_matrix[k,1] = 0
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }else if (cases[k,1] == 0 & cases[k,4] == 1 & cases[k,6] == 1 & cases[k,7] == 0){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2]
      }else{
        output_matrix[k,1] = 0
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else if (cases[k,1] == 0 & cases[k,4] == 1 & cases[k,6] == 1 & cases[k,7] == 1){
      if (cases[k,8] == 1 & ((cases[k,3] == 0 & cases[k,5] == 0) | cases[k,2] > 40)){
        output_matrix[k,1] = output_network[[1]][,2]
      }else{
        output_matrix[k,1] = 0
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }else if (cases[k,1] == 1 & cases[k,4] == 0 & cases[k,6] == 0 & cases[k,7] == 0){
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else if (cases[k,1] == 1 & cases[k,4] == 0 & cases[k,6] == 0 & cases[k,7] == 1){
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }else if (cases[k,1] == 1 & cases[k,4] == 0 & cases[k,6] == 1 & cases[k,7] == 0){
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else if (cases[k,1] == 1 & cases[k,4] == 0 & cases[k,6] == 1 & cases[k,7] == 1){
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      
      if (cases[k,3] == 1 & cases[k,8] == 1){
        output_matrix[k,2] = output_network[[4]][2,3]
      }else{
        output_matrix[k,2] = 0
      }
      
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }else if (cases[k,1] == 1 & cases[k,4] == 1 & cases[k,6] == 0 & cases[k,7] == 0){
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else if (cases[k,1] == 1 && cases[k,4] == 1 && cases[k,6] == 0 && cases[k,7] == 1){
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,8] == 1){
        output_matrix[k,3] = output_network[[6]][2,3]
      }else{
        output_matrix[k,3] = 0
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }else if (cases[k,1] == 1 & cases[k,4] == 1 & cases[k,6] == 1 & cases[k,7] == 0){
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if (cases[k,5] == 1 & cases[k,9] == 1){
        output_matrix[k,4] = output_network[[7]][2,3]
      }else{
        output_matrix[k,4] = 0
      }
    }else {
      if(cases[k,8] == 0 & cases[k,2] < 40){
        output_matrix[k,1] = 0
      }else{
        output_matrix[k,1] = output_network[[1]][,2]
      }
      if(cases[k,3] == 0){
        output_matrix[k,2] = 0
      }else{
        output_matrix[k,2] = output_network[[4]][2,3]
      }
      if(cases[k,5] == 0 | (cases[k,8] == 0 & cases[k,9] == 0)){
        output_matrix[k,3] = 0
      }else{
        output_matrix[k,3] = output_network[[6]][2,3]
      }
      if(cases[k,5] == 0 | cases[k,9] == 0){
        output_matrix[k,4] = 0
      }else{
        output_matrix[k,4] = output_network[[7]][2,3]
      }
    }
    network = output_network
  }
  return(output_matrix)
}