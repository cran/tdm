all.Amianhirss<-function(){
     cat("\n")                                                                       # show input and output information make user convenience to use                                                                        
     cat("--------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR input data information--     \n")
     note_for_Theirss_input()
     cat("--------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmianhirSSpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),
                               value=c(1,65,167,0,1,300,8,8,5.67))                  # edit table of aminophylline anhydrous input data information                                               
     AmianhirSSpar<-edit(AmianhirSSpar)                                             # show table of aminophylline anhydrous input data information for user editing                                                                                                                                           
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(AmianhirSSpar);cat("\n\n")
##     AmianhirSSpar<-ycheck(AmianhirSSpar)                                         # avoid user missing input information
     cat("\n")                                                                      # 空一行
     Amianhir.ss(AmianhirSSpar[9,2],AmianhirSSpar[7,2],AmianhirSSpar[8,2],AmianhirSSpar[6,2],AmianhirSSpar[3,2],AmianhirSSpar[2,2],AmianhirSSpar[5,2],AmianhirSSpar[1,2],AmianhirSSpar[4,2])    # calculate individual aminophylline anhydrous PK parameters and show its prediction
     note_for_convergence_plots()
     ### convergence_plots_sep()
     X <- read.table("params.csv",header=FALSE)
     cl_F <- X[1,2]
     v_F  <- X[2,2]
     cat("---------------------------------------\n")
     cat("       -- Output Annotations--         \n")
     note_for_Theirss_output()
     cat("---------------------------------------\n")
     ### show(samplesStats("*"))                                                     # show predicted PK parameters of aminophylline anhydrous 
     cat("\n") 
     C1<-TheIRsscpr(0.85,AmianhirSSpar[6,2],AmianhirSSpar[7,2],AmianhirSSpar[8,2])   # C1 is obs conc.; calculate predicted steady-state measured concentration of aminophylline anhydrous (equation of _phylline IR concentration)
     ### sim<-matrix(C[1 ,1])                                                        # 取表格[1,1]之答案
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))  # 計算tmax
     C2<-TheIRsscpr(0.85,AmianhirSSpar[6,2],AmianhirSSpar[7,2],Tmax)                 # calculate predicted steady-state peak concentration of aminophylline anhydrous
     C3<-TheIRsscpr(0.85,AmianhirSSpar[6,2],AmianhirSSpar[7,2],AmianhirSSpar[7,2])   # calculate predicted steady-state trough concentration of aminophylline anhydrous
     coutput<-data.frame(conc=c("Cmss_obs","** Cmss_pr","** Cpss_pr","** Ctss_pr"),value=c(AmianhirSSpar[9,2],C1,C2,C3))
     show(coutput)     
     cat("\n")    
     Amianhir.more()                          # can be found in adjustss.more()
}