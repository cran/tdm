note_for_close_window <- function(){
     cat("     Please enter all parameters values at Data Editor          \n")      
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
}


note_for_plot <- function(){
     cat("----------------------------------------------------------\n")
     cat("   You can PgUp/PgDown to scroll through your checking    \n")
     cat("   convergence plots.                                     \n")  
     cat("----------------------------------------------------------\n\n")
}

     
note_for_convergence_plots <- function(){
cat(" ----------------------------------------------------------\n") 
cat(" Please note that a good convergence of Markov chains      \n") 
cat(" does not necessarily to guarantee an accurate prediction. \n")       
cat(" ----------------------------------------------------------\n")        
    }
    
    
    
note_for_infusion_c_to_d <- function(){
     cat("--------------------------------------------\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("--------------------------------------------\n\n")
         }
         
note_for_infusion_d_to_c <- function(){
     cat("--------------------------------------------\n")                                     
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("--------------------------------------------\n\n")
         }

note_for_infusionR_c_to_d <- function(){
     cat("--------------------------------------------------\n")
     cat("  --input data--                                  \n")
     cat("  Css_trough = desired trough conc (mg/L)         \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  R = constant intravenous infusion rate (mg/hr)  \n")
     cat("--------------------------------------------------\n\n")
         }
         
note_for_infusionR_d_to_c <- function(){
     cat("-------------------------------------------------\n")                                     
     cat("  --input data--                                 \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("                                                 \n")
     cat("  --output data--                                \n")
     cat("  Css_trough = predicted trough conc (mg/L)      \n")
     cat("-------------------------------------------------\n\n")
         }

note_for_c_to_d <- function(){ 
     cat("--------------------------------------------\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("--------------------------------------------\n\n")
 }
 
note_for_d_to_c <- function(){ 
     cat("--------------------------------------------\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("--------------------------------------------\n\n")
 } 
 

note_for_c_to_d_ng <- function(){ 
     cat("--------------------------------------------\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (ng/mL)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("--------------------------------------------\n\n")
 }
 
note_for_d_to_c_ng <- function(){ 
     cat("----------------------------------------------\n")
     cat("  --input data--                              \n")
     cat("  D = desired dose (mg)                       \n")
     cat("  tau = desired dosing interval (hr)          \n")
     cat("                                              \n")
     cat("  --output data--                             \n")
     cat("  Css_trough = predicted trough conc (ng/mL)  \n")
     cat("----------------------------------------------\n\n")
 }
 
 
note_for_c_to_d_mcg <- function(){ 
     cat("---------------------------------------------\n")
     cat("  --input data--                             \n")
     cat("  Css_trough = desired trough conc (mcg/mL)  \n")
     cat("  tau = desired dosing interval (hr)         \n")
     cat("                                             \n")
     cat("  --output data--                            \n")
     cat("  Dose = predicted dose (mg)                 \n")
     cat("---------------------------------------------\n\n")
 }
 
note_for_d_to_c_mcg <- function(){ 
     cat("-----------------------------------------------\n")
     cat("  --input data--                               \n")
     cat("  D = desired dose (mg)                        \n")
     cat("  tau = desired dosing interval (hr)           \n")
     cat("                                               \n")
     cat("  --output data--                              \n")
     cat("  Css_trough = predicted trough conc (mcg/mL)  \n")
     cat("-----------------------------------------------\n\n")
 }
 
note_for_c_to_d_meq <- function(){ 
     cat("--------------------------------------------\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mEq/L)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("--------------------------------------------\n\n")
 }
 
note_for_d_to_c_meq <- function(){ 
     cat("----------------------------------------------\n")
     cat("  --input data--                              \n")
     cat("  D = desired dose (mg)                       \n")
     cat("  tau = desired dosing interval (hr)          \n")
     cat("                                              \n")
     cat("  --output data--                             \n")
     cat("  Css_trough = predicted trough conc (mEq/L)  \n")
     cat("----------------------------------------------\n\n")
 }
 
note_for_c_to_d_eno <- function(){ 
     cat("-----------------------------------------------------------------\n")
     cat("  --input data--                                                 \n")
     cat("  Amax = desired anti-Xa maximal activity at stedy state (IU/mL) \n")
     cat("  tau = desired dosing interval (hr)                             \n")
     cat("                                                                 \n")
     cat("  --output data--                                                \n")
     cat("  Dose = predicted dose (IU,1 mg = 100 IU)                       \n")
     cat("-----------------------------------------------------------------\n\n")
 }
 
note_for_d_to_c_eno <- function(){ 
     cat("--------------------------------------------------------------------\n")
     cat("  --input data--                                                    \n")
     cat("  D = desired dose (IU,1mg=100IU)                                   \n")
     cat("  tau = desired dosing interval (hr)                                \n")
     cat("                                                                    \n")
     cat("  --output data--                                                   \n")
     cat("  Amax = predicted anti-Xa maximal activity at stedy state (IU/mL)  \n")
     cat("--------------------------------------------------------------------\n\n")
 }
 
note_for_c_to_d_war <- function(){ 
     cat("--------------------------------------------------\n")
     cat("  --input data--                                  \n")
     cat("  INR = predicted international normalized ratio  \n")
     cat("  tau = desired dosing interval (day)             \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  Dose = desired/predicted dose (mg)              \n")
     cat("--------------------------------------------------\n\n")
 }
 
note_for_d_to_c_war <- function(){ 
     cat("--------------------------------------------------\n")
     cat("  --input data--                                  \n")
     cat("  D = desired dose (mg)                           \n")
     cat("  tau = desired dosing interval (day)             \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  INR = predicted international normalized ratio  \n")
     cat("--------------------------------------------------\n\n")
 }
    
note_for_Amiss_input <- function(){
     cat("-------------------------------------------------------\n")
     cat("    --Aminoglycoside input data information--          \n")
     cat("    Gender: Male=1 ; Female=0                         \n")
     cat("    age = age(yr)                                      \n")
     cat("    bw = body weight(kg)                               \n")
     cat("    Ht = height(cm)                                    \n")
     cat("    Scr = serum creatinine conc.(mg/dL)                \n")
     cat("    D = dose for each dosing interval(mg)              \n")
     cat("    tau = dosing interval(hr)                          \n")
     cat("    tin = infusion time(hr)                            \n")
     cat("    ts = sampling time since infusion end (hr)         \n")
     cat("    c = measured steady-state conc.(mg/L)              \n") 
     cat("-------------------------------------------------------\n")
}

note_for_Amiss_output <- function(){
     cat("------------------------------------------------------------\n")
     cat("    --Aminoglycoside output data information--              \n")
     cat("    cl = estimated clerance (L/hr)                          \n")
     cat("    v = estimated volume of distribution (L)                \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)   \n")
     cat("    Cpss_pr = predicted steady-state peak conc.(mg/L)       \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)     \n")
     cat("------------------------------------------------------------\n")
}

note_for_Amism_input <- function(){
     cat("-----------------------------------------------------\n")
     cat("    --Aminoglycoside input data information--        \n")
     cat("    Gender = Male=1 ; Female=0                       \n")
     cat("    age = age(yr)                                    \n")
     cat("    bw = body weight(kg)                             \n")
     cat("    Ht = height(cm)                                  \n")
     cat("    Scr = serum creatinine conc.(mg/dL)              \n")
     cat("    D = dose for each dosing interval(mg)            \n")
     cat("    tau = dosing interval(hr)                        \n")
     cat("    tin = infusion time(hr)                          \n")
     cat("-----------------------------------------------------\n")
}

note_for_Amism_conc_input <- function(){
     cat("-----------------------------------------------------\n")
     cat("    --Aminoglycoside input data information--        \n")
     cat("    ts = sampling time since infusion end (hr)       \n")
     cat("    c = measured steady-state conc.(mg/L)            \n")
     cat("-----------------------------------------------------\n")
}

note_for_Amism_output <- function(){
     cat("-----------------------------------------------------------\n")
     cat("    --Aminoglycoside output data information--             \n")
     cat("    cl = estimated clerance (L/hr)                         \n")
     cat("    v = estimated volume of distribution (L)               \n")
     cat("    Cpss_pr = predicted steady-state peak conc.(mg/L)      \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)    \n")
     cat("-----------------------------------------------------------\n")
}


note_for_CBZss_input <- function(){
     cat("-------------------------------------------------------------\n")
     cat("    --Carbamazepine input data information--                 \n")
     cat("    TBW = body weight(kg)                                    \n")
     cat("    PB = combine Phenobarbital(Y=1;N=0)                      \n")
     cat("    VPA = taking VPA and VPA daily dose >18 mg/kg(Y=1;N=0)   \n")
     cat("    PHT = combine Phenytoin(Yes=1;No=0)                      \n")
     cat("    E = age more than 65yr(Y=1;N=0)                          \n")
     cat("    D = dose for each dosing interval(mg)                    \n")
     cat("    tau = dosing interval(hr)                                \n")
     cat("    ts = sampling time(hr)                                   \n")
     cat("    c = measured steady-state conc.(mg/L)                    \n")
     cat("-------------------------------------------------------------\n")
}

note_for_CBZss_output <- function(){
     cat("-----------------------------------------------------------------\n")
     cat("    --Carbamazepine output data information--                    \n")
     cat("    cl = estimated clerance/bioavailability (L/hr)               \n")
     cat("    v = estimated volume of distribution/bioavailability (L)     \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)        \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)          \n")
     cat("-----------------------------------------------------------------\n")
}

note_for_CBZsm_input <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Carbamazepine input data information--                  \n")
     cat("    TBW = body weight(kg)                                     \n")
     cat("    PB = combine Phenobarbital(Y=1;N=0)                       \n")
     cat("    VPA = taking VPA and VPA daily dose >18 mg/kg(Y=1;N=0)    \n")
     cat("    PHT = combine Phenytoin(Y=1;N=0)                          \n")
     cat("    E = age more than 65yr(Y=1;N=0)                           \n")
     cat("    D = dose for each dosing interval(mg)                     \n")
     cat("    tau = dosing interval(hr)                                 \n")
     cat("--------------------------------------------------------------\n")
}

note_for_CBZsm_conc_input <- function(){
     cat("---------------------------------------------------\n")
     cat("    --Carbamazepine input data information--       \n")
     cat("    ts = sampling time(hr)                         \n")
     cat("    conc = measured steady-state conc.(mg/L)       \n")
     cat("---------------------------------------------------\n")
}

note_for_CBZsm_output <- function(){
     cat("------------------------------------------------------------------\n")
     cat("    --Carbamazepine output data information--                     \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)              \n")
     cat("    V_F = estimated volume of distribution/bioavailability (L)    \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)           \n")
     cat("------------------------------------------------------------------\n")
}


note_for_Dig_input <- function(){
     cat("------------------------------------------------\n")
     cat("    --Digoxin input data information--          \n")
     cat("    Gender = Male=1 ; Female=0                  \n")
     cat("    age = age(yr)                               \n")
     cat("    bw = body weight(kg)                        \n")
     cat("    scr = serum creatinine conc.(mg/dL)         \n")
     cat("    D = dose for each dosing interval(mg)       \n")
     cat("    tau = dosing interval(hr)                   \n")
     cat("    c = measured steady-state conc.(ng/mL)      \n")
     cat("------------------------------------------------\n")
}

note_for_Dig_output <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Digoxin output data information--                       \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)          \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(ng/mL)    \n")
     cat("--------------------------------------------------------------\n")
}


note_for_Lit_input <- function(){
     cat("-----------------------------------------------------\n")
     cat("    --Lithium carbonate input data information--     \n")
     cat("    age = age (yr)                                   \n") 
     cat("    f = age<50 (Y=0,N=1)                             \n") 
     cat("    bw = body weight(kg)                             \n")
     cat("    Scr = serum creatinine conc (mg/dL)              \n")   
     cat("    D = dose for each dosing interval(mg)            \n")
     cat("    tau = dosing interval(hr)                        \n")
     cat("    c = measured steady-state conc.(mEq/L)           \n")  
     cat("-----------------------------------------------------\n")
}

note_for_Lit_output <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Lithium carbonate output data information--             \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)          \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mEq/L)    \n")
     cat("--------------------------------------------------------------\n")
}


note_for_Litcit_input <- function(){
     cat("----------------------------------------------------\n")
     cat("    --Lithium citrate input data information--      \n")
     cat("    age = age (yr)                                  \n") 
     cat("    f = age<50 (Y=0,N=1)                            \n") 
     cat("    bw = body weight(kg)                            \n")
     cat("    Scr = serum creatinine conc (mg/dL)             \n")   
     cat("    D = dose for each dosing interval(mg)           \n")
     cat("    tau = dosing interval(hr)                       \n")
     cat("    c = measured steady-state conc.(mEq/L)          \n")
     cat("----------------------------------------------------\n") 
}

note_for_Litcit_output <- function(){
     cat("--------------------------------------------------------------\n") 
     cat("    --Lithium citrate output data information--               \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)          \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mEq/L)    \n")
     cat("--------------------------------------------------------------\n") 
}


note_for_Phe_input <- function(){
     cat("----------------------------------------------------\n")
     cat("    --Phenytoin input data information--            \n")
     cat("    bw = body weight(kg)                            \n")
     cat("    D = dose for each dosing interval(mg)           \n")
     cat("    tau = dosing interval(hr)                       \n")
     cat("    c = measured steady-state conc.(mg/L)           \n")
     cat("----------------------------------------------------\n") 
}

note_for_Phe_output <- function(){
     cat("-----------------------------------------------------------------\n") 
     cat("    --Phenytoin output data information--                        \n")
     cat("    Vamx = estimated maximum rate of metabolism (mg/day)         \n")
     cat("    Km = estimated plasma concentration at which metabolism      \n")
     cat("         is occurring at half the maximum rate (mg/L)            \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)        \n")
     cat("-----------------------------------------------------------------\n") 
}

note_for_Eno_input <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Enoxaparin input data information--                     \n")
     cat("    Gender = 1 for Male ; 0 for Female                        \n") 
     cat("    TBW = body weight(kg)                                     \n")   
     cat("    Scr = serum creatinine conc.(mg/dL)                       \n")
     cat("    D = dose for each dosing interval(IU); 1mg = 100 IU       \n")
     cat("    tau = dosing interval(hr)                                 \n")
     cat("    Amax = anti-Xa maximal activity at stedy state (IU/mL)    \n")
     cat("--------------------------------------------------------------\n") 
}

note_for_Eno_output <- function(){
     cat("-----------------------------------------------------------------------------\n") 
     cat("    --Enoxaparin output data information--                                   \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)                         \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)               \n")
     cat("    Amaxss_pr = predicted anti-Xa maximal activity at stedy state (IU/mL)    \n")
     cat("-----------------------------------------------------------------------------\n") 
}



note_for_War_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Warfarin input data information--           \n")
     cat("    D = dose for each dosing interval(mg)         \n")
     cat("    tau = dosing interval(day)                    \n")
     cat("    INR = international normalized ratio          \n")
     cat("--------------------------------------------------\n") 
}

note_for_War_output <- function(){
     cat("-----------------------------------------------------------------\n") 
     cat("  --Warfarin output data information--                           \n")
     cat("  Cpmax = estimated concentration related with maximum           \n")
     cat("          suppression of clotting factors (mg/L)                 \n")
     cat("  cl_F = estimated clerance/bioavailability (L/day)              \n")
     cat("  kc = estimated first order rate constant for clotting          \n")
     cat("       factor degradation (1/day)                                \n")
     cat("  m = estiamted prothrombin complex activity remaining (%/day)   \n")
     cat("  v_F = estimated volume of distribution/bioavailability (L)     \n")
     cat("  INR = predicted international normalized ratio                 \n")
     cat("-----------------------------------------------------------------\n") 
}



note_for_Valss_input <- function(){
     cat("----------------------------------------------------------------\n")
     cat("    --Valpraote input data information--                        \n")
     cat("    age = age (yr)                                              \n")   
     cat("    INDI(indication)=1 for uncontrolled epilepsy, otherwise 0   \n")
     cat("    CBZ = combine CArbamazepine (Y=1, N=0)                      \n")
     cat("    ka = 4 for oral solution, 1 for enteric tablet              \n")
     cat("    D = dose for each dosing interval(mg)                       \n") 
     cat("    tau = dosing interval(hr)                                   \n")
     cat("    ts = sampling time (hr)                                     \n") 
     cat("    c = measured steady-state conc.(mg/L)                       \n")
     cat("----------------------------------------------------------------\n")
}

note_for_Valss_output <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Valpraote output data information--                     \n")
     cat("    cl = estimated clerance (L/hr)                            \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)     \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)       \n")
     cat("--------------------------------------------------------------\n")
}

note_for_Valsm_input <- function(){
     cat("-----------------------------------------------------------------\n")
     cat("    --Valpraote input data information--                         \n")
     cat("    age = age (yr)                                               \n")   
     cat("    CBZ = combine CArbamazepine (Y=1, N=0)                       \n")
     cat("    INDI(indication)=1 for uncontrolled epilepsy, otherwise 0    \n")
     cat("    ka = 4 for oral solution, 1 for enteric tablet               \n")
     cat("    tau = dosing interval(hr)                                    \n")
     cat("    D = dose for each dosing interval(mg)                        \n") 
     cat("-----------------------------------------------------------------\n")
}

note_for_Valsm_conc_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Valpraote input data information--          \n")
     cat("    c = measured steady-state conc.(mg/L)         \n")
     cat("    ts = sampling time (hr)                       \n") 
     cat("--------------------------------------------------\n")
}

note_for_Valsm_output <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Valpraote output data information--                     \n")
     cat("    cl = estimated clerance (L/hr)                            \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)       \n")
     cat("--------------------------------------------------------------\n")
}



note_for_ChiValss_input <- function(){
     cat("-----------------------------------------------------------\n")
     cat("    --Children Valpraote input data information--          \n")
     cat("    TBW = total body weight (Kg)                           \n")   
     cat("    CBZ = combine CArbamazepine (Y=1, N=0)                 \n")
     cat("    D = dose for each dosing interval(mg)                  \n") 
     cat("    tau = dosing interval(hr)                              \n")
     cat("    ts = sampling time (hr)                                \n") 
     cat("    c = measured steady-state conc.(mg/L)                  \n")   
     cat("-----------------------------------------------------------\n")
}

note_for_ChiValss_output <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Children Valpraote output data information--            \n")
     cat("    cl = estimated clerance (L/hr)                            \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)     \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)       \n")
     cat("--------------------------------------------------------------\n")
}

note_for_ChiValsm_input <- function(){
     cat("-----------------------------------------------------------\n")
     cat("    --Children Valpraote input data information--          \n")
     cat("    TBW = total body weight (Kg)                           \n")   
     cat("    CBZ = combine CArbamazepine (Y=1, N=0)                 \n")
     cat("    D = dose for each dosing interval(mg)                  \n") 
     cat("    tau = dosing interval(hr)                              \n")
     cat("-----------------------------------------------------------\n")
}

note_for_ChiValsm_conc_input <- function(){
     cat("--------------------------------------------------------\n")
     cat("    --Children Valpraote input data information--       \n")
     cat("    c = measured steady-state conc.(mg/L)               \n")
     cat("    ts = sampling time (hr)                             \n") 
     cat("--------------------------------------------------------\n")
}

note_for_ChiValsm_output <- function(){
     cat("--------------------------------------------------------------\n")
     cat("    --Children Valpraote output data information--            \n")
     cat("    cl = estimated clerance (L/hr)                            \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)       \n")
     cat("--------------------------------------------------------------\n")
}


note_for_Vanss_input <- function(){
     cat("--------------------------------------------------------\n")
     cat("    --Vancomycin input data information--               \n")
     cat("    Gender = 1 for Male ; 0 for Female                  \n")
     cat("    age = age(yr)                                       \n")
     cat("    bw = body weight (kg)                               \n")   
     cat("    Scr = serum creatinine conc.(mg/dL)                 \n")
     cat("    D = dose for each dosing interval(mg)               \n")
     cat("    tau = dosing interval(hr)                           \n")
     cat("    tin = infusion time(hr)                             \n") 
     cat("    ts = sampling time since infusion end (hr)          \n") 
     cat("    c = measured steady-state conc.(mg/L)               \n")
     cat("--------------------------------------------------------\n")
}

note_for_Vanss_output <- function(){
     cat("--------------------------------------------------------------\n")
      cat("    --Vancomycin output data information--                   \n")
     cat("    cl = estimated clerance (L/hr)                            \n")
     cat("    v = estimated volume of distribution (L)                  \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)     \n")
     cat("    Cpss_pr = predicted steady-state peak conc.(mg/L)         \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)       \n")
     cat("--------------------------------------------------------------\n")
}

note_for_Vansm_input <- function(){
     cat("---------------------------------------------------\n")
     cat("    --Vancomycin input data information--          \n")
     cat("    Gender = 1 for Male ; 0 for Female             \n")
     cat("    age = age(yr)                                  \n")
     cat("    bw = body weight (kg)                          \n")   
     cat("    Scr = serum creatinine conc.(mg/dL)            \n")
     cat("    D = dose for each dosing interval(mg)          \n")
     cat("    tau = dosing interval(hr)                      \n")
     cat("    tin = infusion time(hr)                        \n") 
     cat("---------------------------------------------------\n")
}

note_for_Vansm_conc_input <- function(){
     cat("----------------------------------------------------\n")
     cat("    --Vancomycin input data information--           \n")
     cat("    c = measured steady-state conc.(mg/L)           \n")
     cat("    ts = sampling time since infusion end (hr)      \n") 
     cat("----------------------------------------------------\n")
}

note_for_Vansm_output <- function(){
     cat("-------------------------------------------------------------\n")
     cat("    --Vancomycin output data information--                   \n")
     cat("    cl = estimated clerance (L/hr)                           \n")
     cat("    v = estimated volume of distribution (L)                 \n")
     cat("    Cpss_pr = predicted steady-state peak conc.(mg/L)        \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)      \n")
     cat("-------------------------------------------------------------\n")
}


note_for_Enfss_input <- function(){
     cat("---------------------------------------------------\n")
     cat("    --Enfuvirtide input data information--         \n")
     cat("    Gender : 1 for Male ; 0 for Female             \n")
     cat("    bw = body weight(kg)                           \n")
     cat("    D = dose for each dosing interval(mg)          \n")
     cat("    tau = dosing interval(hr)                      \n")
     cat("    ts = sampling time(hr)                         \n")
     cat("    c = measured steady-state conc.(mg/L)          \n")
     cat("---------------------------------------------------\n")
}

note_for_Enfss_output <- function(){
     cat("----------------------------------------------------------------------\n")
     cat("    --Enfuvirtide output data information--                           \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)                  \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)        \n")
     cat("    ka = estimated absorption rate constant/bioavailability (1/hr)    \n") 
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)             \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)               \n")
     cat("----------------------------------------------------------------------\n")
}

note_for_Enfsm_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Enfuvirtide input data information--        \n")
     cat("    Gender : 1 for Male ; 0 for Female            \n")
     cat("    bw = body weight(kg)                          \n")
     cat("    D = dose for each dosing interval(mg)         \n")
     cat("    tau = dosing interval(hr)                     \n")
     cat("--------------------------------------------------\n")
}

note_for_Enfsm_conc_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Enfuvirtide input data information--        \n")
     cat("    c = measured steady-state conc.(mg/L)         \n")
     cat("    ts = sampling time(hr)                        \n")
     cat("--------------------------------------------------\n")
}

note_for_Enfsm_output <- function(){
     cat("---------------------------------------------------------------------\n")
     cat("    --Enfuvirtide output data information--                          \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)                 \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)       \n")
     cat("    ka = estimated absorption rate constant/bioavailability (1/hr)   \n") 
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)              \n")
     cat("---------------------------------------------------------------------\n")
}


note_for_Indss_input <- function(){
     cat("-------------------------------------------------\n")
     cat("    --Indinavir input data information--         \n")
     cat("    Gender = 1 for Male ; 0 for Female           \n")
     cat("    bw = body weight(kg)                         \n")
     cat("    Rit = combine Ritonavir(Y=1;N=0)             \n")
     cat("    D = dose for each dosing interval(mg)        \n")
     cat("    tau = dosing interval(hr)                    \n")
     cat("    ts = sampling time(hr)                       \n")
     cat("    c = measured steady-state conc.(mg/L)        \n")   
     cat("-------------------------------------------------\n")
}

note_for_Indss_output <- function(){
     cat("----------------------------------------------------------------------\n")
     cat("   --Indinavir output data information--                              \n")
     cat("   cl_F = estimated clerance/bioavailability (L/hr)                   \n")
     cat("   ka = estimated absorption rate constant/bioavailability (1/hr)     \n") 
     cat("   Cmss_pr = predicted steady-state measured conc.(mg/L)              \n")
     cat("   Ctss_pr = predicted steady-state trough conc.(mg/L)                \n")
     cat("----------------------------------------------------------------------\n")
}

note_for_Indsm_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Indinavir input data information--          \n")
     cat("    Gender = 1 for Male ; 0 for Female            \n")
     cat("    bw = body weight(kg)                          \n")
     cat("    Rit = combine Ritonavir(Y=1;N=0)              \n")
     cat("    D = dose for each dosing interval(mg)         \n")
     cat("    tau = dosing interval(hr)                     \n")
     cat("--------------------------------------------------\n")
}

note_for_Indsm_conc_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Indinavir input data information--          \n")
     cat("    c = measured steady-state conc.(mg/L)         \n")
     cat("    ts = sampling time(hr)                        \n")
     cat("--------------------------------------------------\n")
}

note_for_Indsm_output <- function(){
     cat("---------------------------------------------------------------------\n")
     cat("   --Indinavir output data information--                             \n")
     cat("   cl_F = estimated clerance/bioavailability (L/hr)                  \n")
     cat("   ka = estimated absorption rate constant/bioavailability (1/hr)    \n") 
     cat("   Ctss_pr = predicted steady-state trough conc.(mg/L)               \n")
     cat("---------------------------------------------------------------------\n")
}


note_for_Ritss_input <- function(){
     cat("-------------------------------------------------\n")
     cat("    --Ritonavir input data information--         \n")
     cat("    LPv = combine Lopinavir (Y=1;N=0)            \n")
     cat("    D = dose for each dosing interval(mg)        \n")
     cat("    tau = dosing interval(hr)                    \n")
     cat("    ts = sampling time(hr)                       \n")
     cat("    c = measured steady-stat conc.(mg/L)         \n")    
     cat("-------------------------------------------------\n")
}

note_for_Ritss_output <- function(){
     cat("-------------------------------------------------------------------\n")
     cat("    --Ritonavir output data information--                          \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)               \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)     \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)          \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)            \n")
     cat("-------------------------------------------------------------------\n")
}

note_for_Ritsm_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Ritonavir input data information--          \n")
     cat("    LPV = combine Lopinavir (Y=1;N=0)             \n")
     cat("    D = dose for each dosing interval(mg)         \n")
     cat("    tau = dosing interval(hr)                     \n")
     cat("--------------------------------------------------\n")
}

note_for_Ritsm_conc_input <- function(){
     cat("------------------------------------------------\n")
     cat("    --Ritonavir input data information--        \n")
     cat("    c = measured steady-stat conc.(mg/L)        \n")
     cat("    ts = sampling time(hr)                      \n")
     cat("------------------------------------------------\n")
}

note_for_Ritsm_output <- function(){
     cat("------------------------------------------------------------------\n")
     cat("    --Ritonavir output data information--                         \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)              \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)    \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)           \n")
     cat("------------------------------------------------------------------\n")
}



note_for_Cycss_input <- function(){
     cat("-------------------------------------------------\n")
     cat("    --Cyclosporine input data information--      \n")
     cat("    bw = body weight(kg)                         \n")
     cat("    PTD = post-transplant day(day)               \n")
     cat("    Dia = combine Diatiazem(Y=1;N=0)             \n")
     cat("    D = dose for each dosing interval(mg)        \n")
     cat("    tau = dosing interval(hr)                    \n")
     cat("    ts = sampling time(hr)                       \n")
     cat("    c = measured steady-state conc.(mcg/L)       \n")
     cat("-------------------------------------------------\n")
}

note_for_Cycss_output <- function(){
     cat("-------------------------------------------------------------\n")
     cat("   --Cyclosporine output data information--                  \n")
     cat("   cl = estimated clerance (L/hr)                            \n")
     cat("   Cmss_pr = predicted steady-state measured conc.(mcg/L)    \n")
     cat("   Ctss_pr = predicted steady-state trough conc.(mcg/L)      \n")
     cat("-------------------------------------------------------------\n")
}

note_for_Cycsm_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Cyclosporine input data information--       \n")
     cat("    bw = body weight(kg)                          \n")
     cat("    PTD = post-transplant day(day)                \n")
     cat("    Dia = combine Diatiazem(Y=1;N=0)              \n")
     cat("    D = dose for each dosing interval(mg)         \n")
     cat("    tau = dosing interval(hr)                     \n")
     cat("--------------------------------------------------\n")
}

note_for_Cycsm_conc_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Cyclosporine input data information--       \n")
     cat("    conc = measured steady-state conc.(mcg/L)     \n")
     cat("    ts = sampling time(hr)                        \n")
     cat("--------------------------------------------------\n")
}

note_for_Cycsm_output <- function(){
     cat("------------------------------------------------------------\n")
     cat("   --Cyclosporine output data information--                 \n")
     cat("   cl = estimated clerance (L/hr)                           \n")
     cat("   Ctss_pr = predicted steady-state trough conc.(mcg/L)     \n")
     cat("------------------------------------------------------------\n")
}


note_for_Evess_input <- function(){
     cat("-------------------------------------------------\n")
     cat("    --Everolimus input data information--        \n")
     cat("    bw = body weight(kg)                         \n")
     cat("    age = age(yr)                                \n")
     cat("    race = 1 for black ; otherwise 0             \n")
     cat("    Ery = combine Erythromycin(Y=1;N=0)          \n")
     cat("    D = dose for each dosing interval(mg)        \n")
     cat("    tau = dosing interval(hr)                    \n")
     cat("    ts = sampling time(hr)                       \n")
     cat("    c = measured steady-state conc.(mcg/L)       \n")  
     cat("-------------------------------------------------\n")
}

note_for_Evess_output <- function(){
     cat("--------------------------------------------------------------------\n")
     cat("    --Everolimus output data information--                          \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)                \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)      \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mcg/L)          \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mcg/L)            \n")
     cat("--------------------------------------------------------------------\n")
}

note_for_Evesm_input <- function(){
     cat("-------------------------------------------------\n")
     cat("    --Everolimus input data information--        \n")
     cat("    bw = body weight(kg)                         \n")
     cat("    age = age(yr)                                \n")
     cat("    race = 1 for black ; otherwise 0             \n")
     cat("    Ery = combine Erythromycin(Y=1;N=0)          \n")
     cat("    D = dose for each dosing interval(mg)        \n")
     cat("    tau = dosing interval(hr)                    \n")
     cat("-------------------------------------------------\n")
}

note_for_Evesm_conc_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Everolimus input data information--         \n")
     cat("    c = measured steady-state conc.(mcg/L)        \n")
     cat("    ts = sampling time(hr)                        \n")
     cat("--------------------------------------------------\n")
}

note_for_Evesm_output <- function(){
     cat("-------------------------------------------------------------------\n")
     cat("    --Everolimus output data information--                         \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)               \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)     \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mcg/L)           \n")
     cat("-------------------------------------------------------------------\n")
}



note_for_Tacss_input <- function(){
     cat("------------------------------------------------------\n")
     cat("    --Tacrolimus input data information--             \n")
     cat("    Hem = 0 for hematocrit < 30% , otherwise 1        \n")   
     cat("    Alb = 0 for albumin < 3.5g/dL , otherwise 1       \n") 
     cat("    Dil = combine Diltiazem(Y=1, N=0)                 \n")
     cat("    Flu = combine Fluconazole(Y=1, N=0)               \n")
     cat("    D = dose for each dosing interval(mg)             \n")
     cat("    tau = dosing interval(hr)                         \n")
     cat("    ts = sampling time(hr)                            \n")  
     cat("    c = measured steady-state conc.(mcg/mL)           \n")
     cat("------------------------------------------------------\n")
}

note_for_Tacss_output <- function(){
     cat("--------------------------------------------------------------------\n")
     cat("    --Tacrolimus output data information--                          \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)                \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)      \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mcg/mL)         \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mcg/mL)           \n")
     cat("--------------------------------------------------------------------\n")
}

note_for_Tacsm_input <- function(){
     cat("-----------------------------------------------------\n")
     cat("    --Tacrolimus input data information--            \n")
     cat("    Hem = 0 for hematocrit < 30% , otherwise 1       \n")   
     cat("    Alb = 0 for albumin < 3.5g/dL , otherwise 1      \n") 
     cat("    Dil = combine Diltiazem(Y=1, N=0)                \n")
     cat("    Flu = combine Fluconazole(Y=1, N=0)              \n")
     cat("    D = dose for each dosing interval(mg)            \n")
     cat("    tau = dosing interval(hr)                        \n")
     cat("-----------------------------------------------------\n")
}

note_for_Tacsm_conc_input <- function(){
     cat("--------------------------------------------------\n")
     cat("    --Tacrolimus input data information--         \n")
     cat("    c = measured steady-state conc.(mcg/mL)       \n")
     cat("    ts = sampling time(hr)                        \n") 
     cat("--------------------------------------------------\n")
}

note_for_Tacsm_output <- function(){
     cat("-------------------------------------------------------------------\n")
     cat("    --Tacrolimus output data information--                         \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)               \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)     \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mcg/mL)          \n")
     cat("-------------------------------------------------------------------\n")
}



note_for_Imass_input <- function(){
     cat("------------------------------------------------------\n")
     cat("    --Imatinib mesylate input data information--      \n")
     cat("    BW = body weight(kg)                              \n")
     cat("    OCC = 0 for day 1 and 1 for day 29                \n")
     cat("    Hb = Hemoglobin (g/dL)                            \n")
     cat("    WBC = white blood count (10^9/L)                  \n")
     cat("    n = dosing number                                 \n") 
     cat("    D = dose for each dosing interval(mg)             \n") 
     cat("    tau = dosing interval(hr)                         \n")
     cat("    ts = sampling time (hr)                           \n")
     cat("    c = measured conc.(mg/L)                          \n")
     cat("------------------------------------------------------\n")
}

note_for_Imass_output <- function(){
     cat("------------------------------------------------------------------\n")
     cat("    --Imatinib mesylate output data information--                 \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)              \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)    \n")
     cat("    Cm_pr = predicted measured conc.(mg/L)                        \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)           \n")
     cat("------------------------------------------------------------------\n")
}

note_for_Imasm_input <- function(){
     cat("-------------------------------------------------------\n")
     cat("    --Imatinib mesylate input data information--       \n")
     cat("    BW = body weight(kg)                               \n")
     cat("    OCC = 0 for day 1 and 1 for day 29                 \n")
     cat("    Hb = Hemoglobin (g/dL)                             \n")
     cat("    WBC = white blood count (10^9/L)                   \n")
     cat("    n = dosing number                                  \n") 
     cat("    D = dose for each dosing interval(mg)              \n") 
     cat("    tau = dosing interval(hr)                          \n")
     cat("-------------------------------------------------------\n")
}

note_for_Imasm_conc_input <- function(){
     cat("-------------------------------------------------------\n")
     cat("    --Imatinib mesylate input data information--       \n")
     cat("    c = measured conc.(mg/L)                           \n")
     cat("    ts = sampling time (hr)                            \n")
     cat("-------------------------------------------------------\n")
}

note_for_Imasm_output <- function(){
     cat("------------------------------------------------------------------\n")
     cat("    --Imatinib mesylate output data information--                 \n")
     cat("    cl_F = estimated clerance/bioavailability (L/hr)              \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)    \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)           \n")
     cat("------------------------------------------------------------------\n")
}


note_for_Theirss_input <- function(){
     cat("    Gender = Male=1 ; Female=0                             \n")
     cat("    age = age (yr)                                         \n")
     cat("    ht = height (cm)                                       \n") 
     cat("    CHF = 1 for yes ; otherwise = 0                        \n")
     cat("    smoke = 1 for a amoker ; otherwise 0                   \n") 
     cat("    D = dose for each dosing interval (mg)                 \n") 
     cat("    tau = dosing interval (hr)                             \n")
     cat("    ts = sampling time (hr)                                \n")   
     cat("    C = measured steady-state conc.                        \n")
}

note_for_Theirss_output <- function(){
     cat("    cl_F = estimated clerance/bioavailability (L/hr)              \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)    \n")
     cat("    Cmss_pr = predicted steady-state measured conc.(mg/L)         \n")
     cat("    Cpss_pr = predicted steady-state peak conc.(mg/L)             \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)           \n")
}

note_for_Theirsm_input <- function(){
     cat("    Gender = Male=1 ; Female=0                               \n")
     cat("    age = age (yr)                                           \n")
     cat("    ht = height (cm)                                         \n") 
     cat("    CHF = 1 for yes ; otherwise = 0                          \n")
     cat("    smoke = 1 for a amoker ; otherwise 0                     \n") 
     cat("    D = dose for each dosing interval (mg)                   \n") 
     cat("    tau = dosing interval (hr)                               \n")  
}

note_for_Thesm_conc_input <- function(){
     cat("    c = measured steady-state conc.(mg/L)                    \n")
     cat("    ts = sampling time since infusion end  (hr)              \n")
}

note_for_Theirsm_output <- function(){
     cat("    cl_F = estimated clerance/bioavailability (L/hr)              \n")
     cat("    v_F = estimated volume of distribution/bioavailability (L)    \n")
     cat("    Cpss_pr = predicted steady-state peak conc.(mg/L)             \n")
     cat("    Ctss_pr = predicted steady-state trough conc.(mg/L)           \n")
}

note_for_Thecrss_input <- function(){
     cat("    Gender = Male=1 ; Female=0                             \n")
     cat("    age = age (yr)                                         \n")
     cat("    ht = height (cm)                                       \n") 
     cat("    CHF = 1 for yes ; otherwise = 0                        \n")
     cat("    smoke = 1 for a amoker ; otherwise 0                   \n") 
     cat("    D = dose for each dosing interval (mg)                 \n") 
     cat("    tau = dosing interval (hr)                             \n")
     cat("    C = measured steady-state conc.                        \n")
}

note_for_Thecr_output <- function(){
     cat("   cl_F = estimated clerance/bioavailability (L/hr)        \n")
     cat("   Css_pr = predicted steady-state conc.(mg/L)             \n")
}

note_for_Theinfusionss_input <- function(){
     cat("    Gender = Male=1 ; Female=0                             \n")
     cat("    age = age (yr)                                         \n")
     cat("    ht = height (cm)                                       \n") 
     cat("    CHF = 1 for yes ; otherwise = 0                        \n")
     cat("    smoke = 1 for a amoker ; otherwise 0                   \n") 
     cat("    DL = loading dose (mg)                                 \n")
     cat("    tinf = infusion time of loading dose (hr)              \n") 
     cat("    R = constant intravenous infusion rate (mg/hr)         \n") 
     cat("    ts = sampling time since infusion end (hr)             \n")
     cat("    C = measured conc. (mg/L)                             \n")
}

note_for_Theinfusionss_output <- function(){
     cat("   cl = estimated clerance (L/hr)                                    \n")
     cat("   v = estimated volume of distribution (L)                          \n")
     cat("   Cm_pr = predicted measured conc.(mg/L)                            \n")
     cat("   Ctss_pr = predicted steady-state trough conc.(mg/L)               \n")
}

note_for_Theinfusionsm_input <- function(){
     cat("    Gender = Male=1 ; Female=0                             \n")
     cat("    age = age (yr)                                         \n")
     cat("    ht = height (cm)                                       \n") 
     cat("    CHF = 1 for yes ; otherwise = 0                        \n")
     cat("    smoke = 1 for a amoker ; otherwise 0                   \n") 
     cat("    DL = loading dose (mg)                                 \n")
     cat("    tinf = infusion time of loading dose (hr)              \n") 
     cat("    R = constant intravenous infusion rate (mg/hr)         \n") 
}

note_for_Theinfusionsm_output <- function(){
     cat("   cl = estimated clerance (L/hr)                                    \n")
     cat("   v = estimated volume of distribution (L)                          \n")
     cat("   Ctss_pr = predicted steady-state trough conc.(mg/L)               \n")
}

