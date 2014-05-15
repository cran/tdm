note_for_ocIR_input <- function(){
     cat("-----------------------------------------------------\n")
     cat("    --Oxycodone input data information--           \n\n")
     cat("      D = dose for the 1st-dose (mg)                 \n")
     cat("      phenotype: 1=UM, 2=EM, 3=PM                    \n")
     cat("-----------------------------------------------------\n")
}

note_for_OO_conc_input <- function(){
     cat("-----------------------------------------------------\n")
     cat("    --Opioid input data information--              \n\n")
     cat("      ts = sampling time after the 1st dose (hr)     \n")
     cat("    conc = measured opioids conc.(ng/mL)             \n")
     cat("-----------------------------------------------------\n")
}

note_for_OO_output <- function(){
     cat("-----------------------------------------------------------\n")
     cat("    --Opioid output data information--             \n\n")
     cat("       cl_F = estimated clearance (L/hr)             \n")
     cat("        v_F = estimated volume of distribution (L)   \n")
     cat("    Cmax_ss = calculated peak conc.(ng/mL) at SS     \n")
     cat("    Cmin_ss = calculated trough conc.(ng/mL) at SS   \n")
     cat("         SS = steady-state    \n")
     cat("-----------------------------------------------------------\n")
}
note_for_OO_input <- function(){
     cat("-----------------------------------------------------\n")
     cat("    --Opioid input data information--              \n\n")
     cat("      D = dose for the 1st-dose (mg)               \n\n")
     cat(" Note: for fentanyl, the dose unit is mcg.         \n")
     cat("-----------------------------------------------------\n")
}

note_for_c_to_d_OO <- function(){ 
     cat("--------------------------------------------\n")
     cat("  --input data--                            \n")
     cat("  Cmin_ss = desired trough conc (ng/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = calculated dose (mg)                \n")
     cat("  for fentanyl, the dose is mcg              \n")
     cat("--------------------------------------------\n\n")
 }
 
note_for_d_to_c_OO <- function(){ 
     cat("--------------------------------------------\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg); fentanyl - mcg     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Cmin_ss = calculated trough conc (ng/L) \n")
     cat("--------------------------------------------\n\n")
 } 