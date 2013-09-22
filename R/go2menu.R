# List of Drugs Menu
go2menu<-function()
{
  cat("\n")
  options(digits=5)
  file.menu <- c("Aminoglycosides",           ### '***" means it's done with JAGS now.
                 "Anti-HIV drugs",
                 "Carbamazepine",
                 "Digoxin",
                 "Enoxaparin",
                 "Imatinib mesylate",
                 "Immunosuppressants",
                 "Lithium",
                 "Phenytoin",
                 "Theophylline",
                 "Valproate",
                 "Vancomycin")
  pick <- menu(file.menu, title = "<< Drug List (TDM), Press Enter to quit.>>")
  if (pick == 1){
     cat("\n\n")
     Ami.model()
  } else {
    if (pick == 2){
      cat("\n\n") 
      AntiHIV.menu()
    } else {
    if (pick == 3){
      cat("\n\n") 
      Car.model()
    }  else {
    if (pick == 4){
      cat("\n\n") 
      all.Digss()      ### since tdm v3.0.0; only single subject with single Cp will be provided. -YJ
    } else {
    if (pick == 5){
      cat("\n\n") 
      all.Enoss()      
    } else {
    if (pick == 6){
      cat("\n\n") 
      Ima.model()     
    } else {
    if (pick == 7){
      cat("\n\n") 
      Immunosuppressants.menu()      
    } else {
    if (pick == 8){
      cat("\n\n") 
      Lit.menu()
    } else {
    if (pick == 9){
      cat("\n\n") 
      ### Phe.model()   ### since tdm v3.0.0; only single subject with single Cp will be provided. -YJ
      all.Phess() 
    } else {
    if (pick == 10){
      cat("\n\n") 
      Theall.menu()     ### standalone R script! ONLY for this, else are all in this R script.
    } else {
    if (pick == 11){
      cat("\n\n") 
      Val.menu()
    } else {
    if (pick == 12){
      cat("\n\n") 
      Van.model()
    } else {
      if (pick == 0){
        if(file.exists("params.csv")) file.remove("params.csv")
        graphics.off()
        cat("\n  Thank you for using tdm. Bye now. \n\n")     
       }
     }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
  }
}

#Aminoglycoside data file 
Ami.model <- function()
{
  file.menu <- c("single subj with single conc",                                     #list of Aminoglycoside data type   
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for Aminoglycoside >>")          
  if (pick == 1){                                                                    # chosse single subject with single concentration
     all.Amiss()
  } 
   else if (pick == 2){                                                              # chosse single subject with each multiple concentrations    
     all.Amism()
  } 
     else if (pick == 3){                                                            # chosse multiple subjects with each single concentration    
     go2menu()
  } 
}

#Carbamazepine data file 
Car.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for carbamazepine >>")
  if (pick == 1){
     all.Carss()
  }
    else if (pick == 2){
     all.Carsm()
  }     
    else if (pick == 3){
    go2menu()
  }     
}


#Adult Digoxin data file 
### Dig.model <- function()
### {
###   file.menu <- c("single subj with single conc",
###                  "multiple subj with each single conc",
###                  "Go back one upper level")
###   pick <- menu(file.menu, title = "<< data type for digoxin >>")
###   if (pick == 1){
###      all.Digss()
###   } 
###     else if (pick == 2){
###      all.Digms()
###   } 
###   else if (pick == 3){
###      cat("\n\n") 
###      go2menu()
###   }      
### }

#choose Litium salts menu 
Lit.menu <- function()
{
  file.menu <- c("Lithium carbonate",
                 "Lithium citrate",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Lithium Menu >>")
  if (pick == 1){
     cat("\n\n")      
     ### Lit.model()
     all.Litss()
  } 
  else if (pick == 2){
     cat("\n\n") 
     ### Litcit.model()
     all.Litcitss()
  }         
  else if (pick == 3){
     cat("\n\n") 
     go2menu()
  }      
}


#Lithium carbonate data file 
### Lit.model <- function()
### {
###   file.menu <- c("single subj with single conc",
###                  "multiple subj with each single conc",
###                  "Go back one upper level")
###   pick <- menu(file.menu, title = "<< data type for lithium carbonate >>")
###   if (pick == 1){
###      all.Litss()
###   } 
###   else if (pick == 2){
###      all.Litms()
###   } 
###   else if (pick == 3){
###      cat("\n\n") 
###      Lit.menu()
###   }      
### }
### 
### #Lithium citrate data file 
### Litcit.model <- function()
### {
###   file.menu <- c("single subj with single conc",
###                  "multiple subj with each single conc",
###                  "Go back one upper level")
###   pick <- menu(file.menu, title = "<< data type for lithium citrate >>")
###   if (pick == 1){
###      all.Litcitss()
###   } 
###     else if (pick == 2){
###      all.Litcitms()
###   } 
###   else if (pick == 3){
###      cat("\n\n") 
###      Lit.menu()
###   }      
### }


# Phenytoin data file 
### Phe.model <- function()
### {
###   file.menu <- c("single subj with single conc",
###                  "multiple subj with each single conc",
###                  "Go back one upper level")
###   pick <- menu(file.menu, title = "<< data type for phenytoin >>")
###   if (pick == 1){
###      all.Phess()
###   } 
###     else if (pick == 2){
###      all.Phems()
###   } 
###   else if (pick == 3){
###      cat("\n\n") 
###      go2menu()
###   }      
### }

#choose Valproic acid population group
Val.menu <- function()
{
  file.menu <- c("Adult",
                 "Children",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Population Menu for valproic acid >>")
  if (pick == 1){
     cat("\n\n")      
     Val.model()
  } 
  else if (pick == 2){
     cat("\n\n") 
     ChiVal.model()
  }         
  else if (pick == 3){
     cat("\n\n") 
     go2menu()
  }      
}


#choose Valproate data file, estimate PK parameter and calculate dose adjustment
ChiVal.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for children valproate >>")
  if (pick == 1){
     all.ChiValss()
  } 
  else if (pick == 2){
     all.ChiValsm()
  }   
   else if (pick == 3){
     Val.menu()
  } 
}


#choose Valproate data file, estimate PK parameter and calculate dose adjustment
Val.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for valproate >>")
  if (pick == 1){
     all.Valss()
  } 
  else if (pick == 2){
     all.Valsm()
  }   
   else if (pick == 3){
     Val.menu()
  } 
}

#Vancomycin data file 
Van.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for vancomycin >>")
  if (pick == 1){
     all.Vanss()
  } 
   else if (pick == 2){
     all.Vansm()
  } 
     else if (pick == 3){
     go2menu()
  }      
}

#choose AntiHIV drugs menu
AntiHIV.menu <- function()
{
  file.menu <- c("Enfuvirtide",
                 "Indinavir",
                 "Ritonavir",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Anti-HIV drugs Menu >>")
  if (pick == 1){
     cat("\n\n")      
     Enf.model()
  } 
  else if (pick == 2){
     cat("\n\n") 
     Ind.model()
  }      
  else if (pick == 3){
     cat("\n\n") 
     Rit.model()
  }      
  else if (pick == 4){
     cat("\n\n") 
     go2menu()
  }      
}

#Enfuvirtide data file choose
Enf.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for enfuvirtide >>")
  if (pick == 1){
     all.Enfss()
  } 
    else if (pick == 2){
     all.Enfsm()
  } 
     else if (pick == 3){
     AntiHIV.menu()
  } 
}

#Indinavir data file
Ind.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for indinavir >>")
  if (pick == 1){
     all.Indss()
  } 
    else if (pick == 2){
     all.Indsm()
  } 
     else if (pick == 3){
      AntiHIV.menu()
  }
}

#Ritonavir data file
Rit.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for ritonavir >>")
  if (pick == 1){
     all.Ritss()
  } 
    else if (pick == 2){
     all.Ritsm()
  } 
    else if (pick == 3){
     AntiHIV.menu()
  }
}

#choose immunosuppressants menu 
Immunosuppressants.menu <- function()
{
  file.menu <- c("Cyclosporine-A",
                 "Everolimus",
                 "Tacrolimus",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Immunosuppressants Menu >>")
  if (pick == 1){
     cat("\n\n")      
     Cyc.model()
  } 
  else if (pick == 2){
     cat("\n\n") 
     Eve.model()
  }      
  else if (pick == 3){
     cat("\n\n") 
     Tac.model()
  }      
  else if (pick == 4){
     cat("\n\n") 
     go2menu()
  }    
}

#choose Cyclosporine-A data file, estimate PK parameters and calculate dose adjustment 
Cyc.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for cyclosporine A >>")
  if (pick == 1){
     all.Cycss()
  } 
   else if (pick == 2){
     all.Cycsm()
  } 
    else if (pick == 3){
    Immunosuppressants.menu()
  }
}

#Everolimus data file 
Eve.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for everolimus >>")
  if (pick == 1){
     all.Evess()
  } 
    else if (pick == 2){
     all.Evesm()
  } 
    else if (pick == 3){
     Immunosuppressants.menu()
  }
}

#Tacrolimus data file 
Tac.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for tacrolimus >>")
  if (pick == 1){
     all.Tacss()
  } 
    else if (pick == 2){
     all.Tacsm()
  } 
    else if (pick == 3){
     Immunosuppressants.menu()
  }
}

#Enoxaparin data file
### Eno.model <- function()
### {
###   file.menu <- c("single subj with single conc",
###                  "multiple subj with each single conc",
###                  "Go back one upper level")
###   pick <- menu(file.menu, title = "<< data type for enoxaparin >>")
###   if (pick == 1){
###      all.Enoss()
###   } 
###    else if (pick == 2){
###      all.Enoms()
###   }
###   else if (pick == 3){
###      cat("\n\n") 
###      go2menu()
###   }      
### }


# choose Imatinib mesylate data file, estimate PK parameter and calculate dose adjustment
Ima.model <- function()
{
  file.menu <- c("single subj with single conc",
                 "single subj with mutiple conc (more than one conc.)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for imatinib mesylate >>")
  if (pick == 1){
     all.Imass()
  } 
  else if (pick == 2){
     all.Imasm()
  }   
   else if (pick == 3){
     go2menu()
  } 
}


# warfarin data file 
### War.model <- function()
### {
###   file.menu <- c("single subj with single conc",
###                  "Go back one upper level")
###   pick <- menu(file.menu, title = "<< data type for warfarin >>")
###   if (pick == 1){
###      all.Warss()
###   } 
###     else if (pick == 2){
###      cat("\n\n") 
###      go2menu()
###   }      
### }


