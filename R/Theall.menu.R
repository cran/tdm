#all Theophylline menu 
Theall.menu<-function()
{
 cat("\n")
  file.menu <- c("Aminophylline anhydrous",
                 "Aminophylline dihydrous",
                 "Oxtriphylline",
                 "Theophylline",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Theophylline Menu >>")
    if (pick == 1){
     cat("\n\n")
     Amianh.menu()
  } else {
    if (pick == 2){
      cat("\n\n") 
     Amidih.menu()
    } else {
    if (pick == 3){
      cat("\n\n") 
     Oxt.menu()
    }  else {
    if (pick == 4){
      cat("\n\n") 
     The.menu()
    } else {
    if (pick == 5){
      cat("\n\n") 
     run()
    }
    }
    }
    }
    }
    }
    
# Aminophylline anhydrous    
Amianh.menu <- function()                        #list of Aminophylline anhydrous input form  
{
  file.menu <- c("immdediate release",
                 "control release",
                 "iv infusion",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Aminophylline anhydrous input form menu >>")
  if (pick == 1){
     cat("\n\n")      
     Amianhir.model()
  } 
  else if (pick == 2){
     cat("\n\n") 
     Amianhcr.model()
  }         
  else if (pick == 3){
     cat("\n\n") 
     Amianhinfusion.model()
  }   
  else if (pick == 4){
     cat("\n\n") 
     Theall.menu()
  }      
}

# Aminophylline anhydrous immdediate release model          
Amianhir.model <- function()                                       #list of Aminophylline anhydrous IR data type               
{  
  file.menu <- c("single subj with single conc",               
                 "single subj with mutiple conc (sampling times must more than twice)",
                 "multiple subj with each single conc",
                 "multiple subj and mutiple conc (each subj's sampling times must more than twice)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for aminophylline anhydrous IR tablet >>") 
  if (pick == 1){                                                                    # chosse single subject with single concentration                                                                   
    all.Amianhirss() 
  } 
   else if (pick == 2){                                                                   # choose single subject with each multiple concentrations         
     all.Amianhirsm()                                                                                                                                                                                       
  } 
     else if (pick == 3){                                                                   # chosse multiple subjects with each single concentration
     all.Amianhirms()
  } 
     else if (pick == 4){                                                                   # chosse multiple subjects with each multiple concentrations     
     all.Amianhirmm()
  } 
  else if (pick == 5){   # go back to upper layer
     cat("\n\n") 
     Amianh.menu()
  }      
}


# 2 
# Aminophylline anhydrous control release model
Amianhcr.model <- function()
{
  file.menu <- c("single subj with single conc",                 
                 "multiple subj with each single conc",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for aminophylline anhydrous CR tablet  >>") 
  if (pick == 1){                                                                    
     all.Amianhcrss()                                                                                                                                  
  } 
    else if (pick == 2){
     all.Amianhcrms()                                                                                                                                       
  } 
  else if (pick == 3){
     cat("\n\n") 
     Amianh.menu()
  }      
}


Amianhinfusion.model <- function()
{
  file.menu <- c("single subj with single conc",               
                 "single subj with mutiple conc (sampling times must more than twice)",
                 "multiple subj with each single conc",
                 "multiple subj and mutiple conc (each subj's sampling times must more than twice)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for aminophylline anhydrous iv infusion >>") 
  if (pick == 1){                                                                    
     all.Amianhinfusionss()                                                                                                                                                                                                                                    
   } 
   else if (pick == 2){     
     all.Amianhinfusionsm()                                                                                                                                                          
  } 
     else if (pick == 3){
     all.Amianhinfusionms()                                                                                                                                                 
  } 
     else if (pick == 4){     
     all.Amianhinfusionmm()
  } 
  else if (pick == 5){
     cat("\n\n") 
     Amianh.menu()
  }      
}


# Aminophylline dihydrous
Amidih.menu <- function()
{
  file.menu <- c("immdediate release",
                 "control release",
                 "iv infusion",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Aminophylline dihydrous input form menu >>")
  if (pick == 1){
     cat("\n\n")      
     Amidihir.model()
  } 
  else if (pick == 2){
     cat("\n\n") 
     Amidihcr.model()
  }         
  else if (pick == 3){
     cat("\n\n") 
     Amidihinfusion.model()
  }   
  else if (pick == 4){
     cat("\n\n") 
     Theall.menu()
  }      
}


# Aminophylline dihydrous immdediate release model
Amidihir.model <- function()
{
  file.menu <- c("single subj with single conc",               
                 "single subj with mutiple conc (sampling times must more than twice)",
                 "multiple subj with each single conc",
                 "multiple subj and mutiple conc (each subj's sampling times must more than twice)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for aminophylline dihydrous IR tablet >>") 
  if (pick == 1){                                                                    
     all.Amidihirss()                                                                                                                        
  } 
   else if (pick == 2){     
     all.Amidihirsm()                                                                                                                                                                           
  } 
     else if (pick == 3){
     all.Amidihirms()
  } 
     else if (pick == 4){     
     all.Amidihirmm()
  } 
  else if (pick == 5){
     cat("\n\n") 
     Amidih.menu()
  }      
}


# 2 
# Aminophylline dihydrous control release model
Amidihcr.model<-function()
{
   file.menu <- c("single subj with single conc",               
                 "multiple subj with each single conc",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for aminophylline dihydrous CR tablet  >>") 
  if (pick == 1){                                                                    
     all.Amidihcrss()                                                                                                                                               
  } 
     else if (pick == 2){
     all.Amidihcrms()                                                                                                                                                        
  } 
  else if (pick == 3){
     cat("\n\n") 
     Amidih.menu()
  }      
}


#Aminophylline dihydrous iv infusion model
Amidihinfusion.model<-function()
{
  file.menu <- c("single subj with single conc",               
                 "single subj with mutiple conc (sampling times must more than twice)",
                 "multiple subj with each single conc",
                 "multiple subj and mutiple conc (each subj's sampling times must more than twice)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for aminophylline anhydrous iv infusion >>") 
  if (pick == 1){                                                                    
     all.Amidihinfusionss()                                                                                                                                                                                                                         
   } 
   else if (pick == 2){     
     all.Amidihinfusionsm()                                                                                                                                                                         
  } 
     else if (pick == 3){
     all.Amidihinfusionms()                                                                                                                                                   
  } 
     else if (pick == 4){     
     all.Amidihinfusionmm()
  } 
  else if (pick == 5){
     cat("\n\n") 
     Amidih.menu()
  }      
}


# Oxtriphylline
Oxt.menu <- function()
{
  file.menu <- c("immdediate release",
                 "control release",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Oxtriphylline input form menu >>")
  if (pick == 1){
     cat("\n\n")      
     Oxtir.model()
  } 
  else if (pick == 2){
     cat("\n\n") 
     Oxtcr.model()
  }         
  else if (pick == 3){
     cat("\n\n") 
     Theall.menu()
  }      
}

# Oxtriphylline immdediate release model
Oxtir.model <- function()
{
  file.menu <- c("single subj with single conc",               
                 "single subj with mutiple conc (sampling times must more than twice)",
                 "multiple subj with each single conc",
                 "multiple subj and mutiple conc (each subj's sampling times must more than twice)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for Oxtriphylline IR tablet>>") 
  if (pick == 1){                                                                    
     cat("\n")                                                                       
     all.Oxtirss()                                                                                               
  } 
   else if (pick == 2){     
     all.Oxtirsm()                                                                                                                                          
  } 
     else if (pick == 3){
     all.Oxtirms()                                                                                                                                    
  } 
     else if (pick == 4){     
     all.Oxtirmm()
  } 
  else if (pick == 5){
     cat("\n\n") 
     Oxt.menu()
  }      
}



# 2. 
# Oxtriphylline control release model
Oxtcr.model <- function()
{
  file.menu <- c("single subj with single conc",               
                 "multiple subj with each single conc",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for Oxtriphylline CR tablet >>") 
  if (pick == 1){                                                                    
     all.Oxtcrss()                                                                                                                            
  } 
     else if (pick == 2){
     all.Oxtcrms()                                                                                                                                          
  } 
  else if (pick == 3){
     cat("\n\n") 
     Oxt.menu()
  }      
}



# Theophylline
The.menu <- function()
{
  file.menu <- c("immdediate release",
                 "control release",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< Theophylline input form menu >>")
  if (pick == 1){
     cat("\n\n")      
     Their.model()
  } 
  else if (pick == 2){
     cat("\n\n") 
     Thecr.model()
  }           
  else if (pick == 3){
     cat("\n\n") 
     Theall.menu()
  }      
}



# Theophylline immdediate release model
Their.model <- function()
{
  file.menu <- c("single subj with single conc",              
                 "single subj with mutiple conc (sampling times must more than twice)",
                 "multiple subj with each single conc",
                 "multiple subj and mutiple conc (each subj's sampling times must more than twice)",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for theophylline IR tablet >>") 
  if (pick == 1){                                                                    
     all.Theirss()                                                                                                                            
  } 
   else if (pick == 2){     
     all.Theirsm()                                                                                                                                       
  } 
     else if (pick == 3){
     all.Theirms()                                                                                                                          
  } 
     else if (pick == 4){     
     all.Theirmm()
  } 
  else if (pick == 5){
     cat("\n\n") 
     The.menu()
  }      
}



# 2. 
# Theophylline control release model
Thecr.model <- function()
{
  file.menu <- c("single subj with single conc",                 
                 "multiple subj with each single conc",
                 "Go back one upper level")
  pick <- menu(file.menu, title = "<< data type for theophylline CR tablet >>") 
  if (pick == 1){                                                                    
     all.Thecrss()                                                                                                                   
  } 
    else if (pick == 2){
     all.Thecrms()                                                                                                                          
  } 
  else if (pick == 3){
     cat("\n\n") 
     The.menu()
  }      
}



