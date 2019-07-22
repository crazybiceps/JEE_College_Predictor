# Loading Libraries                        ----
library(dplyr)
library(tidyr)
library(keras)
library(rebus)
library(stringr)

w <- read.csv("/home/kawal/D/jee2018.csv" , header = F)

w <- w[ , -1]

colnames(w) <- c("Institute" , "AcademicProgram" , 
                 "Quota" , "Category" ,	"Gender" , 
                 "OpeningRank" , "ClosingRank")

# Idenfying iits , nits and iiits          ----

w <- w %>% 
        mutate(Institute_Type = ifelse(str_detect(Institute , 
                                          pattern = "Indian Institute of Technology") == T , 
                               "IIT" , 
                               ifelse(str_detect(Institute , 
                                          pattern = "National Institute of Technology") == T ,
                                           "NIT" , 
                                            ifelse(str_detect(Institute , 
                                                              pattern = "Indian Institute of Information Technology") == T ,
                                                    "IIIT" ,
                                                    "Others"))))

w$Institute_Type <- as.factor(w$Institute_Type)

# w %>% 
#   filter(Quota == "AI") %>% 
#   filter(Category == "SC") %>% 
#   filter(Institute_Type == "IIT") %>% 
#   View

# Identifying old and new institutes       ----
iit <- c("Indian Institute of Technology ")

old_inst <- c("Bombay" , "Delhi" , "Kharagpur" , "Kanpur" , "Madras" , "Roorkee" , 
              "Guwahati" , "Hyderabad" , "(BHU) Varanasi")


w$age <- "new"
w[sapply(w$Institute , 
         function(x) 
           str_split(x , pattern = iit)[[1]][2] %in% old_inst ) , ]$age <- "old"

nit <- c("National Institute of Technology")

old_nit <- c("Tiruchirappalli" , "Surathkal" , "Warangal" , "Rourkela" , "Nagpur" ,
             "Calicut" , "Kurukshetra" , "Allahabad" , "Durgapur" , "Jaipur" , "Bhopal")


w[sapply(as.character(w$Institute), function(x) 
                    strsplit(strsplit(x , split = nit)[[1]][2] , split = " " )[[1]][2]
       ) %in% old_nit , ] $age <- "old"




w <- w %>% 
        mutate(City = sapply(as.character(Institute) , 
                                   function(x){ 
                                              rev(strsplit(x , split = " ")[[1]])[1] })) 


# The code                                 ----
best_institutes <- function(w , r = 1000 , 
                            category = "General" , institute_type = "IIT" , 
                            quota = "AI"){
  
  start <- ifelse(category == "General" , r - 1000 , r - 1000 )
  end   <- ifelse(category == "General" , r + 1000 , r + 500)
 
  quota <- if(quota == "OS")
    quota = c("OS" , "AI")
  
  if (institute_type == "IIT") {
  w %>% 
    filter(Quota %in% quota | City == "Nagpur" ) %>% 
    filter(Category == category) %>% 
    filter(Institute_Type == institute_type) %>% 
    filter(ClosingRank > start ) %>% 
    arrange(ClosingRank) %>% 
    arrange(desc(age)) %>% 
    # arrange(OpeningRank) %>% 
    View
  }
  else{
    w %>% 
      filter(Quota %in% quota | City == "Nagpur") %>% 
      filter(Category == category) %>% 
      filter(Institute_Type != "IIT") %>% 
      filter(ClosingRank > start ) %>% 
      arrange(ClosingRank) %>% 
      arrange(desc(age)) %>% 
      # arrange(OpeningRank) %>% 
      View
  }  
}

