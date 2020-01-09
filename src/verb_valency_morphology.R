rm(list = ls())



require(XML)
require(tidyverse)
require(stringr)
require(magrittr)
require(purrr)
require(readr)

base::options(warn = 2)


############# a script to find verb arguments
input.dir <- "."
files.v <- dir( pattern=".*xml")

y <- readRDS(file = "col_names_for_base_df.RDS") # directory is 
#C:\Users\rgorm\Documents\SyntaxMetrics\Re-analysis_2019-June\valency lexicon\data


y <- c("id", "form", "lemma", "postag", "relation", "head", "artificial", "sentence_id", "subdoc",  "cte_ref",
       "file_name") # relevant attribute names from input xml



k <- 1
i <- 154
files.v[k]

sentence.counter.v <- 1
holder.l <-vector(mode = "list", length = length(files.v))

for (k in seq_along(files.v) ) {
  
  #read xml files from selected directory
  
  doc.object <-
    xmlTreeParse(file.path( files.v[k]), useInternalNodes = TRUE)
  
  # extract all <word> elements and children into XmlNodeList object
  sent.nodes <- getNodeSet(doc.object, "//sentence")
  
  
  sent.list <- xmlApply(sent.nodes, xmlToList)
  
  # loop to transfer list content to dataframe object and extract values of interest
  
  
  
  for (i in seq_along(sent.list)) {
    
    x <-     (sent.list[[i]] %>% # drop ".attrs" row; this row contains sentence metadata
                length() - 1) 
    
    
    j <- 1
    
    for (j in seq_along(y)) {
      if (j == 1) {
        z <- sent.list[[i]][1:x] %>% map_chr(y[j], .null = NA) %>%
          data.frame(check.names = FALSE, stringsAsFactors = FALSE)
        
      } else {
        z <- sent.list[[i]][1:x] %>% map_chr(y[j], .null = NA) %>%
          cbind(z,  ., stringsAsFactors = FALSE)
        
      }
      
      
    }
    
    colnames(z) <- y
    
    a <- sent.list[[i]]$.attrs
    z[, "sentence_id"] <- a["id"]
    z[, "subdoc"] <- a["subdoc"]
    z[, "cte_ref"] <- a["document_id"]
    z[, "file_name"] <- files.v[k]
    
    z[, "self_relation"] <- z[, "relation"] %>%
      tolower()
    
    
    
    pos.v <- substr(z$postag, 1, 1)
    
    pos.v[which(pos.v == "n")] <- "noun"
    pos.v[which(pos.v == "a")] <- "adjective"
    pos.v[which(pos.v == "r")] <- "preposition"
    pos.v[which(pos.v == "v")] <- "verb"
    pos.v[which(pos.v == "d")] <- "adverb"
    pos.v[which(pos.v == "u")] <- "punctuation"
    pos.v[which(pos.v == "-")] <- NA
    pos.v[which(pos.v == "c")] <- "conjunction"
    pos.v[which(pos.v == "p")] <- "pronoun"
    pos.v[which(pos.v == "l")] <- "article"
    pos.v[which(pos.v == "i")] <- "interjection"
    pos.v[which(pos.v == "_")] <- NA
    
    z[, "self_POS"] <- pos.v
    
    # person.v <- substr(z$postag, 2, 2)
    # 
    # ############## adjust person to fix pronouns
    # 
    # 
    # 
    # person.v[which(person.v == "1") ] <- "firstperson"
    # person.v[which(person.v == "2") ] <- "secondperson"
    # person.v[which(person.v == "3") ] <- "thirdperson"
    # person.v[which(person.v == "-")] <- NA
    # person.v[which(person.v == "_")] <- NA
    # 
    # 
    # z[, "self_person"] <- person.v
    # 
    # 
    # number.v <- substr(z$postag, 3, 3) 
    # 
    # 
    # number.v[which(number.v == "s")] <- "singular"
    # number.v[which(number.v == "p")] <- "plural"
    # number.v[which(number.v == "d")] <- "dual"
    # number.v[which(number.v == "-")] <- NA
    # number.v[which(number.v == "_")] <- NA
    # 
    # z[, "self_number"] <- number.v
    # 
    # 
    # 
    # 
    # tense.v <- substr(z$postag, 4, 4)
    # 
    # tense.v[which(tense.v == "p")] <- "present"
    # tense.v[which(tense.v == "i")] <- "imperfect"
    # tense.v[which(tense.v == "a")] <- "aorist"
    # tense.v[which(tense.v == "r")] <- "perfect"
    # tense.v[which(tense.v == "l")] <- "pluperfect"
    # tense.v[which(tense.v == "f")] <- "future"
    # tense.v[which(tense.v == "t")] <- "futurePerfect"
    # tense.v[which(tense.v == "-")] <- NA
    # tense.v[which(tense.v == "_")] <- NA
    # 
    # z[, "self_tense"] <- tense.v
    # 
    # 
    # mood.v <- substr(z$postag, 5, 5)
    # 
    # mood.v[which(mood.v == "i")] <- "indicative"
    # mood.v[which(mood.v == "s")] <- "subjunctive"
    # mood.v[which(mood.v == "o")] <- "optative"
    # mood.v[which(mood.v == "n")] <- "infinitive"
    # mood.v[which(mood.v == "p")] <- "participle"
    # mood.v[which(mood.v == "m")] <- "imperative"
    # mood.v[which(mood.v == "-")] <- NA
    # mood.v[which(mood.v == "_")] <- NA
    # 
    # z[, "self_mood"] <- mood.v
    # 
    # 
    # voice.v <- substr(z$postag, 6, 6)
    # 
    # voice.v[which(voice.v == "a")] <- "active"
    # voice.v[which(voice.v == "p")] <- "passive"
    # voice.v[which(voice.v == "m")] <- "middle"
    # voice.v[which(voice.v == "e")] <- "deponent"
    # voice.v[which(voice.v == "-")] <- NA
    # voice.v[which(voice.v == "_")] <- NA
    # 
    # z[, "self_voice"] <- voice.v
    # 
    # 
    # gender.v <- substr(z$postag, 7, 7)
    # 
    # gender.v[which(gender.v == "m")] <- "masculine"
    # gender.v[which(gender.v == "f")] <- "feminine"
    # gender.v[which(gender.v == "n")] <- "neuter"
    # gender.v[which(gender.v == "-")] <- NA
    # gender.v[which(gender.v == "_")] <- NA
    # 
    # z[, "self_gender"] <- gender.v
    # 
    # 
    # 
    # case.v <- substr(z$postag, 8, 8)
    # 
    # case.v[which(case.v == "n")] <- "nominative"
    # case.v[which(case.v == "g")] <- "genitive"
    # case.v[which(case.v == "d")] <- "dative"
    # case.v[which(case.v == "a")] <- "accusative"
    # case.v[which(case.v == "b")] <- "ablative"
    # case.v[which(case.v == "v")] <- "vocative"
    # case.v[which(case.v == "l")] <- "locative"
    # case.v[which(case.v == "-")] <- NA
    # case.v[which(case.v == "_")] <- NA
    # 
    # z[, "self_case"] <- case.v
    # 
    # 
    # 
    # 
    # 
    # degree.v <- substr(z$postag, 9, 9)
    # 
    # degree.v[which(degree.v == "p")] <- "positive"
    # degree.v[which(degree.v == "c")] <- "comparative"
    # degree.v[which(degree.v == "s")] <- "superlative"
    # degree.v[which(degree.v == "-")] <- NA
    # degree.v[which(degree.v == "_")] <- NA
    # 
    # z[, "self_degree"] <- degree.v
    # 
    # 
    # 
    
    
    
    ################# values for parents
    parent.v <- z$head %>%
      as.integer()
    
    parent.v[which(parent.v == 0)] <- NA # handle nodes with parent = 0
    
    z[, "parent_relation"] <- z$self_relation [parent.v]
    z[, "parent_POS"] <- z$self_POS [parent.v]
    # z[, "parent_person"] <- z$self_person [parent.v]
    # z[, "parent_number"] <- z$self_number [parent.v]
    # z[, "parent_tense"] <- z$self_tense [parent.v]
    # z[, "parent_mood"] <- z$self_mood [parent.v]
    # z[, "parent_voice"] <- z$self_voice [parent.v]
    # z[, "parent_gender"] <- z$self_gender [parent.v]
    # z[, "parent_case"] <- z$self_case [parent.v]
    # z[, "parent_degree"] <- z$self_degree [parent.v]
    
    
    
    #### extract id numbers of ancenstors
    # 
    # g2_ancestor.v <- z$head [parent.v] %>%
    #   as.integer()
    # 
    # g2_ancestor.v[which(g2_ancestor.v == 0)] <- NA
    # z[, "g2_ancestor_relation"] <- z$self_relation [g2_ancestor.v]
    # z[, "g2_ancestor_POS"] <- z$self_POS [g2_ancestor.v]
    # 
    # 
    # g3_ancestor.v <- z$head [g2_ancestor.v] %>%
    #   as.integer()
    # 
    # g3_ancestor.v[which(g3_ancestor.v == 0)] <- NA
    # z[, "g3_ancestor_relation"] <- z$self_relation [g3_ancestor.v]
    # z[, "g3_ancestor_POS"] <- z$self_POS [g3_ancestor.v]
    # 
    # 
    # g4_ancestor.v <- z$head [g3_ancestor.v] %>%
    #   as.integer()
    # 
    # g4_ancestor.v[which(g4_ancestor.v == 0)] <- NA
    # z[, "g4_ancestor_relation"] <- z$self_relation [g4_ancestor.v]
    # z[, "g4_ancestor_POS"] <- z$self_POS [g4_ancestor.v]
    # 
    # 
    # 
    # g5_ancestor.v <- z$head [g4_ancestor.v] %>%
    #   as.integer()
    # 
    # g5_ancestor.v[which(g5_ancestor.v == 0)] <- NA
    # z[, "g5_ancestor_relation"] <- z$self_relation [g5_ancestor.v]
    # z[, "g5_ancestor_POS"] <- z$self_POS [g5_ancestor.v]
    # 
    # 
    # g6_ancestor.v <- z$head [g5_ancestor.v] %>%
    #   as.integer()
    # 
    # g6_ancestor.v[which(g6_ancestor.v == 0)] <- NA
    # z[, "g6_ancestor_relation"] <- z$self_relation [g6_ancestor.v]
    # z[, "g6_ancestor_POS"] <- z$self_POS [g6_ancestor.v]
    # 
    # 
    # g7_ancestor.v <- z$head [g6_ancestor.v] %>%
    #   as.integer()
    # 
    # g7_ancestor.v[which(g7_ancestor.v == 0)] <- NA
    # z[, "g7_ancestor_relation"] <- z$self_relation [g7_ancestor.v]
    # z[, "g7_ancestor_POS"] <- z$self_POS [g7_ancestor.v]
    # 
    # 
    # g8_ancestor.v <- z$head [g7_ancestor.v] %>%
    #   as.integer()
    # 
    # g8_ancestor.v[which(g8_ancestor.v == 0)] <- NA
    # z[, "g8_ancestor_relation"] <- z$self_relation [g8_ancestor.v]
    # z[, "g8_ancestor_POS"] <- z$self_POS [g8_ancestor.v]
    # 
    # 
    # g9_ancestor.v <- z$head [g8_ancestor.v] %>%
    #   as.integer()
    # 
    # g9_ancestor.v[which(g9_ancestor.v == 0)] <- NA
    # z[, "g9_ancestor_relation"] <- z$self_relation [g9_ancestor.v]
    # z[, "g9_ancestor_POS"] <- z$self_POS [g9_ancestor.v]
    # 
    # 
    # 
    # g10_ancestor.v <- z$head [g9_ancestor.v] %>%
    #   as.integer()
    # 
    # g10_ancestor.v[which(g10_ancestor.v == 0)] <- NA
    # z[, "g10_ancestor_relation"] <- z$self_relation [g10_ancestor.v]
    # z[, "g10_ancestor_POS"] <- z$self_POS [g10_ancestor.v]
    # 
    # 
    # g11_ancestor.v <- z$head [g10_ancestor.v] %>%
    #   as.integer()
    # 
    # g11_ancestor.v[which(g11_ancestor.v == 0)] <- NA
    # z[, "g11_ancestor_relation"] <- z$self_relation [g11_ancestor.v]
    # z[, "g11_ancestor_POS"] <- z$self_POS [g11_ancestor.v]
    # 
    # 
    
    ##########################
    ####################### identify  valency
    
    
    ### mark verbs with immediate sbj descendents
    sbj.v <- str_detect(z$self_relation, "sbj$")
    z$parent_POS[which(sbj.v == TRUE)]
    
    a <- which(sbj.v == TRUE & z$parent_POS == "verb" ) # drop shared sbjs
    
    z[a, "is_immed_sbj"] <- TRUE
    
    a <-  z$head[which(sbj.v == TRUE & z$parent_POS == "verb")] %>%
      as.integer()
    
    # ad index of sbj to parent
    b <- which(sbj.v == TRUE & z$parent_POS == "verb" )
    z[a, "index_of_immed_sbj"] <- b
    
    z[a, "has_immed_sbj"] <- TRUE
    
    ##################### mark verbs with shared sbjs 
    
    a <- which(str_detect(z$self_relation, "sbj$") == TRUE &  str_detect(z$parent_relation, "coord") ) # shared sbj
    
    
    
    b <- z$head[a] %>% # bridge coordinator
      as.integer()
    
    d <- unique(b)
    
    if (length(a) == length(d)) {
      
      z[a, "is_shared_sbj"] <- TRUE
      
      z[b, "is_bridge_to_shared_sbj"] <- TRUE 
      
      c <- unique(b)
      
      b_1 <- NULL
      b_2 <- NULL
      b_3 <- NULL
      b_4 <- NULL
      b_5 <- NULL
      b_6 <- NULL
      
      
      
      for (n in seq_along(c)) {
        str <- paste0("b", "_", n)
        values <- which(z$head == c[n] & (str_detect(z$self_relation, "_co") &  z$self_POS == "verb"    ) )
        assign(str,values)
        
      }
      
      z[b_1, "index_of_shared_sbj"] <- a[1]
      z[b_2, "index_of_shared_sbj"] <- a[2]
      z[b_3, "index_of_shared_sbj"] <- a[3]
      z[b_4, "index_of_shared_sbj"] <- a[4]
      z[b_5, "index_of_shared_sbj"] <- a[5]
      z[b_6, "index_of_shared_sbj"] <- a[6]
      
      
      
      
      
      
      
      
      
      
     
      
    } else {
      
      z[a, "shared_sbj_error"] <- TRUE
    }
    
    
    
    ############################
    
    
    # a <-  z$head[which(sbj.v == TRUE & z$parent_POS != "verb")] %>%
    #   as.integer()
    # 
    # z[a, "is_bridge_to_sbj"] <- TRUE
    
    ##################### mark verbs with multiple sujects
    
    sbj_co.v <- str_detect(z$self_relation, "sbj_co") 
    
    a <- which(sbj_co.v == TRUE)
    z[a, "is_multiple_sbj"] <- TRUE
    
   
    
    a <-  z$head[which(sbj_co.v == TRUE & z$parent_relation == "coord")] %>%
      as.integer()
    
    z[a, "is_bridge_to_multiple_sbjs"] <- TRUE
    # indices of multiple sbj childred
    b <- which(sbj_co.v == TRUE & z$parent_relation == "coord")
    
    c <- a %>%
      unique() # identify separate coords
    
   
    
    # z[, "index_of_multi_sbj_1"] <- NA
    # z[, "index_of_multi_sbj_2"] <- NA
    # z[, "index_of_multi_sbj_3"] <- NA
    # z[, "index_of_multi_sbj_4"] <- NA
    # z[, "index_of_multi_sbj_5"] <- NA
    # z[, "index_of_multi_sbj_6"] <- NA
    # z[, "index_of_multi_sbj_7"] <- NA
    # z[, "index_of_multi_sbj_8"] <- NA
    
    
    b_1 <- NULL
    b_2 <- NULL
    b_3 <- NULL
    b_4 <- NULL
    b_5 <- NULL
    b_6 <- NULL
    
    for (n in seq_along(c)) {
      str <- paste0("b", "_", n)
      values <- b[which(a == c[n])]
      assign(str,values)
      
    }
    
    
    for (n in seq_along(b_1)) {
      c_nomen <- paste0("index_of_multi_sbj_", n)
      z[c[1], c_nomen] <- b_1[n]
    }
    
    for (n in seq_along(b_2)) {
      c_nomen <- paste0("index_of_multi_sbj_", n)
      z[c[2], c_nomen] <- b_2[n]
    }
    
    for (n in seq_along(b_3)) {
      c_nomen <- paste0("index_of_multi_sbj_", n)
      z[c[3], c_nomen] <- b_3[n]
    }
    
    for (n in seq_along(b_4)) {
      c_nomen <- paste0("index_of_multi_sbj_", n)
      z[c[4], c_nomen] <- b_4[n]
    }
    
    for (n in seq_along(b_5)) {
      c_nomen <- paste0("index_of_multi_sbj_", n)
      z[c[5], c_nomen] <- b_5[n]
    }
    
    for (n in seq_along(b_6)) {
      c_nomen <- paste0("index_of_multi_sbj_", n)
      z[c[6], c_nomen] <- b_6[n]
    }
    
    
    a <- which(z$is_bridge_to_multiple_sbjs == TRUE)
    
    
    for (n in seq_along(a)) {
      z[a[n] , "count_of_dep_sbj_co"] <- which(z$self_relation == "sbj_co" & z$head == a[n]) %>%
        length()
    }
    
    
    a <- which(z$is_bridge_to_multiple_sbjs == TRUE & z$parent_relation == "coord" )
   
    b <- z$head[a]
    
    
    z[b, "is_bridge_2_to_multi_sbjs"] <- TRUE
    
    #index of dep bridge to sbjs
    
    b_1 <- NULL
    b_2 <- NULL
    b_3 <- NULL
    b_4 <- NULL
    
  which(z$is_bridge_to_multiple_sbjs & z$head == b[2])
    
   
    
    for (n in seq_along(c)) {
      str <- paste0("b", "_", n)
      values <- which(z$is_bridge_to_multiple_sbjs & z$head == b[n])
      assign(str,values)
      
    }
  
  for (n in seq_along(b_1)) {
    c_nomen <- paste0("index_of_dep_bridge_1_to_sbjs_", n)
    z[b[1], c_nomen] <- b_1[n]
  }
   
  for (n in seq_along(b_2)) {
    c_nomen <- paste0("index_of_dep_bridge_1_to_sbjs_", n)
    z[b[2], c_nomen] <- b_2[n]
  } 
  
  for (n in seq_along(b_3)) {
    c_nomen <- paste0("index_of_dep_bridge_1_to_sbjs_", n)
    z[b[3], c_nomen] <- b_3[n]
  } 
  
  for (n in seq_along(b_4)) {
    c_nomen <- paste0("index_of_dep_bridge_1_to_sbjs_", n)
    z[b[4], c_nomen] <- b_[n]
  }
  
  
    
    
  
    
    #######
    #######
    #######
   
    
    a <- z$head[which(z$is_bridge_to_multiple_sbjs == TRUE & z$parent_POS == "verb")] %>%
      as.integer()
    
    
   
    ###########################
    
    
    ########################
    
    
    
    ### mark verbs with immediate obj descendents
    
    
    obj.v <- str_detect(z$self_relation, "obj$")
    z$parent_POS[which(obj.v == TRUE)]
    
    a <- which(obj.v == TRUE & z$parent_POS == "verb" ) # drops shared obj
    z[a, "is_immed_obj"] <- TRUE
    
    a <-  z$head[which(obj.v == TRUE & z$parent_POS == "verb")] %>% # find parent of each obj
      as.integer()
    
    z[a, "has_immed_obj"] <- TRUE
    
    # ad index of sbj to parent
    b <- which(obj.v == TRUE & z$parent_POS == "verb" )
    z[a, "index_of_immed_obj"] <- b
    
    ############################# 
    ##################### mark verbs with shared object
    
    a <- which(str_detect(z$self_relation, "obj$") == TRUE &  str_detect(z$parent_relation, "coord") ) # shared object
    b <- z$head[a] %>% # bridge coordinator
      as.integer()
    
    d <- unique(b)
    
    if (length(a) == length(d)) {
      
      z[a, "is_shared_obj"] <- TRUE
      
      b <- z$head[a] %>% # bridge coordinator
        as.integer()
      
      z[b, "is_bridge_to_shared_obj"] <- TRUE 
      
      ############ new code here
      
     
      
      c <- unique(b)
      
      b_1 <- NULL
      b_2 <- NULL
      b_3 <- NULL
      b_4 <- NULL
      b_5 <- NULL
      b_6 <- NULL
      
      
      
      for (n in seq_along(c)) {
        str <- paste0("b", "_", n)
        values <- which(z$head == c[n] & (str_detect(z$self_relation, "_co") &  z$self_POS == "verb"    ) )
        assign(str,values)
        
      }
      
      z[b_1, "index_of_shared_obj"] <- a[1]
      z[b_2, "index_of_shared_obj"] <- a[2]
      z[b_3, "index_of_shared_obj"] <- a[3]
      z[b_4, "index_of_shared_obj"] <- a[4]
      z[b_5, "index_of_shared_obj"] <- a[5]
      z[b_6, "index_of_shared_obj"] <- a[6]
      
       
      
      
      z[c, "has_shared_obj"] <- TRUE
      
     
      
    } else {
      if (length(a) > 0) {
        z[a, "shared_obj_error"] <- TRUE
        
      }
      
    }
    
    
   
    ############################
   ##################### mark verbs with multiple objects
   
   obj_co.v <- str_detect(z$self_relation, "obj_co") 
   
   a <- which(obj_co.v == TRUE)
   z[a, "is_multiple_obj"] <- TRUE
   
   a <-  z$head[which(obj_co.v == TRUE & z$parent_relation == "coord")] %>% # find parent of each obj_co
     as.integer()
   
   z[a, "is_bridge_to_multiple_objs"] <- TRUE
   # indices of multiple obj childred
   b <- which(obj_co.v == TRUE & z$parent_relation == "coord")
   
   c <- a %>%
     unique() # identify separate coords
   
   b_1 <- NULL
   b_2 <- NULL
   b_3 <- NULL
   b_4 <- NULL
   b_5 <- NULL
   b_6 <- NULL
   b_7 <- NULL
   b_8 <- NULL
   b_9 <- NULL
   b_10 <- NULL
   b_11 <- NULL
   b_12 <- NULL
   
   
   for (n in seq_along(c)) {
     str <- paste0("b", "_", n)
     values <- b[which(a == c[n])]
     assign(str,values)
     
   }
   
   
   for (n in seq_along(b_1)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[1], c_nomen] <- b_1[n]
   }
   
   for (n in seq_along(b_2)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[2], c_nomen] <- b_2[n]
   }
   
   for (n in seq_along(b_3)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[3], c_nomen] <- b_3[n]
   }
   
   for (n in seq_along(b_4)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[4], c_nomen] <- b_4[n]
   }
   
   for (n in seq_along(b_5)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[5], c_nomen] <- b_5[n]
   }
   
   for (n in seq_along(b_6)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[6], c_nomen] <- b_6[n]
   }
   
   for (n in seq_along(b_7)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[7], c_nomen] <- b_7[n]
   }
   
   for (n in seq_along(b_8)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[8], c_nomen] <- b_8[n]
   }
   
   for (n in seq_along(b_9)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[9], c_nomen] <- b_9[n]
   }
   
   for (n in seq_along(b_10)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[10], c_nomen] <- b_10[n]
   }
   
   for (n in seq_along(b_11)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[11], c_nomen] <- b_11[n]
   }
   
   for (n in seq_along(b_12)) {
     c_nomen <- paste0("index_of_multi_obj_", n)
     z[c[12], c_nomen] <- b_12[n]
   }
   
   
   
   a <- z$head[which(z$is_bridge_to_multiple_objs == TRUE & z$parent_POS == "verb")] %>%
     as.integer()
   
   z[a, "has_multiple_objs"] <- TRUE
   
   
   
   
   
   
   
   
   
   ####################################
   ####################################
   # 
   # obj_co.v <- str_detect(z$self_relation, "obj_co") 
   # 
   # a <- which(obj_co.v == TRUE)
   # z[a, "is_multiple_obj"] <- TRUE
   # 
   # a <-  z$head[which(obj_co.v == TRUE & z$parent_relation == "coord")] %>%
   #    as.integer()
   #  
   #  z[a, "is_bridge_to_multiple_objs"] <- TRUE
   #  
   #  a <- z$head[which(z$is_bridge_to_multiple_objs == TRUE & z$parent_POS == "verb")] %>%
   #    as.integer()
   #  
   #  z[a, "has_multiple_objs"] <- TRUE
    
    
    
    
    
    ####################
    
    
    
    
    
    
    
    ####### coda to combine sentences into database and then store database in list
    
    if (i == 1) {
      treebank.data.df <- z
      
    } else {
      treebank.data.df <- bind_rows(treebank.data.df, z)
      
    }
    
    sentence.counter.v <- sentence.counter.v + 1
    
    
    
  } # end of loop i
  
  holder.l[[k]] <- treebank.data.df
  
  # f_name.v <- gsub(".xml", ".RDS", files.v[k])
  # saveRDS(treebank.data.df, file = file.path("..", "base_variables_df"  , f_name.v) )
  
}

###########################
########################## Code to populate matrix with selected variables



big.df <- do.call(bind_rows, holder.l)
colnames(big.df)

which(big.df$shared_obj_error >0 ) %>%
  length()
