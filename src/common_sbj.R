
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



k <- 12
i <- 44
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
 
    
    ################# values for parents
    parent.v <- z$head %>%
      as.integer()
    
    parent.v[which(parent.v == 0)] <- NA # handle nodes with parent = 0
    
    z[, "parent_relation"] <- z$self_relation [parent.v]
    z[, "parent_POS"] <- z$self_POS [parent.v]
    
    
    g2_ancestor.v <- z$head [parent.v] %>%
        as.integer()

      g2_ancestor.v[which(g2_ancestor.v == 0)] <- NA
      z[, "g2_ancestor_relation"] <- z$self_relation [g2_ancestor.v]
      z[, "g2_ancestor_POS"] <- z$self_POS [g2_ancestor.v]
   
    ##########################
    ####################### identify  valency
    
    
    ### mark verbs with immediate sbj descendents
   
    
    ##################### mark verbs with shared sbjs 
    
    a <- which(str_detect(z$self_relation, "sbj$") == TRUE &  z$parent_relation == "coord" ) # shared sbj
    
    
    
    b <- z$head[a] %>% # bridge coordinator
      as.integer()
    
    d <- unique(b)
    
    if (length(a) > 0 &  length(a) == length(d)) {
      
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
    
    
    ########## find sbj that are coordinated AND shared
   
   
    
    a <- which(z$self_relation == "sbj_co" & z$parent_relation == "coord" & z$g2_ancestor_relation == "coord")
    
    b <- z$head[a] %>% # index bridge coordinator 1
      as.integer()
    
    c <- z$head[b] %>% # index of coord 2
      as.integer()
    
    c <- unique(c)
    
    b_1 <- NULL # nodes dependent on first element in c (dependents of coord 2)
    b_2 <- NULL # nodes dependent on second element in c (dependents of coord 2)
    
    
    for (n in seq_along(c)) { # assign values to b_x
      str <- paste0("b", "_", n)
      values <- which(z$head == c[n]  )
      assign(str,values)
      
    }
   
    
    c_1 <- b_1[which(z$self_POS[b_1] == "verb" & str_detect(z$self_relation[b_1], "_co"))]
    c_2 <- b_2[which(z$self_POS[b_2] == "verb" & str_detect(z$self_relation[b_2], "_co"))]
   
    
   
    
    
    if (length(c_1) > 1 | length(c_2) >1 ) {
      
      sharing.v <- b_1[which(z$self_POS[b_1] == "verb" & str_detect(z$self_relation[b_1], "_co") & # indices of sharing verbs
                               not(str_detect(z$self_relation[b_1], "sbj_co") ) ) ]
      
      problem.v <- b_1[which(z$self_POS[b_1] == "verb" & str_detect(z$self_relation[b_1], "_co") & # likely mismatches
                               str_detect(z$self_relation[b_1], "sbj_co" ) ) ]
      
      if (length(sharing.v) > 0) {
        
        z[sharing.v, "has_shared_sbj_cos"] <- TRUE
        z[problem.v, "check_shared_sbj_cos"] <- TRUE
        
        bridge_1.v <- which(z$head == (z$head[sharing.v] %>% # index for bridge 1 (closest to sbj_cos)
                                         unique() ) & z$self_relation == "coord" )
        
        bridge_1.v <- bridge_1.v[which(bridge_1.v %in%  z$head[which(z$head %in% bridge_1.v & z$self_relation == "sbj_co")] ) ] # drop extraneous coordinators (not connected to sbj_cos of interest)
        
        
        bridge_1.v <- bridge_1.v[!is.na(bridge_1.v)]
        
        
        z[bridge_1.v, "is_bridge_1_from_shared_AND_coord_sbjs"] <- TRUE
        
        z[z$head[sharing.v], "is_bridge_2_from_shared_AND_coord_sbjs"] <- TRUE
        
        z[z$head == bridge_1.v & z$self_relation == "sbj_co", "is_shared_sbj_co"] <- TRUE
        
        shared.v <- which(z$head == bridge_1.v & z$self_relation == "sbj_co")
        
        for (n in seq_along(shared.v)) {
          col_nomen <- paste0("index_of_shared_sbj_co_", n)
          z[sharing.v, col_nomen] <- shared.v[n]
          
        }
        
        
        
      }
      
      
      
      ##
      
      sharing.v <- b_2[which(z$self_POS[b_2] == "verb" & str_detect(z$self_relation[b_2], "_co") & # indices of sharing verbs
                               not(str_detect(z$self_relation[b_2], "sbj_co") ) ) ]
      
      problem.v <- b_2[which(z$self_POS[b_2] == "verb" & str_detect(z$self_relation[b_2], "_co") & # likely mismatches
                               str_detect(z$self_relation[b_2], "sbj_co" ) ) ]
      
      if (length(sharing.v) > 0) {
        
        z[sharing.v, "has_shared_sbj_cos"] <- TRUE
        z[problem.v, "check_shared_sbj_cos"] <- TRUE
        
        bridge_1.v <- which(z$head == (z$head[sharing.v] %>% # index for bridge 1 (closest to sbj_cos)
                                         unique() ) & z$self_relation == "coord" )
        
        
        bridge_1.v <- bridge_1.v[which(bridge_1.v %in%  z$head[which(z$head %in% bridge_1.v & z$self_relation == "sbj_co")] ) ] # drop extraneous coordinators (not connected to sbj_cos of interest)
        
        bridge_1.v <- bridge_1.v[!is.na(bridge_1.v)] # deal with cases where bridge_1.v has only one valid value
        
        z[bridge_1.v, "is_bridge_1_from_shared_AND_coord_sbjs"] <- TRUE      
        
        
        z[z$head[sharing.v], "is_bridge_2_from_shared_AND_coord_sbjs"] <- TRUE
        
        z[z$head == bridge_1.v & z$self_relation == "sbj_co", "is_shared_sbj_co"] <- TRUE
        
        shared.v <- which(z$head == bridge_1.v & z$self_relation == "sbj_co")
        
        for (n in seq_along(shared.v)) {
          col_nomen <- paste0("index_of_shared_sbj_co_", n)
          z[sharing.v, col_nomen] <- shared.v[n]
          
        }
        
      }
      
     
      
      
    }
    
    
   
    
     
    
    
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
