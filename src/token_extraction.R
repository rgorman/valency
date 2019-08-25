### a script to extract treebank data

rm(list = ls())


require(XML)
require(tidyverse)
require(stringr)
require(magrittr)
require(purrr)


input.dir <- "."
files.v <- dir(path=input.dir, pattern=".*xml")



i <- 4
files.v[i]

# read xml structure from file to .R object
doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]), useInternalNodes=TRUE)

# extract all <word> elements and children into XmlNodeList object
sent.nodes <- getNodeSet(doc.object, "//sentence")


sent.list <- xmlApply(sent.nodes, xmlToList)

y <- sent.list[[1]]$word %>%
  names()

holder.l <- vector(mode = "list", length = length(files.v))
seq_along(files.v)


for (k in 4) {
  # read xml structure from file to .R object
  doc.object <-
    xmlTreeParse(file.path(input.dir, files.v[k]), useInternalNodes = TRUE)
  
  # extract all <word> elements and children into XmlNodeList object
  sent.nodes <- getNodeSet(doc.object, "//sentence")
  
  
  sent.list <- xmlApply(sent.nodes, xmlToList)
  
  
  for (i in seq_along(sent.list)) {
    
    x <-     (sent.list[[i]] %>% # drop ".attrs" row; this row contains sentence metadata
                length() - 1) 
    
    
    
    j <- 1
    
    for (j in seq_along(y)) {
      if (j == 1) {
        z <- sent.list[[i]][1:x] %>% map_chr(y[j]) %>%
          data.frame(check.names = FALSE, stringsAsFactors = FALSE)
        
      } else {
        z <- sent.list[[i]][1:x] %>% map_chr(y[j]) %>%
          cbind(z,  ., stringsAsFactors = FALSE)
        
      }
      
      
    }
    
    colnames(z) <- y
    
    z <- mutate(z, locator = paste0("sent_", i, "_token_", z$id))
    
    # z <-      as_tibble(z)
    z$id <- as.integer(z$id)
    z$head <- as.integer(z$head)
    
    z <- mutate(z, parent_id = z$head)
    z$parent_id[which(z$parent_id == 0)] <- NA
    
    
    z <- mutate(z, sibling_id = NA)
    z <- mutate(z, sibling_count = NA)
    
    for (j in seq_len(nrow(z))) {
      d <-   which(z$parent_id == z$parent_id[j] ) %>%
        setdiff( z$id[j]) 
      z$sibling_count[j] <- length(d)
      
      z$sibling_id[j] <- paste0(d, collapse = " ")
      
    }
    z$sibling_id[z$sibling_id == ""] <- NA
    
    
    z <- mutate(z, direct_deps_id = NA)
    z <- mutate(z, direct_deps_count = NA)
    
    
    for (j in 1:nrow(z)) {
      
      z$direct_deps_id[j] <- which(z$head == j) %>%
        paste0(collapse = " ")
      
      z$direct_deps_count[j] <- which(z$head == j) %>%
        length()
      
      
      
      
    }
    
    z$direct_deps_id[z$direct_deps_id==""] <- NA
    
    
    ########## code to identify ancestor tokens
    
    parent.v <- z$head %>%
      as.integer()
    
    parent.v[which(parent.v == 0)] <- NA
    
    parent.lemma.v <- vector(mode = "character", length = x)
    parent.form.v <- vector(mode = "character", length = x)
    
    for (j in 1:length(parent.lemma.v)) {
      if (z$head[j] > 0) {
        
        parent.lemma.v[j] <- z$lemma[z$head][j]
        parent.form.v[j] <- z$form[z$head][j]
        
      }
      
    }
    
   
    
    
    ##### code to find siblings
    
    
    
    ancestor_g2.v <- z$head[parent.v] %>%
      as.integer()
    ancestor_g2.v[which(ancestor_g2.v == 0)] <- NA
    
    
    ancestor_g3.v <- z$head[ancestor_g2.v] %>%
      as.integer()
    ancestor_g3.v[which(ancestor_g3.v == 0)] <- NA
    
    
    
    ancestor_g4.v <- z$head[ancestor_g3.v] %>%
      as.integer()
    ancestor_g4.v[which(ancestor_g4.v == 0)] <- NA
    
    
    ancestor_g5.v <- z$head[ancestor_g4.v] %>%
      as.integer()
    ancestor_g5.v[which(ancestor_g5.v == 0)] <- NA
    
    z <- mutate(z, anc_g2_id = ancestor_g2.v)
    z <- mutate(z, anc_g3_id = ancestor_g3.v)
    z <- mutate(z, anc_g4_id = ancestor_g4.v)
    z <- mutate(z, anc_g5_id = ancestor_g5.v)
    
    
    ## code to spell out postag abbreviations
    
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
    
    person.v <- substr(z$postag, 2, 2)
    
    ############## adjust person to fix pronouns
    
    
    
    person.v[which(person.v == "1") ] <- "1st-person"
    person.v[which(person.v == "2") ] <- "2nd-person"
    person.v[which(person.v == "3") ] <- "3rd-person"
    person.v[which(person.v == "-")] <- NA
    
    number.v <- substr(z$postag, 3, 3)
    number.v[which(number.v == "s")] <- "singular"
    number.v[which(number.v == "p")] <- "plural"
    number.v[which(number.v == "d")] <- "dual"
    number.v[which(number.v == "-")] <- NA
    
    tense.v <- substr(z$postag, 4, 4)
    tense.v[which(tense.v == "p")] <- "present"
    tense.v[which(tense.v == "i")] <- "imperfect"
    tense.v[which(tense.v == "a")] <- "aorist"
    tense.v[which(tense.v == "r")] <- "perfect"
    tense.v[which(tense.v == "l")] <- "pluperfect"
    tense.v[which(tense.v == "f")] <- "future"
    tense.v[which(tense.v == "t")] <- "futurePerfect"
    tense.v[which(tense.v == "-")] <- NA
    
    
    mood.v <- substr(z$postag, 5, 5)
    mood.v[which(mood.v == "i")] <- "indicative"
    mood.v[which(mood.v == "s")] <- "subjunctive"
    mood.v[which(mood.v == "o")] <- "optative"
    mood.v[which(mood.v == "n")] <- "infinitive"
    mood.v[which(mood.v == "p")] <- "participle"
    mood.v[which(mood.v == "d")] <- "gerund"
    mood.v[which(mood.v == "m")] <- "imperative"
    mood.v[which(mood.v == "g")] <- "gerundive"
    mood.v[which(mood.v == "u")] <- "supine"
    mood.v[which(mood.v == "_")] <- NA
    mood.v[which(mood.v == "-")] <- NA
    
    voice.v <- substr(z$postag, 6, 6)
    voice.v[which(voice.v == "a")] <- "active"
    voice.v[which(voice.v == "p")] <- "passive"
    voice.v[which(voice.v == "m")] <- "middle"
    voice.v[which(voice.v == "e")] <- "deponent"
    voice.v[which(voice.v == "-")] <- NA
    
    gender.v <- substr(z$postag, 7, 7)
    gender.v[which(gender.v == "m")] <- "masculine"
    gender.v[which(gender.v == "f")] <- "feminine"
    gender.v[which(gender.v == "n")] <- "neuter"
    gender.v[which(gender.v == "-")] <- NA
    
    case.v <- substr(z$postag, 8, 8)
    case.v[which(case.v == "n")] <- "nominative"
    case.v[which(case.v == "g")] <- "genitive"
    case.v[which(case.v == "d")] <- "dative"
    case.v[which(case.v == "a")] <- "accusative"
    case.v[which(case.v == "b")] <- "ablative"
    case.v[which(case.v == "v")] <- "vocative"
    case.v[which(case.v == "l")] <- "locative"
    case.v[which(case.v == "-")] <- NA
    
    degree.v <- substr(z$postag, 9, 9)
    degree.v[which(degree.v == "p")] <- "positive"
    degree.v[which(degree.v == "c")] <- "comparative"
    degree.v[which(degree.v == "s")] <- "superlative"
    degree.v[which(degree.v == "-")] <- NA
    
    
    ## add columns to z
    z <- mutate(z, self_POS = pos.v)
    
    z <- mutate(z, parent_lemma = parent.lemma.v)
    z <- mutate(z, parent_form = parent.form.v)
    
    z <- mutate(z, self_person = person.v)
    z <- mutate(z, self_number = number.v)
    z <- mutate(z, self_tense = tense.v)
    z <- mutate(z, self_mood = mood.v)
    z <- mutate(z, self_voice = voice.v)
    z <- mutate(z, self_gender = gender.v)
    z <- mutate(z, self_case = case.v)
    z <- mutate(z, self_degree = degree.v)
    
    
    ############ add data for parent token
    z <- mutate(z, parent_relation = z$relation[z$parent_id])
    z <-  mutate(z, parent_POS = z$self_POS[z$parent_id])
    
    z <- mutate(z, parent_lemma = z$lemma[z$parent_id])
    z <- mutate(z, parent_form = z$form[z$parent_id])
    z <- mutate(z, parent_person = z$self_person[z$parent_id])
    z <- mutate(z, parent_number = z$self_number[z$parent_id])
    z <- mutate(z, parent_tense = z$self_tense[z$parent_id])
    z <- mutate(z, parent_mood = z$self_mood[z$parent_id])
    z <- mutate(z, parent_voice = z$self_voice[z$parent_id])
    z <- mutate(z, parent_gender = z$self_gender[z$parent_id])
    z <- mutate(z, parent_case = z$self_case[z$parent_id])
    z <- mutate(z, parent_degree = z$self_degree[z$parent_id])
    
    ### add data for more remote ancestors
    
    z <- mutate(z, g2_relation = z$relation[z$anc_g2_id])
    z <- mutate(z, g2_POS = z$self_POS[z$anc_g2_id])
    
    z <- mutate(z, g3_relation = z$relation[z$anc_g3_id])
    z <- mutate(z, g3_POS = z$self_POS[z$anc_g3_id])
    
    z <- mutate(z, g4_relation = z$relation[z$anc_g4_id])
    z <- mutate(z, g4_POS = z$self_POS[z$anc_g4_id])
    
    z <- mutate(z, g5_relation = z$relation[z$anc_g5_id])
    z <- mutate(z, g5_POS = z$self_POS[z$anc_g5_id])
    
    
    z <- mutate(z, sibling_POS = NA)
    z <- mutate(z, sibling_relation = NA)
    
    
    for (j in seq_len(nrow(z))) {
      
      sib.pos.v <- z$self_POS[z$sibling_id[j] %>%
                                str_split(" ") %>%
                                unlist() %>%
                                as.integer() ]
      
      sib.pos.v <- list(sib.pos.v)
      z$sibling_POS[j] <- sib.pos.v
      
      sib.rel.v <- z$relation[z$sibling_id[j] %>%
                                str_split(" ") %>%
                                unlist() %>%
                                as.integer() ]
      sib.rel.v <-list(sib.rel.v)
      z$sibling_relation[j] <- sib.rel.v
      
    }
    
    
    z <- mutate(z, direct_deps_POS = NA)
    z <- mutate(z, direct_deps_relation = NA)
    
    
    
    for (j in 1:nrow(z)) {
      
      
      if (z$direct_deps_count[j] > 0 ) {
        
        d <- str_split(z$direct_deps_id[j], " ") %>%
          unlist() %>%
          as.integer()
        
        z$direct_deps_POS[j] <-  z$self_POS[d] %>%
          list()
        
        z$direct_deps_relation[j] <-  z$relation[d] %>%
          list()
        
      }
      
      
    }
    
    
    anc.g2.lemma.v <- vector(mode = "character", length = x)
    anc.g2.form.v <- vector(mode = "character", length = x)
    
    
    for (j in 1: nrow(z)) {
      
      if (!is.na(z$anc_g2_id[j]) &   z$anc_g2_id[j] > 0) {
        
        anc.g2.lemma.v[j] <- z$lemma[z$anc_g2_id ][j]
        anc.g2.form.v[j] <- z$form[z$anc_g2_id][j]
        
      }
      
    }
    
    anc.g2.lemma.v[anc.g2.lemma.v == ""] <- NA
    anc.g2.form.v[anc.g2.form.v == ""] <- NA
    
    z <- mutate(z, anc_g2_lemma = anc.g2.lemma.v)
    z <- mutate(z, anc_2g_form = anc.g2.form.v)
    
    
    
    
    
    if (i == 1) {
      treebank.data.df <- z
      
    } else {
      treebank.data.df <- bind_rows(treebank.data.df, z)
      
    }
    
    
  }
  
  treebank.data.df$self_person[which(treebank.data.df$self_POS == "pronoun")] <- NA
  holder.l[[k]] <- treebank.data.df
  
}


colnames(treebank.data.df)
saveRDS(holder.l, file = "base_values_7-1-2019.RDS")

#########################################

rows.v <- NULL

for (i in seq_along(holder.l)) {
  rows.v <- append(rows.v, nrow(holder.l[[i]]))
  
}

sum(rows.v)

result.tib <- as_tibble(treebank.data.df) %>%
  select(self_form)

result.tib %>%
  filter(self_POS == "pronoun") %>%
  dplyr::select(self_POS, self_person) %>%
  group_by(self_POS, self_person) %>%
  summarise(n())

## this works
treebank.data.df$self_person[which(treebank.data.df$self_POS == "pronoun")] <- NA

holder.l[[6]]
y

#############################
colnames(treebank.data.df)

treebank.data.df %>%
  filter(self_POS == "verb") %>%
  filter(self_voice == "active") %>%
  group_by(self_tense, self_mood) %>%
  summarize(count =  n()) %>%
  arrange(desc(count) )
  
  
  
colnames(treebank.data.df)

which(treebank.data.df$relation == "AuxC")

treebank.data.df %>% 
  filter(parent_POS == "verb") %>%
  dplyr::select(self_POS) %>%
  na.omit() %>%
  group_by(self_POS) %>%
  summarize(count =  n())

treebank.data.df %>% 
  filter(parent_POS == "verb") %>%
  filter((self_POS == "noun" | self_POS == "adjective" | self_POS == "pronoun")) %>%
  group_by(relation) %>%
  summarize(count =  n()) %>%
  arrange(desc(count)) %>% 
  mutate(percent = count/1967)
  
  
  

treebank.data.df %>% 
  filter(self_POS == "verb" & self_voice == "passive" & self_person == "2nd-person") %>%
  dplyr::select(form)

treebank.data.df$self_person

treebank.data.df$form[ which(str_detect(treebank.data.df$form, "mini") == TRUE)]

treebank.data.tib %>% 
  filter(parent_relation == "AuxC") %>%
  filter(self_POS == "verb") %>%
  filter(g2_POS == "verb")

treebank.data.tib %>% 
  filter(parent_POS == "verb") %>%
  filter(self_POS == "noun" | self_POS == "pronoun" | self_POS == "adjective") %>%
  dplyr::select(self_case) %>%
  na.omit() %>%
  group_by(self_case) %>%
  summarize(count =  n())


exam.tib <- treebank.data.tib %>% 
  filter(relation == "PNOM_CO") %>%
  dplyr::select(form, lemma, relation, locator)

abl.tib <- treebank.data.tib %>% 
  filter(parent_POS == "verb") %>%
  filter(self_POS == "noun" | self_POS == "pronoun" | self_POS == "adjective") %>%
  filter(relation == "ADV") %>%
  filter(self_case == "ablative") %>%
  dplyr::select(lemma, parent_lemma)

exam.tib <- coord.tib <- treebank.data.tib %>% 
  filter(parent_relation == "COORD")


exam.tib <- treebank.data.tib %>% 
  filter(parent_relation == "AuxC") %>%
  filter(self_POS == "verb") %>%
  filter(g2_POS == "verb")

colnames(treebank.data.df)

treebank.data.tib %>%
  filter(self_POS == "x")

rm(treebank.data.tib)

names(holder.l) <- gsub(".xml", "", files.v)

gsub(".xml", "", files.v)

t <- holder.l[[1]][, 1:2]

names(t[2]) <- "poop"

colnames(t) <- c("stink", "rot")

colnames(test.tib)[2] <- v[1]

a <- holder.l[[1]]  %>%
  mutate(test1 = paste0(holder.l[[1]]$relation, "_", holder.l[[1]]$self_POS)) %>%
  
  dplyr::select(std_ref, locator, test1) %>%
  na.omit() %>%
  group_by(test1) %>%
  summarize(count =  n())

###########################
p <- holder.l[[1]][, c(5, 17:27, 29:45)] %>%
  colnames() %>%
  combn(3)

q <- holder.l[[1]][, c(5, 17:27, 29:45)] %>%
  colnames() %>%
  combn(3, simplify = FALSE)


v_names.l <-  lapply(q, paste0, collapse = "_&_" )


x <- lengths(sent.list)

x <- x -1

mean(x)

hist(x, 50)

which(x < 20)

quantile(x)

which(x > 35 & x < 51)




#################################### loop to populate values





value_holder.l <- vector(mode = "list", length(files.v))

for (k in seq_along(files.v)) {
  
  test.tib <- holder.l[[k]][, c(7, 10)]
  
  for (i in seq_len(ncol(p))) {
    t   <- paste0(unlist(holder.l[[k]][p[1,i]]), "_", unlist(holder.l[[k]][p[2,i]]) )
    test.tib <- add_column(test.tib, i = t)
    colnames(test.tib)[2 + i] <- v[i]
    
    
    
    test.tib[which(test.tib[, 2 + i] %>%
                     unlist() %>%
                     str_detect("NA") == TRUE), 2 + i] <- NA
    
  }
  
  test.tib <- as_tibble(test.tib)
  
  value_holder.l[[k]] <- test.tib
  
}

object.size(value_holder.l)

for (i in seq_len(ncol(p))) {
  t   <- paste0(unlist(holder.l[[1]][p[1,i]]), "_", unlist(holder.l[[1]][p[2,i]]) )
  test.tib <- add_column(test.tib, i = t)
  colnames(test.tib)[2 + i] <- v[i]
  
  
  
  test.tib[which(test.tib[, 2 + i] %>%
                   unlist() %>%
                   str_detect("NA") == TRUE), 2 + i] <- NA
  
}


value_holder.l[[1]]

test.tib <- as_tibble(test.tib)

k <- 5

for (k in seq_along(value_holder.l)) {
  b <- NULL
  a <- NULL
  
  for (i in 3:ncol(value_holder.l[[k]])) {
    a <- value_holder.l[[k]][, i] %>%
      is.na() %>%
      sum() %>%
      subtract(nrow(value_holder.l[[k]]), .)
    
    b <- append(b, a)
    
  }
  
  
  names(b) <- v
  if (k == 1) {
    
    
    sums.m <- matrix(b, nrow = 1, byrow = TRUE)
    
  } else {
    
    sums.m <- rbind(sums.m, b)
  }
  
}

vsums.tib <- sums.m %>%
  as_tibble()

vsums.tib <- vsums.tib %>%
  colSums()

vsums.tib <- sort(vsums.tib, decreasing = TRUE)

## save results in list object

variable_counts.l <- vector(mode = "list", 5)

variable_counts.l[[2]] <- vsums.tib
###############################################  for 3 combined values

holder.l[[1]] %>%
  nrow()

p <- holder.l[[1]][, c(5, 17:27, 29:45)] %>%
  colnames() %>%
  combn(3)

q <- holder.l[[1]][, c(5, 17:27, 29:45)] %>%
  colnames() %>%
  combn(1, simplify = FALSE)


v_names.l <-  lapply(q, paste0, collapse = "_&_" )






#################################### loop to populate values





value_holder.l <- vector(mode = "list", length(files.v))

for (k in seq_along(files.v)) {
  
  test.tib <- holder.l[[k]][, c(7, 10)]
  
  for (i in seq_len(ncol(p))) {
    t   <- paste0(unlist(holder.l[[k]][p[1,i]]), "_", unlist(holder.l[[k]][p[2,i]]) )
    test.tib <- add_column(test.tib, i = t)
    colnames(test.tib)[2 + i] <- v[i]
    
    
    
    test.tib[which(test.tib[, 2 + i] %>%
                     unlist() %>%
                     str_detect("NA") == TRUE), 2 + i] <- NA
    
  }
  
  test.tib <- as_tibble(test.tib)
  
  value_holder.l[[k]] <- test.tib
  
}



for (i in seq_len(ncol(p))) {
  t   <- paste0(unlist(holder.l[[1]][p[1,i]]), "_", unlist(holder.l[[1]][p[2,i]]) )
  test.tib <- add_column(test.tib, i = t)
  colnames(test.tib)[2 + i] <- v[i]
  
  
  
  test.tib[which(test.tib[, 2 + i] %>%
                   unlist() %>%
                   str_detect("NA") == TRUE), 2 + i] <- NA
  
}


value_holder.l[[1]]

test.tib <- as_tibble(test.tib)

k <- 5

for (k in seq_along(value_holder.l)) {
  b <- NULL
  a <- NULL
  
  for (i in 3:ncol(value_holder.l[[k]])) {
    a <- value_holder.l[[k]][, i] %>%
      is.na() %>%
      sum() %>%
      subtract(nrow(value_holder.l[[k]]), .)
    
    b <- append(b, a)
    
  }
  
  
  names(b) <- v
  if (k == 1) {
    
    
    sums.m <- matrix(b, nrow = 1, byrow = TRUE)
    
  } else {
    
    sums.m <- rbind(sums.m, b)
  }
  
}

############################### try again!!

holder.l[[1]][, c(7, 10, 5, 17:27, 29:45)]



value_holder.l <- vector(mode = "list", length(files.v))

variable_counts.l <- vector(mode = "list", 5)


q <- holder.l[[1]][, c(5, 17:27, 29:45)] %>%
  colnames() %>%
  combn(3, simplify = FALSE)


v_names.l <-  lapply(q, paste0, collapse = "_&_" )

seq_along(q)
k <- 1
i <- 1

holder.l[[1]][1, q[[2]] ] %>%
  paste0(collapse = "_&_")

results.m <- matrix( nrow = nrow(holder.l[[1]]))
results.m <- holder.l[[1]][, c(7, 10)]

for (i in seq_along(q)) {
  
  s <- NULL
  
  for (j in seq_len(nrow(holder.l[[k]]))) {
    
    if (holder.l[[k]][j, q[[i]] ] %>%
        is.na() %>%
        is_in(TRUE, .)) {
      
      s <- append(s, NA)
      
    } else {
      
      t <- holder.l[[k]][j, q[[i]] ] %>%
        paste0(collapse = "_&_")
      s <- append(s, t)
      
    }
    
    n <- paste0(q[[i]], collapse = "_&_")
    results.m <-  add_column(results.m, s)
    colnames(results.m)[2 + i] <- n
    
    
    
  }
  
  
  
  
  
  ######
  
  for (k in seq_along(value_holder.l)) {
    b <- NULL
    a <- NULL
    
    for (i in 3:ncol(value_holder.l[[k]])) {
      a <- value_holder.l[[k]][, i] %>%
        is.na() %>%
        sum() %>%
        subtract(nrow(value_holder.l[[k]]), .)
      
      b <- append(b, a)
      
    }
    
    
    names(b) <- v
    if (k == 1) {
      
      
      sums.m <- matrix(b, nrow = 1, byrow = TRUE)
      
    } else {
      
      sums.m <- rbind(sums.m, b)
    }
    
  }
  
  
  ##############################################
  
  
  for (i in 3:ncol(value_holder.l[[k]])) {
    a <- test.tib[, i] %>%
      is.na() %>%
      sum() %>%
      subtract(nrow(test.tib), .)
    
    b <- append(b, a)
    
  }
  
  names(b) <- v
  
  
  
  
  b %>%
    sort(decreasing = TRUE)
  
  a <- test.tib[, i] %>%
    is.na() %>%
    sum() %>%
    subtract(nrow(test.tib), .)
  
  
  t <- test.tib %>%
    as.data.frame()
  
  colnames(t[3]) <- "pig"
  
  rename(test.tib$``, "pig")
  
  test.tib[which(str_detect(unlist(test.tib[, 2 + i], "NA") ) == TRUE) , 2 + i]
  
  test.tib[which(test.tib[, 2 + i] %>%
                   unlist() %>%
                   str_detect("NA") == TRUE), 2 + i] <- NA
  
  which(test.tib[, 2 + i] %>%
          unlist() %>%
          str_detect("NA") == TRUE)
  
  test.tib$`paste0(...)` %>%
    str_detect("NA")
  