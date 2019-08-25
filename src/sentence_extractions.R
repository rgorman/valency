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


x <- sent.list %>%
  lengths() - 1


hist(x, 100)
summary(x)


x <- (sent.list[[1]] %>% # drop ".attrs" row; this row contains sentence metadata
        length() - 1)

qplot(x, geom = "histogram",
      binwidth = 5,
      main = "De Amicitia",
      xlab = "Sentence Length",
      ylab = "Count",
      col=I("black"))



z <- table(x) %>%
  as.data.frame()



theme_update(plot.title = element_text(hjust = 0.5)) # centers title of plot
ggplot(mapping = aes(x)) + geom_histogram(bins = 30) +
  ggtitle("De Amicitia") +
  xlab("Sentence Length")

ggplot(mapping = aes(x)) + geom_histogram(bins = 35) +
  ggtitle("De Amicitia") +
  xlab("Sentence Length")

##########



