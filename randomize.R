people.raw <- "Priyam, Bruno, Roddy, Emeline, Yannick, Ilya, Ismail, Leandro,
               Joe, Carlos, Eckart, Diego, Karina"
                          
journals.raw <- "Mol Ecol, Plos Genet, PNAS, Science, Nature, Nature Genetics,
                 Plos Biol, Plos Comp Biol, eLife, Insectes Sociaux, MBE, Cell,
                 Proc B, Myrmecological News, Bioinformatics, Current Biology,
                 Trends in Genetics, TREE, Genome Res, Genome Biol, Am Nat,
                 Evolution, Journal of Evolutionary Biology, Nature Comms, GBE"

library(reshape2)
library(knitr)
output.file <- paste(Sys.Date(), "-TableOfContentsDuties.md", sep="")

people   <- unlist(strsplit(x     = people.raw,
                            split = ",\\s*",
                            perl  = TRUE
                            ))
journals <- unlist(strsplit(x     = journals.raw,
                            split = ",\\s*",
                            perl  = TRUE
                            ))


## Assign 2 people per journal - in a manner that equalises efforts
people.random   <- sample(people)
journals.random <- sample(journals)

## Feature not a bug: R auto-fills table until there is no more space
message("Ignore the warning - its ok!")
assignments <- matrix(data     = people.random,
                      nrow     = length(journals),
                      ncol     = 2,
                      dimnames = list(journals.random,
                                      c('person1', 'person2')),
                      )

##sanity check:
if (anyDuplicated(paste(row.names(assignments), assignments))) {
   stop("DUPLICATE ASSIGNMENTS") 
}

write(x = kable(assignments, format="markdown"), file= output.file)
write(x = "\n\n\n",                              file= output.file, append=TRUE)

## show assignments per person: 
assignments.long <- melt(assignments)
colnames(assignments.long) <- c("journal", "number", "person")

assignments.perperson <- dcast(data       = assignments.long,
                             formula      = person~"journal",
                             value.var    = "journal",
                             fun.aggregate=function(x) {paste(x, collapse=", ")}
                             )

write(x      = kable(assignments.perperson, format="markdown"),
      file   = output.file,
      append = TRUE)
