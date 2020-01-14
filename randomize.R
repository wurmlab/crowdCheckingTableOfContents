

people.raw <- "Priyam, Yannick, Marian, Magdalena, Federico, Raphaella, Anindita, Gabriel, Raph, Iwo, Guy, Alicja"

journals.raw <- "Mol Ecol, Plos Genet, PNAS, Science, Nature, Nature Genetics,
                 Plos Biol, Plos Comp Biol, eLife, Insectes Sociaux, MBE, Cell,
                 Proc B, Myrmecological News, Bioinformatics, Current Biology,
                 Trends in Genetics, TREE, Genome Res, Genome Biol, Am Nat,
                 Evolution, Journal of Evolutionary Biology, Nature Comms,
                 GBE, Nature Methods"

library(reshape2)
library(knitr)
output.file <- paste(Sys.Date(), "-TableOfContentsDuties.md", sep = "")

people   <- unlist(strsplit(x     = people.raw,
                            split = ",\\s*",
                            perl  = TRUE
                            ))
journals <- unlist(strsplit(x     = journals.raw,
                            split = ",\\s*",
                            perl  = TRUE
                            ))

quotient <- (length(journals)*2) %/% length(people)
remainder <- (length(journals)*2) %% length(people)


## Assign 2 people per journal - in a manner that equalises efforts
journals.random <- sample(journals)
people.random   <- c(sample(rep(people, times = quotient)), sample(people, remainder))
                   ## ugly ugly hack to randomize order while using autofilling bug

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
if (any(assignments[,'person1'] == assignments[,'person2'])) {
   stop("OOPS - SAME JOURNAL GIVEN TO 2 DIFFERENT PEOPLE -",
        "Try again you might get lucky")
}

file.create(output.file)
write(x = "Journal Assignments", file = output.file, append = TRUE)
write(x = "\n\n\n",                              file = output.file, append = TRUE)
write(x = kable(assignments, format="markdown"), file = output.file, append = TRUE)
write(x = "\n\n\n",                              file = output.file, append = TRUE)

## show assignments per person:
assignments.long <- melt(assignments)
colnames(assignments.long) <- c("journal", "number", "person")

assignments.perperson <- dcast(data       = assignments.long,
                             formula      = person~"journal",
                             value.var    = "journal",
                             fun.aggregate = function(x) {paste(x, collapse = ", ")}
                             )


write(x      = kable(assignments.perperson, format = "markdown"),
      file   = output.file,
      append = TRUE)

write(x = "\n\n\n",                              file = output.file, append = TRUE)

## CODING PAIRS CODE

#Randomise the names vector at each run
pair.people <- sample(people)

#Generation of the matrix with the pairs

message("For odd numbers of people there will be an ignorable warning")

codingpairs <- matrix(data     = pair.people,
                      nrow     = ceiling(length(pair.people)/2),
                      ncol     = 2
)

colnames(codingpairs) <- c('person1', 'person2')

# 'If' introducing a group of three if the number of persons participating is uneven
if (length(pair.people) %% 2 != 0) {
  codingpairs[nrow(codingpairs), 2] <- paste(codingpairs[nrow(codingpairs) - 1, 1], "and", codingpairs[nrow(codingpairs) - 1, 2])
  codingpairs <-  codingpairs[-(nrow(codingpairs) - 1), ]
}



#Output the .md file
write(x = "Coding Pairs", file = output.file, append = TRUE)
write(x = "\n\n\n",                              file = output.file, append = TRUE)
write(x = kable(codingpairs, format = "markdown"), file = output.file, append = TRUE)
write(x = "\n\n\n",                              file = output.file, append = TRUE)
