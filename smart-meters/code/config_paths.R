main.folder   <- "../"

rawdata.folder <- file.path(main.folder, "rawdata")
procdata.folder <- file.path(main.folder, "procdata")
work.folder <- file.path(main.folder, "work")

initmeters.folder  <- file.path(procdata.folder, "initmeters")
mymeters.folder   <- file.path(procdata.folder , "mymeters")
aggseries.folder   <- file.path(procdata.folder , "aggseries")

basef.folder  <- file.path(work.folder , "basef")
results.folder <- file.path(work.folder , "pdfs")
loss.folder <- file.path(work.folder , "loss")
insample.folder <- file.path(work.folder , "insample")
permutations.folder <- file.path(work.folder , "permutations")
coverage.folder <- file.path(work.folder , "coverage")

#list.folders <- c(rawdata.folder, procdata.folder, work.folder, initmeters.folder, mymeters.folder, aggseries.folder,
#                  basef.folder, results.folder, loss.folder, insample.folder, permutations.folder, coverage.folder)
#sapply(list.folders, dir.create)