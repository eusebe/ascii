listoffiles <- paste("R/", list.files("R", "^.+\\.r$"), sep = "")
for (i in listoffiles) source(i)
