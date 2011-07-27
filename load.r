listoffiles <- paste("R/", list.files("R", "^.+\\.r$"), sep = "")
for (i in listoffiles) {
  cat(i, "\n")
  source(i)
}
