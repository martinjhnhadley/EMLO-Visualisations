names_df <-
  read.csv(
    "data/names2.txt",
    stringsAsFactors = F,
    quote = "",
    sep = "\t",
    header = F
  )

## ================ Fix mutant Names  =============================
## ===========================================================

## The names2.txt file is garbled and the names need to be reconstructed.

garbled_names <- names_df[names_df$V3 != "",]
garbled_names$V2 <- paste(garbled_names$V2, garbled_names$V3)
names_df[names_df$V3 != "",] <- garbled_names
names_df <- names_df[,1:2]
colnames(names_df) <- c("id","name")
names_df$name <- stringi::stri_trans_general(names_df$name, "latin-ascii")
names_df$name <- iconv(names_df$name, "latin1", "ASCII", sub="")
names_df$name <- gsub("\v","",names_df$name)
names_df$name <- gsub("\032", "", names_df$name)

## ================ \032 =============================
## ===========================================================

sum(grepl("\032", names_df$name))

# write.csv(file = "032_containing_badnames.csv", names_df[grepl("\032", names_df$name),])



## ================ Find all bad =============================
## ===========================================================

names_df[grepl("[!a-z] | [!A-Z]",names_df$name),"name"]

vector_of_bad_strings <- c("ghThim d", "fj\032 d", "dRR �","This here, yes","DeVore")

grepl("[a-z]|[A-Z]", vector_of_bad_strings)

grepl("[^\x20-\x7F]",vector_of_bad_strings)

names_df$name[grepl("[^\x20-\x7F]", names_df$name)]




## ================ Old  =============================
## ===========================================================

sum(grepl("[/]",corrected_names))

## Remove bad characters
names_df$name <- iconv(names_df$name, "latin1", "ASCII", sub="")
names_df[names_df$id == "AU_16314",]$name


names_df[grepl("[\032]",names_df$name),]

write.csv(file = "data/bad_names.csv",names_df[grepl("[\032]",names_df$name),])

names_df$name <- gsub("[\032]","",names_df$name)
names_df$name< gsub("�","",names_df$name)



names_df[names_df$id == "AU_16314",]



## ====

codes_to_names_list[2290:2300]

