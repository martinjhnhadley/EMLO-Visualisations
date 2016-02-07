### Import
list.files()

judgements.df <- read.csv(file = "judgements.csv", header = TRUE, sep = ",")

## Get colnames with "..." in so as to find the judgements
data.columns <- colnames(judgements.df)[grepl("\\.{3}", colnames(judgements.df))]

replace.data.columns.fn <- function(column){
  
  judgements.df[,column] <<- as.factor(paste0(sub("\\..*", "", column),judgements.df[,column]))

}

invisible(lapply(data.columns, function(x) replace.data.columns.fn(x)))

str(judgements.df)

paste0(sub("\\..*", "", data.columns[63]),judgements.df[,data.columns[63]])



data.columns[1]

sub("\\..*", "", data.columns[63])
