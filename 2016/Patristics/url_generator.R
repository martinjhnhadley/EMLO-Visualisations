url_generator <- function(book_name = NA,
                          begin_chapter = NA,
                          begin_v = NA,
                          end_chapter = NA,
                          end_v = NA,
                          link_text = NA) {
  paste0(
    "<a href=",
    "https://www.biblegateway.com/passage/?search=",
    gsub(" ", "+", book_name),
    "+",
    begin_chapter,
    "%3A",
    begin_v,
    "+-+",
    end_chapter,
    "%3A",
    end_v,
    "&version=NRSV",
    ">",
    '<span class="glyphicon glyphicon-new-window" aria-hidden="true"></span>',
    "</a>"
  )
}

example_url <- "https://www.biblegateway.com/passage/?search=1+Corinthians

+

2

%3A

9

+


-
+
2
%3A

9

&

version=NRSV"

paste0("book_gsubbed",
      "+",
      "start_chapter",
      "%3A",
      "start_verse",
      "+",
      "-",
      "+",
      "end_chapter",
      "%3A",
      "end_verse",
      "&version=NRSV"
      )


sample_reference <- references_df[5,]
sample_reference

url_generator(
  book_name = sample_reference$Epistle,
  begin_chapter = sample_reference$`Start Chapter`,
  begin_v = sample_reference$`Start Verse`,
  end_chapter = sample_reference$`End Chapter`,
  end_v = sample_reference$`End Verse`,
  link_text = paste(
    sample_reference$Epistle,
    "(link to passage)"
  )
)