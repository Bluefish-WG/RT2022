
`%>%` <- magrittr::`%>%`

render_ref <- function(refs = "references_spreadsheet.csv", # the file path to the reference spreadsheet
                       dir # directory where references.bib should be saved. only needed because of packaging issues
) {
  data <- utils::read.csv(file = paste(dir, refs, sep = "/"))
  file <- paste0(dir, "/references.bib")
  # print(file)
  sink(file)

  data$title <- ifelse(stringr::str_detect(data$title,
                                             pattern = "^[:upper:]{2,}"),
                         stringr::str_to_sentence(data$title),
                         data$title)

  for (i in 1:nrow(data)) {
    res <- knitr::knit_expand(
      text =
        "@article{ {{keyword}},
        author  = { {{authors}} },
        title   = { {{title}} },
        journal = { {{journal}} },
        year    = { {{year}} },
        number  = { {{issue}} },
        pages   = { {{pages}} },
        volume  = { {{volume}} }
      }",
      keyword = data$keyword[i],
      authors = data$authors[i] %>%
        stringr::str_to_title() %>%
        stringr::str_replace_all(";", " and"),
      title = data$title[i],
      journal = data$journal[i] %>%
        stringr::str_to_title(),
      year = data$year[i],
      issue = data$issue[i],
      pages = data$pages[i],
      volume = data$volume[i]
    )
    cat(res, sep = "\n\n")
  }
  sink()
}

render_ref(refs = "bluefish_references.csv",
                  dir = "data-raw")
