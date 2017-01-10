species <- read.csv(file = 'data-raw/species.csv', stringsAsFactors = FALSE,
                    encoding = 'UTF-8')
devtools::use_data(species, overwrite = TRUE)
