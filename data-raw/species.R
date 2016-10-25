species <- read.csv(file = 'data-raw/species.csv', stringsAsFactors = FALSE)
devtools::use_data(species)
