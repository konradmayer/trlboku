#' @title plot_bericht
#' @description Produces a segment plot used in standard dating reports of the
#'   BOKU tree ring lab based on the overview excel table.
#' @param filename a path to a .xls file used for dating overview in the BOKU
#'   tree ring lab.
#' @param encoding encoding of the .xls file, defaults to 'WINDOWS-1252'
#' @param set_lwd width of the segments
#' @param multi adjusts the height of the output png image
#' @export

plot_bericht <- function(filename, encoding = 'WINDOWS-1252', set_lwd = 50,
                         multi = 0.7) {
  #create lookup table for allowed species and their assigned colors
  # species <- data.frame(Baumart=c('Fichte', 'Tanne', 'L?rche', 'Eiche', 'Zirbe',
  #                                   'Kiefer', 'Nadelholz', 'Ulme', 'Buche'),
  #                         Farbe=c('#056608', '#0018A8', '#D3212D', '#967117', '#FFEF00',
  #                                 '#FF7E00', '#9FA91F', '#5e245e', '#b70505'),
  #                         stringsAsFactors = FALSE)

  data('species')
  rownames(species) <- species[ ,'german']

  #read in files
  header <- gdata::read.xls (filename, sheet = 1, header = F,
                             fileEncoding = encoding)
  header <- header[1:3, 4:5]

  df <-  read.xls (filename, sheet = 1, header = TRUE, pattern='Nr.',
                   fileEncoding=encoding)
  columns <- c(2, 3, 4, 5, 7)
  df[,columns] <- lapply(df[ ,columns], FUN = function(x) as.character(x))

  #test if all species in "Baumart" are specified within the object species
  if(any(!(df[ , 3] %in% species[,'german']))){
    stop('typing error in "Baumart" or species not implemented')
  }

  #tests for typing errors in series length
  if(!is.integer(df[,6])){
    stop('typing error in column "Jahrringanzahl"')
  }

  #tests for typing errors in column "WK"
  if(any(!(df[ ,5] %in% c('keine', 'nein', 'ja')))){
    stop('problem in column "WK"')
  }

  #selects only dated series
  df <- df[!(df[4] %in% c('nicht datiert', 'nicht bearbeitbar')), ]

  #separate date end and unmeasured years from string
  date.end <- as.data.frame(as.numeric(stringr::str_extract(df[ ,4], '-?\\d{1,4}')))
  colnames(date.end) <- 'letztes.Jahr'

  date.begin <- date.end - df[6] + 1
  colnames(date.begin) <- 'erstes.Jahr'

  unmeasured <- substr(df[ ,4], stringr::str_locate(df[ ,4], '-?\\d{1,4}')[ ,2]
                       + 1, nchar(df[,4]))

  replacements <- list(c('\\+', ''), c('min.', ''), c('JR', ''),
                       c(' ', ''))
  unmeasured <- tryCatch(as.double(mgsub(replacements, unmeasured)), warning =
                           function(w){stop('problem in column "letztes Jahr"')})
  unmeasured[is.na(unmeasured)] <- 0

  #compiling data set for plotting
  dat <- data.frame(df[2:3], date.begin, date.end , df[6], Farbe = NA, df[5],
                    unmeasured)
  dat[ ,6] <- species[dat[, 'Holzart'], 'color']

  dat <- dat[order(dat$letztes.Jahr+dat$unmeasured), ]

  #PLOT----------------------
  makeplot <- function() {
    xlim <- c(min(dat[3]) - 10, max(dat[ , 4] + dat[ , 8], na.rm = TRUE) + 10)
    ylim <- c(0, nrow(dat) + 1)

    png(paste(header[1, 2], '.png', sep = ""), units = 'in',
        height = (ylim[2] + 2) * multi, width = 12, res = 150)

    par(mai = c(1, 1, 0.4, 1))

    plot(0, xlim = xlim, main = header[1, 2], type = 'l', bty = 'o', yaxt = 'n',
         ylim = ylim, ylab = 'Probe', xlab = 'Jahr', lwd = set_lwd, xaxs = 'i',
         yaxs = 'i')

    grid(ny = NA, col = 'grey50')

    axis(2, at = seq_len(nrow(dat)), labels = dat[ ,1], las = 1)
    axis(4, at = seq_len(nrow(dat)),
         labels = dat[ , 4] + dat[ , 8], las = 1, outer = F)


    #plot not measured rings
    these_lines <- which(!is.na(dat[ ,8]))
    lapply(these_lines, FUN = function(i) {
      segments(dat[i, 4], i, dat[i, 4] + dat[i, 8],  col = 'grey90',
               lend = 1,lwd = set_lwd, xaxs = 'i', yaxs = 'i')
    })

    #plot segments
    lapply(seq_len(nrow(dat)), FUN = function(i) {
      lines(as.numeric(dat[i, 3:4]), c(i,i), lwd = set_lwd, lend = 3,
            col = dat[i, 6], xaxs = 'i', yaxs = 'i')
      #text(xlim[2] + 0.5, i, dat[i, 4] + dat[i, 8], pos = 4)
    })


    #waldkante
    these_lines <- which(dat[ ,7] == 'ja')
    lapply(these_lines, FUN = function(i) {
      points(dat[i, 4] + dat[i, 8] + 2, i, pch = 20)
      lines(as.numeric(c(dat[i, 4] + dat[i, 8] - 1,dat[i, 4] + dat[i, 8])), c(i, i),
            lwd = set_lwd, lend = 1, col = 'black', xaxs = 'i', yaxs = 'i')
    })


    #legend
    legende <- species[unique(dat$Holzart),]
    legend('bottom', legend = legende[, 'german'], fill = as.character(legende[, 'color']),
           ncol = nrow(legende), bty = 'n')

    dev.off()
  }
  try(makeplot())
}
