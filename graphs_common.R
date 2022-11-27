library(ggrepel)
library(scales)
library(stringr)

mkdir <- function(dir) {
  d <- "plots"
  if (str_length(dir) > 0) {
    d <- paste0(d, "/", trimws(dir, whitespace = '/'))
  }
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
  d
}

mkfile <- function(dir, pattern, ...) {
  d <- mkdir(dir)
  p <- paste0(d, "/", pattern)
  str_interp(p, env = list(...))
}

# cs <- brewer.pal(n = 9, name = "Set1")
# names(cs) <- c('Russia', 'USA', 'Pakistan',
#                'India', 'China', 'South Africa',
#                'UK', 'France', 'North Korea')

cs <- pal_d3("category10")(9)
names(cs) <- c('USA', 'China', 'Pakistan',
               'Russia', 'India', 'UK',
               'France', 'North Korea', 'South Africa')

cs <- append(cs, c(Other = unname(cs['South Africa'])))

# show_col(hue_pal()(9))
# show_col(brewer.pal(n = 9, name = "Set2"))
#show_col(brewer.pal(n = 9, name = "Set1"))
#display.brewer.all(n=NULL, type="qual", select=NULL, exact.n=TRUE)

#library(ggsci) # colors
#show_col(pal_d3("category10")(9))
#show_col(pal_aaas()(9))
#show_col(pal_futurama()(9))
#show_col(pal_igv()(9))
#show_col(pal_jco()(9))
#show_col(pal_lancet()(9))
#show_col(pal_npg()(9))
#show_col(pal_rickandmorty()(9))
#show_col(pal_simpsons()(9))
#show_col(pal_uchicago()(9))
#show_col(pal_ucscgb()(9))

#library(wesanderson)
#show_col(wes_palette(name = "Cavalcanti1"))

#library(rcartocolor)
#show_col(rcartocolor::carto_pal(9, name="Bold"))
#show_col(rcartocolor::carto_pal(9, name="Vivid"))
#show_col(rcartocolor::carto_pal(9, name="Safe"))
#show_col(rcartocolor::carto_pal(9, name="Pastel"))
#show_col(rcartocolor::carto_pal(9, name="Prism"))
