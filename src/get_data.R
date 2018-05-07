# R script to download the raw datafiles from stable URLs
# The script also extracts the data from the zip files

library(utils)

download.file("https://svi.cdc.gov/Documents/Data/2016_SVI_Data/SVI2016_US.zip",
              "~/projects/superfund/superfund/raw/svi2016_us.zip")

download.file("http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_20m.zip",
              "~/projects/superfund/superfund/raw/cb_2017_us_county_20m.zip")

download.file("https://toxmap-classic.nlm.nih.gov/toxmap/download/superfund_shapefile.zip",
              "~/projects/superfund/superfund/raw/superfund_shapefile.zip")

path <- "~/projects/superfund/superfund/raw/"


for(i in list.files(path)) {
  
  unzip(paste0(path, i), exdir = path)

  }