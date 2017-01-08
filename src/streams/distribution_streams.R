
###----------------------------------------------------------------------------
### Occurence of small streams in  the landscape
### 
### 
require(rgrass7)
library(ggplot2)
library(scales)
library(ggmap)
library(reshape2)
library(cowplot)

### ---------------------------------------------------------------------------
### Setup GRASS 
# Done with GRASS7
# to use GRASS6, use spgrass6 and change the parameters accordingly


# Initialize GRASS
loc <- initGRASS(gisBase = "/usr/lib/grass72/", 
                 home = tempdir(),
                 gisDbase = '/home/edisz/GRASSDB/',
                 location = 'Germany',
                 mapset = 'bfg',
                 override = TRUE)
loc


# load raster
execGRASS("r.in.gdal", 
          flags = "overwrite", 
          parameters = list(input = "/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/src/cache/dem_extract.tif", 
                            output = "DEM"))
execGRASS("g.list", parameters = list(type = "rast"))
# set region
execGRASS("g.region", parameters = list(raster = 'DEM'))
gmeta()


# extract streams
execGRASS("r.watershed", 
          flags = "overwrite", 
          parameters = list(elevation = "DEM", 
                            drainage = 'fdir',  # flow direction
                            stream = 'stream',  # calculated streams
                            threshold = 750))

# extract topology
execGRASS("r.stream.order",
          flags = "overwrite",
          parameters = list(stream_rast = "stream",
                            direction = "fdir",
                            strahler = 'strahler'))


# calculate statistics on strahler
a <- execGRASS("r.stream.stats",
          flags = c('o', 'overwrite'),
          parameters = list(stream_rast = "strahler",
                            direction = "fdir",
                            elevation = "DEM"),
          intern = TRUE)
stats <- data.frame(do.call('rbind', strsplit(as.character(a[-c(1, 2)]),
                                              ',',
                                              fixed = TRUE)), 
                    stringsAsFactors = FALSE)
names(stats) <- strsplit(as.character(a[2]), ',', fixed = TRUE)[[1]]
stats <- stats[ , c(1, 2, 8)]
stats$num_of_streams <- as.numeric(stats$num_of_streams)
stats$sum_length <- as.numeric(stats$sum_length)
stats$perc_streams <- stats$num_of_streams / sum(stats$num_of_streams)
stats$perc_length <- stats$sum_length / sum(stats$sum_length)


# make vector of streams for plotting
execGRASS("r.stream.segment",
          flags = "overwrite",
          parameters = list(stream_rast = "strahler",
                            direction = "fdir",
                            elevation = "DEM",
                            segments = "segments",
                            sectors = "sectors"))
streams <- readVECT("segments")
streams <- spTransform(streams, CRS("+init=epsg:4326"))
streamsf <- fortify(streams)
streamsf <- merge(streamsf, data.frame(id = rownames(streams@data),
                           s_order = streams@data$s_order), by = 'id')




# plot streams + histogramm

map <- get_map(maptype = "satellite", location = bbox(streams))
p1 <- ggmap(map) +
  geom_path(data = streamsf, aes(x = long, y = lat, 
                                 col = factor(s_order), 
                                 group = group),
            size = 1) +
  scale_color_discrete('Strahler Order')


statdf <- melt(stats[, c(1, 4, 5)])
p_stats <- ggplot(statdf, aes(x = variable, y = value, fill = order)) +
  geom_bar(stat = 'identity') +
  theme(legend.position = 'none') +
  scale_x_discrete(breaks = c('perc_streams', 'perc_length'),
                   labels = c('Number of Streams', 'Length')) +
  scale_y_continuous(labels = percent) +
  labs(x = '', y = 'Proportion')

p <- plot_grid(p1, p_stats, rel_widths = c(2, 1))
ggsave('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/streams_distr.pdf', p,
       width = 14, height = 7)
