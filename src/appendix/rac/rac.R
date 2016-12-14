
# Download rac list -------------------------------------------------------


rac_original <- tempfile()
download.file('https://webetox.uba.de/webETOX/public/basics/literatur/download.do?id=284', 
              destfile = rac_original)


# digitize ----------------------------------------------------------------

library("tabulizer")
rac_list <- extract_tables(rac_original)

rac_list <- do.call(rbind, rac_list)
# fix two row
rac_list <- rac_list[!rac_list[ ,1] == '', ]



# keep only selected columns and make data.frame
rac <- data.frame(rac_list[ , c(1, 2, 4, 6)], stringsAsFactors = FALSE)

# split columns X2 and X3
library("tidyr")
rac <- separate(data = rac, col = X2, into = c("name", "type"), sep = -3)
rac <- separate(data = rac, col = X3, into = c("CAS", "date"), sep = " ")
# remove date 
rac$date <- NULL

# this is needed to fix an encoding problem
rac$CAS <- gsub('‐', '-', rac$CAS)
library(webchem)
# check if all are valid cas numbers
# Note in a future release of webchem is.cas will be vectorized!
which(vapply(rac$CAS, is.cas, FUN.VALUE = TRUE) == FALSE)
rac$CAS[39] <-  NA

# coerce rac to `numeric` (currently it is text)
rac$rac <- as.numeric(gsub(",", ".", rac$X4))


# select columns
rac <- rac[ , c('name', 'CAS', 'rac')]
# remove substances without rac
rac <- rac[!is.na(rac$rac), ]
# trim whitespaces (leading & trailling)
rac$name <- gsub("^\\s+|\\s+$", "", rac$name)
# set cas for dodin to 2439-10-3
rac$CAS[rac$name == 'Dodin'] <- '2439-10-3'
head(rac)
# # save
write.table(rac, file = '/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/src/appendix/rac//rac.csv', sep = ';', row.names = FALSE)


# query allan wood}
aw_results <- aw_query(rac$CAS, type = 'cas')

# Check for which CAS no data (=NA) was found
which(vapply(aw_results, function(y) length(y) == 1, TRUE))


# remove this from the results and the rac list.
rac <- rac[!rac$CAS == '120983-64-4', ]
aw_results <- aw_results[!names(aw_results) == '120983-64-4']


# extract the subactivity and add it to the rac table:
rac$subactivity <- vapply(aw_results, function(y) y[['subactivity']][1], 'x')

# extract the activity and build 4 groups (insecticides, herbicides, fungicides and others):

rac$type <- gsub('.* (.*)$', '\\1', rac$subactivity)
# fix
rac$type[rac$type == 'insecticidesmolluscicides'] <- 'insecticides'
# build groups
rac$type <- ifelse(!rac$type %in% c('herbicides', 'fungicides', 'insecticides'),
'other',
rac$type)'blue'

# fix chlorpyrifos
rac$type[rac$name == 'Chlorpyrifos'] <- 'insecticides' 
rac$type[rac$name == 'Dimethoat'] <- 'insecticides' 

# save to cache
write.table(rac, 
            file = '/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/src/appendix/rac//rac_fin.csv', 
            sep = ';', row.names = FALSE)

rac <- read.table('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/src/appendix/rac//rac_fin.csv',
                  sep = ';', header = TRUE, stringsAsFactors = FALSE)

# plot
library('ggplot2')
library('esmisc')
library('scales')
library('tikzDevice')
p <-  ggplot(rac, aes(y = rac, x = type)) +
  geom_boxplot(fill = 'grey75') +
  geom_jitter(width = 0.2) +
  scale_y_log10(labels = comma, breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)) +
  labs(x = '', y = 'RAC [ug/L]') +
  theme_edi() +
  ggtitle('105 RACs provided by UBA splitted by group')
p
ggsave('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/tikz/ap_rac.tikz',
       p, 
       device = tikz,
       width = 7,
       height = 5)



p2 <- ggplot(rac[order(rac$rac), ][1:30, ], 
             aes(x = reorder(name, -rac), y = rac, col = type)) +
  geom_point(size = 2) +
  scale_y_log10(labels = comma) +
  labs(x = '', y = 'RAC [ug/L]') +
  theme_edi() +
  coord_flip() +
  theme(legend.position = 'bottom') +
  scale_color_manual('Type', 
                     values = c('blue', "green4", 'red', 'orange')) +
  ggtitle('30 lowest RACs') 
p2

ggsave('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/tikz/ap_rac2.tikz',
       p2, 
       device = tikz,
       width = 6,
       height = 6)






                                                            
