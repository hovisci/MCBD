## Map

# devtools::install_github("ropensci/rnaturalearthdata")
# install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")

source('./scripts/Reference.R')

shp <- ne_countries(scale = 50, returnclass = 'sf') %>% #st_as_sf() %>%
  select(name, iso_a3) %>% ## , economy, income_grp
  # filter(name != 'Antarctica') %>%
  # filter(name == 'France') %>%
  dplyr::mutate(
    iso_a3 = if_else(name == 'France', 'FRA', iso_a3),
    iso_a3 = if_else(name == 'Norway', 'NOR', iso_a3))
# plot(shp[1])

shp %>% filter(name == 'Norway')

### add new country name that are consistant with names uned in SDG data
# library(readxl)
# xls.shp.info <- paste0(dir, '/update_0503_SUM_dist/_input_data/ne_10m_admin_0_countries/Table/ne_10m_admin_0_countries-Export_Output.xls')
#
# nation_new_name <-
#   read_excel(path = xls.shp.info,
#              sheet = "dat", col_names = T) %>%
#   select(ADMIN, ADM0_A3, nation_name)
# names(nation_new_name)
# nation_new_name <- as.data.frame(nation_new_name[,-c(3,4)])




### theme, font --------------------------------------------------------------------------------------
font      <- 'sans' ## = "TT Arial"
font_size <- 6.5 ## max = 7; min = 5 for Nature Sustainability
map_theme <- ggpubr::theme_transparent()+
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = c(0.09, 0.38),
    legend.key.size = unit(0.3, "cm"),
    # legend.key.height = unit(0.5, "cm"),
    legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
    
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    text=element_text(family=font, size=font_size))


### - Read in Data

df99 <- read.csv(paste0(dat.dir, 'survey2_cleaned.csv'))


### - list_countries
ctr_sys <- df99 %>%
  select(paper_id, list_countries) %>%
  data.frame(., do.call(rbind, str_split(.$list_countries,';'))) %>%
  gather(key = 'key', value = list_countries, 3:ncol(.)) %>%
  dplyr::mutate(list_countries = trimws(list_countries)) %>%
  dplyr::distinct(paper_id, list_countries, .keep_all = T) %>%
  filter(list_countries != '') %>%
  arrange(paper_id) %>%
  group_by(list_countries) %>%
  tally()%>%
  arrange(desc(n)) #%>%
# str(ctr_sys)


### join new name table
shp_df <- shp %>%
  left_join(., ctr_sys, by=c("iso_a3" = "list_countries"))

### check the join
shp_df_check <- merge(
  shp, ctr_sys, by.x = "iso_a3", by.y = "list_countries", all.y=T) %>%
  st_drop_geometry()

# str(shp_df)
unique(shp_df$n)


### breaks and colors
var <- shp_df$n
max <- max(var, na.rm = T); max
min <- min(var, na.rm = T); min
hist(var)

breaks <- seq(min, max, 2); breaks; length(breaks)
breaks <- sort(unique(shp_df$n)); breaks; length(breaks)
myPalette <- colorRampPalette((brewer.pal(8, "Greens")));# myPalette; rev
colors <- myPalette(length(breaks)); colors
title <- 'Entire system\nNumber of papers'

fig1 <- ggplot(data = shp_df) +
  geom_sf(aes(fill = n), color='gray50', size=0.01) +
  # scale_fill_manual(values = colors,
  #                   labels = labels) +
  
  # scale_fill_continuous(
  #   low="#F7FBFF", high="#084594",  guide="colorbar", na.value="gray90") +
  
  scale_fill_gradientn(
    name=title,
    colours= colors, na.value = "gray90",
    # values=c(0,0.19,0.2,0.5,0.8,0.81,1),
    limits = c(min,max),
    breaks = breaks) +
  guides(fill = guide_legend(label.hjust = 0, label = T,
                             reverse = T, title = title))+
  map_theme
fig1
dir
fname <- paste0(fig.dir, 'Fig_map_country_sys_.jpg'); fname
ggsave(fname, fig1, width = 18, height = 9, units = 'cm', limitsize = FALSE,
       bg = "transparent")

### - biodiv_countries
ctr_bio <- df99 %>%
  select(paper_id, biodiv_countries) %>%
  data.frame(., do.call(rbind, str_split(.$biodiv_countries,';'))) %>%
  gather(key = 'key', value = biodiv_countries, 3:ncol(.)) %>%
  dplyr::mutate(biodiv_countries = trimws(biodiv_countries)) %>%
  dplyr::distinct(paper_id, biodiv_countries, .keep_all = T) %>%
  filter(biodiv_countries != '') %>%
  arrange(paper_id) %>%
  group_by(biodiv_countries) %>%
  tally()%>%
  arrange(desc(n)) #%>%
# str(ctr_bio)


### join new name table
shp_df <- shp %>%
  left_join(., ctr_bio, by=c("iso_a3" = "biodiv_countries"))

# str(shp_df)
unique(shp_df$n)

### check the join
shp_df_check <- shp %>%
  right_join(., ctr_bio, by=c("iso_a3" = "biodiv_countries"))

### breaks and colors
var <- shp_df$n
max <- max(var, na.rm = T); max
min <- min(var, na.rm = T); min
hist(var)

breaks <- seq(min, max, 2); breaks; length(breaks)
breaks <- sort(unique(shp_df$n)); breaks; length(breaks)
myPalette <- colorRampPalette((brewer.pal(8, "Blues")));# myPalette; rev
colors <- myPalette(length(breaks)); colors
title <- 'Biodiversity system\nNumber of papers'

### plot
fig2 <- ggplot(data = shp_df) +
  geom_sf(aes(fill = n), color='gray50', size=0.01) +
  scale_fill_gradientn(
    name = title,
    colours= colors, na.value = "gray90",
    limits = c(min,max), breaks = breaks) +
  guides(fill = guide_legend(label.hjust = 0, label = T,
                             reverse = T, title = title))+
  map_theme
fig2
dir
fname <- paste0(fig.dir, 'Fig_map_country_bio.jpg'); fname
ggsave(fname, fig2, width = 18, height = 9, units = 'cm', limitsize = FALSE,
       bg = "transparent")

## Percentage of telecoupling

k <- as.data.frame(cbind(df99$biodiv_countries, df99$tele_cat))
colnames(k) <- c('ctr', 'tc')

ctr_ls <- ctr_bio[1]

k$ctr[k$ctr == ""] <- NA

k$tc <- as.character(k$tc)

poo <- separate_rows(k, tc, sep=";", convert = TRUE)
doo <-separate_rows(poo, ctr, sep=";", convert = TRUE)


woo <- paste0(doo$ctr, '_', doo$tc)
woot <- as.data.frame(table(woo))

ok <- str_split_fixed(woot$woo, '_', 2)
woot$ctr <- ok[,1]
woot$tc <- ok[,2]
dplyr::count(tc, name = 'n_tc')


### count by ctr and tc
booty <- doo %>%
  dplyr::select(ctr, tc) %>%
  ### how many tcs in total for each ctr?
  ungroup() %>%
  group_by(ctr) %>%
  add_tally(name = 'total_tc') %>%
  ### the number of each tc
  dplyr::ungroup() %>%
  dplyr::group_by(ctr, total_tc) %>%
  dplyr::count(tc, name = 'n_tc') %>%
  arrange(ctr) %>%
  dplyr::mutate(pct_tc = n_tc/total_tc*100)


booty_check <-  booty %>%
  dplyr::group_by(ctr) %>%
  dplyr::summarise(sum = sum(pct_tc, na.rm = T))

booty2 <- booty[order(-booty$total_tc,booty$ctr, -booty$n_tc),]

write.csv(booty2, paste0(dat.dir,"pct_tc_CLH.csv"))


