require(ggplot2)
require(chinamap)
require(sf)
m = sf::st_read("inst/市界.shp")
require(nCov2019)
x<-get_nCov2019()
map <- tibble::as_tibble(m)
d<-read.xlsx('123.xlsx','sheetIndex'=1,encoding="UTF-8")
#去掉市县后缀
setup_city <- function(city) {
  city <- as.character(city)
  city <- sub("市$", "", city)
  city <- sub("地区$","", city)
  city <- sub("省$","", city)
  city <- sub("特别行政区$","", city)
  city <- sub("土家族","", city)
  city <- sub("蒙古族","", city)
  city <- sub("哈尼族","", city)
  city <- sub("布依族","", city)
  city <- gsub(".族","", city)
  city <- sub("自治","", city)
  city[city == '神农架林区'] = '神农架'
  city[city == '甘孜州'] = '甘孜'
  city[city == '凉山州'] = '凉山'
  return(city)
}


##去掉区市县和自治区

  load(system.file("ncovEnv.rda", package="nCov2019"))
  ncovEnv <- get("ncovEnv")
  setup_city <- get("setup_city", envir = ncovEnv)

  map$NAME <- setup_city(map$NAME)
d$名称<-setup_city(d$名称)
##获取region的数据
get_city_data <- function(x, region, date) {

    stats <- x[region, ]

  names(stats)[1] <- 'NAME'
  return(stats)
}
##获取贵州的经纬数据
map2 <- do.call('rbind', lapply('贵州', function(r) {
 ##stats <- get_city_data(d, r, date)
  d$名称<-factor(d$名称)
  dd<-d
  code <- sub("(\\d{2}).*", "\\1",
              map$ADMINCODE[which(map$NAME == dd[1,1])])
              ##map$ADMINCODE[which(map$NAME == stats[1,1])])

  map[grep(paste0("^", code), map$ADMINCODE),]
}))

##上面已经获取“贵州”数据
##stats <- get_city_data(x, '贵州', date)

##根据名字组合数据和经纬度
rename(d,"名称"="NAME")
map3 <- dplyr::left_join(map2, d, by='NAME')



p <- ggplot(map3, aes_(geometry=~geometry)) +
  theme_minimal() + xlab(NULL) + ylab(NULL) +
  labs(title = '2019nCov',
       subtitle = paste('confirmed cases:'),
       caption=paste("accessed date:")) +
  coord_sf()

fill_scale_continuous <- function(palette = "Reds") {
  cols = RColorBrewer::brewer.pal(6, palette)
  breaks = c(1, 10, 100, 1000, 10000)
  scale_fill_gradient(low=cols[1], high=cols[6],
                      na.value='white', trans='log',
                      breaks=breaks, labels=breaks)
}

p <- p + geom_sf(aes_(fill=~累计确诊)) +
  fill_scale_continuous(palette = "Reds")

p <- p + geom_sf_text(aes_(label=~NAME))

p


