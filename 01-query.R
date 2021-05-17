library(ojodb)

connect_ojo()

oscn_pays <- tibble()

for (i in 2010:2020) {
  oscn_pays <- ojo_tbl(paste0("oscn_pays_", i)) %>%
    filter(court == "TULSA", pay_amt < 2000) %>%
    collect() %>%
    bind_rows(oscn_pays)

  print(paste(i))
}

oscn_fees <- tibble()

for (i in 2010:2020) {
  for (j in c('CF', 'CM')) {
    oscn_fees <- ojo_tbl(paste0("oscn_mins_", i, j)) %>%
      filter(court == "TULSA", fee_amt > 0, fee_amt < 200000) %>%
      collect() %>%
      bind_rows(oscn_fees)

    print(paste(i, j))
  }
}

ftps <- tibble()

for (i in 2010:2020) {
  for (j in c('CF', 'CM')) {
    ftps <- ojo_tbl(paste0("oscn_mins_", i, j)) %>%
      filter(court == "TULSA", min_code %in% c("BWIFP", "BWIFAP")) %>%
      collect() %>%
      bind_rows(ftps)

    print(paste(i, j))
  }
}

### Query defendant data
defs <- ojo_tbl("ojo_def_info") %>%
  filter(court == "TULSA") %>%
  collect()

defs2 <- ojo_tbl("ojo_definfo") %>%
  filter(court == "TULSA") %>%
  collect()

all_defs <- defs %>%
  bind_rows(defs2 %>%
              mutate(def_zip = as.character(def_zip),
                     file_year = as.character(file_year)))

zips <- all_defs %>%
  select(court, casenum, defname, def_zip)

def_fees <- oscn_fees %>%
  group_by(court, casenum, defname) %>%
  summarize(fees = sum(fee_amt))

def_pays <- oscn_pays %>%
  group_by(court, casenum, defname) %>%
  summarize(pays = sum(pay_amt, na.rm = TRUE))

def_ftps <- ftps %>%
  group_by(court, casenum, defname) %>%
  summarize(ftps = n())

d <- zips %>%
  left_join(def_fees) %>%
  left_join(def_pays) %>%
  left_join(def_ftps) %>%
  filter(!is.na(fees))

zip_sum <- d %>%
  group_by(def_zip) %>%
  summarize(fees = sum(fees, na.rm = TRUE),
            pays = sum(pays, na.rm = TRUE),
            ftps = sum(ftps, na.rm = TRUE)) %>%
  arrange(desc(fees)) %>%
  filter(!is.na(def_zip))

pop <- get_acs("zcta",
               variables = c(pop = "B01001_001"),
               year = 2018,
               geometry = TRUE)

black_pop <- get_acs("zcta",
                     variables = c(black_pop = "B01001B_001"),
                     year = 2018)

zip_rank <- zip_sum %>%
  left_join(pop %>%
              mutate(def_zip = str_sub(NAME, 7, 11),
                     pop = estimate) %>%
              select(def_zip, pop)) %>%
  left_join(black_pop %>%
              mutate(def_zip = str_sub(NAME, 7, 11),
                     black_pop = estimate) %>%
              select(def_zip, black_pop)) %>%
  mutate(fees_per_capita = round(fees/pop, 2),
         pays_per_capita = round(pays/pop, 2),
         black_perc = round(black_pop/pop*100, 1)) %>%
  arrange(desc(fees_per_capita)) %>%
  filter(pop >= 2000)

library(tigris)
sh <- zctas(starts_with = "74")

zip_shp <- rgdal::readOGR(dsn = "okzip", layer = "okzip", verbose = FALSE) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

pal <-  colorBin(palette = "RdBu",
                 domain = c(min(d$n_cases), max(d$n_cases)),
                 reverse = FALSE)

leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = zip_shp,
              fillColor = ~pal(shp@data$n_cases),
              weight = 1,
              opacity = .9,
              color = "black",
              fillOpacity = 0.7,
              label = paste("County:", shp@data$name),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))



ten <- d %>%
  filter(str_detect(casenum, "-2010-"), fees > 200) %>%
  group_by(casetype = str_sub(casenum, 1, 2)) %>%
  summarize(total = n(),
            has_ftp = sum(!is.na(ftps)))


sum(d$ftps, na.rm = TRUE)
mclapply(odcr_counties[64:50], function(x) odcr_scrape_open(x, "SC", year(Sys.Date())),
         mc.cores = 8)



