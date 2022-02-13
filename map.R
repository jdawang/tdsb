library(opendatatoronto)
library(tidyverse)
library(tmap)
library(sf)
library(mapview)
library(RColorBrewer)
mapviewOptions(fgb=FALSE)

get_toronto_data <- function(id) {
  resources <- list_package_resources(id)
  datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
  data <- filter(datastore_resources, row_number() == 1) %>%
    get_resource() %>%
    rename_all(stringr::str_to_lower)
  return(data)
}

popupTable2 <- function(...) {
  pt_args <- list(...)
  pop <- do.call(leafpop::popupTable, pt_args)
  return(as.character(pop))
}

tdsb_data <- get_toronto_data("1a714b5c-64c0-4cdf-9739-0086f80fb3ee")
tdsb_data <- tdsb_data %>%
  filter(board_name == "Toronto District School Board") %>%
  mutate(
    name = str_to_title(name),
    lowercase_name = str_to_lower(name),
    lowercase_name = stringi::stri_trans_general(lowercase_name, "Latin-ASCII"),
    lowercase_name = case_when(
      objectid == 162 ~ "burnhamthorpe collegiate institute",
      objectid == 566 ~ "lambton-kingsway junior middle school",
      objectid == 565 ~ "lamberton public school",
      objectid == 852 ~ "shoreham public sports and wellness academy",
      objectid == 344 ~ "emery edvance secondary school",
      objectid == 207 ~ "charles h best junior middle school",
      objectid == 299 ~ "dublin heights elementary and middle school",
      objectid == 366 ~ "faywood arts-based curriculum school",
      objectid == 204 ~ "charles e webster public school",
      objectid == 1185 ~ "york memorial collegiate institute",
      objectid == 54 ~ "annette street junior and senior public school",
      objectid == 368 ~ "fern avenue junior and senior public school",
      objectid == 540 ~ "keele street public school",
      objectid == 668 ~ "mountview alternative school",
      objectid == 752 ~ "parkdale junior and senior public school",
      objectid == 781 ~ "queen victoria public school",
      objectid == 820 ~ "runnymede junior and senior public school",
      objectid == 1031 ~ "swansea junior and senior public school",
      objectid == 1063 ~ "thestudentschool",
      objectid == 1139 ~ "western technical-commercial school",
      objectid == 267 ~ "davisville junior public school",
      objectid == 270 ~ "deer park junior and senior public school",
      objectid == 380 ~ "forest hill junior and senior public school",
      objectid == 574 ~ "ledbury park elementary and middle school",
      objectid == 1189 ~ "yorkdale secondary school",
      objectid == 34 ~ "alexander muir/gladstone ave junior and senior public school",
      objectid == 152 ~ "brock public school",
      objectid == 181 ~ "carleton village junior and senior sports and wellness academy",
      objectid == 291 ~ "dovercourt public school",
      objectid == 410 ~ "givins/shaw junior public school",
      objectid == 506 ~ "island public/natural science school",
      objectid == 729 ~ "ossington/old orchard junior public school",
      objectid == 351 ~ "essex junior and senior public school",
      objectid == 443 ~ "hawthorne ii bilingual alternative junior school",
      objectid == 521 ~ "jesse ketchum junior and senior public school",
      objectid == 551 ~ "king edward junior and senior public school",
      objectid == 585 ~ "lord dufferin junior and senior public school",
      objectid == 586 ~ "lord lansdowne junior public school",
      objectid == 617 ~ "market lane junior and senior public school",
      objectid == 727 ~ "orde street public school",
      objectid == 871 ~ "sprucecourt public school",
      objectid == 1167 ~ "winchester junior and senior public school",
      objectid == 200 ~ "central toronto academy",
      objectid == 1023 ~ "subway academy ii",
      objectid == 700 ~ "northlea elementary and middle school",
      objectid == 1026 ~ "sunny view junior and senior public school",
      objectid == 69 ~ "avondale elementary alternative school",
      objectid == 788 ~ "r j lang elementary and middle school",
      objectid == 70 ~ "avondale secondary alternative school",
      objectid == 697 ~ "north west year round alternative centre",
      objectid == 768 ~ "pleasant view middle school",
      objectid == 1175 ~ "woodbine middle school",
      objectid == 1194 ~ "zion heights middle school",
      objectid == 794 ~ "rene gordon health and wellness academy",
      objectid == 160 ~ "bruce public school",
      objectid == 301 ~ "duke of connaught junior and senior public school",
      objectid == 1154 ~ "wilkinson junior public school",
      objectid == 256 ~ "danforth collegiate and technical institute",
      objectid == 834 ~ "school of life experience",
      objectid == 1022 ~ "subway academy i",
      objectid == 138 ~ "bowmore road junior and senior public school",
      objectid == 304 ~ "earl beatty junior and senior public school",
      objectid == 288 ~ "donwood park public school",
      objectid == 338 ~ "ellesmere-statton public school",
      objectid == 529 ~ "john mccrae public school",
      objectid == 1041 ~ "terraview-willowfield public school",
      objectid == 47 ~ "alternative scarborough education 1",
      objectid == 362 ~ "fairmount public school",
      objectid == 829 ~ "satec @ wa porter collegiate institute",
      objectid == 831 ~ "scarborough centre for alternative studies",
      objectid == 187 ~ "cedarbrook public school",
      objectid == 313 ~ "eastview public school",
      objectid == 19 ~ "agincourt junior public school",
      objectid == 21 ~ "agnes macphail public school",
      objectid == 1150 ~ "white haven public school",
      objectid == 534 ~ "joseph brant public school",
      objectid == 1158 ~ "william g miller public school",
      TRUE ~ lowercase_name
    )
  )
ward_data <- get_toronto_data("5e7a8234-f805-43ac-820f-03d7c360b588")
tdsb_enrolment_data <- readxl::read_xlsx(
  "data/012. P20200813 School Data v8 For LTPAS JD.xlsx", skip=3
) %>%
  rename_all(
      function(x) {
        str_to_lower(x) %>%
          str_replace_all(fixed(" "), "_") %>%
          str_trim()
      }
  ) %>%
  rename(projected_2020 = `projected_oct._2020_(1_yr)`) %>%
  select(
    school_name,
    ward,
    panel,
    grade_range,
    contains("capacity"),
    contains("head_count"),
    contains("projected"),
    contains("utilization"),
    contains("surplus_seats")
  ) %>%
  filter(!is.na(surplus_seats_2020)) %>%
  mutate(
    lowercase_name = stringi::stri_trans_general(
      str_to_lower(school_name), "Latin-ASCII"
    ),
    lowercase_name = ifelse(
      str_detect(lowercase_name, fixed("york memorial collegiate institute")),
      "york memorial collegiate institute",
      lowercase_name
    ),
    lowercase_name = str_trim(lowercase_name)
  )

tdsb_joined <- tdsb_enrolment_data %>%
  inner_join(tdsb_data, by="lowercase_name") %>%
  st_as_sf() %>%
  mutate(
    utilization_2020_capped=ifelse(utilization_2020 > 1, 1, utilization_2020),
    utilization_2020_f=paste0(round(100*utilization_2020), "%")
  )

tdsb_data_unjoined <- tdsb_enrolment_data %>%
  filter(!(school_name %in% tdsb_joined$school_name)) %>%
  select(lowercase_name)

zoning_data <- read_sf(
  "data/2019-zoning-by-law-569-2013-wgs84/ZONING_ZONE_CATAGORIES_WGS84.shp"
) %>%
  rename_all(str_to_lower) %>%
  filter(
    st_is_valid(geometry),
    !st_is_empty(geometry),
    !is.na(zn_zone),
    str_detect(zn_zone, fixed("R"))
  )

census_data <- read_rds("data/census_pop.rds") %>%
  rename_all(str_to_lower) %>%
  mutate(
    change = population_ca21 - population_ca16,
    pct_change = change / population_ca16,
    pct_change_trimmed = case_when(
      pct_change > 0.15 ~ 0.15,
      pct_change < -0.08 ~ -0.08,
      TRUE ~ pct_change
    ) * 100
  ) %>%
  filter(!is.na(pct_change)) %>%
  mutate(percent_change = paste0(round(100*pct_change, 1), "%"))

subway_data <- read_sf("data/ttc-subway-shapefile-wgs84/TTC_SUBWAY_LINES_WGS84.shp") %>%
  rename_all(str_to_lower) %>%
  mutate()

mv <- mapview(
  ward_data,
  label=c("area_desc"),
  alpha.regions=0,
  popup=FALSE,
  legend=FALSE,
  layer.name="Ward boundaries"
) +
  mapview(
    tdsb_joined,
    cex="surplus_seats_2020",
    label="school_name",
    zcol="utilization_2020_capped",
    layer.name="School utilization",
    col.regions=hcl.colors(473,"mako"),
    popup=popupTable2(
      tdsb_joined,
      zcol=c(
        "school_name",
        "grade_range",
        "ward",
        "revised_capacity_2020",
        "total_head_count",
        "surplus_seats_2020",
        "utilization_2020_f"
      ),
      feature.id=FALSE,
      row.numbers=FALSE
    )
  ) +
  mapview(
    census_data,
    zcol="pct_change_trimmed",
    col.regions=brewer.pal(11, "PiYG"),
    alpha=0.4,
    label="percent_change",
    layer.name="% pop. change, 2016 to 2021",
    popup=popupTable2(
      census_data,
      zcol=c("population_ca16", "population_ca21", "change", "percent_change"),
      row.numbers=FALSE,
      feature.id=FALSE
    )
  ) +
  mapview(
    zoning_data,
    zcol="zn_zone",
    label="zn_zone",
    col.regions=brewer.pal(10, "Paired"),
    popup=popupTable2(
      zoning_data,
      zcol=c("zn_zone", "density"),
      row.numbers=FALSE,
      feature.id=FALSE
    ),
    layer.name="Zoning"
  )
mapshot(removeMapJunk(mv@map, junk="homeButton"), url="index")
