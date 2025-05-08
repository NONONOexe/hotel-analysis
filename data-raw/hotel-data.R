# ホテルのデータの読み込み
kanazawa_hotels_file <- here::here("data-raw", "kanazawa-hotels.csv")
kanazawa_hotels_df <- readr::read_csv(kanazawa_hotels_file)
kanazawa_hotels <- sf::st_as_sf(
  kanazawa_hotels_df,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# 観光地のデータの読み込み
kanazawa_poi_file <- here::here("data-raw", "kanazawa-poi.csv")
kanazawa_poi_df <- readr::read_csv(kanazawa_poi_file)
kanazawa_poi <- sf::st_as_sf(
  kanazawa_poi_df,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# 経路の計算
options(osrm.server = "http://127.0.0.1:5000/")
routes_df <- NULL
for (i in seq_len(nrow(kanazawa_hotels))) {
  for (j in seq_len(nrow(kanazawa_poi))) {
    src <- kanazawa_hotels[i, ]
    dst <- kanazawa_poi[j, ]
    route <- osrm::osrmRoute(src, dst, osrm.profile = "foot")

    new_row <- tibble::tibble_row(
      hotel = src$name,
      poi = dst$name,
      distance = route$distance,
      duration = route$duration,
      geometry = route$geometry
    )
    routes_df <- if (is.null(routes_df)) {
      new_row
    } else {
      dplyr::add_row(routes_df, new_row)
    }
  }
}
routes <- sf::st_sf(routes_df)

# データの保存
readr::write_rds(kanazawa_hotels, here::here("myapp", "kanazawa_hotels.rds"))
readr::write_rds(kanazawa_poi, here::here("myapp", "kanazawa_poi.rds"))
readr::write_rds(routes, here::here("myapp", "routes.rds"))
