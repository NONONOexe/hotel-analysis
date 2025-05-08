library(munsell)

kanazawa_hotels <- readr::read_rds("kanazawa_hotels.rds")
kanazawa_poi <- readr::read_rds("kanazawa_poi.rds")
routes <- readr::read_rds("routes.rds")

ui <- shiny::fluidPage(
  mapgl::story_maplibre(
    map_id = "map",
    sections = list(
      "intro" = mapgl::story_section(
        title = "金沢駅周辺のホテル",
        content = list(
          shiny::selectInput(
            "hotel",
            "ホテルを選択",
            choices = sort(unique(kanazawa_hotels$name))
          ),
          shiny::p("スクロールして、情報を表示")
        )
      ),

      "hotel" = mapgl::story_section(
        title = NULL,
        content = list(
          shiny::uiOutput("hotel_text"),
          shiny::div(
            shiny::p("平日料金"),
            shiny::plotOutput("hotel_weekday", height = 200),
            style = "margin-bottom: 20px;"
          ),
          shiny::div(
            shiny::p("休日料金"),
            shiny::plotOutput("hotel_weekend", height = 200),
            style = "margin-bottom: 50px;"
          ),
          shiny::div(
            shiny::p("観光地までの移動（徒歩）"),
            shiny::tableOutput("distances")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$map <- mapgl::renderMaplibre({
    mapgl::maplibre(
      bounds = kanazawa_hotels,
      scrollZoom = FALSE
    ) |>
      mapgl::add_line_layer(
        id = "route-layer",
        source = routes,
        line_width = 4,
        line_color = "#87e4f4",
        visibility = "none"
      ) |>
      mapgl::add_circle_layer(
        id = "hotel-layer",
        source = kanazawa_hotels,
        circle_color = "#87e4f4",
        circle_radius = 5,
        circle_stroke_color = "#33bce3",
        circle_stroke_width = 5,
        circle_opacity = 0.8,
        tooltip = "name",
        hover_options = list(
          circle_radius = 8
        )
      ) |>
      mapgl::add_circle_layer(
        id = "poi-layer",
        source = kanazawa_poi,
        circle_color = "#9be5b1",
        circle_radius = 5,
        circle_stroke_color = "#40cb5d",
        circle_stroke_width = 5,
        circle_opacity = 0.8,
        tooltip = "name",
        hover_options = list(
          circle_radius = 8
        )
      ) |>
      mapgl::add_fullscreen_control() |>
      mapgl::add_navigation_control()
  })

  output$hotel_text <- shiny::renderUI({
    selected_hotel <- kanazawa_hotels[kanazawa_hotels$name == input$hotel,]
    weekday_rate <- format(selected_hotel$weekday, big.mark = ",", scientific = FALSE)
    weekend_rate <- format(selected_hotel$weekend, big.mark = ",", scientific = FALSE)
    hotel_rate <- stringr::str_glue("￥{weekday_rate}（平日） ￥{weekend_rate}（休日）")

    list(
      shiny::h2(input$hotel),
      shiny::h4(hotel_rate)
    )
  })

  output$hotel_weekday <- shiny::renderPlot({
    ggplot2::ggplot(
      kanazawa_hotels,
      ggplot2::aes(x = weekday, fill = name == input$hotel)
    ) +
      ggplot2::geom_dotplot(method = "histodot", binwidth = 1000, color = "white") +
      ggplot2::scale_fill_manual(values = c("#a0a0a0", "#87e4f4")) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(),
        axis.line.x = ggplot2::element_line(),
        axis.ticks.x = ggplot2::element_line(),
        axis.ticks.length = ggplot2::unit(3, "mm")
      )
  })

  output$hotel_weekend <- shiny::renderPlot({
    ggplot2::ggplot(
      kanazawa_hotels,
      ggplot2::aes(x = weekend, fill = name == input$hotel)
    ) +
      ggplot2::geom_dotplot(method = "histodot", binwidth = 1000, color = "white") +
      ggplot2::scale_fill_manual(values = c("#a0a0a0", "#87e4f4")) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(),
        axis.line.x = ggplot2::element_line(),
        axis.ticks.x = ggplot2::element_line(),
        axis.ticks.length = ggplot2::unit(3, "mm")
      )
  })

  output$distances <- shiny::renderTable({
    routes |>
      sf::st_drop_geometry() |>
      dplyr::filter(hotel == "金沢マンテンホテル駅前") |>
      dplyr::select(
        `場所` = poi,
        `距離（km）` = distance,
        `移動時間（分）` = duration
      )
  })

  mapgl::on_section("map", "intro", {
    mapgl::maplibre_proxy("map") |>
      mapgl::set_filter("hotel-layer", NULL) |>
      mapgl::set_layout_property("route-layer", "visibility", "none") |>
      mapgl::fit_bounds(kanazawa_hotels, animate = TRUE)
  })

  mapgl::on_section("map", "hotel", {
    mapgl::maplibre_proxy("map") |>
      mapgl::set_layout_property("route-layer", "visibility", "visible") |>
      mapgl::set_filter("route-layer", filter = list("==", "hotel", input$hotel)) |>
      mapgl::set_filter("hotel-layer", filter = list("==", "name", input$hotel)) |>
      mapgl::fit_bounds(dplyr::bind_rows(
        kanazawa_hotels,
        kanazawa_poi,
        routes
      ), animate = TRUE)
  })
}

shiny::shinyApp(ui, server)
