# These two functions were copied from shiny.semantic to allow responsiveness in UN apps.
# We specifically changed that anything below 1400px resolution is considered mobile
# so we move the sidebar panel to above a plot when possible. This change occurs
# in custom_grid_template. Then we copied sidebar_layout to simply use that
# custom_grid_template.

# copy from sidebar_layout in shiny.semantic
custom_sidebar_layout <- function(
    sidebar_panel,
    main_panel,
    mirrored = FALSE,
    min_height = "auto",
    container_style = "",
    area_styles = list(
      sidebar_panel = "",
      main_panel = ""
    )) {
  sidebar_children <- sidebar_panel$children
  main_children <- main_panel$children
  sidebar_width <- sidebar_panel$width
  main_width <- main_panel$width
  layout_areas <- c("sidebar_panel", "main_panel")
  layout_cols <- c(paste(sidebar_width, "fr", sep = ""), paste(main_width, "fr", sep = ""))

  custom_layout <- custom_grid_template(
    default = list(
      areas = rbind(layout_areas),
      cols_width = layout_cols
    ),
    mobile = list(
      areas = rbind(
        c("sidebar_panel"),
        c("main_panel")
      ),
      rows_height = c("min-content", "auto"),
      cols_width = c("1fr")
    )
  )

  container_style <- paste("\n    gap: 15px;\n    height: auto;\n    min-height: ", min_height, ";\n    ", container_style, "\n  ", sep = "")
  area_styles <- list(
    sidebar_panel = paste("\n      background-color: #f5f5f5;\n      border-radius: 5px;\n      box-shadow: 0 1px 3px rgba(0,0,0,0.4);\n      display: flex;\n      flex-direction: column;\n      min-width: 160px;\n      padding: 10px;\n      ", area_styles$sidebar_panel, "\n    ", sep = ""),
    main_panel = paste("\n      background-color: #fff;\n      border-radius: 5px;\n      box-shadow: 0 1px 3px rgba(0,0,0,0.4);\n      display: flex;\n      flex-direction: column;\n      min-width: 160px;\n      padding: 10px;\n      ", area_styles$main_panel, "\n    ", sep = "")
  )

  # Create the grid layout with additional styles for the sidebar
  shiny.semantic::grid(
    grid_template = custom_layout,
    container_style = container_style,
    area_styles = area_styles,
    sidebar_panel = sidebar_children,
    main_panel = main_children
  )
}


# Internal function from shiny.semantic which I copied to be able to change the mobile
# screen size definition to 1400 so that tablet related content is applied the change
# of moving the sidebar_panel to the top of the main_panel.
custom_grid_template <- function(default = NULL, mobile = NULL) {
  if (!("areas" %in% names(default))) {
    stop(paste(
      "grid_template() default argument must contain list with `areas` definition.",
      "See documentation for examples."
    ))
  }

  area_names <- unique(as.vector(default$areas))
  area_tags <- shiny::tagList(shiny.semantic:::list_of_area_tags(area_names))
  css_grid_template_areas <- shiny.semantic:::data_frame_to_css_grid_template_areas(default$areas)

  css_default <- shiny::tags$style(paste(
    "#{{ grid_id }} {",
    shiny.semantic:::grid_container_css(
      css_grid_template_areas, default$rows_height,
      default$cols_width
    ), "}"
  ))

  css_mobile <- NULL
  if (!is.null(mobile)) {
    if (!("areas" %in% names(mobile))) {
      stop(paste(
        "grid_template() mobile argument must contain list with `areas` definition.",
        "See documentation for examples."
      ))
    }
    css_grid_template_areas <- shiny.semantic:::data_frame_to_css_grid_template_areas(mobile$areas)

    css_mobile <- shiny::tags$style(paste(
      "@media screen and (max-width: 1400px) {",
      "#{{ grid_id }} {", shiny.semantic:::grid_container_css(
        css_grid_template_areas,
        mobile$rows_height, mobile$cols_width
      ), "}",
      "}"
    ))
  }

  template <- htmltools::renderTags(
    shiny::tagList(css_default, css_mobile, shiny::tags$div(
      id = "{{ grid_id }}",
      area_tags
    ))
  )
  return(list(template = template$html, area_names = area_names))
}

