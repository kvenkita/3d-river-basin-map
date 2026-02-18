###################################################
#                3D river basin maps with R
#                 Milos Popovic
#                 2023/11/21
###################################################

libs <- c(
    "tidyverse", "sf", "rnaturalearth",
    "elevatr", "terra", "rayshader",
    "rayrender"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(
    libs, library,
    character.only = T
))

sf::sf_use_s2(F)

# 1. KERALA SF
#-------------

india_states <- rnaturalearth::ne_states(
    country = "India",
    returnclass = "sf"
)

region_sf <- india_states |>
    dplyr::filter(
        name_en == "Kerala"
    )

region_bbox <- sf::st_bbox(
    region_sf
)

# 2. GET RIVERS
#--------------

url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip"

if (!file.exists(basename(url))) {
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}

if (!dir.exists("HydroRIVERS_v10_as_shp")) {
    unzip(basename(url))
}

filename <- list.files(
    path = "HydroRIVERS_v10_as_shp",
    pattern = ".shp",
    full.names = T
)

bbox_wkt <- sprintf(
    "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
    region_bbox["xmin"], region_bbox["ymin"],
    region_bbox["xmin"], region_bbox["ymax"],
    region_bbox["xmax"], region_bbox["ymax"],
    region_bbox["xmax"], region_bbox["ymin"],
    region_bbox["xmin"], region_bbox["ymin"]
)

region_rivers <- sf::st_read(
    filename,
    wkt_filter = bbox_wkt,
    quiet = T
)

# 3. GET BASINS
#--------------

url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_as_lev04_v1c.zip"

if (!file.exists(basename(url))) {
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}

if (!file.exists("hybas_as_lev04_v1c.shp")) {
    unzip(basename(url))
}

region_basin <- sf::st_read(
    "hybas_as_lev04_v1c.shp",
    quiet = T
) |>
    sf::st_intersection(region_sf) |>
    dplyr::select(HYBAS_ID)

# 4. CLIP RIVERS TO BASINS
#-------------------------

region_river_basin <- sf::st_intersection(
    region_rivers,
    region_basin
)

# 5. PALETTE
#-----------

palette <- hcl.colors(
    n = length(unique(region_river_basin$HYBAS_ID)),
    palette = "Dark 3"
) |>
    sample()

names(palette) <- unique(
    region_river_basin$HYBAS_ID
)

pal <- as.data.frame(
    palette
) |>
    tibble::rownames_to_column(
        "HYBAS_ID"
    ) |>
    dplyr::mutate(
        HYBAS_ID = as.numeric(HYBAS_ID)
    )

region_river_basin_pal <- region_river_basin |>
    dplyr::left_join(
        pal,
        by = "HYBAS_ID"
    )

crs_lambert <- "+proj=laea +lat_0=10.5 +lon_0=76.3 +datum=WGS84 +units=m +no_defs"

region_basin_pal <- sf::st_transform(
    region_basin,
    crs = crs_lambert
) |>
    dplyr::inner_join(
        pal,
        by = "HYBAS_ID"
    ) |>
    dplyr::mutate(
        HYBAS_ID = as.factor(HYBAS_ID)
    )

# 6. WIDTH
#---------

region_river_width <- region_river_basin_pal |>
    dplyr::mutate(
        width = as.numeric(
            ORD_FLOW
        ),
        width = dplyr::case_when(
            width == 3 ~ 14,
            width == 4 ~ 12,
            width == 5 ~ 10,
            width == 6 ~ 8,
            width == 7 ~ 7,
            width == 8 ~ 6,
            TRUE ~ 0
        )
    ) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = crs_lambert)

# 7. DEM
#-------

elevation_raster <- elevatr::get_elev_raster(
    locations = region_sf,
    z = 9,
    clip = "locations"
) |>
    terra::rast() |>
    terra::project(crs_lambert)

elevation_matrix <- rayshader::raster_to_matrix(
    elevation_raster
)

# 8. RENDER SCENE
#----------------

h <- nrow(elevation_raster)
w <- ncol(elevation_raster)

elevation_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey60"
            )
        )(256)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            geometry = region_basin_pal,
            extent = elevation_raster,
            heightmap = elevation_matrix,
            linecolor = palette,
            palette = palette,
            data_column_fill = "HYBAS_ID"
        ),
        alphalayer = .6
    ) |>
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            geometry = region_river_width,
            extent = elevation_raster,
            heightmap = elevation_matrix,
            color = region_river_width$palette,
            linewidth = region_river_width$width,
            data_column_width = "width"
        ),
        alphalayer = 1
    ) |>
    rayshader::plot_3d(
        elevation_matrix,
        zscale = 10,
        solid = F,
        shadow = F,
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .515,
        phi = 85,
        theta = 0
    )

rayshader::render_camera(
    phi = 89,
    zoom = .675,
    theta = 0
)

# 9. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/limpopo_golf_course_4k.hdr"

if (!file.exists(basename(u))) {
    download.file(
        url = u,
        destfile = basename(u),
        mode = "wb"
    )
}

rayshader::render_highquality(
    filename = "kerala-3d-river-basins.png",
    preview = T,
    light = F,
    environment_light = basename(u),
    rotate_env = 0,
    intensity_env = .85,
    ground_material = rayrender::diffuse(
        color = "grey10"
    ),
    interactive = F,
    parallel = T,
    width = w,
    height = h
)
