if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(DBI, RSQLite, dplyr, ggplot2, 
               showtext, ggimage, ggtext, glue, magick, svglite)

sysfonts::font_add_google("Oswald", "oswald")
showtext::showtext_auto()

f1_logo_path <- file.path(getwd(), "R", "f1_logos")
db_file_path <- file.path(getwd(), "data", "f1_data.sqlite")

db_con <- DBI::dbConnect(RSQLite::SQLite(), db_file_path)

DBI::dbListTables(db_con)

query <- "
SELECT c.name AS constructor, ra.year
from results r
JOIN races ra on r.raceID = ra.raceID
JOIN constructors c on r.constructorID = c.constructorID
WHERE r.position = 1;
"

f1_data <- DBI::dbGetQuery(db_con, query)

DBI::dbDisconnect(db_con)

f1_data_for_viz <- f1_data %>%
  dplyr::filter(year < 2020) %>%
  dplyr::mutate(decade = floor(year/10) * 10) %>%
  dplyr::group_by(constructor, decade) %>%
  dplyr::summarise(wins = n(), .groups = "drop") %>%
  dplyr::group_by(decade) %>%
  dplyr::arrange(dplyr::desc(wins)) %>%
  dplyr::slice(1:3)

team_colors <- c(
  "Ferrari"     = "#DC0000",  # Ferrari red
  "Alfa Romeo"  = "#900000",  # Deep Alfa red
  "Vanwall"     = "#006400",  # British racing green
  "Lotus-Climax"= "#FFD700",  # Classic Lotus yellow
  "BRM"         = "#004225",  # Dark green with orange nose (choose green)
  "Team Lotus"  = "#009739",  # Lotus green
  "Tyrrell"     = "#0033A0",  # Blue
  "McLaren"     = "#FF8700",  # Papaya orange
  "Williams"    = "#005AFF",  # Blue
  "Renault"     = "#FFD100",  # Yellow
  "Mercedes"    = "#00D2BE",  # Petronas green
  "Red Bull"    = "#1E41FF",  # Blue
  "AlphaTauri"  = "#2B4562"   # Dark blue
)



#f1_data_for_viz$constructor %>% unique() %>% clipr::write_clip()


f1_data_for_viz$logo <- file.path(f1_logo_path, 
                                  paste0(f1_data_for_viz$constructor, ".png"))


g <- ggplot2::ggplot(f1_data_for_viz, aes(x = as.factor(decade), y = wins, 
                                     fill = constructor)) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggimage::geom_image(aes(image = logo),
                      size = 0.035,
                      by = "width",
                      nudge_y = 3,
                      position = position_dodge(width = 0.9)) +
  ggplot2::labs(x = "Decade", y = "Wins", fill = "Constructor",
                title = glue::glue("<img src='{f1_logo_path}/f1_only_sym.png' width='45'/> Constructors' Wins by Decade"),
                subtitle = "Distribution of race victories across decades"
                ) +
  ggplot2::scale_fill_manual(values = team_colors) +
  ggplot2::theme_minimal(base_size = 16, base_family = "oswald") +
  ggplot2::theme(
    axis.text = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(2)),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_markdown(lineheight = 1.5, hjust = 0.5, 
                                  face = "bold", size = rel(3)),
    plot.subtitle = element_text(hjust = 0.5, size = rel(2.5))
  ) +
  ggplot2::expand_limits(y = max(f1_data_for_viz$wins) + 10) +
  ggplot2::guides(fill = guide_legend(nrow = 1, byrow = TRUE))

  
ggplot2::ggsave("fig/f1_constructor_total_win_decade.png", 
                width = 10, height = 8, dpi = 300, units = "in",
                bg = "white")

