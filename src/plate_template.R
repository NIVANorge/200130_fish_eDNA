require(here)
require(magrittr)
require(tidyverse)



layouts <- list.files(path = here("plate_layouts"), pattern = "layout.xlsx")


makelong <- function(file) {
  df <- readxl::read_xlsx(here("plate_layouts", file), col_types = "text")
  plt_layout_long <- pivot_longer(df, cols = -Row, names_to = "Column", values_to = "*Sample Name") %>%
    dplyr::filter(!is.na(`*Sample Name`)) %>%
    mutate(`*Target Name` = NA) %>%
    dplyr::select(Row, Column,`*Target Name`,`*Sample Name`)

  prefix <- sub("_layout.*", "", file)
  write_csv(plt_layout_long, file = here("plate_layouts", paste0(prefix, "_template.csv")))

}

map(layouts, makelong)



