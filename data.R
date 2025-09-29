
desezoniranje_config <- list(
  bruto_placa = list(
    category = "Bruto pla\u010da",
    base_path = "O:/DESEZONIRANJE/Bruto plača/Novo",
    year_folder_pattern = "\\d{4}",
    month_folder_pattern = "\\d{2} \\d{4}",
    file_pattern = "^BP",
    table_id = "BP",
    expected_columns = c("Skupaj", "Zasebni sektor", "Javni sektor",
                         "Sektor država", "Javne družbe"),
    column_codes = c("period_id", "SK", "ZS", "JS", "SD", "JD"),
    description = "Bruto plače v javnem in zasebnem sektorju",
    interval = "M"
  ),
  delovno_aktivni = list(
    category = "Trg dela",
    base_path = "O:/DESEZONIRANJE/Trg dela/Delovno aktivni/Vsi",
    year_folder_pattern = "Leto \\d{4}",
    month_folder_pattern = "\\d{2} \\d{4}",
    file_pattern = "^DA",
    table_id = "DA",
    expected_columns = c("DA"),
    column_codes = c("period_id", "VSI"),
    description = "Delovno aktivni - vsi",
    interval = "M"
  ),
  delovno_aktivni_brez_kmetov = list(
    category = "Trg dela",
    base_path = "O:/DESEZONIRANJE/Trg dela/Delovno aktivni/Brez kmetov",
    year_folder_pattern = "Leto \\d{4}",
    month_folder_pattern = "\\d{2} \\d{4}",
    file_pattern = "^DA brez",
    table_id = "DA",
    expected_columns = c("DA brez kmetov"),
    column_codes = c("period_id", "BK"),
    description = "Delovno aktivni - brez kmetov",
    interval = "M"
  ),
  delovno_aktivni_disagr = list(
    category = "Trg dela",
    base_path = "O:/DESEZONIRANJE/Trg dela/Delovno aktivni po podpodročjih dejavnosti/Skupaj",
    year_folder_pattern = "Leto \\d{4}",
    month_folder_pattern = "\\d{2} \\d{4}",
    file_pattern = "^DA",
    table_id = "DA",
    expected_columns = c("Predelovalne", "Gradbeništvo", "Tržne storitve",
                         "Javne storitve", "Ostale storitve"),
    column_codes = c("period_id", "PR", "GR", "TS", "JS", "OS"),
    description = "Delovno aktivni po dejavnostih",
    interval = "M"
  ),
  ilo_brezposelni = list(
    category = "Trg dela",
    base_path = "O:/DESEZONIRANJE/Trg dela/ILO/Brezposelni",
    year_folder_pattern = "Leto \\d{4}",
    month_folder_pattern = "Q\\d \\d{4}",
    file_pattern = "^ILO",
    table_id = "ILO",
    expected_columns = c("ILO brezposelni"),
    column_codes = c("period_id", "BP"),
    description = "ILO število brezposelnih",
    interval = "Q"
  ),
  ilo_stopnja = list(
      category = "Trg dela",
      base_path = "O:/DESEZONIRANJE/Trg dela/ILO/Stopnja brezposelnosti",
      year_folder_pattern = "Leto \\d{4}",
      month_folder_pattern = "Q\\d \\d{4}",
      file_pattern = "^Stopnja ILO",
      table_id = "ILO",
      expected_columns = c("St# ILO brezposelnih"),
      column_codes = c("period_id", "ST"),
      description = "ILO stopnja brezposelnih",
      interval = "Q"
    ),
  ilo_zaposleni = list(
    category = "Trg dela",
    base_path = "O:/DESEZONIRANJE/Trg dela/ILO/Zaposleni",
    year_folder_pattern = "Leto \\d{4}",
    month_folder_pattern = "Q\\d \\d{4}",
    file_pattern = "^ILO Zaposleni",
    table_id = "ILO",
    expected_columns = c("ILO zaposleni"),
    column_codes = c("period_id", "ZP"),
    description = "ILO število zaposlenih",
    interval = "Q"
  ),
  reg_brezposelni = list(
    category = "Trg dela",
    base_path = "O:/DESEZONIRANJE/Trg dela/Registrirani brezposelni/Stevilo",
    year_folder_pattern = "Leto \\d{4}",
    month_folder_pattern = "\\d{2} \\d{4}",
    file_pattern = "^Stevilo reg",
    table_id = "RB",
    expected_columns = c("Število reg# brezposelnih"),
    column_codes = c("period_id", "BP"),
    description = "Število registriranih brezposelnih",
    interval = "M"
  ),
  reg_brezposelni_stopnja = list(
    category = "Trg dela",
    base_path = "O:/DESEZONIRANJE/Trg dela/Registrirani brezposelni/Stopnje/Skupaj",
    year_folder_pattern = "Leto \\d{4}",
    month_folder_pattern = "\\d{2} \\d{4}",
    file_pattern = "^Stopnja reg",
    table_id = "RB",
    expected_columns = c("Stopnja brezposelnosti (v %)"),
    column_codes = c("period_id", "ST"),
    description = "Število registriranih brezposelnih",
    interval = "M"
  )
)

usethis::use_data(desezoniranje_config,
                  internal = FALSE,
                  overwrite = TRUE)
