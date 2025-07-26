

# Installerer pakker for visualisering om de ikke allerede er installert
required_packages <- c("tidyverse", "ggplot2", "scales", "here", "plotly", 
                       "shiny", "DT", "shinydashboard", "viridis", "RColorBrewer")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


# Liste over Europeiske land

european_countries <- c(
  "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", 
  "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
  "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", 
  "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", 
  "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova", 
  "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", 
  "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", 
  "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", 
  "Ukraine", "United Kingdom", "Vatican City"
)

# Leser inn CSV-filen 

file_path <- NULL

if (file.exists("data/World_Energy_Consumption.csv")) {
  file_path <- "data/World_Energy_Consumption.csv"
} else if (file.exists("World_Energy_Consumption.csv")) {
  file_path <- "World_Energy_Consumption.csv"
} else if (file.exists("C:/Users/Ridwan/Downloads/World_Energy_Consumption.csv")) {
  file_path <- "C:/Users/Ridwan/Downloads/World_Energy_Consumption.csv"
} else {
  cat("Filen ble ikke funnet på vanlige steder.\n")
  cat("Velg filen manuelt:\n")
  file_path <- file.choose()
}

# Leser inn data

tryCatch({
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  cat("Data lest inn vellykket. Antall rader:", nrow(data), "\n")
}, error = function(e) {
  stop("Feil ved innlesing av data: ", e$message)
})

# Sjekker data og filtrerer for europeiske land

required_cols <- c("country", "year", "fossil_energy_per_capita", 
                   "renewables_energy_per_capita", "per_capita_electricity",
                   "renewables_share_energy", "primary_energy_consumption")

missing_cols <- required_cols[!required_cols %in% names(data)]
if (length(missing_cols) > 0) {
  stop("Manglende kolonner i datasettet: ", paste(missing_cols, collapse = ", "))
}

# Filtrerer data for europeiske land

available_european <- european_countries[european_countries %in% unique(data$country)]
cat("Europeiske land funnet i datasettet:", length(available_european), "av", length(european_countries), "\n")

data_europe <- data %>%
  filter(country %in% available_european) %>%
  filter(!is.na(year) & year >= 2000 & year <= 2022) %>%
  filter(!is.na(fossil_energy_per_capita) & !is.na(renewables_energy_per_capita)) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(country, year)

cat("Filtrerte data - Antall rader:", nrow(data_europe), "\n")
cat("Antall land inkludert:", length(unique(data_europe$country)), "\n")
cat("Tidsperiode:", min(data_europe$year), "-", max(data_europe$year), "\n")

# Opprett output-mappe
if (!dir.exists("plots_europe")) {
  dir.create("plots_europe")
}



# Funksjon for å lage regionale farger

get_region_colors <- function(countries) {
  # Definerer regioner
  nordic <- c("Norway", "Sweden", "Denmark", "Finland", "Iceland")
  western <- c("Germany", "France", "United Kingdom", "Netherlands", "Belgium", 
               "Austria", "Switzerland", "Ireland", "Luxembourg")
  southern <- c("Italy", "Spain", "Portugal", "Greece", "Malta", "Cyprus")
  eastern <- c("Poland", "Czech Republic", "Hungary", "Slovakia", "Slovenia", 
               "Croatia", "Romania", "Bulgaria", "Estonia", "Latvia", "Lithuania")
  balkans <- c("Serbia", "Bosnia and Herzegovina", "Montenegro", "Albania", 
               "North Macedonia", "Kosovo")
  
  colors <- rep("#808080", length(countries))  # Default grå
  colors[countries %in% nordic] <- "#1f77b4"     # Blå
  colors[countries %in% western] <- "#ff7f0e"    # Oransje
  colors[countries %in% southern] <- "#2ca02c"   # Grønn
  colors[countries %in% eastern] <- "#d62728"    # Rød
  colors[countries %in% balkans] <- "#9467bd"    # Lilla
  
  return(colors)
}

# funksjon for landvalg 

select_countries_interactive <- function(available_countries) {
  cat("\n=== INTERAKTIV LANDVALG ===\n")
  cat("Tilgjengelige europeiske land:\n")
  
  for (i in seq_along(available_countries)) {
    cat(sprintf("%2d. %s\n", i, available_countries[i]))
  }
  
  cat("\nVelg land ved å skrive nummer (separert med komma), eller 'all' for alle land:\n")
  cat("Eksempel: 1,5,10 eller 'all'\n")
  
  user_input <- readline("Ditt valg: ")
  
  if (tolower(trimws(user_input)) == "all") {
    return(available_countries)
  } else {
    tryCatch({
      indices <- as.numeric(unlist(strsplit(user_input, ",")))
      indices <- indices[indices > 0 & indices <= length(available_countries)]
      selected <- available_countries[indices]
      cat("Du valgte:", paste(selected, collapse = ", "), "\n")
      return(selected)
    }, error = function(e) {
      cat("Ugyldig input. Bruker alle land.\n")
      return(available_countries)
    })
  }
}

# meny for visualiseringstype 
select_visualization_type <- function() {
  cat("\n=== VELG VISUALISERINGSTYPE ===\n")
  cat("1. Linjediagram (alle land)\n")
  cat("2. Heatmap (land vs år)\n") 
  cat("3. Scatter plot (sammenligning av variabler)\n")
  cat("4. Top/Bottom N land (rangering)\n")
  cat("5. Regional sammenligning\n")
  cat("6. Alle visualiseringer\n")
  
  choice <- readline("Velg type (1-6): ")
  return(as.numeric(choice))
}

#  Visualiseringsfunksjoner

# 1. Linjediagram

create_line_plot <- function(data, selected_countries, variable, title_suffix) {
  plot_data <- data %>% filter(country %in% selected_countries)
  
  p <- ggplot(plot_data, aes_string(x = "year", y = variable, color = "country")) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 1.5, alpha = 0.7) +
    labs(title = paste(title_suffix, "(2000-2022)"),
         subtitle = paste("Valgte land:", length(selected_countries)),
         x = "År", y = title_suffix, color = "Land") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
    scale_y_continuous(labels = comma_format()) +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (length(selected_countries) <= 10) {
    p <- p + scale_color_brewer(type = "qual", palette = "Set3")
  } else {
    p <- p + scale_color_viridis_d()
  }
  
  return(p)
}

# 2. Heatmap

create_heatmap <- function(data, variable, title_suffix) {
  heatmap_data <- data %>%
    select(country, year, all_of(variable)) %>%
    filter(!is.na(.data[[variable]]))
  
  p <- ggplot(heatmap_data, aes_string(x = "year", y = "country", fill = variable)) +
    geom_tile() +
    scale_fill_viridis_c(name = title_suffix) +
    labs(title = paste("Heatmap:", title_suffix),
         x = "År", y = "Land") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  return(p)
}

# 3. Top/Bottom N land

create_ranking_plot <- function(data, variable, title_suffix, year_filter = 2020, top_n = 10) {
  ranking_data <- data %>%
    filter(year == year_filter) %>%
    filter(!is.na(.data[[variable]])) %>%
    arrange(desc(.data[[variable]])) %>%
    slice(1:top_n)
  
  p <- ggplot(ranking_data, aes_string(x = "reorder(country, -" %p% variable %p% ")", y = variable)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    labs(title = paste("Top", top_n, "land -", title_suffix, "(", year_filter, ")"),
         x = "Land", y = title_suffix) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
    scale_y_continuous(labels = comma_format())
  
  return(p)
}

# 4. Region sammenligning

create_regional_plot <- function(data, variable, title_suffix) {
  # Definer regioner(u ok dawg?)
  regional_data <- data %>%
    mutate(region = case_when(
      country %in% c("Norway", "Sweden", "Denmark", "Finland", "Iceland") ~ "Nordisk",
      country %in% c("Germany", "France", "United Kingdom", "Netherlands", "Belgium", 
                     "Austria", "Switzerland", "Ireland", "Luxembourg") ~ "Vest-Europa",
      country %in% c("Italy", "Spain", "Portugal", "Greece", "Malta", "Cyprus") ~ "Sør-Europa",
      country %in% c("Poland", "Czech Republic", "Hungary", "Slovakia", "Slovenia", 
                     "Croatia", "Romania", "Bulgaria", "Estonia", "Latvia", "Lithuania") ~ "Øst-Europa",
      TRUE ~ "Andre"
    )) %>%
    filter(region != "Andre") %>%
    group_by(region, year) %>%
    summarise(avg_value = mean(.data[[variable]], na.rm = TRUE), .groups = 'drop')
  
  p <- ggplot(regional_data, aes(x = year, y = avg_value, color = region)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_point(size = 2) +
    labs(title = paste("Regional sammenligning -", title_suffix),
         subtitle = "Gjennomsnitt per region",
         x = "År", y = paste("Gjennomsnittlig", title_suffix), color = "Region") +
    theme_minimal() +
    scale_color_brewer(type = "qual", palette = "Set2") +
    scale_y_continuous(labels = comma_format()) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "top")
  
  return(p)
}

# Hovedfunksjon for interaktiv analyse

run_interactive_analysis <- function() {
  available_countries <- sort(unique(data_europe$country))
  
  # Velg land
  selected_countries <- select_countries_interactive(available_countries)
  
  # Velg visualiseringstype
  viz_type <- select_visualization_type()
  
  # Velg variabel
  cat("\n=== VELG VARIABEL ===\n")
  variables <- list(
    "1" = list(var = "fossil_energy_per_capita", title = "Fossilt brensel per innbygger (kWh)"),
    "2" = list(var = "renewables_energy_per_capita", title = "Fornybar energi per innbygger (kWh)"),
    "3" = list(var = "per_capita_electricity", title = "Elektrisitetsforbruk per innbygger (kWh)"),
    "4" = list(var = "renewables_share_energy", title = "Andel fornybar energi (%)")
  )
  
  for (i in names(variables)) {
    cat(paste0(i, ". ", variables[[i]]$title, "\n"))
  }
  
  var_choice <- readline("Velg variabel (1-4): ")
  if (!var_choice %in% names(variables)) var_choice <- "1"
  
  selected_var <- variables[[var_choice]]$var
  selected_title <- variables[[var_choice]]$title
  
  # Generer visualiseringer basert på valg
  plots <- list()
  
  if (viz_type == 1 || viz_type == 6) {
    # Linjediagram
    p1 <- create_line_plot(data_europe, selected_countries, selected_var, selected_title)
    plots[["line"]] <- p1
    print(p1)
    ggsave(paste0("plots_europe/line_", selected_var, ".png"), p1, width = 12, height = 8, dpi = 300)
  }
  
  if (viz_type == 2 || viz_type == 6) {
    # Heatmap
    p2 <- create_heatmap(data_europe, selected_var, selected_title)
    plots[["heatmap"]] <- p2
    print(p2)
    ggsave(paste0("plots_europe/heatmap_", selected_var, ".png"), p2, width = 14, height = 10, dpi = 300)
  }
  
  if (viz_type == 4 || viz_type == 6) {
    # Top N land
    p3 <- create_ranking_plot(data_europe, selected_var, selected_title)
    plots[["ranking"]] <- p3
    print(p3)
    ggsave(paste0("plots_europe/ranking_", selected_var, ".png"), p3, width = 10, height = 8, dpi = 300)
  }
  
  if (viz_type == 5 || viz_type == 6) {
    # Region sammenligning
    p4 <- create_regional_plot(data_europe, selected_var, selected_title)
    plots[["regional"]] <- p4
    print(p4)
    ggsave(paste0("plots_europe/regional_", selected_var, ".png"), p4, width = 12, height = 8, dpi = 300)
  }
  
  if (viz_type == 3 || viz_type == 6) {
    # Scatter plot sammenligning
    latest_year_data <- data_europe %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      filter(!is.na(fossil_energy_per_capita) & !is.na(renewables_energy_per_capita))
    
    p5 <- ggplot(latest_year_data, aes(x = fossil_energy_per_capita, y = renewables_energy_per_capita)) +
      geom_point(aes(color = country), size = 3, alpha = 0.7) +
      geom_text(aes(label = country), vjust = -0.5, size = 3, check_overlap = TRUE) +
      labs(title = paste("Fossil vs. Fornybar energi per innbygger (", max(latest_year_data$year), ")"),
           x = "Fossilt brensel per innbygger (kWh)",
           y = "Fornybar energi per innbygger (kWh)") +
      theme_minimal() +
      scale_color_viridis_d() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    
    plots[["scatter"]] <- p5
    print(p5)
    ggsave("plots_europe/scatter_fossil_vs_renewable.png", p5, width = 12, height = 8, dpi = 300)
  }
  
  return(plots)
}

#  Statistisk analyse

perform_statistical_analysis <- function() {
  cat("\n=== STATISTISK ANALYSE ===\n")
  
  # Sammendrag per land (siste tilgjengelige år)
  latest_data <- data_europe %>%
    group_by(country) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    ungroup()
  
  # Top 5 per kategori
  categories <- list(
    "Høyest fossilt brensel per innbygger" = "fossil_energy_per_capita",
    "Høyest fornybar energi per innbygger" = "renewables_energy_per_capita", 
    "Høyest elektrisitetsforbruk per innbygger" = "per_capita_electricity",
    "Høyest andel fornybar energi" = "renewables_share_energy"
  )
  
  for (cat_name in names(categories)) {
    var_name <- categories[[cat_name]]
    top5 <- latest_data %>%
      filter(!is.na(.data[[var_name]])) %>%
      arrange(desc(.data[[var_name]])) %>%
      slice(1:5) %>%
      select(country, value = all_of(var_name))
    
    cat("\n", cat_name, ":\n")
    for (i in 1:nrow(top5)) {
      cat(sprintf("%d. %s: %s\n", i, top5$country[i], 
                  format(top5$value[i], big.mark = ",", digits = 1)))
    }
  }
  
  # Trendanalyse

  cat("\n=== TRENDANALYSE (korrelasjoner med år) ===\n")
  trends <- data_europe %>%
    group_by(country) %>%
    summarise(
      fossil_trend = cor(year, fossil_energy_per_capita, use = "complete.obs"),
      renewable_trend = cor(year, renewables_energy_per_capita, use = "complete.obs"),
      electricity_trend = cor(year, per_capita_electricity, use = "complete.obs"),
      renewable_share_trend = cor(year, renewables_share_energy, use = "complete.obs"),
      .groups = 'drop'
    ) %>%
    filter(!is.na(fossil_trend))
  
  # Land med sterkest økende fornybar energi

  increasing_renewable <- trends %>%
    arrange(desc(renewable_trend)) %>%
    slice(1:5)
  
  cat("Land med sterkest økning i fornybar energi:\n")
  for (i in 1:nrow(increasing_renewable)) {
    cat(sprintf("%d. %s (korrelasjon: %.3f)\n", i, 
                increasing_renewable$country[i], 
                increasing_renewable$renewable_trend[i]))
  }
  
  return(trends)
}

#  Kjør hovedanalyse 
cat("=== VELKOMMEN TIL  EUROPEISK ENERGIANALYSE ===\n")
cat("Denne applikasjonen lar deg utforske energiforbruk i europeiske land.\n")

# Kjør  analyse
plots <- run_interactive_analysis()

# Kjør statistisk analyse
trends <- perform_statistical_analysis()

# Spør om ytterligere analyser
continue_analysis <- function() {
  cat("\n=== TILLEGGSANALYSER ===\n")
  cat("1. Kjør ny analyse med andre parametere\n")
  cat("2. Generer sammendrag for alle land\n")
  cat("3. Eksporter data til CSV\n")
  cat("4. Avslutt\n")
  
  choice <- readline("Velg handling (1-4): ")
  
  if (choice == "1") {
    plots <- run_interactive_analysis()
    continue_analysis()
  } else if (choice == "2") {
    # Generer sammendrag
    summary_data <- data_europe %>%
      group_by(country) %>%
      summarise(
        avg_fossil = mean(fossil_energy_per_capita, na.rm = TRUE),
        avg_renewable = mean(renewables_energy_per_capita, na.rm = TRUE),
        avg_electricity = mean(per_capita_electricity, na.rm = TRUE),
        avg_renewable_share = mean(renewables_share_energy, na.rm = TRUE),
        years_covered = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_renewable_share))
    
    print(summary_data)
    continue_analysis()
  } else if (choice == "3") {
    # Eksporter data
    write.csv(data_europe, "europe_energy_data.csv", row.names = FALSE)
    write.csv(trends, "europe_energy_trends.csv", row.names = FALSE)
    cat("Data eksportert til 'europe_energy_data.csv' og 'europe_energy_trends.csv'\n")
    continue_analysis()
  } else {
    cat("Analyse fullført. Takk for at du brukte applikasjonen!\n")
  }
}

# Start interaktiv meny
continue_analysis()

cat("\n=== ANALYSE FULLFØRT ===\n")
cat("Alle filer er lagret i 'plots_europe' mappen.\n")
cat("Tilgjengelige visualiseringer avhenger av dine valg.\n")
