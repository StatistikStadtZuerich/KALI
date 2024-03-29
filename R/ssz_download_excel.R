ssz_download_excel <- function(filteredData, file, nameVote){
    
    # Data Paths
    hauptPfad <- "www/Titelblatt.xlsx"
    imagePfad <- "www/logo_stzh_stat_sw_pos_1.png"
    
    # Read Data
    data <- read_excel(hauptPfad, sheet = 1)
    
    # Manipulate Data
    # Data Sheet 1
    data <- data %>%
      mutate(
        Date = ifelse(is.na(Date), NA, paste0(format(Sys.Date(), "%d"), ".", format(Sys.Date(), "%m"), ".", format(Sys.Date(), "%Y"))),
        Titel = ifelse(is.na(Titel), NA, paste0("Resultat der Gemeinderatswahlen für Ihre Auswahl: ", nameVote))
        )
    
    selected <- list(c("T_1", "Resultat der Gemeinderatswahlen für Ihre Auswahl:", paste(nameVote), " ", " ","Quelle: Statistik Stadt Zürich, Präsidialdepartement")) %>% 
      as.data.frame()
      
    # Data Sheet 2
    # Styling
    sty <- createStyle(fgFill="#ffffff")
    styConcept <- createStyle(textDecoration=c("bold"),
                            valign = "top",
                            wrapText = TRUE)
    styDefinition <- createStyle(valign = "top",
                                 wrapText = TRUE)
    styTitle <- createStyle(fontName = "Arial Black")
    
    # Create Workbook
    wb <- createWorkbook()
    
    # Add Sheets
    addWorksheet(wb, sheetName = "Inhalt", gridLines = FALSE)
    addWorksheet(wb, sheetName = "T_1", gridLines = TRUE)
    
    # Write Table Sheet 1
    writeData(wb, sheet = 1, x = data,
                 colNames = FALSE, rowNames = FALSE,
                 startCol = 2,
                 startRow = 7,
                 withFilter = FALSE)
    
    # Write Table Sheet 2
    writeData(wb, sheet = 2, x = selected,
              colNames = FALSE, rowNames = FALSE,
              startCol = 1,
              startRow = 1,
              withFilter = FALSE)
    writeData(wb, sheet = 2, x = filteredData,
            colNames = TRUE, rowNames = FALSE,
            startCol = 1,
            startRow = 9,
            withFilter = FALSE)
    
    # Insert Logo on Sheet 1
    insertImage(wb, imagePfad, sheet = 1, startRow= 2, startCol = 2, width = 1.75 , height = 0.35)

    # Add Styling
    addStyle(wb, 1, style = sty, row = 1:19, cols = 1:6, gridExpand = TRUE)
    addStyle(wb, 1, style = styTitle, row = 14, cols = 2, gridExpand = TRUE)
    addStyle(wb, 2, style = styConcept, row = 9, cols = 1:50, gridExpand = TRUE)
    modifyBaseFont(wb, fontSize = 8, fontName = "Arial")
    
    # Set Column Width for content
    setColWidths(wb, sheet = 2, cols = "A", widths = 10)
    setColWidths(wb, sheet = 2, cols = "B", widths = 40)
    setColWidths(wb, sheet = 2, cols = "C", widths = 8)
    setColWidths(wb, sheet = 2, cols = "D", widths = 12)
    setColWidths(wb, sheet = 2, cols = "E", widths = 40)
    setColWidths(wb, sheet = 2, cols = "F", widths = 15)
    setColWidths(wb, sheet = 2, cols = "G", widths = 40)
    setColWidths(wb, sheet = 2, cols = "H", widths = 35)
    setColWidths(wb, sheet = 2, cols = "I", widths = 12)
    
    # Set Column Width for overview sheet
    setColWidths(wb, sheet = 1, cols = "A", widths = 1)
    setColWidths(wb, sheet = 1, cols = "B", widths = 4)
    setColWidths(wb, sheet = 1, cols = "D", widths = 40)
    setColWidths(wb, sheet = 1, cols = "E", widths = 10)
    
    # Save Excel
    saveWorkbook(wb, file, overwrite = TRUE) ## save to working directory
}
