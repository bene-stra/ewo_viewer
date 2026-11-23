library(shiny)
library(data.table)
library(ggplot2)
library(DT)

id_spalten <- c("staatsangehoerigkeit_1_id", "nationalitaet_id",
                "migration_id", "geschlecht_id")

## ── Daten einlesen ─────────────────────────────────────────────────────────
## Hier die Datenbank-Abfragen einfügen. Erwartet werden:
##   daten       : data.table mit abzug_dat, alter_jahr, personen und *_id
##   <spalte>_st : Schlüsseltabellen, z. B. geschlecht_st mit
##                 geschlecht_id und geschlecht (Beschriftung)
## Beispielwerte stellen die Funktion der App sicher.
daten <- data.table(
  abzug_dat = rep(as.Date(c("2023-12-31", "2024-03-31", "2024-06-30",
                            "2024-09-30", "2024-12-31")), each = 8),
  staatsangehoerigkeit_1_id = rep(rep(c(1, 2), each = 4), 5),
  nationalitaet_id = rep(rep(c(10, 11), each = 4), 5),
  migration_id = rep(c(1, 2), 20),
  geschlecht_id = rep(rep(c(1, 2, 3, 1), 2), 5),
  alter_jahr = c(25, 34, 41, 19, 55, 62, 30, 47, 38, 22, 29, 51, 33, 44, 27, 58,
                 21, 36, 49, 63, 28, 40, 31, 52, 26, 37, 43, 24, 59, 46, 35, 20,
                 39, 54, 32, 48, 23, 61, 42, 30),
  personen = c(3, 5, 2, 7, 4, 1, 6, 3, 8, 2, 4, 5, 6, 3, 7, 2, 5, 4, 3, 8, 2, 6,
               4, 3, 7, 5, 2, 4, 6, 3, 5, 8, 4, 2, 3, 7, 5, 6, 2, 4)
)

staatsangehoerigkeit_1_st <- data.table(
  staatsangehoerigkeit_1_id = 1:2,
  staatsangehoerigkeit_1 = c("Deutsche", "Ausländer")
)

nationalitaet_st <- data.table(
  nationalitaet_id = 10:11,
  nationalitaet = c("Deutsche", "Ausländer")
)

migration_st <- data.table(
  migration_id = 1:2,
  migration = c("mit Migrationshintergrund", "ohne Migrationshintergrund")
)

geschlecht_st <- data.table(
  geschlecht_id = 1:3,
  geschlecht = c("männlich", "weiblich", "divers")
)
## ── Ende Daten einlesen ────────────────────────────────────────────────────

label_spalten <- sub("_id$", "", id_spalten)

for (sp in id_spalten) {
  zn <- sub("_id$", "", sp)
  st <- get(paste0(zn, "_st"))
  daten[st, (zn) := get(zn), on = sp]
}
daten[, (id_spalten) := NULL]

ui <- fluidPage(
  titlePanel("Abzüge – Dashboard"),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "zeitraum", "Zeitraum (abzug_dat)",
        start = min(daten$abzug_dat),
        end = max(daten$abzug_dat),
        min = min(daten$abzug_dat),
        max = max(daten$abzug_dat),
        format = "dd.mm.yyyy",
        language = "de"
      ),
      selectizeInput(
        "spalten", "Differenzierung nach Spalten",
        choices = label_spalten,
        multiple = TRUE,
        selected = NULL,
        options = list(
          placeholder = "keine Auswahl = nur Gesamtsumme",
          plugins = list("remove_button")
        )
      ),
      selectizeInput(
        "plot_spalten", "Linien im Diagramm",
        choices = NULL,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      width = 3
    ),

    mainPanel(
      fluidRow(
        column(6, DTOutput("stichtag_tab")),
        column(6, plotOutput("zeit_plot", height = "500px"))
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {

  stichtag_tabelle <- reactive({
    dt <- daten[abzug_dat >= input$zeitraum[1] & abzug_dat <= input$zeitraum[2]]
    sel <- input$spalten

    if (length(sel) == 0) {
      agg <- dt[, .(spaltenname = "Insgesamt", personen = sum(personen)),
                by = .(Stichtag = abzug_dat)]
    } else if (length(sel) == 1) {
      agg <- dt[, .(personen = sum(personen)),
                by = .(Stichtag = abzug_dat, spaltenname = get(sel))]
      ord <- agg[, .(t = sum(personen)), by = spaltenname][order(-t)]$spaltenname
      agg[, spaltenname := factor(spaltenname, levels = ord)]
    } else {
      aussen <- sel[-length(sel)]
      innen <- sel[length(sel)]
      dt[, gruppe := Reduce(paste, mget(aussen))]
      dt[, kombi := paste(gruppe, get(innen))]
      tot <- dt[, .(personen = sum(personen)),
                by = .(Stichtag = abzug_dat, spaltenname = paste(gruppe, "insgesamt"))]
      det <- dt[, .(personen = sum(personen)),
                by = .(Stichtag = abzug_dat, spaltenname = kombi)]
      agg <- rbind(tot, det)
      gruppen <- dt[, .(t = sum(personen)), by = gruppe][order(-t)]$gruppe
      stufen <- unlist(lapply(gruppen, function(g) {
        innen_ord <- dt[gruppe == g, .(t = sum(personen)),
                        by = c(innen)][order(-t)]
        c(paste(g, "insgesamt"), paste(g, innen_ord[[innen]]))
      }))
      agg[, spaltenname := factor(spaltenname, levels = stufen)]
    }

    tab <- dcast(agg, Stichtag ~ spaltenname, value.var = "personen", fill = 0)
    setorderv(tab, "Stichtag", -1)
    tab
  })

  observe({
    tab <- stichtag_tabelle()
    spalten <- setdiff(names(tab), "Stichtag")
    aktuell <- intersect(input$plot_spalten, spalten)
    if (length(aktuell) == 0) {
      aktuell <- grep("insgesamt$", spalten, value = TRUE)
    }
    if (length(aktuell) == 0) {
      aktuell <- spalten
    }
    updateSelectizeInput(session, "plot_spalten",
                         choices = spalten, selected = aktuell,
                         server = TRUE)
  })

  output$stichtag_tab <- renderDT({
    tab <- copy(stichtag_tabelle())
    req(nrow(tab) > 0)
    tab[, Stichtag := format(Stichtag, "%d.%m.%Y")]
    datatable(
      tab,
      rownames = FALSE,
      options = list(pageLength = 15, dom = "t")
    )
  })

  output$zeit_plot <- renderPlot({
    tab <- stichtag_tabelle()
    req(nrow(tab) > 0)
    spalten <- intersect(input$plot_spalten, setdiff(names(tab), "Stichtag"))
    req(length(spalten) > 0)
    lang <- melt(tab, id.vars = "Stichtag", measure.vars = spalten,
                 variable.name = "Reihe", variable.factor = FALSE)
    lang <- lang[value > 0]
    ggplot(lang, aes(Stichtag, value, color = Reihe, group = Reihe)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_date(date_labels = "%d.%m.%Y") +
      labs(x = "Stichtag", y = "Personen", color = NULL) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
