# Second-Life Battery Allocation System - screening/planning tool (not a safety certification)
library(shiny); library(ggplot2)
for (f in list.files("R", full.names = TRUE)) source(f)
D <- load_all()
pages <- c("Home","Select Battery","Profile","Assessment","Find Second Lives","Results","Pathways","Reuse vs Replacement",
           "Lifecycle","Population","Comparison","Circularity","Sources")
badge <- function(res) tags$span(class = paste("badge-r", switch(res, "Potential Match"="b-pm","Further Assessment Required"="b-fa",
  "Insufficient Information"="b-ins","Recycling Assessment"="b-rec","Potential Second Life"="b-pm","Further Assessment"="b-fa","b-ins")), res)
disc <- function() div(class = "disc", DISCLAIMER)
navrow <- function(i) div(class = "nav-row",
  if (i > 1) actionButton(paste0("back_", i), "← Back") else span(),
  if (i < length(pages)) actionButton(paste0("next_", i), "Next →", class = "btn-primary") else span())
page <- function(i, ...) tabPanel(pages[i], div(class = "wrap", h2(pages[i]), ...,  navrow(i), disc()))
kpi <- function(v, l) div(class = "card kpi", tags$b(v), l)
# Visuals are BATTERY images (no vehicles). Nissan LEAF: real pack photo from Wikimedia Commons (file page lists author/licence).
# Other models: labelled schematic drawn from the verified reference values. A local file in www/images/ overrides everything.
IMG_STYLE <- "width:260px;height:150px;object-fit:cover;border-radius:10px;flex:none"
img_key <- function(m) tolower(gsub("[^A-Za-z0-9]+", "-", m))
battery_svg <- function(l1, l2, w = 260, h = 150) HTML(sprintf(paste0(
  '<svg viewBox="0 0 260 150" width="%d" height="%d" style="border-radius:10px;flex:none" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Battery schematic">',
  '<rect width="260" height="150" rx="10" fill="#e8eef4"/><rect x="36" y="34" width="172" height="62" rx="8" fill="#fff" stroke="#12263a" stroke-width="3"/>',
  '<rect x="208" y="55" width="10" height="20" rx="2" fill="#12263a"/>',
  paste0('<rect x="', 44 + 26 * 0:5, '" y="42" width="21" height="46" rx="3" fill="#1d4e6b"/>', collapse = ""),
  '<text x="130" y="120" text-anchor="middle" font-size="12" font-weight="600" fill="#12263a" font-family="Arial">%s</text>',
  '<text x="130" y="138" text-anchor="middle" font-size="11" fill="#4a5b6c" font-family="Arial">%s</text></svg>'), w, h, l1, l2))
hero_svg <- function() HTML(paste0(
  '<svg viewBox="0 0 300 170" width="300" height="170" style="border-radius:10px;flex:none" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Battery modules in a reuse cycle">',
  '<rect width="300" height="170" rx="10" fill="#e8eef4"/>',
  paste0('<rect x="', 52 + 48 * 0:3, '" y="48" width="38" height="52" rx="5" fill="#fff" stroke="#12263a" stroke-width="2.5"/><rect x="', 58 + 48 * 0:3,
         '" y="', 54 + c(18, 10, 4, 0), '" width="26" height="', 40 - c(18, 10, 4, 0), '" rx="3" fill="#1d4e6b"/>', collapse = ""),
  '<path d="M60 128 Q150 165 240 128" fill="none" stroke="#1d4e6b" stroke-width="3"/><polygon points="240,128 229,126 236,138" fill="#1d4e6b"/>',
  '<path d="M240 30 Q150 -4 60 30" fill="none" stroke="#7a8ea2" stroke-width="3"/><polygon points="60,30 71,32 64,20" fill="#7a8ea2"/></svg>'))
model_visual <- function(r) {
  key <- img_key(paste(r$manufacturer, r$vehicle_model))
  ext <- Filter(function(e) file.exists(file.path("www/images", paste0(key, ".", e))), c("jpg","jpeg","png","webp"))
  if (length(ext)) return(tags$img(src = paste0("images/", key, ".", ext[1]), style = IMG_STYLE, alt = key))
  if (r$manufacturer == "Nissan") return(div(style = "flex:none",
    tags$img(src = "https://commons.wikimedia.org/wiki/Special:FilePath/Battery-Pack-Leaf.jpg?width=520", style = IMG_STYLE, alt = "Nissan LEAF battery pack"),
    div(class = "disc", style = "margin:2px 0 0", tags$a("Photo: Wikimedia Commons (author/licence)", target = "_blank", rel = "noopener",
      href = "https://commons.wikimedia.org/wiki/File:Battery-Pack-Leaf.jpg"))))
  div(style = "flex:none", battery_svg(paste(r$manufacturer, r$vehicle_model), paste0(r$battery_variant, if (!is.na(r$nominal_voltage_v)) paste0(" | ", r$nominal_voltage_v, " V") else "")),
      div(class = "disc", style = "margin:2px 0 0", "Schematic from reference data, not a photo"))
}

ui <- navbarPage("Second-Life Battery Allocation", id = "nav", header = tags$head(includeCSS("www/styles.css")),
  tabPanel(pages[1], div(class = "wrap",
    div(class = "card hero", div(h1("Second-Life Battery Allocation System"),
      p(class = "sub", "Screening Used Batteries for Potential Second-Life Applications"),
      p("Batteries may retain useful characteristics after their original application. This tool compares the information you provide with transparent screening criteria for lower-demand applications. The output is a planning/screening result, not a safety certification."),
      actionButton("start", "Start Battery Assessment →", class = "btn-primary")),
      if (file.exists("www/images/hero.jpg")) tags$img(src = "images/hero.jpg", style = "width:300px;border-radius:10px;flex:none") else hero_svg()),
    disc())),
  page(2, div(class = "card", selectInput("model", "Reference battery (official manufacturer data)", D$ref$battery_key, width = "100%"),
    uiOutput("sel_specs"), uiOutput("model_img"))),
  page(3, div(class = "card", div(class = "hero", uiOutput("profile"), uiOutput("profile_img")))),
  page(4, div(class = "card", span(class = "src", "User-provided assessment data"),
    fluidRow(
      column(4, textInput("bid", "Battery ID *"), numericInput("age", "Age (years)", NA, min = 0), numericInput("cyc", "Cycle count", NA, min = 0)),
      column(4, numericInput("cap", "Current capacity (kWh) - same basis as rated", NA, min = 0), numericInput("soh", "State of health (%)", NA, min = 0, max = 100),
        selectInput("cond", "Physical condition", CONDITION_LEVELS, "Unknown")),
      column(4, selectInput("insp", "Inspection status", c("Not entered","Not started","In progress","Completed")),
        selectInput("dmg", "Damage status", c("Not entered","None reported","Damage reported")),
        textInput("temp", "Temperature history"), textInput("notes", "Notes"))),
    actionButton("save", "Save assessment record"), uiOutput("val"), uiOutput("health"))),
  page(5, div(class = "card", p("Compares the selected reference battery, your assessment and the project screening criteria."),
    actionButton("go", "FIND POTENTIAL SECOND LIVES", class = "btn-primary btn-lg"))),
  page(6, uiOutput("results")),
  page(7, uiOutput("pathway")),
  page(8, div(class = "card", fluidRow(
    column(3, numericInput("c1", "Reuse/repurposing cost", NA, min = 0), numericInput("c2", "Testing cost", NA, min = 0)),
    column(3, numericInput("c3", "Installation cost", NA, min = 0), numericInput("c4", "Other costs", NA, min = 0)),
    column(3, numericInput("c5", "New replacement cost", NA, min = 0))), tableOutput("cost"),
    p("Neutral arithmetic on user-entered figures; no option is recommended."))),
  page(9, div(class = "card life", lapply(c("Manufacturing","Original Application","Used Battery","Assessment","Potential Second Life",
    "Second-Life Application","End of Second Life","Recycling"), function(s) tagList(div(s), if (s != "Recycling") span("↓"))))),
  page(10, uiOutput("pop_ui")),
  page(11, div(class = "card", uiOutput("cmp_pick"), tableOutput("cmp"))),
  page(12, uiOutput("circ")),
  page(13, div(class = "card",
    p(span(class = "src", "Official manufacturer data"), " battery_reference.csv - each record carries its own source URL (see table). Blank = not stated in the source."),
    p(span(class = "src", "User assessment data"), " battery_assessment.csv - entered by the user; nothing is pre-filled."),
    p(span(class = "src", "Project screening criteria"), " application_requirements.csv - project assumptions, NOT engineering standards; edit before real use."),
    uiOutput("srctab"), tableOutput("reqtab")))
)

server <- function(input, output, session) {
  rv <- reactiveValues(res = NULL, assess = D$assess)
  ref1 <- reactive(D$ref[D$ref$battery_key == input$model, ])
  cur <- reactive(list(battery_id = input$bid, selected_model = input$model, age_years = input$age, cycle_count = input$cyc,
    current_capacity_kwh = input$cap, state_of_health_percent = input$soh, physical_condition = input$cond,
    inspection_status = input$insp, damage_status = input$dmg, temperature_history = input$temp,
    assessment_date = as.character(Sys.Date()), notes = input$notes))
  observeEvent(input$start, updateNavbarPage(session, "nav", pages[2]))
  lapply(seq_along(pages), function(i) {
    observeEvent(input[[paste0("next_", i)]], updateNavbarPage(session, "nav", pages[i + 1]))
    observeEvent(input[[paste0("back_", i)]], updateNavbarPage(session, "nav", pages[i - 1]))
  })
  specs <- function(r) tags$table(class = "table", lapply(list(Manufacturer = r$manufacturer, Model = r$vehicle_model, Variant = r$battery_variant,
    Chemistry = r$chemistry, "Rated capacity" = fmt(r$rated_capacity_kwh, "kWh"), "Capacity basis" = r$capacity_basis,
    "Nominal voltage" = fmt(r$nominal_voltage_v, "V"), Power = fmt(r$power_kw, "kW"), Source = r$battery_source), function(v) NULL) ,
    Map(function(k, v) tags$tr(tags$th(k), tags$td(v)), c("Manufacturer","Model","Variant","Chemistry","Rated capacity","Capacity basis","Nominal voltage","Power","Source"),
      list(r$manufacturer, r$vehicle_model, r$battery_variant, fmt(r$chemistry), fmt(r$rated_capacity_kwh, "kWh"), fmt(r$capacity_basis),
           fmt(r$nominal_voltage_v, "V"), fmt(r$power_kw, "kW"), r$battery_source)))
  model_img <- function() model_visual(ref1())
  output$model_img <- renderUI(model_img())
  output$profile_img <- renderUI(model_img())
  output$sel_specs <- renderUI(tagList(span(class = "src", "Official manufacturer data"), specs(ref1())))
  output$profile <- renderUI(tagList(span(class = "src", "Official manufacturer data"), specs(ref1()),
    tags$a("View Official Source", href = ref1()$source_url, target = "_blank", class = "btn btn-default"),
    div(style = "margin-top:12px", "Rated capacity", div(class = "bar", div(style = "width:100%")))))
  output$val <- renderUI({ m <- validate_assessment(cur()); if (length(m)) div(class = "bad", lapply(m, p)) })
  output$health <- renderUI({
    a <- cur(); rated <- ref1()$rated_capacity_kwh
    ret <- if (is_blank(a$current_capacity_kwh)) NA else round(a$current_capacity_kwh / rated * 100, 1)
    fields <- c(a$age_years, a$cycle_count, a$current_capacity_kwh, a$state_of_health_percent)
    comp <- round(mean(c(!is.na(fields), a$physical_condition != "Unknown", a$inspection_status != "Not entered", a$damage_status != "Not entered", !is_blank(a$temperature_history))) * 100)
    tagList(hr(), p("Rated (manufacturer): ", fmt(rated, "kWh"), " | Entered current: ", fmt(a$current_capacity_kwh, "kWh"), " | Calculated retention: ", fmt(ret, "%")),
      "Capacity retention", div(class = "bar", div(style = paste0("width:", min(100, ifelse(is.na(ret), 0, ret)), "%"))),
      "Data completeness", div(class = "bar", div(style = paste0("width:", comp, "%"))), p(comp, "% of assessment fields entered"))
  })
  observeEvent(input$save, {
    a <- cur(); m <- validate_assessment(a)
    if (is_blank(a$battery_id)) return(showNotification("Battery ID is required.", type = "error"))
    if (length(m)) return(showNotification(paste(m, collapse = " "), type = "error"))
    row <- as.data.frame(lapply(a, function(x) if (is_blank(x)) NA else x), stringsAsFactors = FALSE)
    save_assessment(row); rv$assess <- load_assessment(); showNotification("Record saved to battery_assessment.csv")
  })
  observeEvent(input$go, {
    a <- cur(); if (length(validate_assessment(a))) return(showNotification("Fix validation errors first.", type = "error"))
    rv$res <- screen_battery(a, D$req); updateNavbarPage(session, "nav", pages[6])
  })
  output$results <- renderUI({
    if (is.null(rv$res)) return(div(class = "card", "No results yet - run the screening on the previous page."))
    d <- rv$res$details; icon <- c(Met = "✓", "Not met" = "✗", Missing = "⚠"); cls <- c(Met = "ok", "Not met" = "bad", Missing = "warn")
    lapply(seq_len(nrow(rv$res$summary)), function(i) { s <- rv$res$summary[i, ]; dd <- d[d$application == s$application, ]
      div(class = "card", h4(s$application), badge(s$result),
        tags$table(class = "table", lapply(seq_len(nrow(dd)), function(j) tags$tr(tags$td(dd$parameter[j]), tags$td(class = cls[dd$status[j]], icon[dd$status[j]]), tags$td(dd$detail[j])))),
        p(strong("Reason: "), s$explanation)) })
  })
  output$pathway <- renderUI({
    if (is.null(rv$res)) return(div(class = "card", "Run the screening first."))
    s <- rv$res$summary; n <- table(factor(s$result, levels = c("Potential Match","Further Assessment Required","Recycling Assessment","Insufficient Information")))
    div(class = "card", p("Overall pathway shown: ", badge(as.character(rv$res$pathway))),
      p("Potential Match: ", n[1], " | Further Assessment Required: ", n[2], " | Recycling Assessment: ", n[3], " | Insufficient Information: ", n[4]),
      p("The overall pathway is 'Potential Second Life' if any application is a Potential Match; otherwise 'Further Assessment' if any is missing information; otherwise 'Recycling Assessment'."),
      p(strong("This screening result does not replace professional battery testing, safety evaluation, certification, or engineering assessment.")))
  })
  output$cost <- renderTable({ x <- cost_summary(input$c1, input$c2, input$c3, input$c4, input$c5)
    names(x) <- c("Reuse-related cost","Replacement cost","Difference","% difference"); x })
  pop <- reactive(population_table(rv$assess, D$ref, D$req))
  output$pop_ui <- renderUI({ if (nrow(pop()) == 0) return(div(class = "card", "No complete assessment records yet. Save a record on the Assessment page."))
    tagList(fluidRow(column(4, h4("Models"), tableOutput("f1")), column(4, h4("Condition"), tableOutput("f2")), column(4, h4("Pathway"), tableOutput("f3"))),
      plotOutput("p1", height = 260)) })
  output$f1 <- renderTable(freq_table(pop()$selected_model)); output$f2 <- renderTable(freq_table(pop()$physical_condition))
  output$f3 <- renderTable(freq_table(pop()$pathway))
  output$p1 <- renderPlot(ggplot(pop(), aes(capacity_retention_pct)) + geom_histogram(bins = 10, fill = "#1d4e6b") + theme_minimal() + labs(x = "Capacity retention (%)", y = "Batteries"))
  output$cmp_pick <- renderUI(selectInput("cmp_ids", "Select batteries (assessment records)", pop()$battery_id, multiple = TRUE))
  output$cmp <- renderTable({ req(input$cmp_ids, nrow(pop()) > 0); t(comparison_view(pop(), D$ref, input$cmp_ids)) }, rownames = TRUE, colnames = FALSE)
  output$circ <- renderUI({ p <- pop()
    tagList(kpi(nrow(p), "Batteries assessed"), kpi(sum(p$pathway == "Potential Second Life"), "Potential second-life matches"),
      kpi(sum(p$pathway == "Further Assessment"), "Further assessment"), kpi(sum(p$pathway == "Recycling Assessment"), "Recycling assessment"),
      kpi(nrow(D$ref), "Reference models"), kpi(length(unique(D$req$application)), "Application categories"),
      kpi(fmt(sum(p$current_capacity_kwh, na.rm = TRUE), "kWh"), "Capacity represented")) })
  output$srctab <- renderUI(tags$table(class = "table", tags$tr(tags$th("Battery"), tags$th("Source"), tags$th("Link")),
    lapply(seq_len(nrow(D$ref)), function(i) tags$tr(tags$td(D$ref$battery_key[i]), tags$td(D$ref$battery_source[i]),
      tags$td(tags$a("Open source document", href = D$ref$source_url[i], target = "_blank", rel = "noopener"))))))
  output$reqtab <- renderTable(unique(D$req[, c("application","source","source_type")]))
}
shinyApp(ui, server)
