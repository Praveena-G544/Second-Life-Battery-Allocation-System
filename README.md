# Second-Life Battery Allocation System

A **screening/planning tool** (R/Shiny) that compares a used battery's information with transparent project screening criteria for lower-demand second-life applications. **It is not a safety certification system** and never states that a battery is safe or should be reused.

## Run
```r
install.packages(c("shiny","ggplot2"))
shiny::runApp()   # from the project folder
```

## Structure
`app.R` (frontend: pages/UI) · `R/` (backend: data loading, calculations, matching engine, dashboard, helpers) · `data/` (three CSVs) · `www/` (CSS, JS, images)

## Three CSVs (kept separate)
| File | Nature |
|---|---|
| battery_reference.csv | Official manufacturer data only, with source URL per record. Blank = not stated in the source. |
| battery_assessment.csv | User-entered data about a specific battery. Ships empty (header only). |
| application_requirements.csv | **Project screening assumptions** (not engineering standards). Thresholds are illustrative placeholders - review and edit them. |

Reference sources: Nissan Australia LEAF spec sheet (40/62 kWh, 350 V); Hyundai Motor UK KONA Electric spec document (39.2 kWh/327 V/113 kW, 64 kWh/356 V/170 kW); Ford 2025 F-150 Lightning Technical Specifications (98/123/131 kWh usable). Nissan/Hyundai documents do not state whether capacity is gross or usable, so this is recorded as such.

## Data-frame concepts
5a `read.csv` in `data_loading.R` · 5b `table()` frequency tables in `dashboard.R` · 5c factors with custom levels (`CONDITION_LEVELS`, `PATHWAY_LEVELS`) · 5d retention, loss, cost difference, % difference · 5e drop incomplete records and unneeded columns.

## Matching methodology
Each requirement row is checked as **Met / Not met / Missing**. Per application: all Missing -> Insufficient Information; any Not met -> Recycling Assessment; any Missing -> Further Assessment Required; otherwise Potential Match. No hidden score.

## Limitations
Small reference set; visuals are battery-only: a Nissan LEAF pack photo from Wikimedia Commons (file page lists author and licence) and labelled schematics generated from the reference data for the other models; a local file in `www/images/` overrides them (hero.jpg, nissan-leaf.jpg, hyundai-kona-electric.jpg, ford-f-150-lightning.jpg; jpg/png/webp); placeholders show until added; screening criteria are assumptions; results do not replace professional testing, safety evaluation or certification.

> This system provides preliminary screening and planning information based on available battery data. It does not certify battery safety, suitability, performance, or compliance.

## Live Demo

[Click here to view the live Shiny App](https://praveena010203.shinyapps.io/Second-Life-Battery-Allocation-System/)
