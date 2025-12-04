# app.R — REDs CAT2 static classifier (client-only, Shinylive-ready)
# Host on GitHub Pages via shinylive::export()

library(shiny)

# =====================
# 1) Mapping table (from REDs-Scoring-ToolV2.xlsx → combination sheet)
#    Sex-specific colour by counts of PRIMARY and SECONDARY criteria.
#    Colours are lowercase for CSS classes.
# =====================

mapping <- do.call(rbind, list(
  data.frame(sex="Female", primary=0, secondary=0, colour="green"),
  data.frame(sex="Female", primary=0, secondary=1, colour="green"),
  data.frame(sex="Female", primary=0, secondary=2, colour="yellow"),
  data.frame(sex="Female", primary=0, secondary=3, colour="yellow"),
  data.frame(sex="Female", primary=0, secondary=4, colour="yellow"),
  data.frame(sex="Male", primary=0, secondary=0, colour="green"),
  data.frame(sex="Male", primary=0, secondary=1, colour="green"),
  data.frame(sex="Male", primary=0, secondary=2, colour="yellow"),
  data.frame(sex="Male", primary=0, secondary=3, colour="yellow"),
  data.frame(sex="Female", primary=1, secondary=0, colour="yellow"),
  data.frame(sex="Female", primary=1, secondary=1, colour="yellow"),
  data.frame(sex="Female", primary=1, secondary=2, colour="yellow"),
  data.frame(sex="Female", primary=1, secondary=3, colour="orange"),
  data.frame(sex="Female", primary=1, secondary=4, colour="orange"),
  data.frame(sex="Male", primary=1, secondary=0, colour="yellow"),
  data.frame(sex="Male", primary=1, secondary=1, colour="yellow"),
  data.frame(sex="Male", primary=1, secondary=2, colour="orange"),
  data.frame(sex="Male", primary=1, secondary=3, colour="orange"),
  data.frame(sex="Female", primary=2, secondary=0, colour="yellow"),
  data.frame(sex="Female", primary=2, secondary=1, colour="yellow"),
  data.frame(sex="Female", primary=2, secondary=2, colour="orange"),
  data.frame(sex="Female", primary=2, secondary=3, colour="orange"),
  data.frame(sex="Female", primary=2, secondary=4, colour="red"),
  data.frame(sex="Male", primary=2, secondary=0, colour="yellow"),
  data.frame(sex="Male", primary=2, secondary=1, colour="orange"),
  data.frame(sex="Male", primary=2, secondary=2, colour="orange"),
  data.frame(sex="Male", primary=2, secondary=3, colour="red"),
  data.frame(sex="Female", primary=3, secondary=0, colour="orange"),
  data.frame(sex="Female", primary=3, secondary=1, colour="orange"),
  data.frame(sex="Female", primary=3, secondary=2, colour="orange"),
  data.frame(sex="Female", primary=3, secondary=3, colour="red"),
  data.frame(sex="Female", primary=3, secondary=4, colour="red"),
  data.frame(sex="Male", primary=3, secondary=0, colour="orange"),
  data.frame(sex="Male", primary=3, secondary=1, colour="orange"),
  data.frame(sex="Male", primary=3, secondary=2, colour="red"),
  data.frame(sex="Male", primary=3, secondary=3, colour="red"),
  data.frame(sex="Female", primary=4, secondary=0, colour="orange"),
  data.frame(sex="Female", primary=4, secondary=1, colour="orange"),
  data.frame(sex="Female", primary=4, secondary=2, colour="red"),
  data.frame(sex="Female", primary=4, secondary=3, colour="red"),
  data.frame(sex="Female", primary=4, secondary=4, colour="red"),
  data.frame(sex="Male", primary=4, secondary=0, colour="orange"),
  data.frame(sex="Male", primary=4, secondary=1, colour="red"),
  data.frame(sex="Male", primary=4, secondary=2, colour="red"),
  data.frame(sex="Male", primary=4, secondary=3, colour="red"),
  data.frame(sex="Female", primary=5, secondary=0, colour="orange"),
  data.frame(sex="Female", primary=5, secondary=1, colour="red"),
  data.frame(sex="Female", primary=5, secondary=2, colour="red"),
  data.frame(sex="Female", primary=5, secondary=3, colour="red"),
  data.frame(sex="Female", primary=5, secondary=4, colour="red"),
  data.frame(sex="Male", primary=5, secondary=0, colour="red"),
  data.frame(sex="Male", primary=5, secondary=1, colour="red"),
  data.frame(sex="Male", primary=5, secondary=2, colour="red"),
  data.frame(sex="Male", primary=5, secondary=3, colour="red"),
  data.frame(sex="Female", primary=6, secondary=0, colour="red"),
  data.frame(sex="Female", primary=6, secondary=1, colour="red"),
  data.frame(sex="Female", primary=6, secondary=2, colour="red"),
  data.frame(sex="Female", primary=6, secondary=3, colour="red"),
  data.frame(sex="Female", primary=6, secondary=4, colour="red"),
  data.frame(sex="Male", primary=6, secondary=0, colour="red"),
  data.frame(sex="Male", primary=6, secondary=1, colour="red"),
  data.frame(sex="Male", primary=6, secondary=2, colour="red"),
  data.frame(sex="Male", primary=6, secondary=3, colour="red"),
  data.frame(sex="Female", primary=7, secondary=0, colour="red"),
  data.frame(sex="Female", primary=7, secondary=1, colour="red"),
  data.frame(sex="Female", primary=7, secondary=2, colour="red"),
  data.frame(sex="Female", primary=7, secondary=3, colour="red"),
  data.frame(sex="Female", primary=7, secondary=4, colour="red"),
  data.frame(sex="Male", primary=7, secondary=0, colour="red"),
  data.frame(sex="Male", primary=7, secondary=1, colour="red"),
  data.frame(sex="Male", primary=7, secondary=2, colour="red"),
  data.frame(sex="Male", primary=7, secondary=3, colour="red")
))
row.names(mapping) <- NULL

# =====================
# 2) QUESTION DEFINITIONS (placeholders)
#    Replace labels with your actual CAT2 item texts.
#    There are 7 PRIMARY questions for both sexes.
#    SECONDARY has 4 for Female, 3 for Male. Total: F=11, M=10.
# =====================

primary_labels <- list (
  Female = c("Primary amenorrhea (Females: primary amenorrhea is indicated when there has been a failure to menstruate by age 15 in the presence of normal secondary sexual development (two SD above the mean of 13 years), or within 5 years after breast development if that occurs before age 10; or prolonged secondary amenorrhea (absence of 12 or more consecutive menstrual cycles) due to FHA", 
             "Secondary amenorrhea (Females: absence of 3–11 consecutive menstrual cycles) caused by FHA", 
             "Subclinically or clinically low total or free T3 (within or below the lowest 25% (quartile) of the reference range)", 
             "History of ≥1 high-risk (femoral neck, sacrum, pelvis) or ≥2 low-risk BSI (all other BSI locations) within the previous 2 years or absence of ≥6 months from training due to BSI in the previous 2 years",
             "Pre-menopausal females and males <50 years old: BMD Z-score* <-1 at the lumbar spine, total hip, or femoral neck or decrease in BMD Z-score from prior testing
Children/adolescents: BMD Z-score* <−1 at the lumbar spine or TBLH or decrease in BMD Z-score from prior testing (can occur from bone loss or inadequate bone accrual).", 
             "A negative deviation of a paediatric or adolescent athlete’s previous growth trajectory (height and/or weight)", 
             "An elevated score for the EDE-Q global (>2.30 in females; >1.68 in males) and/or clinically diagnosed DSM-5- TR- defined Eating Disorder (only 1 primary indicator for either or both outcomes)"),
  Male = c("Clinically low free or total testosterone (Males: below the reference range)", 
           "Subclinically low total or free testosterone (Males: within the lowest 25% (quartile) of the reference range)", 
           "Subclinically or clinically low total or free T3 (within or below the lowest 25% (quartile) of the reference range)", 
           "History of ≥1 high-risk (femoral neck, sacrum, pelvis) or ≥2 low-risk BSI (all other BSI locations) within the previous 2 years or absence of ≥6 months from training due to BSI in the previous 2 years",
           "Pre-menopausal females and males <50 years old: BMD Z-score* <-1 at the lumbar spine, total hip, or femoral neck or decrease in BMD Z-score from prior testing
Children/adolescents: BMD Z-score* <−1 at the lumbar spine or TBLH or decrease in BMD Z-score from prior testing (can occur from bone loss or inadequate bone accrual).", 
           "A negative deviation of a paediatric or adolescent athlete’s previous growth trajectory (height and/or weight)", 
           "An elevated score for the EDE-Q global (>2.30 in females; >1.68 in males) and/or clinically diagnosed DSM-5- TR- defined Eating Disorder (only 1 primary indicator for either or both outcomes)")
)


secondary_labels <- list(
  Female = c("Elevated total or LDL cholesterol (above reference range)", 
             "History of 1 low-risk BSI (see high vs low-risk definition above) within the previous 2 years and absence of <6 months from training due to BSI in the previous 2 years", 
             "Clinically diagnosed depression and/or anxiety (only 1 secondary indicator for either or both outcomes)",
             "Oligomenorrhea caused by FHA (>35 days between periods for a maximum of 8 periods/year)"),
  Male   = c("Elevated total or LDL cholesterol (above reference range)", 
             "History of 1 low-risk BSI (see high vs low-risk definition above) within the previous 2 years and absence of <6 months from training due to BSI in the previous 2 years", 
             "Clinically diagnosed depression and/or anxiety (only 1 secondary indicator for either or both outcomes)")
)

yesno <- c("No"=0, "Yes"=1)
mkYN <- function(id, label) radioButtons(id, label, choices=yesno, inline=TRUE, selected=0)

# =====================
# 3) UI
# =====================
ui <- fluidPage(
  tags$head(tags$style(HTML('
    .pill {display:inline-block; padding:10px 16px; border-radius:999px; color:#fff; font-weight:700;}
    .green {background:#22c55e;} .yellow{background:#eab308;} .orange{background:#f97316;} .red{background:#ef4444;}
    .muted{color:#555;} .card{border:1px solid #e5e7eb; border-radius:12px; padding:16px; margin-bottom:12px;}
  '))),
  titlePanel("IOC REDs CAT2 Severity/Risk Stratification Calculator"),
  tags$div(style="font-size:10px; margin-top:-10px; margin-bottom:15px;",
           HTML("Calculator based on: Stellingwerff, T., M. Mountjoy, W. T. McCluskey, K. E. Ackerman, E. Verhagen and I. A. Heikura (2023). \"Review of the scientific rationale, development and validation of the International Olympic Committee Relative Energy Deficiency in Sport Clinical Assessment Tool: V.2 (IOC REDs CAT2) — by a subgroup of the IOC consensus on REDs.\" <i>Br J Sports Med</i> 57(17): 1109–1118.<br>
<a href='https://bjsm.bmj.com/content/57/17/1109' target='_blank'>https://bjsm.bmj.com/content/57/17/1109</a>")
  ),
  
  fluidRow(
    column(3,
           div(class="card",
               shinyWidgets::prettyRadioButtons("sex","Biological Sex", choices = c("Male","Female"), inline = FALSE, status = "primary", animation = "jelly")
               )
    ),
    column(5,
           div(class="card",
               h4("REDs Severity/Risk Status"),
               uiOutput("badge")
           )
    ),
    column(4,
           div(class="card",
               htmlOutput("detail"),
               div(class="muted", "Primary 1 counts as 2 points.")
           )
    )
  ),
  fluidRow(
    uiOutput("primary_ui")
  ),
  fluidRow(
    uiOutput("secondary_ui")
  ),
  fluidRow(
    tags$div(style="font-size:10px; margin-top:-10px; margin-bottom:15px;",
             HTML("Every indicator below requires consideration of a non-LEA-mediated differential diagnosis. All indicators apply to females and males unless indicated. Menstrual cycle status and endogenous sex hormone levels cannot be accurately assessed in athletes who are taking sex hormone-altering medications (eg, hormone-based contraceptives), and thyroid hormone status indicators cannot be accurately assessed in athletes who are taking thyroid medications. All laboratory values should be interpreted in the context of age-and sex-appropriate and laboratory-specific reference ranges. Most REDs data and associated thresholds have been established in pre-menopausal/ andropausal adults unless indicated.<br>
Disclaimer:  This tool should not be used in isolation nor solely for diagnosis, as every indicator requires clinical consideration of a non-LEA- mediated differential diagnosis. Furthermore, the tool is less reliable in situations where it is impossible to assess all indicators (eg, menstrual cycle status in females who are using hormonal contraception). This tool is not a substitute for professional clinical diagnosis, advice and/or treatment from a physician-led team of REDs health and performance experts.<br>
*BMD assessed via DXA within ≤6 months. In some situations, using a Z-score from another skeletal site may be warranted [eg, distal 1/3 radius when other sites cannot be measured or including proximal femoral measurements in some older (>15 years) adolescents for whom longitudinal BMD monitoring into adulthood is indicated].  A true BMD decrease (from prior testing) is ideally assessed in comparison to the individual facilities DXA’s Least Significant Change (LSC) based on the facilities calculated coefficient of variation (%CV). As established by ISCD, at the very least, LSC should be 5.3%, 5.0% and 6.9% for the lumbar spine, hip and femoral neck to detect a clinical change.<br>
†Potential indicators are purposefully vague in quantification, pending further research to quantify parameters and cut-offs more accurately.<br>
Adolescent, <18 years of age; BMD, bone mineral density; BMI, Body Mass Index; BP, blood pressure; BSI, bone stress injuries; DSM-5-TR, Diagnostic and Statistical Manual of Mental Disorders, 5th edition, text revision; DXA, dual-energy X-ray absorptiometry; EDE-Q, Eating Disorder Examination Questionnaire; FFM, fat-free mass; FHA, functional hypothalamic amenorrhea; GI, gastrointestinal; HR, heart rate; IGF-1, insulin-like growth factor 1; kcal, kilocalories; LDL, low-density lipoprotein; RMR, resting metabolic rate; T3, triiodothyronine; T, testosterone; TBLH, total body less head.")
    )
  )
)

# =====================
# 4) SERVER
# =====================
server <- function(input, output, session) {
  # --- Mutual exclusivity helpers ---
  setNo <- function(id) updateRadioButtons(session, id, selected = 0)
  
  
  # (A) Global: Only one of primary4 (p4) OR secondary2 (s2) may be Yes
  observeEvent(input$p4, ignoreInit = TRUE, {
    if (identical(as.integer(input$p4), 1L)) setNo("s2")
  })
  observeEvent(input$s2, ignoreInit = TRUE, {
    if (identical(as.integer(input$s2), 1L)) setNo("p4")
  })
  
  
  # (B) Female-only: only one of primary1 (p1) OR primary2 (p2) OR secondary4 (s4)
  observeEvent(list(input$sex, input$p1, input$p2, input$s4), ignoreInit = TRUE, {
    if (!identical(input$sex, "Female")) return()
    chosen <- c(p1 = as.integer(input$p1 %||% 0L),
                p2 = as.integer(input$p2 %||% 0L),
                s4 = as.integer(input$s4 %||% 0L))
    if (sum(chosen == 1L) > 1L) {
      # If multiple are Yes, keep the most recent and turn the others to No
      # Heuristic: prioritize the one that just changed by checking which is 1 and last triggered
      # Simpler: if p1 is 1, turn others off; else if p2 is 1, turn others off; else if s4 is 1, turn others off.
      if (chosen["p1"] == 1L) { setNo("p2"); setNo("s4") }
      else if (chosen["p2"] == 1L) { setNo("p1"); setNo("s4") }
      else if (chosen["s4"] == 1L) { setNo("p1"); setNo("p2") }
    }
  })
  
  
  # (C) Male-only: only one of primary1 (p1) OR primary2 (p2)
  observeEvent(list(input$sex, input$p1, input$p2), ignoreInit = TRUE, {
    if (!identical(input$sex, "Male")) return()
    if ((as.integer(input$p1 %||% 0L) + as.integer(input$p2 %||% 0L)) > 1L) {
      # If both are Yes, prefer the most recent; simple rule as above
      if (as.integer(input$p1 %||% 0L) == 1L) setNo("p2") else setNo("p1")
    }
  })
  
  # Render secondary questions based on sex
  output$secondary_ui <- renderUI({
    labs <- secondary_labels[[input$sex]]
    
    if (is.null(labs)) labs <- character(0)
    tagList(
      div(class="card",
          h4("Secondary Indicators"),
          lapply(seq_along(labs), function(i) mkYN(paste0("s", i), labs[i]))
      )
    )
  })
  
  output$primary_ui <- renderUI({
    labs <- primary_labels[[input$sex]]
    if (is.null(labs)) labs <- character(0)
    tagList(
      div(class="card",
          h4("Primary Indicators"),
          lapply(seq_along(labs), function(i) mkYN(paste0("p", i), labs[i]))
      )
    )
  })
  
  # Compute counts
  primary_count <- reactive({
    p1 <- as.integer(input[["p1"]] %||% 0L)
    nsec <- length(primary_labels[[input$sex]])
    # sum(vapply(1:nsec, function(i) as.integer(input[[paste0("p", i)]] %||% 0L), 1L))
    rest <- sum(vapply(2:nsec, function(i) as.integer(input[[paste0("p", i)]] %||% 0L), 1L))
    p1*2 + rest
  })
  secondary_count <- reactive({
    nsec <- length(secondary_labels[[input$sex]])
    sum(vapply(1:nsec, function(i) as.integer(input[[paste0("s", i)]] %||% 0L), 1L))
  })
  
  # Lookup colour
  risk_colour <- reactive({
    sx <- input$sex %||% "Female"
    pc <- primary_count(); sc <- secondary_count()
    hit <- subset(mapping, sex==sx & primary==pc & secondary==sc)
    if (nrow(hit)==1) hit$colour else NA_character_
  })
  
  output$badge <- renderUI({
    col <- risk_colour()
    if (is.na(col)) return(div("No mapping found.", class="muted"))
    div(class=paste("pill", col), toupper(col))
  })
  
  output$detail <- renderText({
    sprintf("Primary count: %d<br>Secondary count: %d", primary_count(), secondary_count())
  })
}

# Safe helper for NULL inputs
`%||%` <- function(a,b) if (is.null(a)) b else a

shinyApp(ui, server)
