library(shiny)
library(shinythemes)
library(DT)
library(plotrix)

ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  
  navbarPage("Logistic Regression", 
             
             tabPanel("Home",
                      img(src = "Home2.png", width = "100%")),
             
             tabPanel("Model Analysis",
                      sidebarPanel(
                        img(src = "logo.jpg", width = "100%"),
                        fileInput("userdata", "Upload data in .csv format:", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),     
                        
                        uiOutput("selecty"),
                        uiOutput("selectx"),
                        sliderInput(inputId = "sig", label = "Significance level:", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                        actionButton("getresult", icon = icon("download"), "Get result!", class = "btn-primary", width = "100%"),
                        width = 3), 
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Data Viewer", DT::dataTableOutput("datatablereg")),
                          tabPanel("Data Visualization",  plotOutput("histplot"), plotOutput("regplot")),   tabPanel("Descriptive Statistics", verbatimTextOutput("descriptive")),
                          tabPanel("Table Stack", DT::DTOutput("tablestack")),
                          tabPanel("Model Fitting", verbatimTextOutput("modelsummary")),        
                          tabPanel("Prediction",uiOutput("input_ui"),actionButton("predict_btn", "Predict"),verbatimTextOutput("modelprediction")),
                          tabPanel("Model Accuracy", DT::dataTableOutput("datareg1"), plotOutput("accuracyplot"),DT::dataTableOutput("datareg")
),
                          tabPanel("Web App Developers", 
                                   h3("🌟 Web App Developers 🌟", align = "center"), 
                                   br(), 
                                   fluidRow(
                                     column(12, align = "center",
                                            tags$div(
                                              tags$img(src = "sukanya.jpg", height = "200px"), 
                                              h4("6510210335 Sukanya Thawornjit",br(),h4("Statistics, Prince of Songkla University"))
                                            )))))))))

                      
server <- function(input, output){
  dataInput1 <- reactive({
    userdata <- input$userdata
    if(is.null(userdata)){return(NULL)}
    data1 <- read.csv(file = userdata$datapath, header = TRUE, sep = ",")
    return(data1)})
  
  output$selecty <- renderUI({
    y <- dataInput1()
    selectInput(inputId = "y1", "Response variable (Y):", names(y))})
  
  output$selectx <- renderUI({
    x <- dataInput1()
    selectizeInput(inputId = "x1", "Independent variables (X):", names(x), multiple = TRUE)})
  
  observeEvent(input$getresult, {
    output$datatablereg <- DT::renderDataTable({
      datareg <- dataInput1()
      DT::datatable(datareg)})
    
    data <- dataInput1()
    vary <- data[, input$y1]
    varx <- data[, input$x1, drop = FALSE]  
    
    # ---------- Pie Chart + Histogram ----------
    output$histplot <- renderPlot({
      req(input$y1, input$x1, dataInput1())  # รอให้ข้อมูลครบก่อนรัน
      
      data <- dataInput1()
      vary <- data[[input$y1]]
      varx <- data[, input$x1, drop = FALSE]
      num_x <- length(input$x1)
      
      par(mfrow = c(1 + (num_x > 1), min(num_x, 2)), mar = c(4, 4, 4, 2))
      
      # Pie Chart 3D
      pie_data <- table(vary)
      pie_labels <- paste(names(pie_data), "\n(", pie_data, ")", sep = "")
      pastel_colors <- c("#FFB6C1", "#ADD8E6")
      
      pie3D(pie_data, labels = pie_labels, explode = 0.1, 
            main = paste("Pie Chart for", input$y1),
            col = pastel_colors, labelcex = 1.4,  
            cex.main = 1.5, radius = 1.7, start = pi/3)
      
      # Histogram
      lapply(1:num_x, function(i) {
        hist(varx[, i], main = paste("Histogram for", input$x1[i]), 
             xlab = input$x1[i], col = "lightblue", border = "white",
             cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
      })
    })
    
    
    # ---------- Box Plot ----------
    output$regplot <- renderPlot({
      req(input$y1, input$x1, dataInput1())  # รอให้ข้อมูลครบก่อนรัน
      
      data <- dataInput1()
      vary <- data[[input$y1]]
      num_x <- length(input$x1)
      
      nrow_plot <- ceiling(sqrt(num_x))
      ncol_plot <- ceiling(num_x / nrow_plot)
      par(mfrow = c(nrow_plot, ncol_plot), mar = c(4, 4, 4, 2))  # จัด layout ให้สวยงาม
      
      for (var in input$x1) {
        boxplot(data[[var]] ~ vary, 
                col = c("#FFB6C1", "#ADD8E6"), 
                xlab = input$y1, 
                ylab = var, 
                main = paste("Box Plot of", var, "by", input$y1),
                cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1) 
      }
    })
   
    output$descriptive <- renderPrint({
      
      create_box <- function(title) {
        width <- nchar(title) + 6  
        top_bottom <- paste0("╔", paste(rep("═", width), collapse = ""), "╗")
        middle <- paste0("║  ", title, "  ║")
        bottom <- paste0("╚", paste(rep("═", width), collapse = ""), "╝")
        return(paste(top_bottom, middle, bottom, sep = "\n"))}
      
      # แสดงกรอบหัวข้อ Outcome
      cat("\n", create_box(paste("🔥 DESCRIPTIVE STATISTICS FOR", toupper(input$y1), "🔥")), "\n\n")
      print(summary(vary))
      
      # แสดงกรอบหัวข้อ Independent Variables
      cat("\n", create_box("📊 DESCRIPTIVE STATISTICS FOR SELECTED INDEPENDENT VARIABLES 📊"), "\n")
      
      # ใช้ invisible() เพื่อปิดการแสดงผลของ index [[1]], [[2]]
      invisible(lapply(names(varx), function(var) {
        cat("\n", create_box(paste("🔹 VARIABLE:", toupper(var))), "\n")
        print(summary(varx[[var]]))}))
    })
    
    
    #install.packages("tableone")
    
    library(tableone)
    
    output$tablestack <- DT::renderDataTable({
  req(dataInput1(), input$x1, input$y1)
  data <- dataInput1()
  
  # กรองเฉพาะตัวแปร numeric จาก input$x1
  numeric_vars <- input$x1[sapply(data[, input$x1, drop = FALSE], is.numeric)]

  # ตรวจสอบการแจกแจงแบบปกติด้วย Shapiro-Wilk test
  nonnormal_vars <- numeric_vars[sapply(data[, numeric_vars, drop = FALSE], function(x) {
    # ตรวจสอบว่ามี sample size มากพอ (ต้อง > 3) ก่อนทดสอบ
    if (length(na.omit(x)) > 3) {
      p <- tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
      return(!is.na(p) && p < 0.05)
    } else {
      return(TRUE)  # ถือว่าไม่ปกติถ้าเล็กเกินไป
    }
  })]

  # สร้างตารางโดยใช้ CreateTableOne และ print โดยระบุ nonnormal
  tab1 <- CreateTableOne(vars = input$x1, strata = input$y1, data = data)
  df <- print(tab1, nonnormal = nonnormal_vars, printToggle = FALSE, quote = FALSE, noSpaces = TRUE)
  
  DT::datatable(as.data.frame(df), options = list(pageLength = 10, scrollX = TRUE))
})

   
    library(MASS)
    
    # สร้างโมเดล
    formula_str <- paste(input$y1, "~", paste(input$x1, collapse = " + "))
    model_base <- glm(as.formula(formula_str), data = data, family = binomial)  
    
    # ใช้ stepAIC เลือกโมเดลที่ดีที่สุด
    model_final <- stepAIC(model_base, direction = "both", trace = FALSE)  
    
    
    output$modelsummary <- renderPrint({
    
      create_box <- function(title) {
        width <- nchar(title) + 6  
        top_bottom <- paste0("╔", paste(rep("═", width), collapse = ""), "╗")
        middle <- paste0("║  ", title, "  ║")
        bottom <- paste0("╚", paste(rep("═", width), collapse = ""), "╝")
        return(paste(top_bottom, middle, bottom, sep = "\n"))
      }
      
      # ฟังก์ชันจัดรูปแบบการแสดงผลค่า Coefficients พร้อม OR
      format_coef <- function(model) {
        coef_table <- coef(summary(model)) 
        OR_table <- exp(coef(model)) 
        CI_table <- exp(confint(model))  
        
        cat("             Estimate    Std.Error    z value     Pr(>|z|)      OR       95% CI (Lower - Upper)\n")
        for (i in 1:nrow(coef_table)) {
          p_value <- coef_table[i, 4]  # ดึงค่า p-value
          signif_code <- ifelse(p_value < 0.001, "***",
                                ifelse(p_value < 0.01, "**",
                                       ifelse(p_value < 0.05, "*",
                                              ifelse(p_value < 0.1, ".", " "))))
          
          cat(sprintf("%-12s %-10.6f  %-10.6f  %-10.2f  %-10.4f %-2s %-8.4f  (%.4f - %.4f)\n", 
                      rownames(coef_table)[i], coef_table[i, 1], coef_table[i, 2], coef_table[i, 3], 
                      p_value, signif_code, OR_table[i], CI_table[i, 1], CI_table[i, 2]))
        }
      }
    
      
      cat("\n", create_box("BASE MODEL (All Variables)"), "\n\n")
      print(summary(model_base)$call)
      cat("\nFormula:", deparse(formula(model_base)), "\n\n")
      
      cat("Coefficients:\n")
      format_coef(model_base)
      
      cat("\nAIC:", AIC(model_base), "\n")
      cat("Residual Deviance:", deviance(model_base), "on", df.residual(model_base), "degrees of freedom\n")
      
      cat("\n", create_box("FINAL MODEL (Stepwise AIC)"), "\n\n")
      print(summary(model_final)$call)
      cat("\nFinal Model Formula:", deparse(formula(model_final)), "\n\n")
      
      cat("Coefficients:\n")
      format_coef(model_final)
      
      cat("\nAIC:", AIC(model_final), "\n")
      cat("Residual Deviance:", deviance(model_final), "on", df.residual(model_final), "degrees of freedom\n")
    })
    
    # สร้าง dynamic input fields ตามตัวแปรใน model_final
    output$input_ui <- renderUI({
      req(model_final)  # รอให้ model สร้างเสร็จก่อน
      
      vars <- names(coef(model_final))[-1]  # เอาชื่อตัวแปรทั้งหมด ยกเว้น Intercept
      input_list <- lapply(vars, function(var) {
        numericInput(inputId = paste0("input_", var),
                     label = paste("Enter value for", var),
                     value = 0)
      })
      
      tagList(input_list)
    })
    
    # ใช้เมื่อผู้ใช้กดปุ่ม "Predict"
    observeEvent(input$predict_btn, {
      vars <- names(coef(model_final))[-1]
      input_values <- sapply(vars, function(var) {
        input[[paste0("input_", var)]]
      })
      
      newdata <- as.data.frame(t(input_values))
      colnames(newdata) <- vars
      
      # พยากรณ์ความน่าจะเป็น
      pred <- predict(model_final, newdata = newdata, type = "response")
      
      output$modelprediction <- renderText({
        prob <- round(pred * 100, 1)
        odds <- round(pred / (1 - pred), 2)
        
        paste0(" The model predicts that this individual has a ", prob, "% chance (odds = ", odds, ") of developing ", input$y1, ".")
      })
    })
    
    # ---------- Model Accuracy ----------
    output$datareg1 <- DT::renderDataTable({
      req(model_final)  
      fitted_values <- fitted(model_final)  
      residuals <- resid(model_final)  
      
      datareg <- data.frame(Y = vary, X = varx, Y_Fitted = round(fitted_values,2),Residuals = round(residuals,2))
      DT::datatable(datareg)
    })
    
    library(pROC)
    library(caret)
    
    output$accuracyplot <- renderPlot({
      req(model_final)  
      
      # คำนวณค่า probability prediction
      prob_pred <- predict(model_final, type = "response")
      
      # กำหนด class prediction โดยใช้ threshold 0.5
      class_pred <- ifelse(prob_pred > 0.5, 1, 0)
      
      # คำนวณ ROC Curve
      roc_obj <- roc(vary, prob_pred)
      
      # คำนวณ Confusion Matrix
      cm <- table(Actual = vary, Predicted = class_pred)
      
      # แสดง 2 กราฟใน 1 หน้าจอ
      par(mfrow = c(1, 2))  
      
      # วาดกราฟ ROC Curve
      plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")
      abline(a = 0, b = 1, lty = 2, col = "gray")   
      text(0.6, 0.2, paste("AUC =", round(auc(roc_obj), 3)), col = "red", cex = 1.2)
      
      # วาด Confusion Matrix ใช้ fourfoldplot()
      fourfoldplot(cm, color = c("#FF9999", "#99CCFF"), main = "Confusion Matrix")
    })
  })
}

shinyApp(ui, server)

