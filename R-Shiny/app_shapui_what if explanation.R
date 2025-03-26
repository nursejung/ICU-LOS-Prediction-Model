library(shiny)
library(shinyjs)
library(DT)
library(tidyr)
library(dplyr)
library(splines)
library(jsonlite)

ui <- fluidPage(
  titlePanel("What if explanation"),
  fileInput("file", "Choose a CSV file"),
  DTOutput("datatable"),

  tags$head(
        tags$style(HTML("
            .redirect-button {
                position: absolute;
                top: 100px;
                right: 30px;
                z-index: 100;
            }
            .additional-button {
                position: absolute;
                top: 100px;
                right: 130px;  
                z-index: 100;
            }
			.dataTables_wrapper .table > tbody > tr > td {
        		background-color: white !important;
      		}
        "))
    ),
    
    # Additional button
    actionButton("additional_button", "Satisfaction assessment", class = "additional-button"),

    # 'Go to URL' button
    actionButton("redirect_button", "Trust assessment", class = "redirect-button")
)

server <- function(input, output, session) {
  
  data_reactive <- reactiveVal(NULL)
  Colors_reactive <- reactiveVal(NULL)
  prediction <- reactiveVal(NULL)
  lowest <- reactiveVal(NULL)
  highest <- reactiveVal(NULL)  

  dict_col <- list(
	key1 = 6,
	key2 = 7,
    key3 = 10,
    key4 = 11,
    key5 = 12,
    key6 = 13,
    key7 = 15,
    key8 = 25,
    key9 = 26,
    key10 = 34,
    key11 = 35,
    key12 = 37,
    key13 = 38,
    key14 = 40,
    key15 = 41,
    key16 = 46,
    key17 = 47,
    key18 = 48,
    key19 = 49,
    key20 = 58
 )

  observeEvent(input$file, {

	### Normalize input data
  	req(input$file)

    # dat <- read.csv(input$file$datapath, fileEncoding = "EUC-KR")
    dat <- read.csv(input$file$datapath, fileEncoding = "utf-8")
	dat <- round(dat, digits=1)
	features <- colnames(dat)

print(dat)
	write.csv(dat, file = "./file/temp.csv")

	dat_normal <- dat
	maxmin <- read.csv('./file/minmax.csv')
	max <- maxmin[1, ]
	min <- maxmin[2, ]
	

	print(max)
	print(min)

	normalize_column <- function(column, max, min) {
  		return ((column - min) / (max - min))
	}

	for (col in colnames(dat_normal)) {
  		dat_normal[[col]] <- normalize_column(dat_normal[[col]],max[[col]],min[[col]])
	}

	write.csv(dat_normal, file = "./file/temp_normal.csv")

	### Predict 
	predict <- system2("python", c("get_prob.py ./trained/RF_matrix_trained_12hr_60hr.joblib ./file/temp_normal.csv"), stdout = TRUE)

	prediction(predict)


	### Calculate shap
	shap <- system2("python", c("get_shapval.py ./trained/RF_matrix_trained_12hr_60hr.joblib ./file/temp_normal.csv"), stdout = TRUE)
	shap_df <- read.csv(text = shap, header = TRUE)  # Assuming the output is a CSV-like string with header
	shap_df <- round(shap_df, digits=4)

	write.csv(shap_df, file = "./file/shapval_all.csv")
	# print(shap_df)

	### Set colormap
	# lowest_val  <- min(unlist(shap_df), na.rm = TRUE)
    # highest_val <- max(unlist(shap_df), na.rm = TRUE)

	lowest_val <- -0.08
	highest_val <- 0.22

	print(lowest_val)
	print(highest_val)

	lowest(lowest_val) # -0.039
    highest(highest_val) # 0.19

    interpfun <- splinefun(
      c(lowest_val, 0, highest_val),
      c(0, 0.5, 1)
    )

    colfunc <- colorRamp(c("blue", "white", "red"))

	subset <- features[2:59]

    Colors <- lapply(shap_df[, subset], function(x) {
      y <- interpfun(x)
      cols <- colfunc(y)

      # Check for NA values in cols and replace them with 0
      cols[is.na(cols)] <- 0

      rgb(cols[, 1L], cols[, 2L], cols[, 3L], maxColorValue = 255)
    })

	data_reactive(dat)
	Colors_reactive(Colors)
  })
 
  # Observe cell edits
  observeEvent(input$datatable_cell_edit, {

    info <- input$datatable_cell_edit
    
    cat("Row: ", info$row, "\n")
    cat("Column: ", info$col, "\n")
    cat("Value: ", info$value, "\n")
    
    ### Normalize edited data
	x <- info$col
    x_str <- toString(x)
	prefix <- 'key'
	key = paste(prefix, x, sep = '')

	value <- dict_col[[key]]

    if (is.null(info$value)) {
      print('error: input null')
    }
    
	index_col <- info$col

    dat <- data_reactive()
    dat[info$row, index_col] <- as.numeric(info$value)
	dat <- round(dat, digits=4)
	features <- colnames(dat)

	write.csv(dat, file = "./file/temp.csv")

	dat_normal <- dat
	maxmin <- read.csv('./file/minmax.csv')
	max <- maxmin[1, ]
	min <- maxmin[2, ]

	normalize_column <- function(column, max, min) {
  		return ((column - min) / (max - min))
	}

	for (col in colnames(dat_normal)) {
  		dat_normal[[col]] <- normalize_column(dat_normal[[col]],
                                                  max[[col]],
                                                  min[[col]])
	}

	write.csv(dat_normal, file = "./file/temp_normal.csv")

	### Predict 
	predict <- system2("python", c("get_prob.py ./trained/RF_matrix_trained_12hr_60hr.joblib ./file/temp_normal.csv"), stdout = TRUE)

	prediction(predict)

	### Calculate shap
	shap <- system2("python", c("get_shapval.py ./trained/RF_matrix_trained_12hr_60hr.joblib ./file/temp_normal.csv"), stdout = TRUE)
	shap_df <- read.csv(text = shap, header = TRUE)  # Assuming the output is a CSV-like string with header

	lowest_val <- -0.08
	highest_val <- 0.22

	lowest(lowest_val)
    highest(highest_val)

    interpfun <- splinefun(
      c(lowest_val, 0, highest_val),
      c(0, 0.5, 1)
    )

    colfunc <- colorRamp(c("blue", "white", "red"))

	subset <- features[2:59]

    Colors <- lapply(shap_df[, subset], function(x) {
      y <- interpfun(x)
      cols <- colfunc(y)

      # Check for NA values in cols and replace them with 0
      cols[is.na(cols)] <- 0

      rgb(cols[, 1L], cols[, 2L], cols[, 3L], maxColorValue = 255)
    })

	data_reactive(dat)
	Colors_reactive(Colors)

  })
  
  # Create table
  output$datatable <- renderDT({

	dat <- data_reactive()
    Colors <- Colors_reactive()
	features <- colnames(dat)
	predict <- prediction()

    if (is.null(dat) || is.null(Colors)) {
      return(NULL)
    }

	predict_df <- read.csv(text = predict, header = TRUE)
	print(predict_df)

	# index_ <- c(6,7,10,11,12,13,15,25,26,34,35,37,38,40,41,46,47,48,49,58)
	# subset <- features[index_]
	subset <- features[1:59]

	df = dat[,subset]
	dat_table <- cbind(df[1], predict_df, df[,2:59])
	col_names <- colnames(dat_table)

	dtable <- datatable(
  		dat_table, rownames = FALSE, editable = TRUE,
		options = list(
      		pageLength = 25  # Set the number of items per page
    	)
	)
		

	subset <- features[2:59]
	for (feature in subset) {
	   # print(dat[[feature]])
	   # print(Colors[[feature]])

       dtable <- dtable %>%
        formatStyle(
          feature,
          backgroundColor = styleEqual(dat[[feature]], Colors[[feature]])	
        )
    }
	

	dtable

  })
}

host <- "0.0.0.0"  
port <- 8902

# Create a Shiny app
shinyApp(
  ui = ui,
  server = function(input, output, session) {
    server(input, output, session)
  },
  options = list(
    host = host,
    port = port
  )
)
