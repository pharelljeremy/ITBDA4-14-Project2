# 1.2 MAPPER

mapper <- function(file_name, keyword) {
  lines <- readLines(file_name, warn = FALSE)
  tokens <- unlist(strsplit(tolower(lines), "[^a-z0-9]+"))
  tokens <- tokens[tokens != ""]
  hits <- tokens[tokens == keyword]
  if (length(hits) == 0) {
    return(data.frame(key = character(0), value = integer(0)))
  }
  data.frame(
    key = rep(keyword, length(hits)),
    value = rep(1L, length(hits)),
    stringsAsFactors = FALSE
  )
}

# 1.3 REDUCER

reducer <- function(pairs) {
  if (nrow(pairs) == 0) {
    return(data.frame(key = character(0), count = integer(0)))
  }
  agg <- aggregate(value ~ key, data = pairs, FUN = sum)
  names(agg)[2] <- "count"
  agg
}

# 1.4 HADOOP MAPREDUCE CONFIGURATION SETTINGS

streaming_mapper_code <- '
keyword <- tolower(Sys.getenv("KEYWORD"))
con <- file("stdin", "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  tokens <- unlist(strsplit(tolower(line), "[^a-z0-9]+"))
  tokens <- tokens[tokens != ""]
  hits <- tokens[tokens == keyword]
  for (h in hits) cat(h, "\t1\n", sep = "")
}
close(con)
'
writeLines(streaming_mapper_code, "mapper_stream.R")

streaming_reducer_code <- '
con <- file("stdin", "r")
current_key <- NULL
current_sum <- 0
process <- function(k, s) {
  if (!is.null(k)) cat(k, "\t", s, "\n", sep = "")
}
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  parts <- strsplit(line, "\t")[[1]]
  if (length(parts) < 2) next
  k <- parts[1]
  v <- as.integer(parts[2])
  if (is.null(current_key) || k == current_key) {
    current_key <- k
    current_sum <- current_sum + v
  } else {
    process(current_key, current_sum)
    current_key <- k
    current_sum <- v
  }
}
process(current_key, current_sum)
close(con)
'
writeLines(streaming_reducer_code, "reducer_stream.R")

run_hadoop_streaming <- function(keyword, input_path, output_path) {
  hadoop_bin <- Sys.getenv("HADOOP_HOME", unset = "")
  streaming_jar <- file.path(
    hadoop_bin, "share", "hadoop", "tools", "lib", "hadoop-streaming.jar"
  )
  if (!file.exists(streaming_jar)) {
    stop("hadoop-streaming.jar not found. Set HADOOP_HOME correctly.")
  }
  args <- c(
    "jar", streaming_jar,
    "-files", "mapper_stream.R,reducer_stream.R",
    "-mapper", "Rscript mapper_stream.R",
    "-reducer", "Rscript reducer_stream.R",
    "-cmdenv", paste0("KEYWORD=", tolower(keyword)),
    "-input", input_path,
    "-output", output_path
  )
  system2("hadoop", args = args, stdout = TRUE, stderr = TRUE)
}

# LOCAL DEMONSTRATION DRIVER

run_local <- function(keyword, input_dir = "input",
                      output_file = "output/keyword_count.csv") {
  keyword <- tolower(keyword)
  if (!dir.exists(input_dir)) {
    stop("Input directory does not exist: ", input_dir)
  }
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  files <- list.files(input_dir, full.names = TRUE)
  if (length(files) == 0) {
    stop("No input files found in ", input_dir)
  }
  mapped <- do.call(rbind, lapply(files, mapper, keyword = keyword))
  if (is.null(mapped)) {
    mapped <- data.frame(key = character(0), value = integer(0))
  }
  result <- reducer(mapped)
  print(result)
  write.csv(result, output_file, row.names = FALSE)
  invisible(result)
}
