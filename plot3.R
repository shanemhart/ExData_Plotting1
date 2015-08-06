gen_plot3 <-function(dat) {
  ## function generates the plot
  d <- as.character(dat[,1])
  t <- as.character(dat[,2])
  x <- paste(d,t)
  
  x <- strptime(x, format = '%d/%m/%Y %H:%M:%S')
  
  plot(x, dat$Sub_metering_1, pch ='.', ylab = 'Energy sub metering', xlab ='')
  points(x, dat$Sub_metering_2, col = 'red', pch = '.')
  points(x, dat$Sub_metering_3, col = 'blue', pch = '.')
  lines(x, dat$Sub_metering_1)
  lines(x, dat$Sub_metering_2, col ='red')
  lines(x, dat$Sub_metering_3, col ='blue')
  legend('topright', pch = NA, lty = 1, col = c('black', 'red', 'blue'), legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
}

getData <- function() {
  ## attempts to scan the file and only take out the needed data.
  file_name = "household_power_consumption.txt"
  d_range = as.Date(c("01/02/2007", "02/02/2007"), format = "%d/%m/%Y")
  
  ## Determine number of records to view
  count = 0
  in_range = TRUE
  con = file(file_name, "r")
  buff <- read.table(con, header = TRUE, sep = ';', nrows = 200)
  header <- names(buff)
  
  while(in_range) {
    for(i in 1:nrow(buff)) {
      date <- as.Date(buff[i,1], format ="%d/%m/%Y")
      if(date >= d_range[1]) {
        if(date <= d_range[2]) {
          count = count + 1
        }
        else
          in_range = FALSE
      }
    }
    buff <- read.table(con, header = FALSE, sep = ';', nrows = 200)
  }
  close(con)
  
  ##create empy data recepticle and store data
  con = file(file_name, "r")
  buff <- read.table(con, header = TRUE, sep = ';', nrows = 200)
  d = vector(length = count)
  t = vector(length = count)
  power = matrix(NA, nrow = count, ncol = 7)
  in_range = TRUE
  count = 0
  
  while(in_range) {
    for(i in 1:nrow(buff)) {
      date <- as.Date(buff[i,1], format ="%d/%m/%Y")
      if(date >= d_range[1]) {
        if(date <= d_range[2]) {
          count = count + 1
          d[count] <- as.character(buff[i,1])
          t[count] <- as.character(buff[i,2])
          power[count,1] = buff[i,3]
          power[count,2] = buff[i,4]
          power[count,3] = buff[i,5]
          power[count,4] = buff[i,6]
          power[count,5] = buff[i,7]
          power[count,6] = buff[i,8]
          power[count,7] = buff[i,9]
        }
        else
          in_range = FALSE
      }
    }
    buff <- read.table(con, header = FALSE, sep = ';', nrows = 200)
  }
  close(con)
  
  ##build matrix and output
  
  frame = data.frame(d, t, power)
  colnames(frame) <- header
  frame
}

export <-function(name) {
  ## function exports file
  dev.copy(png, file = name)
  dev.off()
}

plot3 <- function() {
  ## main driving function
  frame <-getData()
  gen_plot3(frame)
  export('plot3.png')
}