#' Plot output from a single saved Habplan flow file
#'
#' Creates a figure to visualize flow outputs across time periods for a single flow file
#' @param flow.data Flow file saved by Habplan
#' @param nyear Number of years or time periods interested in (have data for)
#' @return A figure comparing multiple flows across time period
#' @examples
#' #Read in the example flows:
#' flow1 <- read.csv("./saveFlow1", sep="")
#'
#' #Input the flow file into the function, and number of years
#' flowPlot(flow.data = flow1, nyear = 35)
#'
#' @export

flowPlot <- function(flow.data, nyear){
  #Set the length below as the number of years in model
  flow.list <- vector(mode = "list", length = nyear)

  #Format flow file to remove commas and extra column
  for (i in 1:ncol(flow.data)) {
    flow.data[,i] <- gsub(",", "", flow.data[,i])
  }
  flow1 <- flow.data[,-1]
  
  for (i in 1:length(flow.list)) {
    std <- cbind(flow1[1], flow1[i+2], flow1[i+37])
    colnames(std) <- c("id", "year", "flow")
    std$flow <- as.numeric(std$flow)
    tot.flow <- sum(std$flow)
    flow.list[i] <- tot.flow
  }

  year <- c(1:35)
  flow <- t(flow.list)
  flow <- t(flow)
  f1.output <- cbind(year, flow)
  f1.output <- as.data.frame(f1.output)
  colnames(f1.output) <- c("year", "flow")
  f1.output$flow <- as.numeric(f1.output$flow)
  f1.output$year <- as.numeric(f1.output$year)

  #library(ggplot2)
  f1.plot <- ggplot(data = f1.output) +
    geom_line(aes(x = year, y = flow)) +
    geom_area(aes(x = year, y = flow), fill = "grey", alpha = 0.7) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle("Flow output over time") +
    xlab("Year") +
    ylab("Flow") +
    theme_classic() +
    theme(legend.position = "none",
         text = element_text(size=18))

  ggsave(file = paste0("./Single_flow.png"), width = 200, height = 120,
         dpi = 600, units = "mm")

  return(f1.plot)
}
