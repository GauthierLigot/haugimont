
######## load interesting and not conflicting packages (eg. lme4 and nlme)
library(grid)
library(gridExtra)
library(ggplot2)

################################################
################################################
############ ggplot2 ###########################
################################################
################################################

######## define a theme for ggplot2
theme_map <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      #       axis.line=element_blank(),
      axis.title=element_text(size=12),
      axis.text=element_text(size=10),
      #       axis.ticks=element_blank(),
      #       axis.ticks.length=unit(0.3, "lines"),
      #       axis.ticks.margin=unit(0.5, "lines"),
      #       axis.title.x=element_blank(),
      #       axis.title.y=element_blank(),
      #       legend.background=element_rect(fill="white", colour=NA),
      #       legend.key=element_rect(colour="white"),
      #       legend.key.size=unit(1.2, "lines"),
      #       legend.position="right",
      #       legend.text=element_text(size=rel(0.8)),
      #       legend.title=element_text(size=rel(0.8), face="bold", hjust=0),
      panel.background=element_blank(),
      panel.border=element_rect(color="black",fill=NA),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank()
      #       panel.margin=unit(0, "lines"),
      #       plot.background=element_blank(),
      #       plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"),
      #       plot.title=element_text(size=rel(1.2)),
      #       strip.background=element_rect(fill="grey90", colour="grey50"),
      #       strip.text.x=element_text(size=rel(0.8)),
      #       strip.text.y=element_text(size=rel(0.8), angle=-90) 
    )   
}
theme_set(theme_map())

################ ggplot2 functions
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend2 <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  g<-arrangeGrob(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
  return(g)
}


