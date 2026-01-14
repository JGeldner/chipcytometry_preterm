
clustered_trajectories<-function(annotations, remove, filename, angle=90, border_colors, subscript=FALSE){

library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

  #function for rounded edgges

wrap_with_border <- function(grob,
                             padding = unit(10, "pt"),
                             r = unit(0.2, "snpc"),
                             color = "blue") {
  
  # Step 1: Make sure input is a gtable
  if (!inherits(grob, "gtable")) {
    grob <- ggplotGrob(grob)
  }
  
  # Step 2: Create a container table with padding around the original grob
 padded <- gtable_add_rows(grob, padding, pos = 0)  # top
 padded <- gtable_add_rows(padded, padding, pos = nrow(padded))  # bottom
  padded <- gtable_add_cols(padded, padding, pos = 0)  # left
  padded <- gtable_add_cols(padded, padding, pos = ncol(padded))  # right
  
  # Step 3: Get total size of the padded gtable
  total_width <- sum(padded$widths)
  total_height <- sum(padded$heights)
  
  # Step 4: Create the rounded border grob sized to the full padded gtable
  border <- roundrectGrob(
    x = 0.48, y = 0.5,
    width = unit(0.95, "npc"),
    height = unit(0.95, "npc"),
    r = r,
    just = "center",
    gp = gpar(col = color, lwd = 10, fill=NA)
  )
  
  # Step 5: Add the border to the background layer of the padded gtable
  padded <- gtable_add_grob(
    padded,
    grobs = border,
    t = 1, l = 1, b = nrow(padded), r = ncol(padded),
    z = Inf,
    name = "border"
  )
  
  return(padded)
}

  

  
# --- Plotting functions ---
plot_text_facet <- function(data) {
  ggplot(data) +
    geom_text(aes(y = (y/2)-1, x = x, label = label), size = 10, hjust=0) +
    coord_cartesian(clip = "off") +
    theme_void() +
    scale_y_continuous(limits = c(-3, 0), expand = c(0, 0)) +
   scale_x_continuous(expand = c(0, 0), limits = c(0, 3.8)) +
    theme(aspect.ratio = 0.5, 
          strip.text = element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          panel.spacing = unit(0, "pt"),
          panel.margin = unit(0, "pt"),       # older ggplot2 versions
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

plot_line_facet <- function(data, show_x = FALSE, subscript=subscript, color="black") {
  
   
  if(subscript==FALSE){
  
  ggplot(data = data, aes(x = name, y = value, group = celltype)) +
    geom_smooth(se = FALSE, linewidth = 3, color = color) +
    theme_bw() +
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1), breaks = c(-1,0,1)) +
    theme(
      axis.title = element_blank(),
      aspect.ratio = 0.6,
      axis.text.y = element_text(size = 70),
      axis.text.x = if (show_x) element_text(size = 80, angle = angle, vjust = 0.5) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      panel.spacing = unit(4, "lines")
    )
 
  } else if (subscript==TRUE){
    
        labels <- c(
  expression(TP1[C]),
  expression(TP1[P]),
  "TP2",
  "TP3"
    )  

  ggplot(data = data, aes(x = name, y = value, group = celltype)) +
    geom_smooth(se = FALSE, linewidth = 3, color = color) +
    theme_bw() +
    scale_x_discrete(expand = c(0.1, 0.1), labels=labels) +
    scale_y_continuous(expand = c(0.1, 0.1), breaks = c(-1,0,1)) +
    theme(
      axis.title = element_blank(),
      aspect.ratio = 0.6,
      axis.text.y = element_text(size = 70),
      axis.text.x = if (show_x) element_text(size = 80, angle = angle, vjust = 0.5) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      panel.spacing = unit(4, "lines")
    )
  }
  
}

#---setup--------------

facet_levels <- sort(unique(annotations$group))
n <- length(facet_levels)
annotations$x<-annotations$x-0.8
# --- Generate individual plots ---
data_list_text <- split(annotations, annotations$group)
data_list_lines <- split(remove, remove$clusters)

plots_text <- lapply(data_list_text, plot_text_facet)
plots_line <- lapply(seq_along(facet_levels), function(i) {
  plot_line_facet(data_list_lines[[i]], show_x = FALSE, subscript = subscript, color=border_colors[i])
})


# --- Extract x-axis grob before removing from the plot ---
x_axis_plot<-plot_line_facet(data_list_lines[[1]], show_x = TRUE, subscript = subscript)+
   theme(
    axis.text.x = element_text(size = 70),  # Shrink font size
  )

get_x_axis_grob <- function(plot) {
  g <- ggplotGrob(plot)
  x_axis_index <- grep("axis-b", g$layout$name)[1]
  g$grobs[[x_axis_index]]
}

x_axis_grob <- get_x_axis_grob(x_axis_plot)

# Padding as a fraction of width, e.g., 0.15 = 15% left margin
left_margin_fraction <- 0.2

# Wrap the x-axis grob in a viewport that shifts its content right
x_axis_shifted <- grobTree(
  x_axis_grob,
  vp = viewport(x = unit(left_margin_fraction, "npc"), just = "left", width = unit(1 - left_margin_fraction-0.1, "npc"))
)


# --- Convert to grobs ---
text_grobs <- lapply(plots_text, ggplotGrob)
line_grobs <- lapply(plots_line, ggplotGrob)


# --- Combine each row: line | text ---
combined_rows <- mapply(function(g1, g2) {
  arrangeGrob(g1, g2, ncol = 2)
}, line_grobs, text_grobs, SIMPLIFY = FALSE)


combined_rows_bordered<-list()

#for(i in 1:length(combined_rows)){
#  combined_rows_bordered[[i]]<-wrap_with_border(combined_rows[[i]], color = border_colors[i])
#}

#combined_rows_bordered <- lapply(combined_rows, wrap_with_border)

# --- Build axis row below left column ---
# Calculate widths from first combined row
combined_first <- combined_rows[[1]]
left_width <- sum(combined_first$widths[1:(length(combined_first$widths)/2)])
right_width <- sum(combined_first$widths[(length(combined_first$widths)/2 + 1):length(combined_first$widths)])

# Axis row with two columns: axis + blank
axis_row <- gtable(
  widths = unit.c(left_width, right_width),
  heights = unit(5, "cm")
)

axis_row <- gtable_add_grob(axis_row, x_axis_shifted, t = 1, l = 1)
axis_row <- gtable_add_grob(axis_row, nullGrob(), t = 1, l = 2)

# --- Optional spacer row ---
spacer <- gtable(widths = unit(1, "null"), heights = unit(3, "mm"))

# --- Assemble all parts vertically ---
final_plot <- arrangeGrob(
  grobs = c(combined_rows_bordered, list(spacer), list(axis_row)),
  ncol = 1,
  heights = unit.c(
    rep(unit(10, "cm"), length(combined_rows_bordered)), #rowheight
    unit(10, "mm"), #spacer
    unit(3, "cm") #axis height
  )
)


# Draw the final plot

tiff(filename=filename, width=1150, height=2000)
grid.newpage()
pushViewport(viewport(clip = "off"))
grid.draw(final_plot)
dev.off()
}
