require(lidR)
require(ggplot2)

tree1 = read.csv("profile_tree1.csv")
t3 = ggplot(tree1) + geom_line(aes(dist, dtm), colour = "black") +
 geom_line(aes(dist, dsm), colour = "gray") + geom_vline(xintercept = 263.69, colour = "red") +
 theme_bw() + ylab("Elevation (m)") + 
 xlab("Horizontal distance (m)") +
 annotate("text", x = 50, y = 210, label = "North", size = 2) +
 annotate("text", x = 600, y = 210, label = "South", size = 2) +
 theme(legend.background = element_blank(), 
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  text=element_text(size=8, family="sans"),
  axis.text.x = element_text(size = 8))

tree2 = read.csv("profile_tree2.csv")
t4 = ggplot(tree2, aes(dist, height)) + geom_line(aes(dist, dtm), colour = "black") +
 geom_line(aes(dist, dsm), colour = "gray") + geom_vline(xintercept = 709.08, colour = "red") +
 theme_bw() + ylab("Elevation (m)") + xlab("Horizontal distance (m)") +
 annotate("text", x = 30, y = 210, label = "West", size = 2) +
 annotate("text", x = 1120, y = 210, label = "East", size = 2) +
 theme(legend.background = element_blank(), 
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  text=element_text(size=8, family="sans"),
  axis.text.x = element_text(size = 8))

jpeg("webfigure 3.jpg", width = 15, height = 11.5, units = "cm", res = 300)
plot_grid(t3, t4, labels = c("(a)", "(b)"), nrow=2, align = "v", label_size = 9, label_fontfamily = "sans")
dev.off()

plot.las = readLAS("tree25.las")
plot.las = plot.las@data

 
plot.las$Z = plot.las$Z - 217.27
plot.las[plot.las$Z <= 0.01,'Z'] = 0.01
plot.las$Znorm = plot.las$Z
gmax = max(plot.las$Z)
bin = 0.02 * gmax
write.csv(plot.las, "tree coordinates.csv")

plot.las$Znorm = plot.las$Znorm-3.54

g1d = ggplot(plot.las, aes(X, Znorm)) + 
 #geom_point(alpha = 1/10, colour = "black", size = 0.8) +
 geom_point(colour = "darkgreen", size = 0.8) +
 xlab("X coordinate") + ylab("Height (m)") +
 ylim(0, 100) +
	#scale_x_continuous(label=comma, breaks = scales::pretty_breaks(n = 2)) +
  theme(legend.background = element_blank(), 
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  text=element_text(size=8, family="sans"),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8))

jpeg("the tree.jpg", width = 10, height = 14, units = "cm", res = 300)
  print(g1d)
dev.off() 

g1hist = ggplot(plot.las, aes(Znorm)) +
 geom_histogram(binwidth = bin, fill = "black") + coord_flip() +
 xlab("Height (m)") + ylab("Frequency") +
 xlim(0, 100) +
 theme(legend.background = element_blank(), 
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  text=element_text(size=8, family="sans"),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8))

jpeg("figure 1.jpg", width = 15, height = 11.5, units = "cm", res = 300)
  plot_grid(g1d, g1hist, labels = c("(a)", "(b)"), label_size = 9, label_fontfamily = "sans")
dev.off() 
