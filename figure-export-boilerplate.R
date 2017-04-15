# A few useful lines to export plots
# 1. base R graph
png("../../plots/.png", 5.5, 4, units='in', pointsize=9, res=600)
dev.off()

# 2. single ggplot
ggsave("../../plots/.png", width=5.5, height=4, units='in', dpi=600)

# 3. plot matrix from library(gridExtra)
g <- arrangeGrob(p1, p2, p3, p4, ncol=2)
ggsave("../../plots/.png", g, width=5.5, height=4, units='in', dpi=600)
