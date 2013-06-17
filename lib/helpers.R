helper.function <- function()
{
  return(1)
}


.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by)) { out <- out[order(out[[order.by]], decreasing=decreasing), ] }
    if (head) { out <- head(out, n) }
    out
}

lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, 
                head=TRUE, n=n)
}







library(ggplot2)
tema <- theme_bw() 
theme_set(tema)
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
		"#CC79A7", '#000000' ,'#CCCC99')

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
col.fill <-  scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
col.ptos <-   scale_colour_manual(values=cbPalette)


coord.etiquetas <- function(df, x, y, 
    etiq = "etiq", size = 3.5) {
        plot(df[, x], df[, y])
        salida.plabel <- pointLabel(df[, x], df[, y], 
            df[, etiq], 
            doPlot = TRUE, 
            cex = 2*size/5, xpd=TRUE)  
        dev.off()
        df$a <- salida.plabel$x
        df$b <- salida.plabel$y
        df
}    


library(gridExtra)
tabla_grafica <- function(tablas.m){
  #tablas.m <- melt(tab, id.vars=c('var','dim'))
  
      dat.ann <- subset(tablas.m,var==unique(tablas.m$var)[1])
    num.cat <- length(unique(dat.ann$variable))
    num.reng <- length(unique(dat.ann$dim))
  dat.ann.1 <- dat.ann[1,,drop=FALSE]
  dat.ann.1$num.reng <- num.reng
  dat.ann.1$num.cat <- num.cat
  dat.ann.1$variable <- NULL
  dat.ann.1 <- data.frame(dat.ann.1, variable=unique(dat.ann$variable))
  #print(num.cat)
  #print(num.reng)
  if(length(unique(tablas.m$color))>1){
    paleta <- scale_color_brewer(palette='RdGy')
  } else {
    paleta <- scale_color_manual(values=c('gray80','black'))
  }
  tablas.m$variable <- factor(tablas.m$variable, levels=unique(dat.ann$variable))
  p <- ggplot(tablas.m, aes(y=dim,x=variable, label=value)) +
    geom_text(aes(colour=color)) + facet_grid(var~., scales='free_y',
      as.table=T, space='free_y') +
    geom_text(colour='black', alpha=0.4)+
    xlab('')+ ylab('')+
    theme(axis.text.y=element_text(size=12))+
   theme(text=element_text(size=12, family="Gill Sans MT"))+
    theme(title=element_text(size=12, family="Gill Sans MT"))+
    theme(#panel.grid=element_blank(),
    panel.border=element_blank(),
      axis.ticks=element_blank(),
      strip.background=element_blank(),
      legend.position='none',
      axis.text.x = element_blank(),
      panel.border=element_blank(),
      panel.margin=unit(1, 'lines'),
      strip.text.y=element_text(angle=90, colour='gray'),
      legend.background=element_blank()
      )+
    paleta +
  geom_text(data=dat.ann.1,aes(y=num.reng, x=1:num.cat, 
    label=as.character(unique(variable))),
    colour='gray20', vjust=-1.5, size=3.8,family="Gill Sans MT")  
  gt <- ggplot_gtable(ggplot_build(p))
 gt$layout$clip[gt$layout$name == "panel"] <- "off"
print(grid.draw(gt))
#p
}

corregir <- function(latex_table){
  printed_table = (latex_table)
printed_table = sub("backslash", "\\", printed_table)
printed_table = sub("\\\\}", "}", printed_table)
printed_table = sub("\\\\\\{", "{", printed_table)
printed_table = sub("\\$", "\\", printed_table)
printed_table = sub("\\$", "\\", printed_table)
  printed_table
}