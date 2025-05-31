#' Scatterplot matrices with ggplot
#'
#' This function creates Scatterplot matrices with ggplot.
#' With thanks to Gaston Sanchez, who posted this code online.
#' https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
#' @param data_matrix data.matrix
#' @keywords Scatterplot, matrices, ggplot
#' @export
#' @examples
#' create_pairs_plot()
create_pairs_plot <- function(data_matrix) {
  if (is.null(names(data_matrix))) {
    names(data_matrix) <- paste0("row", 1:nrow(data_matrix))
  }
  exp_grid <- expand.grid(x = 1:ncol(data_matrix), y = 1:ncol(data_matrix))
  exp_grid <- exp_grid[exp_grid$x != exp_grid$y,]
  all_panels <- do.call("rbind", lapply(1:nrow(exp_grid), function(i) {
    xcol <- exp_grid[i, "x"]
    ycol <- exp_grid[i, "y"]
    data.frame(xvar = names(data_matrix)[ycol],
               yvar = names(data_matrix)[xcol],
               x = data_matrix[, xcol],
               y = data_matrix[, ycol],
               data_matrix)
  }))
  all_panels$xvar <- factor(all_panels$xvar, levels = names(data_matrix))
  all_panels$yvar <- factor(all_panels$yvar, levels = names(data_matrix))
  densities <- do.call("rbind", lapply(1:ncol(data_matrix), function(i) {
    data.frame(xvar = names(data_matrix)[i],
               yvar = names(data_matrix)[i],
               x = data_matrix[, i])
  }))
  return(list(all = all_panels, densities = densities))
}


#' Plotting colour schemes
#'
#' Get nice plotting colour schemes for very general colour variables
#' @param plot_out
#' @param colour_by
#' @param colour_by_name
#' @param fill logical value
#' @keywords colour, schemes
#' @export
#' @examples
#' resolve_plot_colours()
resolve_plot_colours <- function(plot_out, colour_by, colour_by_name, fill = FALSE) {

  if (is.null(colour_by)) {
    return(plot_out)
  }

  # Picking whether to fill or not.
  if (fill) {
    VIRIDFUN <- viridis::scale_fill_viridis
    SCALEFUN <- scale_fill_manual
  } else {
    VIRIDFUN <- viridis::scale_color_viridis
    SCALEFUN <- scale_color_manual
  }

  # Set a sensible colour scheme and return the plot_out object
  if (is.numeric(colour_by)) {
    plot_out <- plot_out + VIRIDFUN(name = colour_by_name)
  } else {
    nlevs_colour_by <- nlevels(as.factor(colour_by))
    if (nlevs_colour_by <= 10) {
      plot_out <- plot_out + SCALEFUN(
        values = get_color_palette("tableau10medium"),
        name = colour_by_name)
    } else {
      if (nlevs_colour_by > 10 && nlevs_colour_by <= 20) {
        plot_out <- plot_out + SCALEFUN(
          values = get_color_palette("tableau20"),
          name = colour_by_name)
      } else {
        plot_out <- plot_out + VIRIDFUN(
          name = colour_by_name, discrete = TRUE)
      }
    }
  }
  return(plot_out)
}


#' Define color palettes
#'
#' Function to define color palettes.
#' @param palette_name
#' @keywords palette, color
#' @export
#' @examples
#' get_color_palette()
get_color_palette <- function(palette_name) {
  switch(palette_name,
         tableau20 = c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C",
                       "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5",
                       "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F",
                       "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5"),
         tableau10medium = c("#729ECE", "#FF9E4A", "#67BF5C", "#ED665D",
                             "#AD8BC9", "#A8786E", "#ED97CA", "#A2A2A2",
                             "#CDCC5D", "#6DCCDA"),
         colorblind10 = c("#006BA4", "#FF800E", "#ABABAB", "#595959",
                          "#5F9ED1", "#C85200", "#898989", "#A2C8EC",
                          "#FFBC79", "#CFCFCF"),
         trafficlight = c("#B10318", "#DBA13A", "#309343", "#D82526",
                          "#FFC156", "#69B764", "#F26C64", "#FFDD71",
                          "#9FCD99"),
         purplegray12 = c("#7B66D2", "#A699E8", "#DC5FBD", "#FFC0DA",
                          "#5F5A41", "#B4B19B", "#995688", "#D898BA",
                          "#AB6AD5", "#D098EE", "#8B7C6E", "#DBD4C5"),
         bluered12 = c("#2C69B0", "#B5C8E2", "#F02720", "#FFB6B0", "#AC613C",
                       "#E9C39B", "#6BA3D6", "#B5DFFD", "#AC8763", "#DDC9B4",
                       "#BD0A36", "#F4737A"),
         greenorange12 = c("#32A251", "#ACD98D", "#FF7F0F", "#FFB977",
                           "#3CB7CC", "#98D9E4", "#B85A0D", "#FFD94A",
                           "#39737C", "#86B4A9", "#82853B", "#CCC94D"),
         cyclic = c("#1F83B4", "#1696AC", "#18A188", "#29A03C", "#54A338",
                    "#82A93F", "#ADB828", "#D8BD35", "#FFBD4C", "#FFB022",
                    "#FF9C0E", "#FF810E", "#E75727", "#D23E4E", "#C94D8C",
                    "#C04AA7", "#B446B3", "#9658B1", "#8061B4", "#6F63BB")
  )
}

# TODO Vincent: think about adding parameters.
create_paired_pcs_plot <- function(pca_r_squared = pca_r_squared,
                                   pc_scores = pc_scores,
                                   variable = variable,
                                   independent_vars = independent_vars,
                                   theme_size = 14) {
  
  names(pca_r_squared) <- colnames(pc_scores)
  colnames(pc_scores) <- paste0(colnames(pc_scores), 
                                "\n(R-squared ",
                                formatC(signif(as.numeric(pca_r_squared), digits = 2),
                                        digits = 2,
                                        format = "fg",
                                        flag = "#"), 
                                ")")
  top5 <- order(pca_r_squared, decreasing = TRUE)[1:5]
  #top5 <- names(sort(pca_r_squared, decreasing = TRUE))[1:5]
  
  var_vals <- independent_vars[[variable]]
  colour_by <- NULL
  if (is.numeric(var_vals)) {
    # TODO: Vincent: Check what are good methods for discretization.
    # https://recipes.tidymodels.org/reference/discretize.html
    # https://www.math.uzh.ch/pages/varrank/reference/discretization.html
    colour_by <- varrank::discretization(data.df = var_vals,
                                         discretization.method = "fd",
                                         frequency=FALSE)[,1]
  } else {
    colour_by <- var_vals  
  }
  
  #df_to_expand <- as_tibble(pc_scores[, ..top5]) # data.table specific subsetting!
  df_to_expand <- pc_scores[, top5] 
  names(df_to_expand) <- colnames(df_to_expand)
  
  g <- create_pairs_plot(as.matrix(df_to_expand))
  
  df_to_plot_big <- data.frame(g$all, colour_by)
  
  p <- ggplot(df_to_plot_big,
             aes_string(x = "x",
                        y = "y")) +
    geom_point(aes_string(fill = "colour_by"),
               colour = "gray40",
               shape = 21,
               alpha = 0.65) +
    facet_grid(xvar ~ yvar, scales = "free") +
    stat_density(aes_string(x = "x",
                            y = "(..scaled.. * diff(range(x)) + min(x))"),
                 data = g$densities,
                 position = "identity",
                 colour = "grey20", geom = "line") +
    xlab("") +
    ylab("") +
    theme_bw(theme_size)
  
  final_p <- resolve_plot_colours(p,
                                  colour_by,
                                  get("variable"),
                                  fill = TRUE)
  
  return(final_p)
}

# TODO Vincent: think about adding parameters.
create_vars_pcs_plot <- function(pca_r_squared = pca_r_squared,
                                 pc_scores = pc_scores,
                                 variable = variable,
                                 dependent_vars = dependent_vars,
                                 independent_vars = independent_vars,
                                 theme_size = 14) {
  
  names(pca_r_squared) <- colnames(pc_scores)
  colnames(pc_scores) <- paste0(colnames(pc_scores), 
                                "\n(R-squared ",
                                formatC(signif(as.numeric(pca_r_squared), digits = 2),
                                        digits = 2,
                                        format = "fg",
                                        flag = "#"), 
                                ")")
  top6 <- order(pca_r_squared, decreasing = TRUE)[1:6]
  #top6 <- names(sort(pca_r_squared, decreasing = TRUE))[1:6]
  df_to_plot <- reshape2::melt(pc_scores[, top6])
  xvar <- independent_vars[[variable]]
  df_to_plot$xvar <- rep(xvar, 6)
  p <- ggplot(df_to_plot,
              aes_string(x = "xvar",
                         y = "value"),
              colour = "black") +
    facet_wrap(~Var2,
               nrow = 3,
               scales = "free_y") +
    xlab(variable) +
    ylab("Principal component value") +
    theme_bw(theme_size)
  typeof_x <- get_var_type(independent_vars, variable)
  if (typeof_x == "discrete") {
    p <- p + geom_violin(fill = "aliceblue",
                         colour = "gray60",
                         alpha = 0.6,
                        scale = "width") +
      geom_boxplot(width = 0.25, outlier.size = 0) +
      ggforce::geom_sina(shape = 1,
                         alpha = 0.6, 
                         width = 0.02,
                         color = "#4169E1")
    # if (ncol(dependent_vars) <= 150) {
    #   p <- p + geom_dotplot(fill = "gray10",
    #                         alpha = 0.6,
    #                         binaxis = "y",
    #                         stackdir = "center",
    #                         dotsize = 1)
    # }
  } else {
    p <- p + geom_point(fill = "gray10",
                        alpha = 0.6,
                        shape = 21) +
         stat_smooth(aes(group = 1), formula = y ~ x, method = "lm", alpha = 0.3)
  }
  return(p)
}

# Source: https://stackoverflow.com/questions/15505607/diagonal-labels-orientation-on-x-axis-in-heatmaps
draw_colnames_45 <- function(coln, gaps, ...) {
  coord = pheatmap:::find_coordinates(length(coln), gaps)
  x = coord$coord - 0.5 * coord$size
  res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = 1, rot = 45, gp = gpar(...))
  return(res)
}

draw_colnames_0 <- function(coln, gaps, ...) {
  coord = pheatmap:::find_coordinates(length(coln), gaps)
  x = coord$coord - 0.5 * coord$size
  res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 1.1, hjust = 0.5, rot = 0, gp = gpar(...))
  return(res)
}

## 'Overwrite' default draw_colnames with your own version 
assignInNamespace(x="draw_colnames", value="draw_colnames_0",
                  ns=asNamespace("pheatmap"))


create_pc_heatmap <- function(mat, pcs_use_val, comp) {
  # bottom, left, top, and right.
  par(mar=c(2.1,8.1,2.1,2.1))
  #custom_colors <- scales::alpha(colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
  #                                                  "#77AADD", "#4477AA"))(100), alpha = 0.7)
  custom_colors <- scales::alpha(colorRampPalette(c("blue","green","red","yellow"))(100), alpha = 0.4)
  
  title <- paste0("R-squared values * 100 (%) of variables for relevant ",comp,"s")
  # NMF::aheatmap(as.matrix(mat[, 1:pcs_use_val]*100), 
  #               fontsize = 14,
  #               txt = round(as.matrix(mat[, 1:pcs_use_val]*100), digits = 2),
  #               treeheight = 60,
  #               main = title)
  # NMF::aheatmap(as.matrix(mat[, 1:pcs_use_val]*100), 
  #               fontsize = 14,
  #               txt = round(as.matrix(mat[, 1:pcs_use_val]*100), digits = 2),
  #               treeheight = 60,
  #               legend=T,
  #               annLegend = F,
  #               color = custom_colors,
  #               breaks = seq(0, 100, length.out = 101),
  #               #breaks=101L,
  #               main = title)
  pheatmap::pheatmap(mat = as.matrix(mat[, 1:pcs_use_val]*100),
                     main = title, 
                     fontsize = 14,
                     display_numbers = round(as.matrix(mat[, 1:pcs_use_val]*100), digits = 2),
                     number_color = 'black',
                     breaks = seq(0, 100, length.out = 101),
                     color = custom_colors)
}

create_feature_corr_matrix <- function(mat) {
  custom_colors <- scales::alpha(colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                                    "#77AADD", "#4477AA"))(100), alpha = 0.7)
  corrplot::corrplot(mat,
                     number.digits=3, # Does the rounding of the cell values.
                     method="color",
                     title="Correlation of CFs through R-squared",
                     col=custom_colors,
                     type="upper",
                     order="hclust",
                     addCoef.col="black", # Add coefficient of correlation
                     addgrid.col='black',
                     addCoefasPercent=FALSE,
                     tl.col="black",
                     tl.srt=25, #Text label color and rotation
                     # Combine with significance
                     # p.mat = p.mat,
                     #sig.level=0.01,
                     insig="blank",
                     mar=c(0,0,2,0), # bottom, left, top, and right
                     tl.offset=1,
                     diag=FALSE)
}

create_paired_and_var_pc_plots <- function(dependent_vars=dependent_vars,
                                           independent_vars=independent_vars,
                                           pc_scores=pc_scores,
                                           features=features,
                                           r_squared_df=r_squared_df,
                                           file_path=file_path) {
  counter <- 0
  for (var in rownames(features)) {
    counter <- counter + 1
    png(paste0(file_path,'g',counter,'.png'), width=1200, height=1000)
    g <- create_paired_pcs_plot(pca_r_squared=r_squared_df[counter,],
                                pc_scores=as.matrix(pc_scores),
                                variable=var,
                                independent_vars=independent_vars)
    print(g)
    dev.off()
    png(paste0(file_path,'p',counter,'.png'), width=1200, height=1000)
    p <- create_vars_pcs_plot(pca_r_squared=r_squared_df[counter,],
                              pc_scores=as.matrix(pc_scores),
                              variable=var,
                              dependent_vars=dependent_vars,
                              independent_vars=independent_vars)
    print(p)
    dev.off()
  }
}  






