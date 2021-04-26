# library with the rect function
library(graphics)

# library to degrade color when overlapping points
library(scales)

# library with many color palettes
library(RColorBrewer)

# library with many color palettes
library(colorspace)

# library to plot in 3D
library(rgl)

# library to plot the matrices
library(corrplot)

# function to plot the tree when we have 1 or two predictors
miss_tree.plot <- function(tree,
                         y_name = NULL,
                         in2D = TRUE,
                         pred = TRUE,
                         round_plot = 2,
                         cex_plot = 1.2,
                         color_text = 'darkred',
                         color_border = 'gray40',
                         eps_x = 0.1,
                         eps_y = 0.1,
                         add = TRUE,
                         fill_color = TRUE,
                         fill_palette = NULL,
                         point_color = TRUE,
                         point_palette = NULL,
                         point_breaks = NULL,
                         n1 = 20,
                         n2 = 20,
                         rad = 1){
  
  #About the parameters
  
  # in2D:          just when p=2, should be plotted in 2D? else in 3D
  # pred:          when in2D, should add the predicted value of y?
  # round_plot:    when pred, round the predicted value of y
  # cex_plot:      when pred, cex the predicted value of y
  # color_text:    color of the regression function/predicted value of y
  # color_border:  color border of the final partition
  # epsilon:       if necessary to add margin to the plot, how much?
  # add:           should the data points be added?
  # fill_color:    when p=2, and 2D, fill the partitions?
  # fill_palette:  when fill_color, palette color to fill the partitions
  # point_color:   when p=1, points with different color acording to y values?
  # point_palette: if point_color, palette color to fill the points
  # point_breaks:  if point_color, number of breaks in the palette
  
  # tree must be an object created with my_regTree
  X_space <- tree$X_space
  p       <- dim(X_space)[2]
  
  if(p!=1 && p!=2){stop('predictor dimension is not 1 or 2')}
  
  X       <- tree$X.Partition
  y       <- tree$y.Partition
  y_pred  <- tree$y.Predict
  Pfinal  <- tree$Partition
  X.lab   <- X_space %>% colnames
  
  if(is.null(y_name)){y_name <- 'y'}
  
  if(p==1){
    x_plot       <- unlist(X)
    y_plot       <- unlist(y)
    y_pred_plot  <- unlist(y_pred)
    P_plot       <- unlist(Pfinal)
    
    
    x_min <- X_space[1,1]
    x_max <- X_space[2,1]
    
    if(x_min==min(x_plot)){x_min <- x_min-eps_x}
    if(x_max==max(x_plot)){x_max <- x_max+eps_x}
    
    y_min <- min(y_plot)
    y_max <- max(y_plot)
    
    if(add){
      
      df <- cbind(x_plot, y_plot) %>% as.data.frame
      l <- duplicated(df)
      
      if(point_color){
        
        if(is.null(point_palette)){
          point_palette <- brewer.pal(9,'Purples')
          if(length(point_palette)>3){
            point_palette <- point_palette[3:length(point_palette)] 
          }
        }
        
        if(is.null(point_breaks)){
          point_breaks <- length(y_plot)
        }else{
          if(length(y_plot)<point_breaks){
            point_breaks <- length(y_plot)}
        }
        
        my_colors   <- colorRampPalette(point_palette)
        colors      <- my_colors(point_breaks)[
          as.numeric(cut(y_plot,breaks = point_breaks))]
      }else{
        colors <- 'black'
      }
      
      if(sum(l)!=0){
        plot(x_plot, y_plot,
             xlim = c(x_min, x_max),
             ylim = c(y_min, y_max), pch=19,
             xlab = X.lab,
             ylab = y_name,
             col  = alpha(colors, 0.7))
      }else{
        plot(x_plot, y_plot,
             xlim = c(x_min, x_max),
             ylim = c(y_min, y_max), pch=19,
             xlab = X.lab,
             ylab = y_name,
             col  = colors)
      }
    }else{
      plot(x_plot, y_plot,
           xlim = c(x_min, x_max),
           ylim = c(y_min, y_max), pch=19,
           xlab = X.lab,
           ylab = y_name, type='n')
    }
    
    y_aux <- y_pred_plot %>% sort %>% unique
    
    if(fill_color){
      if(is.null(fill_palette)){
        fill_palette <- brewer.pal(9,'Purples')
        if(length(fill_palette)>5){
          fill_palette <- fill_palette[3:7]
        }
      }
      my_colors   <- colorRampPalette(fill_palette)
      sort_colors <- my_colors(length(y_aux))
    }else{
      sort_colors <- rep('black', length(y_pred_plot))
    }
    
    div <- c(0)
    
    for (i in 1:(length(P_plot)/2)){
      place <- which(y_aux==y_pred_plot[i])
      color <- sort_colors[place]
      
      segments(P_plot[2*i-1], y_pred_plot[i],
               P_plot[2*i], y_pred_plot[i], col = color, lwd=2)
      if (i ==1){
        vert <- P_plot[2*i-1]
        abline(v=vert, col=color_border, lty=2)
        div <- c(div, vert)
      }
      vert <- P_plot[2*i]
      if (!vert %in% div){
        abline(v=vert, col=color_border, lty=2)
        div <- c(div, vert)
      }
    }
  }
  
  if(p==2){
    x_plot <- X[[1]]
    leaf   <- rep(1,dim(X[[1]])[1])
    if(length(X)>1){
      for(i in 2:length(X)){
        x_plot <- rbind(x_plot, X[[i]])
        leaf   <- c(leaf, rep(i,dim(X[[i]])[1]))
      }
    }
    z_pred_plot <- round(y_pred, round_plot)
    z_plot <- unlist(y)
    
    df <- cbind(x_plot, z_plot)
    df <- distinct(df)
    
    x1_min <- X_space[1,1]
    x1_max <- X_space[2,1]
    x2_min <- X_space[1,2]
    x2_max <- X_space[2,2]
    
    notmiss1 <- !is.na(x_plot[,1])
    notmiss2 <- !is.na(x_plot[,2])
    
    x1_min_bis <- min(x_plot[notmiss1,1])
    x1_max_bis <- max(x_plot[notmiss1,1])
    x2_min_bis <- min(x_plot[notmiss2,2])
    x2_max_bis <- max(x_plot[notmiss2,2])
    
    if(x1_min==x1_min_bis){x1_min <- x1_min-eps_x}
    if(x1_max==x1_max_bis){x1_max <- x1_max+eps_x}
    if(x2_min==x2_min_bis){x2_min <- x2_min-eps_y}
    if(x2_max==x2_max_bis){x2_max <- x2_max+eps_y}
    
    if(in2D){
      
      # We draw the space
      plot(x_plot[notmiss1 & notmiss2,],
           xlim = c(x1_min, x1_max),
           ylim = c(x2_min, x2_max), pch=19,
           xlab = X.lab[1],
           ylab = X.lab[2], type='n')
      
      if(fill_color){
        if(is.null(fill_palette)){
          fill_palette <- brewer.pal(9,'Purples')
          if(length(fill_palette)>5){
            fill_palette <- fill_palette[1:5]
          }
        }
        
        y_aux <- z_pred_plot %>% sort %>% unique
        
        my_colors   <- colorRampPalette(fill_palette)
        sort_colors <- my_colors(length(y_aux))
      }
      
      # We draw the partition
      for(i in 1:length(Pfinal)){
        x0 <- Pfinal[[i]][1,1]
        y0 <- Pfinal[[i]][1,2]
        x1 <- Pfinal[[i]][2,1]
        y1 <- Pfinal[[i]][2,2]
        
        if(x0==x1_min_bis){x0 <- x1_min}
        if(y0==x2_min_bis){y0 <- x2_min}
        if(x1==x1_max_bis){x1 <- x1_max}
        if(y1==x2_max_bis){y1 <- x2_max}
        
        if(fill_color){
          place <- which(y_aux==z_pred_plot[i])
          color <- sort_colors[place]
          
          rect(x0, y0, x1, y1,
               border=color_border, lwd=2, lty=2,
               col=color)
        }else{
          rect(x0, y0, x1, y1,
               border=color_border, lwd=2, lty=2)
        }
      }
      
      if(add){
        if(point_color){
          
          if(is.null(point_palette)){
            point_palette <- brewer.pal(9,'Purples')
            if(length(point_palette)>3){
              point_palette <- point_palette[3:length(point_palette)] 
            }
          }
          
          if(is.null(point_breaks)){
            point_breaks <- length(z_plot)
          }else{
            if(length(z_plot)<point_breaks){
              point_breaks <- length(z_plot)}
          }
          
          my_colors <- colorRampPalette(point_palette)
          colors    <- my_colors(point_breaks)[as.numeric(cut(z_plot,
                                                              breaks = point_breaks))]
        }else{
          colors <- 'black'
        }
        
        l <- duplicated(x_plot[notmiss1 & notmiss2,])
        
        if(sum(l)!=0){
          points(x_plot[notmiss1 & notmiss2,],
                 xlim = c(x1_min, x1_max),
                 ylim = c(x2_min, x2_max), pch=19,
                 xlab = X.lab[1],
                 ylab = X.lab[2],
                 col  = alpha(colors[notmiss1 & notmiss2],0.7))
          
          if(sum(notmiss1 & notmiss2)!=nrow(x_plot)){
            if (sum(notmiss1)!=nrow(x_plot)){
              for (i in which(!notmiss1)){
                cell <- leaf[i]
                segments(Pfinal[[cell]][1,1], x_plot[i,2],
                         Pfinal[[cell]][2,1], x_plot[i,2],
                         lty=3, lwd=3,
                         col = alpha(colors[i],0.7))
              }
            }
            if (sum(notmiss2)!=nrow(x_plot)){
              for (i in which(!notmiss2)){
                cell <- leaf[i]
                segments(x_plot[i,1], Pfinal[[cell]][1,2],
                         x_plot[i,1], Pfinal[[cell]][2,2],
                         lty=3, lwd=3,
                         col = alpha(colors[i],0.7))
              }
            }
          }
          
        }else{
          points(x_plot[notmiss1 & notmiss2,],
                 xlim = c(x1_min, x1_max),
                 ylim = c(x2_min, x2_max), pch=19,
                 xlab = X.lab[1],
                 ylab = X.lab[2],
                 col=colors[notmiss1 & notmiss2])
          
          if(sum(notmiss1 & notmiss2)!=nrow(x_plot)){
            if (sum(notmiss1)!=nrow(x_plot)){
              for (i in which(!notmiss1)){
                segments(x1_min, x_plot[i,2],
                         x1_max, x_plot[i,2],
                         lty=3, lwd=3,
                         col = colors[!notmiss1])
              }
            }
            if (sum(notmiss2)!=nrow(x_plot)){
              for (i in which(!notmiss2)){
                segments(x_plot[i,1], x2_min,
                         x_plot[i,1], x2_max,
                         lty=3, lwd=3,
                         col = colors[!notmiss2])
              }
            }
          }
          
        }
      }
      
      if(pred){
        for(i in 1:length(Pfinal)){
          center <- c(c(Pfinal[[i]][1,1],Pfinal[[i]][2,1]) %>% mean,
                      c(Pfinal[[i]][1,2],Pfinal[[i]][2,2]) %>% mean)
          text(center[1], center[2],
               labels = z_pred_plot[i], col=color_text, cex=cex_plot)
        }
      }
      
    }else{
      
      # Hacer dibujo en 3D
      aux <- function(x,y){
        df <- cbind(x,y)
        df <- as.data.frame(df)
        miss_tree.pred(df,tree)$y_pred}
      
      f <- Vectorize(aux, vectorize.args = c("x", "y"))
      
      X1_seq <- seq(from=min(x_plot[notmiss1,1]),
                    to=max(x_plot[notmiss1,1]),length=n1)
      X2_seq <- seq(from=min(x_plot[notmiss2,2]),
                    to=max(x_plot[notmiss2,2]),length=n2)
      
      Z <- outer(X1_seq, X2_seq, f)
      z_pred <- round(y_pred,3)
      
      if(is.null(fill_palette)){
        fill_palette <- brewer.pal(9,'Purples')
        if(length(fill_palette>5)){
          fill_palette <- fill_palette[6:length(fill_palette)]
        }
      }
      
      my_colors   <- colorRampPalette(fill_palette)
      sort_colors <- my_colors(length(z_pred))
      
      colorlevels <- rep(0,n1*n2)
      
      for (i in 1:length(z_pred)){
        places <- which(Z==z_pred[i])
        colorlevels[places] <- sort_colors[i]
      }
      
      open3d()
      persp3d(X1_seq, X2_seq, Z,
              color=colorlevels,
              alpha=0.9,
              xlab = X.lab[1],
              ylab = X.lab[2],
              zlab = y_name)
      
      if(add){
        if(point_color){
          
          if(is.null(point_palette)){
            point_palette <- brewer.pal(9,'Purples')
            if(length(point_palette)>5){
              point_palette <- point_palette[6:length(point_palette)] 
            }
          }
          
          if(is.null(point_breaks)){
            point_breaks <- length(z_plot)
          }else{
            if(length(z_plot)<point_breaks){
              point_breaks <- length(z_plot)}
          }
          
          my_colors <- colorRampPalette(point_palette)
          colors    <- my_colors(point_breaks)[as.numeric(cut(z_plot,
                                                              breaks = point_breaks))]
        }else{
          colors <- 'black'
        }
        
        l <- which(notmiss1 & notmiss2)
        for(i in l){
          pch3d(x=x_plot[i,1],y=x_plot[i,2],z=z_plot[i],
                pch=21, bg=colors[i], radius=rad)
        }
      }
    }
  }
}

miss_RF.plot <- function(forest,
                       y_name = NULL,
                       eps_x = 0.1,
                       eps_y = 0.1,
                       add = TRUE,
                       fill_color = TRUE,
                       fill_palette = NULL,
                       point_color = TRUE,
                       point_palette = NULL,
                       point_breaks = NULL,
                       n1 = 20,
                       n2 = 20,
                       rad = 1){
  
  #About the parameters
  
  # in2D:          just when p=2, should be plotted in 2D? else in 3D
  # pred:          when in2D, should add the predicted value of y?
  # round_plot:    when pred, round the predicted value of y
  # cex_plot:      when pred, cex the predicted value of y
  # color_text:    color of the regression function/predicted value of y
  # color_border:  color border of the final partition
  # epsilon:       if necessary to add margin to the plot, how much?
  # add:           should the data points be added?
  # fill_color:    when p=2, and 2D, fill the partitions?
  # fill_palette:  when fill_color, palette color to fill the partitions
  # point_color:   when p=1, points with different color acording to y values?
  # point_palette: if point_color, palette color to fill the points
  # point_breaks:  if point_color, number of breaks in the palette
  
  X_space <- forest$X_space
  p       <- dim(X_space)[2]
  
  if(p!=1 && p!=2){stop('predictor dimension is not 1 or 2')}
  
  X.lab   <- X_space %>% colnames
  
  if(is.null(y_name)){y_name <- 'y'}
  
  if(p==1){
    X <- forest$X
    y <- forest$y
    
    x_min <- X_space[1,1]-eps_x
    x_max <- X_space[2,1]+eps_x
    
    x0 <- seq(X_space[1,1], X_space[2,1], length = n1)
    y0 <- vector(length = n1)
    
    forest_trees <- forest$trees
    
    for (i in 1:n1){
      y0[i] = my_RF.pred(x = x0[i], trees = forest_trees)
    }
    
    y_min <- min(min(y0),min(y))
    y_max <- max(max(y0),max(y))
    
    if(add){
      
      df <- cbind(X, y) %>% as.data.frame
      l <- duplicated(df)
      
      if(point_color){
        
        if(is.null(point_palette)){
          point_palette <- brewer.pal(9,'Purples')
          if(length(point_palette)>3){
            point_palette <- point_palette[3:length(point_palette)] 
          }
        }
        
        if(is.null(point_breaks)){
          point_breaks <- length(y)
        }else{
          if(length(y)<point_breaks){
            point_breaks <- length(y)}
        }
        
        my_colors   <- colorRampPalette(point_palette)
        colors      <- my_colors(point_breaks)[as.numeric(cut(y,
                                                              breaks = point_breaks))]
      }else{
        colors <- 'black'
      }
      
      if(sum(l)!=0){
        plot(X[,1], y,
             xlim = c(x_min, x_max),
             ylim = c(y_min, y_max), pch=19,
             xlab = X.lab,
             ylab = y_name,
             col  = alpha(colors, 0.7))
      }else{
        plot(X[,1], y,
             xlim = c(x_min, x_max),
             ylim = c(y_min, y_max), pch=19,
             xlab = X.lab,
             ylab = y_name,
             col  = colors)
      }
    }else{
      plot(X[,1], y,
           xlim = c(x_min, x_max),
           ylim = c(y_min, y_max), pch=19,
           xlab = X.lab,
           ylab = y_name, type='n')
    }
    
    y_aux <- y0 %>% sort %>% unique
    
    if(fill_color){
      if(is.null(fill_palette)){
        fill_palette <- brewer.pal(9,'Purples')
        if(length(fill_palette)>5){
          fill_palette <- fill_palette[3:7]
        }
      }
      my_colors   <- colorRampPalette(fill_palette)
      sort_colors <- my_colors(length(y0))
    }else{
      sort_colors <- rep('black', length(y0))
    }
    
    segments(x0[-length(x0)],y0[-length(y0)],x0[-1L],y0[-1L],col=sort_colors)
    #points(x0, y0, col=sort_colors) 
  }
  if(p==2){
    X <- forest$X.keep #x_plot
    y <- forest$y.keep
    
    df <- cbind(X, y)
    df <- distinct(df)
    
    x_plot <- df[1:2]
    z_plot <- df[[3]]
    
    x1_min <- X_space[1,1]
    x1_max <- X_space[2,1]
    x2_min <- X_space[1,2]
    x2_max <- X_space[2,2]
    
    notmiss1 <- !is.na(X[,1])
    notmiss2 <- !is.na(X[,2])
    
    x1_min_bis <- min(X[notmiss1,1])
    x1_max_bis <- max(X[notmiss1,1])
    x2_min_bis <- min(X[notmiss2,2])
    x2_max_bis <- max(X[notmiss2,2])
    
    if(x1_min==x1_min_bis){x1_min <- x1_min-eps_x}
    if(x1_max==x1_max_bis){x1_max <- x1_max+eps_x}
    if(x2_min==x2_min_bis){x2_min <- x2_min-eps_y}
    if(x2_max==x2_max_bis){x2_max <- x2_max+eps_y}
    
    # Hacer dibujo en 3D
    aux <- function(x,y){
      df <- cbind(x,y)
      df <- as.data.frame(df)
      miss_RF.pred(df,forest)}
    
    f <- Vectorize(aux, vectorize.args = c("x", "y"))
    
    X1_seq <- seq(from=min(X[notmiss1,1]),
                  to=max(X[notmiss1,1]),length=n1)
    X2_seq <- seq(from=min(X[notmiss2,2]),
                  to=max(X[notmiss2,2]),length=n2)
    
    Z <- outer(X1_seq, X2_seq, f)
    
    if(is.null(fill_palette)){
      fill_palette <- brewer.pal(9,'Purples')
      if(length(fill_palette>5)){
        fill_palette <- fill_palette[6:length(fill_palette)]
      }
    }
    
    my_colors   <- colorRampPalette(fill_palette)
    sort_colors <- my_colors(length(Z))
    
    colorlevels <- rep(0,n1*n2)
    
    for (i in 1:length(Z)){
      places <- which(Z==Z[i])
      colorlevels[places] <- sort_colors[i]
    }
    
    open3d()
    persp3d(X1_seq, X2_seq, Z,
            color=colorlevels,
            alpha=0.9,
            xlab = X.lab[1],
            ylab = X.lab[2],
            zlab = y_name)
    
    if(add){
      if(point_color){
        
        if(is.null(point_palette)){
          point_palette <- brewer.pal(9,'Purples')
          if(length(point_palette)>5){
            point_palette <- point_palette[6:length(point_palette)] 
          }
        }
        
        if(is.null(point_breaks)){
          point_breaks <- length(z_plot)
        }else{
          if(length(z_plot)<point_breaks){
            point_breaks <- length(z_plot)}
        }
        
        my_colors <- colorRampPalette(point_palette)
        colors    <- my_colors(point_breaks)[as.numeric(cut(z_plot,
                                                            breaks = point_breaks))]
      }else{
        colors <- 'black'
      }
      
      l <- which(notmiss1 & notmiss2)
      for(i in l){
        pch3d(x=x_plot[i,1],y=x_plot[i,2],z=z_plot[i],
              pch=21, bg=colors[i], radius=rad)
      }
    }
  }
}

matrix.plot <- function(matrix, palette = NULL,
                        point_breaks =  NULL,
                        groups = NULL,
                        main = NULL,
                        mar = c(1,1,2,1)){
  
  if (is.null(palette)){
    palette <- colorRampPalette(c("#fcfbfd","white","#3f007d"))
    if(is.null(point_breaks)){point_breaks <- 65}
    
    colors <- palette(point_breaks)
  }
  else{
    colors <- palette
  }
  
  if(is.null(groups)){
    corrplot(matrix, method = "color", col = colors,
             tl.pos = 'n', is.corr = FALSE, cl.lim = c(-0.001,1.001),
             main = main, mar = mar)
  }
  else{
    corrplot(matrix, method = "color", col = colors,
             tl.pos = 'n', is.corr = FALSE, cl.lim = c(-0.001,1.001),
             main = main, mar = mar)
    corrRect(c(50,50,50), col = "black", lwd = 2)
  }
  
}


