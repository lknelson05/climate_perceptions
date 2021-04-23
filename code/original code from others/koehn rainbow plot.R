# you set the ranges for your X and Y axes
# this makes vectors of values from your min x or y to your max x or y
sensrange <- seq(0,1, length.out = 100)
exprange <- seq(0,1, length.out = 100)

riskrange <- expand.grid(x = exprange, y = sensrange)

ranges = data.frame(exprange, sensrange, sqrt(exprange^2 + sensrange^2))
# the last part (the sqrt part) needs to be whatever you calculation of risk is - i.e. how are you combining your x and y
colnames(ranges) = c("x", "y", "z")
xColor <- seq(0,1,length.out=100) # scale of color for x and y,
yColor <- seq(0,1,length.out=100)
df <- cbind(expand.grid(x=xColor, y=yColor), expand.grid(x=exprange, y=sensrange)) #grid for colors
colnames(df)<-c("xColor","yColor","x2","y2")
df$Risk <- sqrt(df$xColor^2+df$yColor^2) # the color factor for radius - again here should be however you're combining your x and y

library(ggplot2)
library(grid)
library(RColorBrewer)
library(ggrepel)
cols = brewer.pal(9, "Spectral") # or whatever color palette you want to use
FMPmatrix = data.frame(FMPmatrix) # this was my species risk data so just make sure your data is in a data frame

# plotting - key thing here is sometimes you are calling your data frame (mine is "FMPmatrix") and sometimes your calling the data frame of colors ("df")
p1 <- ggplot() +
  xlab("") + ylab("") + ggtitle("IPSL model") +
  geom_raster(data =df, aes(x = x2, y = y2,fill = Risk))+
  scale_fill_gradientn(colours =rev(cols)) +
  geom_text_repel(data = FMPmatrix, aes(x = ipsl_exp, y = ipsl_sens,
                                        label = rownames(FMPmatrix)), size = 4, fontface = "bold")
#geom_text_repel is if you want labels. If you just want points, you'll need to use something like geom_point(data = FMPmatrix, aes(x = ipsl_exp, y = ipsl_sens))
p1

I ended up using the a color palette from the scico package because they are color blind friendly
devtools::install_github("thomasp85/scico")
library(scico)
cols = scico(9, palette = 'roma') #this is kind of a rainbowy palette