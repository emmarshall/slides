library(ggplot2)
library(ggimage)
library(cropcircles)
library(geomtextpath)
library(patchwork)

plot_image_label<-function(image,
                           label,
                           font_color="black", 
                           position="top",
                           hjust=0.2){
  
  #crop the image into a circle shape
  cropped_image = cropcircles::circle_crop(image)
  
  t = seq(0, 1, length.out = 100) * pi
  
  #set up params based on top or bottom
  if(position=="top"){
    data = data.frame(x = cos(t),y = sin(t))
    vjust=1.1
    ymax=1.2
    ymin=-0.9}
  
  else if(position=="bottom"){
    data=data.frame(x = cos(t),y = sin(t)*-1)
    vjust=-0.1
    ymax=0.9
    ymin=-1.2}
  
  #plot
  ggplot() +
    geom_image(aes(x=0, y=0, image = cropped_image), asp=2.4/2.1, size=.7) +
    scale_x_continuous(limits = c(-1.2, 1.2))+
    scale_y_continuous(limits=c(ymin, ymax))+
    geom_textpath(data = data, aes(x,y,label =label), linecolor=NA, color=font_color,
                  size = 14.5,  fontface="bold", vjust = vjust, hjust=hjust)+
    coord_equal()+
    theme_void()
}


#images
image_url<-"https://news.harvard.edu/gazette/story/2018/07/constitutions-wide-latitude-for-a-president-causing-major-legal-headaches/"


#make labeled plots
plot_image_label(image=image_url, label="Federal Constitution")



#save image