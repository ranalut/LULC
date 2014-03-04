i tried the method with xlim and ylim and it works when i just plot my map alone. What i try to do is to insert this map in my animation. Here is the code of my animation : 

pres.ani <- function() 
{ 
for (i in 1:length(unique(d$an))) { 
image(res[,,i],xlab='',ylab='',main=unique(d$an)[i]) 
contour(res[,,i],add=T) 
#points(d3[[i]]) 
Sys.sleep(.5) 
} 

} 
ani.start(interval=.5,title = "movie presence lynx", outdir = getwd()) 

pres.ani() 
ani.stop() 

res[,,i] is my KDE. When i create this animation, the x and y axes range of the window is 0:1. How can i put my map (coordinate system : Lambert II) as a background  in this animation ? 
I hope it's enough clear... 
