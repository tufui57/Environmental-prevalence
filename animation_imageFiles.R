
### Create new directory and store all image files you want to put in the animation
### You must not have any other image files in the directory.
setwd("Y://New folder//global map")

list.files()

# convert the image (.tif) files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
### convert no longer used in ImageMagick.
### Use magick instead.
### https://stackoverflow.com/questions/38163849/imagemagick-issue-with-windows-and-convert-function

system("magick -delay 80 *.png example_1.gif")

### The created .gif file is the animation file.


dir.create("examples")
setwd("examples")

# example 1: simple animated countdown from 10 to "GO!".
png(file="example%02d.png", width=200, height=200)
for (i in c(10:1, "G0!")){
  plot.new()
  text(.5, .5, i, cex = 6)
}
dev.off()

list.files()
# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system("magick -delay 80 *.png example_1.gif")


