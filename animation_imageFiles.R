
### Create new directory and store all image files you want to put in the animation
### You must not have any other image files in the directory.
setwd("Y://New folder")


# convert the image (.tif) files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
### convert no longer used in ImageMagick.
### Use magick instead.
### https://stackoverflow.com/questions/38163849/imagemagick-issue-with-windows-and-convert-function

system("magick -delay 80 *.tif example_1.gif")

# to not leave the directory with the single png files
# I remove them.
file.remove(list.files(pattern=".png"))

### The created .gif file is the animation file.