import sys
import os
import PIL
from PIL import Image
import math

folderPath = sys.argv[1]
newFolderPath = sys.argv[2]

path = "./%s/" % folderPath
newpath = "./%s/" % newFolderPath


for filename in os.listdir(path):
    if filename != ".DS_Store":
        try:
            if not os.path.exists(newpath):
                os.makedirs(newpath)
            file = newpath + filename.split('.')[0]+'.1.png', "png"
            img = Image.open(path + filename)
            width, height = img.size
            crop_img1 = img.crop((40,80, 1040,390))
            crop_img2 = img.crop((40,390, 1040,700))
            crop_img3 = img.crop((40,700, 1040,1010))

            crop_img1 = crop_img1.save(newpath + filename.split('.')[0]+'.1.png', format='png')
            crop_img2 = crop_img2.save(newpath + filename.split('.')[0]+'.2.png', format='png')
            crop_img3 = crop_img3.save(newpath + filename.split('.')[0]+'.3.png', format='png')
        except:
            continue
        