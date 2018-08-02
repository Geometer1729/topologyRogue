import cv2
import argparse
import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--file",required=True,help="Filepath to image")
args = vars(parser.parse_args())

filepath = args["file"]
im = cv2.imread(filepath)

def getRGBfromI(RGBint):
    blue =  RGBint & 255
    green = (RGBint >> 8) & 255
    red =   (RGBint >> 16) & 255
    return red, green, blue

def getIfromRGB(rgb):
    red = rgb[0]
    green = rgb[1]
    blue = rgb[2]
    RGBint = (red<<16) + (green<<8) + blue
    return RGBint

def colorEqual(a,b):
    #a and b are tuples of (b,g,r)
    bdist = abs(a[0]-b[0])
    gdist = abs(a[1]-b[1])
    rdist = abs(a[2]-b[2])
    return (bdist + gdist + rdist) < 10


newshape = im.shape[0]*im.shape[1]

newim = im.reshape(newshape,3)

colors = []

colorset = set()

for c in newim:
    if (c == np.array([255,255,255])).all():
        continue
    else:    
        i = getIfromRGB(c)
        colorset.add(i)

for c in colorset:
    colors.append(getRGBfromI(c))

realColors = []
for c in colors:
    add = True
    for cother in realColors:
        if colorEqual(c,cother):
            add = False
    if add:
        realColors.append(c)

#Nice, now we have the real colors!
#we gon contour this bitch now

#secret tool for later
finalcode = filepath[filepath.rfind("/")+1:-4] + "= ["
#first, bitmask
for color in realColors:
    #compute bound for color
    bound = np.array(color)
    #apply mask
    mask = cv2.inRange(im,bound-10,bound+10)
    res = cv2.bitwise_and(im,im,mask=mask)
    #make black and white image of shape for contour
    bw = cv2.cvtColor(res,cv2.COLOR_BGR2GRAY)
    _,conts,h = cv2.findContours(bw,cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    c = max(conts, key=cv2.contourArea)
    #c is the contour we want
    #now to make it workable
    epsilon = 0.01 * cv2.arcLength(c,True)
    poly = cv2.approxPolyDP(c,epsilon,True)
    print(poly)
    #woohoo, polygon!
    #code gen time, strap in boys
    code = "(Pol ["
    color = "makeColorI " + str(color[2]) + " " + str(color[1]) + " " + str(color[0]) + " 255"
    for p in poly:
        point = p[0]
        x = point[0]
        y = point[1]
        code += "("+str(x)+","+str(y)+"),"
    code = code[:-1] + "],"
    code += color + ")"
    #print(code)
    finalcode += code + ","
finalcode = finalcode[:-1] + "]"
print(finalcode)
    
