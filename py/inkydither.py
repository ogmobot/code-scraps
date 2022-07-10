import inky
import PIL.Image
import hitherdither
import mtginspirational as mtgi
import random
import subprocess

IS_INKY = False

if IS_INKY:
    display = inky.auto(verbose=True)
else:
    display = inky.inky_uc8159.Inky()

def dither(orig, display):
    img = PIL.Image.new("RGB", orig.size)
    img.paste(orig)
    saturation = 0.5
    thresholds = [64, 64, 64]
    palette = hitherdither.palette.Palette(display._palette_blend(saturation, dtype='uint24'))

    dithered = hitherdither.ordered.bayer.bayer_dithering(img, palette, thresholds, order=8)
    #dithered = hitherdither.ordered.cluster.cluster_dot_dithering(img, palette, thresholds, order=8)
    #dithered = hitherdither.diffusion.error_diffusion_dithering(img, palette, method="stucki", order=2) # unoptimized!
    return dithered.convert("RGBA")

if random.choice([True, False]):
    img_alpha = mtgi.random_flavour_text()
else:
    img_alpha = mtgi.random_fortune()

img = PIL.Image.new("RGB", img_alpha.size)
img.paste(img_alpha)
img = dither(img, display)

if IS_INKY:
    display.set_image(img.convert("P"))
    display.show()
else:
    img.convert("P").show()
