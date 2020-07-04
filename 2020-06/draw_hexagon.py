import math
from PIL import Image, ImageDraw

im = Image.new('RGB', (1000, 1000), (256, 256, 256))
draw = ImageDraw.Draw(im)
offset = 500

def ring(r, aoff, lines):
    x_old, y_old = 2 * r, 0
    x_old = math.cos(aoff) * 2 * r
    y_old = math.sin(aoff) * 2 * r

    for n in range(1,7):
        angle = n * 2 * math.pi / 6
        x = math.cos(angle + aoff) * 2 * r
        y = math.sin(angle + aoff) * 2 * r
        if lines == True:
            draw.line((offset + x, offset + y, offset + x_old, offset + y_old), fill=(0, 0, 0), width=1)
            draw.line((offset + x, offset + y, offset, offset), fill=(0, 0, 0), width=1)
        draw.ellipse((offset + x-r, offset + y-r, offset + x+r, offset + y+r), outline=(0, 0, 0), width=3)
        x_old, y_old = x, y
    return 1

def main():
    r = 100
    k = 0.386106105
    draw_hex = True
    draw.ellipse((offset - (3*r), offset -(3*r), offset +(3*r), offset +(3*r)), outline=(0, 0, 0), width=3)
    ring(r, 0, False)   # r, angle_offset, radial 0 or 1
    ring(r* k**1, math.pi/6, draw_hex)
    if not draw_hex:
        ring(r* k**2, 0, draw_hex)
        ring(r* k**3, math.pi/6, draw_hex)
        ring(r* k**4, 0, draw_hex)
        ring(r* k**5, math.pi/6, draw_hex)
        ring(r* k**6, 0, draw_hex)
    im.save('janestreet_june_2020.jpg', quality=95)
    return 1

main()
