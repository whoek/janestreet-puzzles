#
# Program used for Jane Street Puzzle - 2020-02
# willemhoek.com
#
import math

def hit(x, y):
    return (x < 0 and x > -1 and y > 0 and y < 1) or \
           (x > 1 and x <  2 and y > 0 and y < 1) or \
           (y < 0 and y > -1 and x > 0 and x < 1) or \
           (y > 1 and y <  2 and x > 0 and x < 1)

def pol2cart(rho, phi):
    x = rho * math.cos(phi)
    y = rho * math.sin(phi)
    return(x, y)

SIDE_INC = 200
RAD_STEP = 200
TOTAL = SIDE_INC * SIDE_INC * RAD_STEP 
PI2 = 2 * 3.1415926535897931

rpho = 0
end = 1.11
while rpho < end:
    hit_count = 0
    total_count = 0
    for x in range(0,SIDE_INC):
        x_start = x / SIDE_INC
        for y in range(0,SIDE_INC):
            y_start = y / SIDE_INC
            for s in range(0, RAD_STEP):
                sigma = PI2 * s / RAD_STEP
                xx, yy = pol2cart(rpho,sigma )
                x_end = x_start + xx
                y_end = y_start + yy
                total_count += 1
                if hit(x_end,y_end):
                    hit_count += 1

    print(rpho, hit_count / total_count,hit_count, total_count)
    rpho += 0.1
