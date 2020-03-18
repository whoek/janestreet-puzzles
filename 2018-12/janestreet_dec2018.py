import time
import sys

sys.stdout.flush()

MAX_COUNT = 100000 * 1000000
counter = [0]
max_seq = [0]

# grid[row][col] => [allocated_value,product]
#   where product is True(1) or False (0)
grid = [[[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]],
        [[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]],
        [[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]],
        [[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]],
        [[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]],
        [[0,0],[0,0],[0,0],[1,0],[0,0],[0,0],[0,0]],
        [[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]]]


# values[n] => [value, rule, PLACED?]
#     where:  PLACED = 0 not yet / 1 YES
# rule 1 - region must be empty
# rule 2 - other cell is a 2
# rule 3 - other cell is a 3
# rule 4 - other cell is a 4

# 2,3,4  == good, 5--6x, 6--3x, 7--2x :
# score: 222  32M
values = [[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],
          [3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],
          [4,1,0],[4,1,0],
          [5,4,0],[5,4,0],[5,3,0],[5,3,0],[5,2,0],[5,2,0],
          [4,3,0],[4,3,0],[4,3,0],[4,3,0],[4,3,0],
          [6,2,0],[6,2,0],[6,2,0],
          [7,2,0],[7,2,0]]

# 221   -- 8 M
values = [[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],
          [2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],
          [4,1,0],[4,1,0],
          [6,2,0],[6,2,0],[7,2,0],
          [5,4,0],[5,4,0],[5,3,0],[5,3,0],[5,2,0],[5,2,0],
          [4,3,0],[4,3,0],[4,3,0],[4,3,0],[4,3,0],
          [6,2,0],[6,2,0]]

# 220 -- 35M
values = [
    [2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],
          [3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],
          [4,1,0],[4,1,0],
          [6,2,0],[6,2,0],[6,2,0],
          [5,4,0],[5,4,0],[5,3,0],[5,3,0],[5,2,0],[5,2,0],
          [4,3,0],[4,3,0],[4,3,0],[4,3,0],[4,3,0],
          [6,2,0],[6,2,0]]

#print("2,3,4, 5=>4x,6=2X ") ==35M
#Print("2,3,4, 5=>4x,6=4X ") != 200M no solution
#print("2,3,4, 5=>4x,6=3X ") !== 94 M

values = [[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],
          [3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],
          [4,1,0],[4,1,0],
          [6,2,0],[6,2,0],[6,2,0],[6,2,0],
         [4,3,0],[4,3,0],[4,3,0],[4,3,0],[4,3,0],
           [5,4,0],[5,4,0],[5,3,0],[5,3,0],[7,2,0],
          [7,2,0],[7,2,0]]

print("perfect -- run forever") 
values = [[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],[3,1,0],
          [2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],[2,1,0],
          [4,1,0],[4,1,0],
          [6,2,0],[6,2,0],[4,3,0],
          [6,2,0],[5,2,0],[4,3,0],
          [6,2,0],[5,4,0],[4,3,0],
          [6,4,0],[5,3,0],[4,3,0],
          [5,3,0],[5,2,0],[4,3,0],
          [5,2,0]]



# region[row][col] => region
region = [[1,2,2,3,3,4,4],
          [1,1,2,5,3,4,8],
          [6,6,5,5,7,8,8],
          [10,6,11,11,7,7,9],
          [10,10,12,11,13,9,9],
          [14,12,12,16,13,13,15],
          [14,14,16,16,16,15,15]]

# cells_in_region[region] => [row, col]
cells_in_region =[[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]]
for row in range(7):
    for col in range(7):
        the_region = region[row][col]
        cells_in_region[the_region].append([row,col])

def next_number(num,values):
    for i in range(len(values)):
        if values[i][2] == 0:
            num[0] =  values[i][0]
            num[1] =  values[i][1]
            num[2] =  i
            return True
    return False

def get_prod(grid,r,c):
    prod = 1
    reg = region[r][c]
    for [rr,cc] in cells_in_region[reg]:
        if grid[rr][cc][0] != 0:
            prod = prod * grid[rr][cc][0] 
    return prod

def print_grid(grid):
    total = 0
    # add PRODUCT
    for r in range(7):
        for c in range(7):
            if grid[r][c][1] == "X":
                total += grid[r][c][0]

    # PRINT
    print("")
    for r in range(7): 
        for c in range(7): 
            print("%3s" % grid[r][c][0], end='') 
            if c != 6 : print(",", end='') 
        print("")
    print("\nTotal = ",total)

# -------- checks ----------------

def empty(grid,row,col):
    if grid[row][col][0] == 0:
        return True
    return False

def row_ok(grid,row,num): 
    # for i in range(7):
    #     if(grid[row][i][0] == num):
    #         return False
    # if row == 0:
    #     the_rows = [0,1]
    # elif row == 6:
    #     the_rows = [5,6]
    # else:
    #     the_rows = [row-1,row,row+1]
    for the_row in range(7): #the_rows:
        for c in range(7):
            cv = grid[the_row][c][0]
            for cc in range(6,c,-1):
                ccv = grid[the_row][cc][0]
                if cv == ccv and cv != 0:
                    return False
    return True

def col_ok(grid,col,num): 
    # for i in range(7):
    #     if(grid[i][col][0] == num):
    #         return False
    # if col == 0:
    #     the_cols = [0,1]
    # elif col == 6:
    #     the_cols = [5,6]
    # else:
    #     the_cols = [col-1,col,col+1]
    for the_col in range(7):  #the_cols:
        for r in range(7):
            rv = grid[r][the_col][0]
            for rr in range(6,r,-1):
                rrv = grid[rr][the_col][0]
                if rv == rrv and rv != 0:
                    return False
    return True

# rule 1 - region must be empty
# rule 2 - other cell is a 2
# rule 3 - other cell is a 3
def region_ok(grid,row,col,num,rule):
    reg = region[row][col]
    tel = 0
    for [r,c] in cells_in_region[reg]:
        v = grid[r][c][0]
        if [r,c] != [row,col] and v != 1 and v != 0:
            the_one = grid[r][c][0]
            tel += 1
    if rule == 1:
        if not(tel  != 1):            
            return False
    elif rule == 2:
        if not(tel ==  1):
#        if not(tel ==  1 and the_one == 2):
            return False
    elif rule == 3:
        if not(tel == 1):
#       if not(tel == 1 and the_one == 3):
            return False
    elif rule == 4:
        if not(tel == 1):
#        if not(tel == 1 and the_one == 4):
            return False
    return True

def adjacent_product(grid,row,col): 
    #  check ROW
    if row == 0:
        if grid[row+1][col][1] == "X":
            return True
    elif row == 6:
        if grid[row-1][col][1] == "X":
            return True
    elif row in [1,2,3,4,5]:
        if grid[row+1][col][1] == "X" or grid[row-1][col][1] == "X":
            return True
    # check COL
    if col == 0:
        if grid[row][col+1][1] == "X":
            return True
    elif col == 6:
        if grid[row][col-1][1] == "X":
            return True
    elif col in [1,2,3,4,5]:
        if grid[row][col+1][1] == "X" or grid[row][col-1][1] == "X":
            return True
    return False

def all_ok(grid,row,col,number,rule):
    return empty(grid,row,col) and  \
        col_ok(grid,col,number) and  \
        row_ok(grid,row,number) and \
        region_ok(grid,row,col,number,rule)

# -------- solve ----------------
def solve_grid(grid,values): 
    n = [0,0,0]
    if not (next_number(n,values)):
        return True   # stop when all is allocated
    number = n[0]
    rule = n[1]
    seq = n[2]

    if [seq] > max_seq:
        max_seq[0] = seq
        print("Seq: ",seq)
        sys.stdout.flush()
    
    # go through all ROWS and COLS
    for row in range(7):
        for col in range(7):
            counter[0] += 1
            if counter[0] == MAX_COUNT:
                return True
            if all_ok(grid,row,col,number,rule):

                # check for adjacent product
                if rule != 1:
                    reg = region[row][col]
                    for [r,c] in cells_in_region[reg]:
                        if grid[r][c][0] == 0 and [r,c] != [row,col]:
                            if adjacent_product(grid,r,c):
                                return False
                                 
                # make tentative assignment
                values[seq][2] = 1
                grid[row][col][0] = number
                if rule == 2 or rule == 3 or rule == 4:
                    reg = region[row][col]
                    for [r,c] in cells_in_region[reg]:
                        if grid[r][c][0] == 0 and [r,c] != [row,col]:
                            grid[r][c][0] = get_prod(grid,r,c)
                            grid[r][c][1] = "X"

                # return, if success, ya! 
                if(solve_grid(grid,values)): 
                    return True

                # failure, unmake & try again 
                grid[row][col][0] = 0
                values[seq][2] = 0
                reg = region[row][col]
                for [r,c] in cells_in_region[reg]:
                    if grid[r][c][1] == "X":
                        grid[r][c][0] = 0
                        grid[r][c][1] = 0

                        
    return False

print(time.ctime())
print("\n")
if solve_grid(grid,values):
    print_grid(grid)
    if counter == MAX_COUNT:
        "..time out"
else: 
    print ("No solution exists")
print("Count: ",counter)
print(time.ctime())


