import sys, os

def Position(arena, element):
    y = [i for i,j in enumerate(arena) if element in arena[i]][0]
    x = arena[y].index(element)
    return (x,y)

def Direction(coord1, coord2):
    d0 = coord1[0]-coord2[0]
    d1 = coord1[1]-coord2[1]
    if d1!=0:
        a = ['N','S'][d1<0]
    else: a = ""
    if d0!=0:
        b = ['W','E'][d0<0]
    else: b = ""
    return a+b

def Shortest(coord1,coord2):
    d = abs(coord1[0]-coord2[0])-abs(coord1[1]-coord2[1])
    if d>0: a = 'EW'
    if d<=0: a = 'NS'
    return a

input = sys.argv[1].splitlines()
arena = input[0:10]
life = input[10].split(" ")
stuff = input[12:]
path = os.path.dirname(__file__)
f1 = os.path.join(path,'state','RAS')
try:
    with open(f1, 'r') as f:
        fired = int(f.read())
except:
    fired = 0

me = Position(arena, "Y")
other = Position(arena, "X")
target = Direction(me,other)
m = []
if len(stuff):
    s = [i.split(" ") for i in stuff]
    for i in s:
        if i[0]=='L': m += [(int(i[1]),int(i[2]))]


near = [(me[0]+i,me[1]) for i in range(-1,2,2)]+[(me[0],me[1]+i) for i in range(-1,2,2)]+[(5+me[0],5+me[1]) for i in range(-1,2,2)]
closeMines = [i for i in m if i in near]
dirmines = []
for j in closeMines:
    dirmines += Direction(me, j)


if target in ['N','S','E','W']:
    if int(life[1])>1 and fired==0:
        action = "P"
        with open(f1,'w') as f:
            f.write('2')
    else:
        if fired==2:
            action = "M "+target
            with open(f1,'w') as f:
                f.write('1')
        if fired==1:
            action = "B "+target
            with open(f1,'w') as f:
                f.write('0')
        if int(life[1])==1:
            action = "M "+target
else:
    s = Shortest(me,other)
    d1 = Direction((me[0],other[1]), other)
    d2 = Direction((other[0],me[1]), other)
    if s=='EW' and d1 not in dirmines:
        action = d1
    if s=='NS' and d2 not in dirmines:
        action = d2
    else:
        if d2 not in dirmines: action = d2
        if d1 not in dirmines: action = d1
        else: action = 0


sys.stdout.write(action)
