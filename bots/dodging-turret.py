import sys

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

def GetPath(coord, direction):
    if direction=='N': path = [(coord[0],coord[1]-i) for i in xrange(3)]
    if direction=='S': path = [(coord[0],coord[1]+i) for i in xrange(3)]
    if direction=='E': path = [(coord[0]+i,coord[1]) for i in xrange(3)]
    if direction=='W': path = [(coord[0]-i,coord[1]) for i in xrange(3)]
    if direction=='NE': path = [(coord[0]+i,coord[1]-i) for i in xrange(3)]
    if direction=='NW': path = [(coord[0]-i,coord[1]-i) for i in xrange(3)]
    if direction=='SE': path = [(coord[0]+i,coord[1]+i) for i in xrange(3)]
    if direction=='SW': path = [(coord[0]-i,coord[1]+i) for i in xrange(3)]
    return path

def Danger(coord, stuff):
    if len(stuff):
        s = [i.split(" ") for i in stuff]
        for i in s:
            if i[0] in ['M','B']:
                path = GetPath((int(i[1]),int(i[2])),i[3])
                if coord in path:
                    return ['unsafe',path]
        return ['safe',()]
    else:
        return ['safe',()]

input = sys.argv[1].splitlines()
arena = input[0:10]
stuff = input[12:]
me = Position(arena, "Y")
center = Direction(me, (5,5))
if center != "":
    action = center
else:
    d = Danger(me,stuff)
    if d[0]=='safe':
        other = Position(arena,"X")
        target = Direction(me, other)
        action = 'M '+target
    if d[0]=='unsafe':
        escape = [(me[0]+i,me[1]) for i in range(-1,2,2)]+[(me[0],me[1]+i) for i in range(-1,2,2)]+[(5+me[0],5+me[1]) for i in range(-1,2,2)]
        esc_choice = [i for i in escape if i not in d[1]][0]
        action = Direction(me,esc_choice)

sys.stdout.write(action)
