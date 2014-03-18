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
