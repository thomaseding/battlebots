def getInput()
    inputlines=ARGV[0].split(/\n/)
    return [inputlines[0, 10], inputlines[10, 2], inputlines[12..-1]]
end

def getMyPos(arena)
    pos=[]
    arena.each_with_index{|str, index| pos=[str.index('Y'), index] if(!str.index('Y').nil?)}
    return pos
end

def parseProjectiles(projectiles)
    projectiles.map!{|prj| prj.split(' ')}
    missiles=projectiles.select{|prj| prj[0]=='M'}
    bullets=projectiles.select{|prj| prj[0]=='B'}
    landmines=projectiles.select{|prj| prj[0]=='L'}
    return [missiles, bullets, landmines]
end

def haveFired?(ypos, direction, projectiles)
    return projectiles.select{|prj| prj[2]==ypos.to_s && prj[3]==direction}.size>0
end

arena, botenergy, projectiles=getInput()
missiles, bullets, landmines=parseProjectiles(projectiles)

myposX=getMyPos(arena)[0]
myposY=getMyPos(arena)[1]

direction="WE"[myposX!=0 ? 0 : 1]

if haveFired?(myposY, direction, missiles)
    if myposY==0
        print "S"
    elsif myposY==9
        print "N"
    else
        if haveFired?(myposY-1, direction, missiles)
            print "S"
        elsif haveFired?(myposY+1, direction, missiles)
            print "N"
        else
            if(Random.rand(2)==0)
                print "N"
            else
                print "S"
            end
        end
    end
else
    print "M "+direction
end
