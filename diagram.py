from pylab import *

xlim(-0.5, 5)
ylim(-0.5, 5)

fill([0,2,0], [0,0,4])

xlabel("x")
ylabel("y")


def obj(x, y):
    return str(1.5*x+y)

plot([1], [1], 'ko')
annotate("(x0, y0), objective=" + obj(1,1), (0.95, 0.85))

plot([1.1], [1], 'ro')
annotate("(x0+d, y0), objective=" + obj(1.1, 1), (0.95, 1.1), color='r')



plot([0.75], [4-2*0.75], 'ko')
annotate("(x0, y0), objective=" + obj(0.75, 4-2*0.75), (0.80, 4-2*0.75-0.05))

arrow( 0.75, 4-2*0.75, -0.05, 0.1, ec='g', fc='g', lw=4, head_width=0.04)

plot([0.65], [4-2*0.65], 'ro')
annotate("(x0, y0) + d, objective=" + obj(0.65, 4-2*0.65), (0.7, 4-2*0.65-0.05), color='r')

plot([0.85], [4-2*0.85], 'yo')
annotate("(x0, y0) - d, objective=" + obj(0.85, 4-2*0.85), (0.9, 4-2*0.85-0.05), color='y')

savefig("polygon_diagram.png")
