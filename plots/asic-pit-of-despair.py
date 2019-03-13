import numpy as np
import matplotlib.pyplot as plt

plt.xkcd()

def gen_fun1():
    points = np.array([(0, 0.5),
                    (0.5, 0.51),
                    (1, 0.49),
                    (1.5, 0.49),
                    (2, 0.5),
                    (2.5, 0.49),
                    (3, 0.5),
                    (3.5, 0.4),
                    (4, 0.41),
                    (4.7, 0.4),
                    (4.9, 0.4)])

    # get x and y vectors
    x = points[:,0]
    y = points[:,1]

    # calculate polynomial
    z = np.polyfit(x, y, 8)
    f = np.poly1d(z)

    return f

def gen_fun2():
    points = np.array([(5, 0.001),
                       (6, 0.01),
                       (6.5, 0.05),
                       (7, 0.2),
                       (7.5, 0.25),
                       (8, 0.28),
                       (8.5, 0.28),
                       (9, 0.3),
                       (9.5, 0.35),
                       (10, 0.4),
                       (11, 0.4)])

    # get x and y vectors
    x = points[:,0]
    y = points[:,1]

    # calculate polynomial
    z = np.polyfit(x, y, 3)
    f = np.poly1d(z)

    return f

fun1 = gen_fun1()
fun2 = gen_fun2()

def pfun(x):
    if x <= 4.9:
        return fun1(x)
    elif x < 5.0:
        return 0.1
    else:
        return fun2(x)

def fun(xs):
    return map(pfun, xs)

x = np.linspace(0, 10)
y = fun(x)

# Cannot find Humor... :(
# font = {'family' : 'Humor Sans',
        # 'weight' : 'normal',
        # 'size'   : 16,
        # }
# plt.rc('font', **font)

fig = plt.figure()
ax = fig.add_subplot(1, 1, 1)
plt.xticks([])
plt.yticks([])
plt.ylim([0, 1])

px = [4.8, 5.15]
py = [0.4, -0.015]

plt.plot(x, y, px, py, 'o')

plt.xlim([x[0]-1, x[-1] + 1 ])
plt.ylim(-0.05, 0.7)

plt.text(5.5, -0.035, 'Monero before fork')
plt.text(5.05, 0.4, 'Monero after fork')

plt.figtext(0.03, 0.91, 'Decentralization')


plt.annotate(
    'PIT OF DESPAIR',
    xy=(5.05, -0.01), arrowprops=dict(arrowstyle='->'), xytext=(0.4, 0.2))

ax.spines['right'].set_color('none')
ax.spines['top'].set_color('none')
ax.xaxis.set_ticks_position('bottom')
ax.set_xticks((0.5, 2, 3.5, 5))
ax.set_xticklabels(('CPU', 'GPU', 'FPGA', 'ASIC'))
ax.set_yticks([])
ax.tick_params(axis=u'both', which=u'both',length=0)


plt.savefig('monero.png')
plt.savefig('monero.svg', format="svg")
