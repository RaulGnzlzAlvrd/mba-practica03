import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import random
import math
import bisect

iteraciones = 500
d = 2
precision = 9 # 9 es para 6 decimales de precision
pc = 0.5
pm = 0.01
tamanio_poblacion = 100

def bit_string_to_float(bits_string):
    flotante = 0
    for i, b in enumerate(bits_string, start=1):
        flotante += b * (2 ** -i)
    return flotante

def normaliza_fenotipo(fenotipo):
    return fenotipo * 1000 - 500

def genotipo_to_fenotipo(genotipo):
    return list(map(lambda x: normaliza_fenotipo(bit_string_to_float(x)), break_list(genotipo, precision)))

def fitness(genotipo, f):
    return f(genotipo_to_fenotipo(genotipo))

"""
Descompone una lista l en sublistas de tamaño n

arguments: 
    l: list
    n: number
return: list of list
    La lista donde cada elemento es de tamaño n
"""
def break_list(l, n):
    return [l[x:x+n] for x in range(0, len(l), n)]

"""
Calcula la función Schwefel con el vector proporcionado,
el vector puede ser de d dimensiones

arguments:
    vector: number
return: number Evaluación de la función Schwefel en el vector
"""
def schwefel(vector):
    result = 0
    for x in vector:
        result += x * math.sin(math.sqrt(abs(x)))
    return result

"""
Genera un individuo aleatorio
"""
def genera_individuo(precision, d):
    return [random.choice([0,1]) for i in range(precision * d)]

def average(l):
    return sum(l) / len(l)

def algoritmo_evolutivo_estandar():
    poblacion = [genera_individuo(precision, d) for i in range(tamanio_poblacion)]
    fenotipos = list(map(lambda g: genotipo_to_fenotipo(g), poblacion))
    evaluados = list(map(lambda f: schwefel(f), fenotipos))
    evaluacion = ajuste(evaluados) 
    nueva_poblacion = []

    best = show_best(evaluacion, poblacion, 0, verbose=True)
    mejores = [best[-1]]
    promedios = [average(evaluados)]
    generaciones = [(fenotipos, evaluados)]

    for iteracion in range(iteraciones):
        if iteracion in [10, 50, 100]:
            generaciones.append((fenotipos, evaluados))

        for i in range(tamanio_poblacion // 2):
            p1, p2 = selecciona_padres(poblacion, evaluacion)
            if random.random() < pc:
                h1, h2 = recombina(p1, p2)
            else:
                h1, h2 = p1, p2 
            map(lambda g : muta(g, pm), [h1, h2])
            nueva_poblacion = nueva_poblacion + [h1, h2]

        poblacion = nueva_poblacion
        fenotipos = list(map(lambda g: genotipo_to_fenotipo(g), poblacion))
        evaluados = list(map(lambda f: schwefel(f), fenotipos))
        nueva_poblacion = []
        
        evaluacion = ajuste(evaluados) 
        best = show_best(evaluacion, poblacion, iteracion + 1, verbose=True)
        mejores.append(best[-1])
        promedios.append(average(evaluados))
    return mejores, promedios, generaciones

def show_best(evaluacion, poblacion, generacion, verbose = False):
    indice = evaluacion.index(max(evaluacion))
    genotipo = poblacion[indice]
    fenotipo = genotipo_to_fenotipo(genotipo)
    valor = schwefel(fenotipo)
    if verbose:
        print("*" * 20)
        print("Generación ", generacion)
        print("Genotipo: ", genotipo)
        print("Fenotipo: ", fenotipo)
        print("Schwefel: ", valor)
    return generacion, genotipo, fenotipo, valor

def selecciona_padres(poblacion, evaluacion):
    i1 = ruleta(evaluacion)
    p1 = poblacion[i1]
    new_poblacion = remove_element(poblacion, i1)
    new_evaluacion = remove_element(evaluacion, i1)
    i2 = ruleta(new_evaluacion)
    p2 = new_poblacion[i2]
    return p1, p2

def remove_element(lista, indice):
    return lista[:indice] + lista[indice + 1:]

def recombina(p1, p2):
    indice = random.randint(0, len(p1))
    h1 = p1[:indice] + p2[indice:]
    h2 = p2[:indice] + p1[indice:]
    return h1, h2

def muta(genotipo, pm):
    for i, g in enumerate(genotipo):
        if random.random() < pm:
            genotipo[i] = complemento(g) 

def complemento(n):
    if n == 0:
        return 1
    else:
        return 0
    
def ruleta(lista):
    acc_list = lista_acumulativa(lista)
    aleatorio = random.randint(1, math.floor(acc_list[-1]))
    index = bisect.bisect_left(acc_list, aleatorio)
    return index

def ajuste(lista):
    minimo = min(lista)
    if minimo < 0:
        agregado = abs(minimo) + 1
        return list(map(lambda x : x + agregado, lista))
    return lista

def lista_acumulativa(lista):
    acc = 0
    acc_list = []
    for e in lista:
        acc += e
        acc_list.append(acc)
    return acc_list

data = algoritmo_evolutivo_estandar()

plt.plot(data[0], label='Best')
plt.ylabel('fitness')
plt.xlabel('generacion')

plt.plot(data[1], label='Average')
plt.ylabel('fitness')
plt.xlabel('generacion')

plt.legend()
plt.show()

def z_fun(x, y):
    X = x * np.sin(np.sqrt(np.abs(x)))
    Y = y * np.sin(np.sqrt(np.abs(y)))
    return X + Y

fig = plt.figure()
ax = plt.axes(projection="3d")
x = np.linspace(-500, 500)
y = np.linspace(-500, 500)
X, Y = np.meshgrid(x, y)
Z = z_fun(X, Y)

ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap='coolwarm', edgecolor='none')
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')

colors = ['red', 'blue', 'green', 'yellow']
markers = ['o', '^', '*', 'x']
for i, (xy, z) in enumerate(data[2]):
    xs = np.array(xy)[:,0]
    ys = np.array(xy)[:,1]
    ax.scatter(xs, ys, z, c=colors[i], marker=markers[i])
plt.show()

