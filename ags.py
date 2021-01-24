import random
import math

d = 2
precision = 9

def obtener_fenotipo(genotipo):
    fenotipo = 0
    for i, g in enumerate(genotipo, start=1):
        fenotipo += g * (2 ** -i)
    return fenotipo

def normaliza_fenotipo(fenotipo):
    return fenotipo * 1000 - 500

def fitness(genotipo, f):
    fenotipo_normalizado = map(lambda x : normaliza_fenotipo(obtener_fenotipo(x)), break_list(genotipo, precision))
    return f(fenotipo_normalizado)

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

# Se van a usar arreglos de tamaño 9 para la precisión de 6 decimales 
individuo = [random.choice([0,1]) for i in range(precision * d)]
print(individuo, fitness(individuo, schwefel))

