import matplotlib.pyplot as plt
import random

# Parametros iniciales
N = 30
PROB_RECOMB = 1
PROB_MUTA = .8
TOP_PADRES = 2
N_PADRES = 5
N_POBLACION = 100
MAX_ITER = 10000
SEMILLA = 10

# La semilla permite obtener la misma secuencia de numeros aleatorios
# random.seed(SEMILLA)

sum = 0
# Función auxiliar para ordenar usando la función fitness
def sort_eval(p):
  eval = evalua(p)
  global sum
  sum += eval
  return eval

# Función fitness para una configuración p. q(p) = x | x es el numero de amenazas
def evalua(p):
  conflictos = 0
  for i in range(0, N):
    for j in range(i+1, N):
      if p[j] == p[i] + j - i or p[j] == p[i] - (j - i):
        conflictos += 1
  return conflictos

# Función auxiliar de recombinación
def cut_crossfill(p1, p2):
  corte = random.randint(0, N)
  h1 = p1[0:corte]
  h2 = p2[0:corte]
  for i in range(0, N):
    if p2[i] not in h1:
      h1.append(p2[i])
    if p1[i] not in h2:
      h2.append(p1[i])
  return h1, h2

# Función auxiliar de intercambio en una lista
def swap(l, a, b):
  tmp = l[a]
  l[a] = l[b]
  l[b] = tmp

# Se inicializa la lista de reinas y la lista de poblacion
reinas = [*range(1, N+1)]
poblacion = []

data = [None] * 2
data[0] = [] # Lista para guardar la mejor evaluación de configuración en su generación
data[1] = [] # Lista para guardar los promedios de las evaluaciones de las configuraciones en su generación

# Se agregan N_POBLACION permutaciones aleatorias a la poblacion
for i in range(0, N_POBLACION):
  random.shuffle(reinas)
  poblacion.append(reinas[:])

# Se realiza el ciclo del algoritmo genetico a lo más MAX_ITER veces
for generacion in range(0, MAX_ITER):

  # Condición de terminación: si existe una configuración en la población
  # cuya evaluación fitness es 0 (no hay amenazas)
  if evalua(poblacion[0]) == 0:
    print("Generacion: ", generacion)
    print("Solucion: ", poblacion[0])
    break

  # Se seleccionan los N_PADRES de manera aleatoria
  padres = []
  for i in range(0, N_PADRES):
    r = random.randint(0, N_POBLACION - 1)
    padres.append(poblacion[r])

  # Se ordenan los padres en función de su evaluación fitness
  # y se toman los mejores TOP_PADRES
  padres.sort(key=sort_eval)
  padres = padres[0:TOP_PADRES]

  # Se usan las parejas de padres en orden para generar parejas de hijos
  # usando la función de recombinación
  hijos = []
  for i in range(0, TOP_PADRES, 2):
    if i+1 >= TOP_PADRES:
      break
    p1 = padres[i]
    p2 = padres[i+1]
    h1, h2 = cut_crossfill(p1, p2)
    hijos.append(h1)
    hijos.append(h2)

  # Se usa la PROB_MUTA para determinar si los hijos generados
  # deben mutarse
  muta = random.random()
  if muta < PROB_MUTA:
    for i in range(0, len(hijos), 2):
      a = random.randint(0, N-1)
      b = random.randint(0, N-1)
      swap(hijos[i], a, b)
      a = random.randint(0, N-1)
      b = random.randint(0, N-1)
      swap(hijos[i+1], a, b)

  # Se agregan los hijos a la poblacion y se evalua toda la poblacion
  # para eliminar los peores
  poblacion = poblacion + hijos
  sum = 0
  poblacion.sort(key=sort_eval)
  for i in range(0,len(hijos)):
    poblacion.pop()

  data[0].append(evalua(poblacion[0]))
  avg = sum / N_POBLACION
  data[1].append(avg)

if evalua(poblacion[0]) > 0:
    print("No se encontró una solución en el límite de iteraciones.")

plt.plot(data[0], label="Mejor aptitud")
plt.ylabel('fitness')
plt.xlabel('generación')

plt.plot(data[1], label='Promedio')
plt.ylabel('fitness')
plt.xlabel('generación')

plt.legend()
plt.show()