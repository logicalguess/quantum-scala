# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import util

def build_circuit(x):
    q = QuantumRegister(1)

    qc = QuantumCircuit(q)

    qc.h(q[0])
    qc.rz(np.pi*x, q[0])
    qc.h(q[0])

    return qc, None, None


def init(q, qc, x):
    a = 1/math.sqrt(2)

    state = [
        a,
        a*complex(np.cos(np.pi*x), np.sin(np.pi*x))
    ]
    qc.initialize(state, q)


if __name__ == "__main__":
    x = 1 #0, 1/2, 1/4, 3/4
    qc, _, _ = build_circuit(x)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print(hist)
    visualization.plot_histogram(hist)

    print("probability of 0 = ", math.pow(np.cos(np.pi*(x/2)), 2))
    print("probability of 1 = ", math.pow(np.sin(np.pi*(x/2)), 2))

    print("x = ", math.asin(math.sqrt(hist.get('1', 0)))*2/np.pi)


