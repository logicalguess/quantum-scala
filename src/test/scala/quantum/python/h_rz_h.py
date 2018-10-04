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


if __name__ == "__main__":
    phi = 1/4 #0, 1/2, 1/4, 3/4
    qc, _, _ = build_circuit(phi)

    from qiskit.tools.visualization import plot_circuit
    plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    visualization.plot_histogram(hist)

    print("probability of 0 = cos^2(pi*phi/2)", np.round(math.pow(np.cos(np.pi*phi/2), 2), 5))
    print("probability of 1 = sin^2(pi*phi/2)", np.round(math.pow(np.sin(np.pi*phi/2), 2), 5))

    print("x = ", np.round(math.asin(math.sqrt(hist.get('1', 0)))*2/np.pi, 5))



