# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util

# prepare state: e^(i*theta) * sqrt(1/2)* |0> + e^(-i*theta) * sqrt(1/2)* |1>


def build_circuit(theta):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.rx(-2*theta, q[0])
    qc.h(q[0])

    return qc, None, None


if __name__ == "__main__":
    theta = 0.258

    qc, _, _ = build_circuit(theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    visualization.plot_histogram(hist)

    print("sqrt(1/2)*cos(theta) = ", np.round(math.sqrt(0.5)*np.cos(theta), 5))
    print("sqrt(1/2)*sin(theta = ", np.round(math.sqrt(0.5)*np.sin(theta), 5))
