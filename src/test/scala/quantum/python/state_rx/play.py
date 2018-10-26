# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util

# change signs of angles in rx


def build_circuit(theta):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.rx(theta, q[0])
    qc.h(q[0])
    qc.rx(-2*theta, q[0])
    qc.h(q[0])
    qc.rx(-theta, q[0])

    return qc, None, None


if __name__ == "__main__":
    theta = 0.258

    qc, _, _ = build_circuit(theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    visualization.plot_histogram(hist)

    print("cos^2(theta)", np.round(math.pow(np.cos(theta), 2), 5))
    print("sin^2(theta)", np.round(math.pow(np.sin(theta), 2), 5))

    print("sin(theta)*cos(theta) = ", np.round(np.sin(theta)*np.cos(theta), 5))
    print("cos(theta) = ", np.round(np.cos(theta), 5))
    print("sin(theta) = ", np.round(np.sin(theta), 5))
