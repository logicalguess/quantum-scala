# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util

# cos(theta)|0> + e^(i*phi)*sin(theta)|1>


def build_circuit(theta, phi):
    q = QuantumRegister(1)

    qc = QuantumCircuit(q)

    qc.h(q[0])
    qc.rz(2*theta, q[0])
    qc.h(q[0])

    qc.rz(np.pi/2 - theta, q[0])

    qc.x(q[0])
    qc.rz(-theta, q[0])
    qc.x(q[0])

    qc.rz(phi, q[0])


    return qc, None, None


if __name__ == "__main__":
    theta = 0.258
    phi = 0.321

    qc, _, _ = build_circuit(theta, phi)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    visualization.plot_histogram(hist)

    print("probability of 0 = cos^2(theta)", np.round(math.pow(np.cos(theta), 2), 5))
    print("probability of 1 = sin^2(theta)", np.round(math.pow(np.sin(theta), 2), 5))

    print("|th| from prob of 1 = ", np.round(math.asin(math.sqrt(hist.get('1', 0))), 5))
    print("|th| from prob of 0 = ", np.round(math.acos(math.sqrt(hist.get('0', 0))), 5))

    print("cos(phi)*cos(theta) = ", np.round(np.cos(phi) * np.sin(theta), 5))
    print("sin(phi)*cos(theta) = ", np.round(np.sin(phi) * np.sin(theta), 5))

    print("cos(theta) = ", np.round(np.cos(theta), 5))
    print("sin(theta) = ", np.round(np.sin(theta), 5))





