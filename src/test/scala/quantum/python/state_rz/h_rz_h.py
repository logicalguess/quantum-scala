# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util

# e^(i*theta)*cos(theta)|0> + e^(i*(theta - pi/2))*sin(theta)|1>

# (p + i*sqrt(p*(1-p))|0> + (1-p - i*sqrt(p*(1-p))|1>
# p = cos^2(theta)


def build_circuit(theta):
    q = QuantumRegister(1)

    qc = QuantumCircuit(q)

    qc.h(q[0])
    qc.rz(2*theta, q[0])
    qc.h(q[0])

    return qc, None, None


if __name__ == "__main__":

    # bias = 0.7
    # theta = bias * np.pi/2

    theta = 0.258

    qc, _, _ = build_circuit(theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    visualization.plot_histogram(hist)

    print("probability of 0 = cos^2(theta)", np.round(math.pow(np.cos(theta), 2), 5))
    print("probability of 1 = sin^2(theta)", np.round(math.pow(np.sin(theta), 2), 5))

    print("theta = ", np.round(math.asin(math.sqrt(hist.get('1', 0))), 5))

    # print("bias of getting 1 = ", np.round(math.asin(math.sqrt(hist.get('1', 0)))*2/np.pi, 5))

    print("sin(theta)*cos(theta) = ", np.round(np.sin(theta)*np.cos(theta), 5))
    print("cos(theta) = ", np.round(np.cos(theta), 5))
    print("sin(theta) = ", np.round(np.sin(theta), 5))



