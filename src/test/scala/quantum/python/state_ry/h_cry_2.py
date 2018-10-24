# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import math
import numpy as np

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util


# '0000': sqrt(1/8)
# '0001': 0
# '0010': sqrt(1/8)*cos(4*theta)
# '0011': sqrt(1/8)*sin(4*theta)
# '0100': sqrt(1/8)*cos(2*theta)
# '0101': sqrt(1/8)*sin(2*theta)
# '0110': sqrt(1/8)*cos(6*theta)
# '0111': sqrt(1/8)*sin(6*theta)
# '1000': sqrt(1/8)*cos(theta)
# '1001': sqrt(1/8)*sin(theta)
# '1010': sqrt(1/8)*cos(5*theta)
# '1011': sqrt(1/8)*sin(5*theta)
# '1100': sqrt(1/8)*cos(3*theta)
# '1101': sqrt(1/8)*sin(3*theta)
# '1110': sqrt(1/8)*cos(7*theta)
# '1111': sqrt(1/8)*sin(7*theta)

def cry(theta, qc, q_control, q_target):
    qc.ry(theta/2, q_target)
    qc.cx(q_control, q_target)
    qc.ry(-theta/2, q_target)
    qc.cx(q_control, q_target)


def build_circuit(n_qbits, phi, theta):
    q = QuantumRegister(3)
    t = QuantumRegister(1)
    qc = QuantumCircuit(q, t)

    qc.ry(2*phi, t[0])

    # qc.h(q[0])
    # qc.h(q[1])
    # qc.h(q[2])

    for i in range(n_qbits):
        qc.h(q[i])

    # cry(2*theta, qc, q[0], t[0])
    # cry(4*theta, qc, q[1], t[0])
    # cry(8*theta, qc, q[2], t[0])

    for i in range(n_qbits):
        cry(2**(i+1)*theta, qc, q[i], t[0])

    return qc, None, None


if __name__ == "__main__":

    phi = 0.152
    theta = 0.235

    qc, _, _ = build_circuit(3, phi, theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)

    visualization.plot_histogram(hist)

    print("sqrt(1/8)*cos(phi + theta) = ", np.round(math.sqrt(1/8)*np.cos(phi + theta), 5))
    print("sqrt(1/8)*sin(phi + theta) = ", np.round(math.sqrt(1/8)*np.sin(phi + theta), 5))

    print("sqrt(1/8)*cos(phi + 2*theta) = ", np.round(math.sqrt(1/8)*np.cos(phi + 2*theta), 5))
    print("sqrt(1/8)*sin(phi + 2*theta) = ", np.round(math.sqrt(1/8)*np.sin(phi + 2*theta), 5))

    print("sqrt(1/8)*cos(phi + 3*theta) = ", np.round(math.sqrt(1/8)*np.cos(phi + 3*theta), 5))
    print("sqrt(1/8)*sin(phi + 3*theta) = ", np.round(math.sqrt(1/8)*np.sin(phi + 3*theta), 5))

    print("sqrt(1/8)*cos(phi + 4*theta) = ", np.round(math.sqrt(1/8)*np.cos(phi + 4*theta), 5))
    print("sqrt(1/8)*sin(phi + 4*theta) = ", np.round(math.sqrt(1/8)*np.sin(phi + 4*theta), 5))

    print("sqrt(1/8)*cos(phi + 5*theta) = ", np.round(math.sqrt(1/8)*np.cos(phi + 5*theta), 5))
    print("sqrt(1/8)*sin(phi + 5*theta) = ", np.round(math.sqrt(1/8)*np.sin(phi + 5*theta), 5))

    print("sqrt(1/8)*cos(phi + 6*theta) = ", np.round(math.sqrt(1/8)*np.cos(phi + 6*theta), 5))
    print("sqrt(1/8)*sin(phi + 6*theta) = ", np.round(math.sqrt(1/8)*np.sin(phi + 6*theta), 5))

    print("sqrt(1/8)*cos(phi + 7*theta) = ", np.round(math.sqrt(1/8)*np.cos(phi + 7*theta), 5))
    print("sqrt(1/8)*sin(phi + 7*theta) = ", np.round(math.sqrt(1/8)*np.sin(phi + 7*theta), 5))
