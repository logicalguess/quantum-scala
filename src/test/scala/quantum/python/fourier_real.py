# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import math
import numpy as np

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

# k = first 3 bits reversed
# |k>|0>: sqrt(1/8)*cos(k*theta)
# |k>|1>: sqrt(1/8)*sin(k*theta)

def cry(theta, qc, q_control, q_target):
    qc.ry(theta/2, q_target)
    qc.cx(q_control, q_target)
    qc.ry(-theta/2, q_target)
    qc.cx(q_control, q_target)


def build_circuit(n_qbits, phi, theta):
    q = QuantumRegister(n_qbits)
    a = QuantumRegister(1)
    qc = QuantumCircuit(q, a)

    qc.ry(2*phi, a[0])

    # hadamard on control qubits
    for i in range(n_qbits):
        qc.h(q[i])

    # controlled rotations
    for i in range(n_qbits):
        cry(2**(i+1)*theta, qc, q[i], a[0])

    return qc


if __name__ == "__main__":

    phi = 0
    theta = 0.235

    qc = build_circuit(3, phi, theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)

    # visualization.plot_histogram(hist)

    print("sqrt(1/8)*cos(phi) = ", np.round(math.sqrt(1/8)*np.cos(phi), 5))
    print("sqrt(1/8)*sin(phi) = ", np.round(math.sqrt(1/8)*np.sin(phi), 5))

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
