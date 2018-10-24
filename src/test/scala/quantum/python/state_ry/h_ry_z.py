# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister

import numpy as np

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util

# I = Z Ry(-pi/2) H


def build_circuit(state):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.initialize(state, q)

    qc.h(q[0])
    qc.ry(-np.pi / 2, q[0])
    qc.z(q[0])

    return qc, None, None


if __name__ == "__main__":
    qc, _, _ = build_circuit([1, 0])
    hist = util.get_probs((qc, None, None), 'sim')

    qc, _, _ = build_circuit([0, 1])
    hist = util.get_probs((qc, None, None), 'sim')
