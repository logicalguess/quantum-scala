# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util

# H = Ry(pi/2) Z


def build_circuit(state):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.initialize(state, q)

    hadamard(qc, q[0])

    return qc, None, None


def hadamard(qc, qbit):
    qc.z(qbit)
    qc.ry(np.pi / 2, qbit)


if __name__ == "__main__":
    qc, _, _ = build_circuit([1, 0])
    hist = util.get_probs((qc, None, None), 'sim')

    qc, _, _ = build_circuit([0, 1])
    hist = util.get_probs((qc, None, None), 'sim')
