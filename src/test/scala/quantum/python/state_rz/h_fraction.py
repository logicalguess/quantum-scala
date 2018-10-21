# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util


def build_circuit(x):
    q = QuantumRegister(1)

    qc = QuantumCircuit(q)

    init(q, qc, x)

    return qc, None, None


def init(q, qc, x):
    a = 1/math.sqrt(2)

    # prepare state: (|0> + e^(i*np.pi*x)|1>)/sqrt(2)

    state = [
        a,
        a*complex(np.cos(np.pi*x), np.sin(np.pi*x))
    ]
    qc.initialize(state, q)


if __name__ == "__main__":
    qc, _, _ = build_circuit(0)
    hist = util.get_probs((qc, None, None), 'sim')

    qc, _, _ = build_circuit(1)
    hist = util.get_probs((qc, None, None), 'sim')