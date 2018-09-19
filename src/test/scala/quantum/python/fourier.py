# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np

import util


def qft(qc, q):
    for j in range(len(q)):
        qc.h(q[j])
        for k in range(j + 1, len(q)):
            theta_jk = np.pi/float(2**(k - j))
            qc.cu1(theta_jk, q[k], q[j])


def iqft(qc, q):
    for j in range(len(q))[::-1]:
        qc.h(q[j])
        for k in range(j)[::-1]:
            theta_jk = -np.pi/float(2**(j-k))
            qc.cu1(theta_jk, q[j], q[k])


def build_circuit():
    q = QuantumRegister(3)

    qc = QuantumCircuit(q)

    #init1(q, qc)
    init2(q, qc)

    qft(qc, q)
    iqft(qc, q)

    return qc, None, None


def init2(q, qc):
    import math
    a = math.sqrt(0.125)
    state = [
        a * complex(1, 0),
        a * complex(0, 1),
        a * complex(1, 0),
        a * complex(1, 0),
        a * complex(0, 1),
        a * complex(1, 0),
        a * complex(0, 1),
        a * complex(1, 0),
    ]
    qc.initialize(state, q)


def init1(q, qc):
    qc.x(q[2])


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
