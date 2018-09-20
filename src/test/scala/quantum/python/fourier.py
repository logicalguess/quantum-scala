# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np

import util


def qft(qc, q):
    for j in range(len(q)):
        qc.h(q[j])
        for k in range(j + 1, len(q)):
            qc.crz(np.pi/float(2**(k - j)), q[k], q[j])


def iqft(qc, q):
    for j in reversed(range(len(q))):
        qc.h(q[j])
        for k in reversed(range(j)):
            qc.crz(-np.pi/float(2**(j-k)), q[j], q[k])


def build_circuit():
    q = QuantumRegister(3)

    qc = QuantumCircuit(q)

    init1(q, qc)
    #init2(q, qc)

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
    qc, _, _ = build_circuit()

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print(hist)
    visualization.plot_histogram(hist)


