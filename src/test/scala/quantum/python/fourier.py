# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
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

    qc.x(q[2])

    # import math
    # state = [
    # 1 / math.sqrt(16) * complex(0, 1),
    # 1 / math.sqrt(8) * complex(1, 0),
    # 1 / math.sqrt(16) * complex(1, 1),
    # 0,
    # 0,
    # 1 / math.sqrt(8) * complex(1, 2),
    # 1 / math.sqrt(16) * complex(1, 0),
    # 0]
    #
    # qc.initialize(state, q)

    qft(qc, q)
    iqft(qc, q)

    return qc, None, None


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
