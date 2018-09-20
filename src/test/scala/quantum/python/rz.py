# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import math

import util


def build_circuit(state, theta):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.initialize(state, q)

    qc.rz(theta, q[0])

    return qc, None, None


if __name__ == "__main__":
    a = math.sqrt(0.5)
    state = [
        a * complex(1, 0),
        a * complex(0, 1)
    ]

    theta = math.pi/4
    print(math.sqrt(0.5)*complex(math.cos(theta), math.sin(theta)))

    qc, _, _ = build_circuit(state, theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print(hist)
    visualization.plot_histogram(hist)


