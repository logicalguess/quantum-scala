# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import math

import util

# rotation by z equivalent to phase kickback
# changes the amplitude of 1, but not the probability

def build_circuit(state, theta):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.initialize(state, q)

    qc.rz(theta, q[0])

    return qc, None, None


if __name__ == "__main__":
    a = math.sqrt(0.5)
    state = [
        a,
        a
    ]

    theta = math.pi/16
    print(math.sqrt(0.5)*complex(math.cos(theta), math.sin(theta)))

    qc, _, _ = build_circuit(state, theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print(hist)
    visualization.plot_histogram(hist)


