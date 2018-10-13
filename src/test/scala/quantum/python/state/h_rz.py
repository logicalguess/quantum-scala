# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util

# rotation by z equivalent to phase kickback
# changes the amplitude of 1, but not the probability
# (|0> + e^(i*theta)*sin(theta)|1>)/sqrt(2)


def build_circuit(theta):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.h(q[0])
    qc.rz(theta, q[0])

    return qc, None, None


if __name__ == "__main__":

    phi = 1/10

    qc, _, _ = build_circuit(math.pi*phi)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    print("e^(i*pi*phi)/sqrt(2) = ",
        math.sqrt(0.5)*complex(math.cos(math.pi*phi), math.sin(math.pi*phi)))

    visualization.plot_histogram(hist)


