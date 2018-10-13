# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import math

import util

# rotation by z equivalent to phase kickback
# changes the amplitude of 1, but not the probability


def build_circuit(theta):
    q = QuantumRegister(1)
    qc = QuantumCircuit(q)

    qc.h(q[0])
    qc.rz(theta, q[0])

    return qc, None, None


if __name__ == "__main__":

    phi = 1/8
    print("e^(2*pi*i*phi) = ",
          math.sqrt(0.5)*complex(math.cos(2*math.pi*phi), math.sin(2*math.pi*phi)))

    qc, _, _ = build_circuit(2*math.pi*phi)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    visualization.plot_histogram(hist)


