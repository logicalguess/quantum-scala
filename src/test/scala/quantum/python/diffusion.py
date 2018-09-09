# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
from qiskit.tools import visualization
import util


def build_circuit():
    q = QuantumRegister(3)
    c = ClassicalRegister(3)

    qc = QuantumCircuit(q, c)

    # superposition
    qc.h(q[0])
    qc.h(q[1])
    qc.h(q[2])

    # diffusion, inversion by the mean (change amplitude of 0 to its negative
    qc.x(q[0])
    qc.x(q[1])
    qc.x(q[2])

    # ccz
    qc.h(q[2])
    qc.ccx(q[0], q[1], q[2])
    qc.h(q[2])

    qc.x(q[0])
    qc.x(q[1])
    qc.x(q[2])

    return qc, q, c


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
