# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
from qiskit.tools import visualization
import util


def build_circuit():
    q = QuantumRegister(3)
    c = ClassicalRegister(3)

    qc = QuantumCircuit(q, c)

    # set last bit to 1
    qc.x(q[2])

    # superposition
    qc.h(q[0])
    qc.h(q[1])
    qc.h(q[2])


    # oracle
    oracle_01(q, qc)

    # diffusion
    diffusion(q, qc)

    return qc, q, c


def oracle_01(q, qc):
    qc.x(q[0])
    qc.ccx(q[0], q[1], q[2])
    qc.x(q[0])

def oracle_11(q, qc):
    qc.ccx(q[0], q[1], q[2])

def diffusion(q, qc):
    qc.h(q[0])
    qc.h(q[1])
    qc.x(q[0])
    qc.x(q[1])

    qc.cz(q[0], q[1])

    qc.x(q[0])
    qc.x(q[1])
    qc.h(q[0])
    qc.h(q[1])


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
