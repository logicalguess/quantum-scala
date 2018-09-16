# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
from qiskit.tools import visualization
import math
import util


def build_circuit(n, m):
    q = QuantumRegister(n)
    a = QuantumRegister(n - 1, "anc")
    t = QuantumRegister(1)

    qc = QuantumCircuit(q, a, t)

    # set last bit to 1
    qc.x(t[0])

    # superposition
    for i in range(0, n):
        qc.h(q[i])

    qc.h(t[0])

    for i in range(0, int(math.sqrt(2**n))):
        # oracle
        oracle(n, m, qc, q, a, t)

        # diffusion
        diffusion(qc, q, a)

    return qc, None, None


def is_bit_not_set(m, k):
    return not (m & (1 << k))


def oracle(n, m, qc, q, a, t):
    for i in range(0, n):
        if is_bit_not_set(m, i):
            qc.x(q[n - 1 - i])

    util.controlled_X(qc, q, a, t)

    for i in range(0, n):
        if is_bit_not_set(m, i):
            qc.x(q[n - 1 - i])


def diffusion(qc, q, a):
    for i in range(0, len(q)):
        qc.h(q[i])
        qc.x(q[i])

    # controlled Z
    util.controlled_Z(qc, [q[i] for i in range(0, len(q) - 1)], a, [q[len(q) - 1]])

    for i in range(0, len(q)):
        qc.x(q[i])
        qc.h(q[i])


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(5, 31), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
