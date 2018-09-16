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

    controlled(qc, q, a, t)

    for i in range(0, n):
        if is_bit_not_set(m, i):
            qc.x(q[n - 1 - i])


def diffusion(qc, q, a):
    for i in range(0, len(q)):
        qc.h(q[i])
        qc.x(q[i])

    # controlled Z
    controlled(qc, [q[i] for i in range(0, len(q) - 1)], a, [q[len(q) - 1]], cc_gate = lambda qc, ctrl1, ctrl2, tgt: qc.ccx(ctrl1, ctrl2, tgt), c_gate = lambda qc, ctrl, tgt: qc.cz(ctrl, tgt))

    for i in range(0, len(q)):
        qc.x(q[i])
        qc.h(q[i])


def controlled(qc, ctrl, anc, tgt, cc_gate = lambda qc, c1, c2, t: qc.ccx(c1, c2, t), c_gate = lambda qc, c, t: qc.cx(c, t)):
    n = len(ctrl)

    # compute
    cc_gate(qc, ctrl[0], ctrl[1], anc[0])
    for i in range(2, n):
        cc_gate(qc, ctrl[i], anc[i-2], anc[i-1])

    # copy
    c_gate(qc, anc[n-2], tgt[0])

    # uncompute
    for i in range(n-1, 1, -1):
        cc_gate(qc, ctrl[i], anc[i-2], anc[i-1])
    cc_gate(qc, ctrl[0], ctrl[1], anc[0])


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(5, 31), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
