# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
from qiskit.tools import visualization
import util


def build_circuit(m):
    q = QuantumRegister(4)
    a = QuantumRegister(3, "anc")
    t = QuantumRegister(1)

    qc = QuantumCircuit(q, a, t)

    # set last bit to 1
    qc.x(t[0])

    # superposition
    for i in range(0, 4):
        qc.h(q[i])

    qc.h(t[0])

    for i in range(0, 4):
        # oracle
        oracle(m, qc, q, a, t)

        # diffusion
        diffusion(qc, q, a)

    return qc, None, None


def is_bit_not_set(m, k):
    return not (m & (1 << k))


def oracle(m, qc, q, a, t):
    for i in range(0, 4):
        if is_bit_not_set(m, i):
            qc.x(q[4 - 1 - i])

    controlled(qc, q, a, t)
    # # compute
    # qc.ccx(q[0], q[1], a[0])
    # # qc.ccx(q[2], a[0], a[1])
    # # qc.ccx(q[3], a[1], a[2])
    # for i in range(2, 4):
    #     qc.ccx(q[i], a[i-2], a[i-1])
    #
    # # copy
    # qc.cx(a[2], t[0])
    #
    # # uncompute
    # for i in range(3, 1, -1):
    #     qc.ccx(q[i], a[i-2], a[i-1])
    # # qc.ccx(q[3], a[1], a[2])
    # # qc.ccx(q[2], a[0], a[1])
    # qc.ccx(q[0], q[1], a[0])

    for i in range(0, 4):
        if is_bit_not_set(m, i):
            qc.x(q[4 - 1 - i])


def diffusion(qc, q, a):
    qc.h(q[0])
    qc.h(q[1])
    qc.h(q[2])
    qc.h(q[3])
    qc.x(q[0])
    qc.x(q[1])
    qc.x(q[2])
    qc.x(q[3])

    controlled(qc, [q[0], q[1], q[2]], a, [q[3]], lambda qc, c1, c2, t: qc.ccx(c1, c2, t), lambda qc, c, t: qc.cz(c, t))
    # # compute
    # qc.ccx(q[0], q[1], a[0])
    # # qc.ccx(q[2], a[0], a[1])
    # for i in range(2, 3):
    #     qc.ccx(q[i], a[i-2], a[i-1])
    #
    # # copy
    # qc.cz(a[1], q[3])
    #
    # # uncompute
    # for i in range(2, 1, -1):
    #     qc.ccx(q[i], a[i-2], a[i-1])
    # # qc.ccx(q[2], a[0], a[1])
    # qc.ccx(q[0], q[1], a[0])

    qc.x(q[0])
    qc.x(q[1])
    qc.x(q[2])
    qc.x(q[3])
    qc.h(q[0])
    qc.h(q[1])
    qc.h(q[2])
    qc.h(q[3])


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
    hist = util.get_probs(build_circuit(15), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
