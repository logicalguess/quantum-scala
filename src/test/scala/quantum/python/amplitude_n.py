from typing import Any, Callable, Union

import numpy as np

# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization


def cry(theta, qc, q_control, q_target):
    qc.ry(theta / 2, q_target)
    qc.cx(q_control, q_target)
    qc.ry(-theta / 2, q_target)
    qc.cx(q_control, q_target)


def crz(theta, qc, q_control, q_target):
    qc.rz(theta / 2, q_target)
    qc.cx(q_control, q_target)
    qc.rz(-theta / 2, q_target)
    qc.cx(q_control, q_target)


def iqft(qc, q):
    for j in range(len(q))[::-1]:
        qc.h(q[j])
        for k in range(j)[::-1]:
            crz(-np.pi / float(2 ** (j - k)), qc, q[j], q[k])


def build_circuit(n_qbits, phi, theta):
    q = QuantumRegister(n_qbits)
    t = QuantumRegister(1)
    qc = QuantumCircuit(q, t)

    qc.ry(2 * phi, t[0])

    for i in range(n_qbits):
        qc.h(q[i])

    for i in range(n_qbits):
        cry(2 ** (i + 1) * theta, qc, q[i], t[0])

    iqft(qc, [q[i] for i in range(n_qbits)])

    return qc


def combine(n_bits, probs):

    def tick(y):
        return 2 * y/2 ** n_bits

    combined = {}
    for b, c in probs.items():
        key = tick(int(b[0:n_bits], 2))
        combined[key] = combined.get(key, 0) + c
    return combined


def process_estimates(estimates):
    rounded = dict(
        map(lambda item: (item[0], np.round(item[1] if item[0] == 1.0 else 2.0 * item[1], 5)), estimates.items()))
    filtered = dict(filter(lambda item: item[0] <= 1.0, rounded.items()))
    ordered = sorted(filtered.items(), key=lambda x: x[1], reverse=True)
    print("outcomes = ", ordered)

    if len(ordered) == 1:
        print("estimate = ", ordered[0][0])
    else:
        p1 = ordered[0][1]
        p2 = ordered[1][1]
        s = ordered[0][0]
        e = ordered[1][0]
        print("estimate = ", s + (e - s) * p2 / (p1 + p2))


if __name__ == "__main__":
    n_ctrl_bits = 5
    theta = 0.23 * np.pi

    qc = build_circuit(n_ctrl_bits, theta / 2, theta)
    # visualization.plot_circuit(qc)

    import util
    probs = util.get_probs((qc, None, None), 'sim')
    # visualization.plot_histogram(probs)

    estimates = combine(n_ctrl_bits, probs)
    process_estimates(estimates)
