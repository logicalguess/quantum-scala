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
    a = QuantumRegister(1)
    qc = QuantumCircuit(q, a)

    qc.ry(2 * phi, a[0])

    # hadamard on control qubits
    for i in range(n_qbits):
        qc.h(q[i])

    # prepare an eigenvector of ry(2*theta): i/sqrt(2)*|0> + 1/sqrt(2)*|1>
    # the corresponding eigenvalue is: cos(theta) + i*sin(theta)
    qc.rx(np.pi/2, a[0])
    qc.z(a[0])
    qc.x(a[0])

    # controlled rotations
    for i in range(n_qbits):
        cry(2 ** (i + 1) * theta, qc, q[i], a[0])

    # inverse fourier to retrieve best approximations
    iqft(qc, [q[i] for i in range(n_qbits)])

    return qc


def combine(n_bits, probs):

    def tick(y):
        return y/2 ** n_bits

    combined = {}
    for b, c in probs.items():
        key = tick(int(b[0:n_bits], 2))
        combined[key] = combined.get(key, 0) + c
    return combined


def process_estimates(estimates):
    rounded = dict(
        map(lambda item: (item[0], np.round(item[1], 5)), estimates.items()))
    ordered = sorted(rounded.items(), key=lambda x: x[1], reverse=True)
    print("outcomes = ", ordered)

    sines = list(map(lambda item: (np.round(np.power(np.sin(np.pi*item[0]), 2), 4), item[1]), ordered))
    print("sines = ", sines)

    combined_sines = {}
    for k, v in sines:
        combined_sines[k] = combined_sines.get(k, 0) + v
    print("combined_sines = ", combined_sines)

    if len(ordered) == 1:
        print("estimate = ", ordered[0][0])
    else:
        p1 = ordered[0][1]
        p2 = ordered[1][1]
        s = ordered[0][0]
        e = ordered[1][0]
        estimate = s + (e - s) * p2 / (p1 + p2)
        print("estimate = ", estimate)

        print("p = ", np.power(np.sin(np.pi*estimate), 2))


if __name__ == "__main__":
    n_ctrl_bits = 3
    p = 0.3
    theta = 2*np.arcsin(np.sqrt(p))
    #theta = 0.875*2*np.pi
    #theta = 0.125*2*np.pi

    print("fraction of 2*pi = ", 0.5*theta/np.pi)
    print("p = ", np.power(np.sin(theta/2), 2))

    qc = build_circuit(n_ctrl_bits, 0, theta)
    # visualization.plot_circuit(qc)

    import util
    probs = util.get_probs((qc, None, None), 'sim')
    # visualization.plot_histogram(probs)

    estimates = combine(n_ctrl_bits, probs)
    process_estimates(estimates)
