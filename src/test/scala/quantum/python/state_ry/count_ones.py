# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import numpy as np
import math

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util


def is_bit_set(m, k):
    return m & (1 << k)


def build_circuit(n, m, theta):
    t = QuantumRegister(1, "tgt")

    qc = QuantumCircuit(t)

    for i in range(0, n):
        if is_bit_set(m, i):
            qc.ry(theta, t[0])

    return qc, None, None


if __name__ == "__main__":

    n = 5
    m = 14
    theta = np.pi/n

    qc, _, _ = build_circuit(n, m, theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)
    #visualization.plot_histogram(hist)

    print("binary representation of ", m, " = ", bin(m)[2::].rjust(n, '0'))
    print("number of 1s in", m, " = ", bin(m).count("1"))

    print("count from prob of last bit 1 = ", int(2*math.asin(math.sqrt(hist.get('1', 0)))/theta + 0.5))
