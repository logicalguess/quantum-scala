# importing QISKit
from qiskit import QuantumCircuit, QuantumRegister
from qiskit.tools import visualization

import math
import numpy as np

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import util


# {'00': 1/srt(2)*cos(phi), '01': 1/srt(2)*sin(phi), '10': 1/sqrt(2)*cos(phi + theta), '11': 1/sqrt(2)*sin(phi + theta)}

def cry(theta, qc, q_control, q_target):
    qc.ry(theta/2, q_target)
    qc.cx(q_control, q_target)
    qc.ry(-theta/2, q_target)
    qc.cx(q_control, q_target)


def build_circuit(phi, theta):
    q = QuantumRegister(2)
    qc = QuantumCircuit(q)

    qc.ry(2*phi, q[1])

    qc.h(q[0])

    cry(2*theta, qc, q[0], q[1])

    return qc, None, None


if __name__ == "__main__":

    phi = 0.7*np.pi
    theta = 0.2*np.pi

    qc, _, _ = build_circuit(phi, theta)

    # from qiskit.tools.visualization import plot_circuit
    # plot_circuit(qc)

    hist = util.get_probs((qc, None, None), 'sim')
    print("Probabilities:", hist)

    visualization.plot_histogram(hist)

    print("1/sqrt(2)*cos(phi) = ", math.sqrt(0.5)*np.round(np.cos(phi), 5))
    print("1/sqrt(2)*sin(phi) = ", math.sqrt(0.5)*np.round(np.sin(phi), 5))

    print("probability of 00 = cos^2(phi)/2 = ", 0.5*np.round(math.pow(np.cos(phi), 2), 5))
    print("probability of 01 = sin^2(phi)/2 = ", 0.5*np.round(math.pow(np.sin(phi), 2), 5))

    print("1/sqrt(2)*cos(phi + theta) = ", math.sqrt(1/2)*np.round(np.cos(phi + theta), 5))
    print("1/sqrt(2)*sin(phi + theta) = ", math.sqrt(1/2)*np.round(np.sin(phi + theta), 5))

    print("probability of 10 = cos^2(phi + theta)/2 = ", 0.5*np.round(math.pow(np.cos(phi + theta), 2), 5))
    print("probability of 11 = sin^2(phi + theta)/2 = ", 0.5*np.round(math.pow(np.sin(phi + theta), 2), 5))
