import numpy as np

# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
from qiskit.tools import visualization
import util

# https://cstheory.stackexchange.com/questions/38538/oracle-construction-for-grovers-algorithm
# look at states ending in 1
def build_circuit():
    q = QuantumRegister(3)
    c = ClassicalRegister(3)

    qc = QuantumCircuit(q, c)

    qc.x(q[0])
    qc.x(q[1])

    qc.h(q[0])
    qc.h(q[1])


    # oracle compute
    qc.ccx(q[0], q[1], q[2])

    qc.z(q[2])

    # oracle uncompute
    qc.ccx(q[0], q[1], q[2])

    # diffusion
    util.cx0(qc, q[0], q[1])


    # oracle compute
    qc.ccx(q[0], q[1], q[2])

    qc.z(q[2])

    return qc, q, c


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
