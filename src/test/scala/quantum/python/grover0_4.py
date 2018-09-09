import numpy as np

# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
from qiskit.tools import visualization
import util

# https://cstheory.stackexchange.com/questions/38538/oracle-construction-for-grovers-algorithm
# look at states ending in 1
def build_circuit():
    q = QuantumRegister(4)
    c = ClassicalRegister(4)

    qc = QuantumCircuit(q, c)

    qc.x(q[0])
    qc.x(q[1])
    qc.x(q[2])

    qc.h(q[0])
    qc.h(q[1])
    qc.h(q[2])


# oracle compute
    qc.ccx(q[1], q[2], q[3])

    qc.z(q[3])

    # oracle uncompute
    qc.ccx(q[1], q[2], q[3])

    # diffusion
    util.ccx0(qc, q[0], q[1], q[2])


    # oracle compute
    qc.ccx(q[1], q[2], q[3])

    qc.z(q[3])

    return qc, q, c


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
