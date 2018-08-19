import numpy as np

# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister, execute
from qiskit.tools import visualization


def cry(theta, qc, q_control, q_target):
    qc.ry(theta/2, q_target)
    qc.cx(q_control, q_target)
    qc.ry(-theta/2, q_target)
    qc.cx(q_control, q_target)


def fib_circuit(n):
    q = QuantumRegister(n)
    c = ClassicalRegister(n)

    qc = QuantumCircuit(q, c)

    for i in range(0, n):
        qc.h(q[i])

    for i in range(0,  n - 1):
        cry(-np.pi/2, qc, q[i], q[i + 1])

    for i in range(0, n):
        qc.measure(q[i], c[i])

    return qc


def run(n):
    qc = fib_circuit(n)
    # visualization.plot_circuit(qc)
    job = execute([qc], backend='local_qasm_simulator', shots=int(np.power(2, n + 2)))
    result = job.result()
    counts = result.get_counts()
    print("F(", n, ") = ", len(counts))
    # visualization.plot_histogram(counts)


if __name__ == "__main__":
    for i in range(1, 10):
        run(i)

    # F( 1 ) =  2
    # F( 2 ) =  3
    # F( 3 ) =  5
    # F( 4 ) =  8
    # F( 5 ) =  13
    # F( 6 ) =  21
    # F( 7 ) =  34
    # F( 8 ) =  55
    # F( 9 ) =  89