import numpy as np

# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister, compile, execute, register, available_backends, get_backend
from qiskit.tools import visualization
import Qconfig


def cry(theta, qc, q_control, q_target):
    qc.ry(theta/2, q_target)
    qc.cx(q_control, q_target)
    qc.ry(-theta/2, q_target)
    qc.cx(q_control, q_target)


def build_circuit(n):
    q = QuantumRegister(n)
    c = ClassicalRegister(n)

    qc = QuantumCircuit(q, c)

    for i in range(0, n):
        qc.h(q[i])

    for i in range(0,  n - 1):
        cry(-np.pi/2, qc, q[i], q[i + 1])

    return qc, q, c


def run(n, qc, cfg, backend = None):
    if 'url' in cfg.keys():
        register(cfg['token'], cfg['url'], cfg['hub'], cfg['group'], cfg['project'])
        print(available_backends())

    if backend is None:
        backend = cfg['backend']

    backend_config = get_backend(backend).configuration
    backend_coupling = backend_config['coupling_map']

    qc_compiled = compile([qc], backend=backend, coupling_map=backend_coupling, seed=0)
    qc_compiled_qasm = qc_compiled['circuits'][0]['compiled_circuit_qasm']
    #print(qc_compiled_qasm)

    job = execute([qc], backend=backend, shots=int(np.power(2, n + 2)))
    result = job.result()

    return result


def get_counts(n, cfg, backend = None):
    qc, qr, cr = build_circuit(n)
    qc.measure(qr, cr)
    result = run(n, qc, Qconfig.cfg[cfg], backend)
    counts = result.get_counts()
    # visualization.plot_circuit(qc)
    return counts


def histogram(state):
    #print(state)
    probs = [np.round(abs(a)*abs(a), 5) for a in state]

    keys = [bin(i)[2::].rjust(3, '0') for i in range(0, 8)]
    hist = dict(zip(keys, probs))
    filtered_hist = dict(filter(lambda p: p[1] > 0, hist.items()))
    return filtered_hist


def get_probs(n, cfg):
    qc, _, _ = build_circuit(n)
    # visualization.plot_circuit(qc)
    result = run(n, qc, Qconfig.cfg[cfg], 'local_statevector_simulator')
    state = np.round(result.get_data(qc)['statevector'], 5)
    return histogram(state)


if __name__ == "__main__":
    for i in range(1, 10):
        hist = get_counts(i, 'sim')
        print("F(", i, ") = ", len(hist))

    # F( 1 ) =  2
    # F( 2 ) =  3
    # F( 3 ) =  5
    # F( 4 ) =  8
    # F( 5 ) =  13
    # F( 6 ) =  21
    # F( 7 ) =  34
    # F( 8 ) =  55
    # F( 9 ) =  89