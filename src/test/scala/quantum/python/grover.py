import numpy as np

# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister, compile, execute, register, available_backends, get_backend
from qiskit.tools import visualization
import Qconfig


def build_circuit(n):
    q = QuantumRegister(3)
    c = ClassicalRegister(3)

    qc = QuantumCircuit(q, c)

    # set last bit to 1
    qc.x(q[2])

    # superposition
    qc.h(q[0])
    qc.h(q[1])
    qc.h(q[2])


    # oracle
    qc.ccx(q[0], q[1], q[2])

    # diffusion
    qc.h(q[0])
    qc.h(q[1])
    qc.x(q[0])
    qc.x(q[1])

    qc.cz(q[0], q[1])

    qc.x(q[0])
    qc.x(q[1])
    qc.h(q[0])
    qc.h(q[1])

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
    n = len(state)
    pow = int(np.log2(n))
    keys = [bin(i)[2::].rjust(pow, '0')[::-1] for i in range(0, n)]
    print(dict(zip(keys, state)))

    probs = [np.round(abs(a)*abs(a), 5) for a in state]
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
    hist = get_probs(2, 'sim')
    print("F(", 2, ") = ", len(hist))
    visualization.plot_histogram(hist)
