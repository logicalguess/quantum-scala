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


def ccry(theta, qc, q_control_1, q_control_2, q_target):
    qc.ry(theta/2, q_target)
    qc.ccx(q_control_1, q_control_2, q_target)
    qc.ry(-theta/2, q_target)
    qc.ccx(q_control_1, q_control_2, q_target)


a = 2.0
b = 3.0
c = 5.0


def build_circuit():
    qr = QuantumRegister(3)
    cr = ClassicalRegister(3)

    qc = QuantumCircuit(qr, cr)

    for i in range(0, 2):
        qc.h(qr[i])

    # qc.x(qr[0])
    # qc.x(qr[1])

    qc.ry(2*c, qr[2])
    cry(2*(4*a + 2*b), qc, qr[0], qr[2])
    ccry(2*4*a, qc, qr[0], qr[1], qr[2])
    cry(2*(a + b), qc, qr[1], qr[2])

    return qc, qr, cr


def run(qc, cfg, backend = None):
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

    job = execute([qc], backend=backend, shots=128)
    result = job.result()

    return result

def get_counts(cfg, backend = None):
    qc, qr, cr = build_circuit()
    qc.measure(qr, cr)
    result = run(qc, Qconfig.cfg[cfg], backend)
    counts = result.get_counts()
    # visualization.plot_circuit(qc)
    return counts


def get_probs(cfg):
    qc, _, _ = build_circuit()
    # visualization.plot_circuit(qc)
    result = run(qc, Qconfig.cfg[cfg], 'local_statevector_simulator')
    state = result.get_data(qc)['statevector']
    return histogram(state)


def histogram(state):
    print(state)
    probs = [abs(a)*abs(a) for a in state]

    keys = [bin(i)[2::].rjust(3, '0')[::-1] for i in range(0, 8)]
    hist = dict(zip(keys, probs))
    filtered_hist = dict(filter(lambda p: p[1] > 0, hist.items()))
    return filtered_hist

if __name__ == "__main__":
    hist = get_probs('sim')

    print(hist)
    visualization.plot_histogram(hist)

    sin_i = [np.math.sin(a*i*i + b*i + c)*np.math.sin(a*i*i + b*i + c)/4 for i in range(0, 4)]
    print('sin^2(p(i))/4:', dict(zip(range(0,4), sin_i)))

