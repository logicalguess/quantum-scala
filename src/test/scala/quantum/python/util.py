import numpy as np

# importing QISKit
from qiskit import compile, execute, register, available_backends, get_backend
import Qconfig

def controlled_X(qc, ctrl, anc, tgt):
    return controlled(qc, ctrl, anc, tgt)

def controlled_Z(qc, ctrl, anc, tgt):
    return controlled(qc, ctrl, anc, tgt, c_gate = lambda qc, ctrl, tgt: qc.cz(ctrl, tgt))

def controlled(qc, ctrl, anc, tgt, c_gate = lambda qc, c, t: qc.cx(c, t), cc_gate = lambda qc, c1, c2, t: qc.ccx(c1, c2, t)):
    n = len(ctrl)

    # compute
    cc_gate(qc, ctrl[0], ctrl[1], anc[0])
    for i in range(2, n):
        cc_gate(qc, ctrl[i], anc[i-2], anc[i-1])

    # copy
    c_gate(qc, anc[n-2], tgt[0])

    # uncompute
    for i in range(n-1, 1, -1):
        cc_gate(qc, ctrl[i], anc[i-2], anc[i-1])
    cc_gate(qc, ctrl[0], ctrl[1], anc[0])

def cx0(qc, c, t):
    qc.x(c)
    qc.cx(c, t)
    qc.x(c)


def ccx0(qc, c1, c2, t):
    qc.x(c1)
    qc.x(c2)
    qc.ccx(c1, c2, t)
    qc.x(c1)
    qc.x(c2)

def run(shots, qc, cfg, backend = None):
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

    job = execute([qc], backend=backend, shots=shots)
    result = job.result()

    return result


def get_counts(c, cfg, backend = None):
    qc, qr, cr = c
    qc.measure(qr, cr)
    result = run(1024, qc, Qconfig.cfg[cfg], backend)
    counts = result.get_counts()
    # visualization.plot_circuit(qc)
    return counts


def histogram(state):
    n = len(state)
    pow = int(np.log2(n))
    keys = [bin(i)[2::].rjust(pow, '0')[::-1] for i in range(0, n)]
    #keys = [bin(i)[2::].rjust(pow, '0') for i in range(0, n)]

    print(dict(zip(keys, state)))

    probs = [np.round(abs(a)*abs(a), 5) for a in state]
    hist = dict(zip(keys, probs))
    filtered_hist = dict(filter(lambda p: p[1] > 0, hist.items()))
    return filtered_hist


def get_probs(c, cfg):
    qc, _, _ = c
    # visualization.plot_circuit(qc)
    result = run(1, qc, Qconfig.cfg[cfg], 'local_statevector_simulator')
    state = np.round(result.get_data(qc)['statevector'], 5)
    return histogram(state)