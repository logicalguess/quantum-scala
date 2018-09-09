from qiskit import QuantumRegister, QuantumCircuit

# https://quantumcomputing.stackexchange.com/questions/2177/how-can-i-implement-an-n-bit-toffoli-gate
def build_circuit(ctrl, tgt, cc_gate, c_gate):
    n = len(ctrl)
    anc = QuantumRegister(n-1, 'anc')

    qc = QuantumCircuit(ctrl, anc, tgt)

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

    return qc

if __name__ == "__main__":
    n = 5  # must be >= 2

    ctrl = QuantumRegister(n, 'ctrl')
    tgt = QuantumRegister(1, 'tgt')

    def c_gate(qc, ctrl, tgt):
        #qc.cx(ctrl, tgt)
        qc.cz(ctrl, tgt)

    def cc_gate(qc, ctrl1, ctrl2, tgt):
        qc.ccx(ctrl1, ctrl2, tgt)

    qc = build_circuit(ctrl, tgt, cc_gate, c_gate)

    from qiskit.tools.visualization import plot_circuit
    plot_circuit(qc)

