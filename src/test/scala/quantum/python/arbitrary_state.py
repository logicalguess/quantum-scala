# importing QISKit
from qiskit import QuantumCircuit, ClassicalRegister, QuantumRegister
from qiskit.tools import visualization
import util


def build_circuit():
    q = QuantumRegister(3)
    c = ClassicalRegister(3)

    qc = QuantumCircuit(q, c)

    import math
    state = [
        1 / math.sqrt(16) * complex(0, 1),
        1 / math.sqrt(8) * complex(1, 0),
        1 / math.sqrt(16) * complex(1, 1),
        0,
        0,
        1 / math.sqrt(8) * complex(1, 2),
        1 / math.sqrt(16) * complex(1, 0),
        0]

    qc.initialize(state, q)

    return qc, q, c


if __name__ == "__main__":
    hist = util.get_probs(build_circuit(), 'sim')
    print(hist)
    visualization.plot_histogram(hist)
