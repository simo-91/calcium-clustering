import numpy as np
from suite2p.extraction import dcnv
F = np.load('F.npy')
Fneu = np.load('Fneu.npy')
ops = np.load('ops.npy', allow_pickle=True).item()
dF = F.copy() - 0.7*Fneu
dF = dcnv.preprocess(dF, ops['baseline'], ops['win_baseline'], 
                                   ops['sig_baseline'], ops['fs'], ops['prctile_baseline'])
np.save("dF", dF, allow_pickle = True)
