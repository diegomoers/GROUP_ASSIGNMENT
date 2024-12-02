'''
This file estimates a mixture model with two categories for the survey data.
'''

import pandas as pd
import numpy as np
import topicmodels

K = 2

ceo_data = pd.read_csv("../baseline_results/survey_response_data.csv",
                       low_memory=False)
ceo_data.set_index(['id'], inplace=True)
ceo_data = ceo_data[ceo_data.type != 'personal_family']
ceo_data = ceo_data[(ceo_data.level1 == 'interacting')]

agg_data = ceo_data.groupby(ceo_data.index).\
    agg({'all_combined': lambda x: ' '.join(x)})

docsobj = topicmodels.RawDocs(agg_data.all_combined)
docsobj.term_rank("tokens")
docsobj.rank_remove("df", "tokens", 30)

bowobj = topicmodels.BOW(docsobj.tokens)

emobjs = []
loglik = []

for i in range(25):
    emobj = topicmodels.multimix.EM(bowobj.bow, K)
    emobj.estimate()
    loglik.append(emobj.loglik[-1])
    emobjs.append(emobj)

em_best = emobjs[np.argmax(loglik)]

dt = pd.DataFrame(em_best.type_prob, index=agg_data.index,
                  columns=['PB' + str(k) for k in range(K)])

dt.to_csv('index_mixture.csv', index=True)
