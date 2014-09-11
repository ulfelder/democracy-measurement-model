
import numpy as np
import pandas as pd
from pystan import stan


## convert data to long format
df = pd.read_csv('democracies.csv')
df.set_index(['sftgcode', 'year'], inplace=True)
del df['country']

melted = df.stack().astype(np.int)
melted.index.names = ['country', 'year', 'expert']

## organize data for Stan
data = {
    'country': np.array(melted.index.labels[0]) + 1,
    'year': np.array(melted.index.labels[1]) + 1,
    'expert': np.array(melted.index.labels[2]) + 1,
    'labels': melted.values,
}

## counts
data['num_countries'] = len(np.unique(data['country']))
data['num_years'] = len(np.unique(data['year']))
data['num_experts'] = len(np.unique(data['expert']))
data['num_obs'] = len(data['labels'])

## run the stan model (takes about 5-10 minutes)
m1 = stan(
    file='democracy_model.stan',
    model_name='democracy_model',
    data=data,
    chains=1,
    iter=100,
    init='random',
)

def print_scalar_param(m, name):
    v = m.extract(name)[name]
    print '{0} = {1:.2f}'.format(name, v.mean())
    print '95% CI: [{0:.2f}, {1:.2f}]'.format(np.percentile(v, 2.5, 0), np.percentile(v, 97.5, 0))

## hyper parameters
print_scalar_param(m1, 'country_var')
print_scalar_param(m1, 'time_var')

## expert biases
bias = m1.extract('expert_bias')['expert_bias']
experts = melted.index.levels[2]

print 'expert bias'
for expert, b, se in zip(experts, bias.mean(axis=0), bias.std(axis=0)):
    print '{0} = {1:.2f} ({2:.2f})'.format(expert, b, se)

## expert variances
vari = m1.extract('expert_var')['expert_var']

print 'expert variance'
for expert, b, se in zip(experts, vari.mean(axis=0), vari.std(axis=0)):
    print '{0} = {1:.2f} ({2:.2f})'.format(expert, b, se)

## reassemble the matrix into a dataframe
truth = m1.extract('democracy')['democracy']
mean = pd.DataFrame(
    truth.mean(axis=0),
    # this -1 is weird no idea why i have to do this
    columns=melted.index.levels[1][:-1], 
    index=melted.index.levels[0]
).stack()
se = pd.DataFrame(
    truth.std(axis=0),
    columns=melted.index.levels[1][:-1], 
    index=melted.index.levels[0]
).stack()
truth = pd.DataFrame({'mean': mean, 'se': se})

from ggplot import *

## plot USA
p = ggplot(truth.xs('USA').reset_index(), aes(x='year', y='mean', ymin='mean - 1.96*se', ymax = 'mean + 1.96*se'))
p += geom_smooth() 
p += geom_line()
ggsave('figures/usa_democracy.pdf', p)

## plot Russia
p = ggplot(truth.xs('RUS').reset_index(), aes(x='year', y='mean', ymin='mean - 1.96*se', ymax = 'mean + 1.96*se'))
p += geom_smooth() 
p += geom_line()
ggsave('figures/rus_democracy.pdf', p)
