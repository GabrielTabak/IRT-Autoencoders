# IRT Autoencoders With Evolutionary approach

Algorithms for an evolutionary approach applied to IRT autoencoders. 

IRT autoencoders needs a specific decoder structure, but the encoder can be freely set. The evolutionary approach tries to find best architectures for the encoder.

We have three different scenarios:

# First simulation -- 

One simulated data set (First Data set created in Replicas Sim 2). 
Algorithm: Evolutionary IRT Autoencoder Sim 1.ipynb
Results and charts: Results Simulation 1.R

# Second simulation --

100 Replicas: Replicas Sim 2.R
Algorithm Evo + IRT Autoencoder: Simulation 2 Evolutionary Autoencoder IRT.ipynb
Algorithm IWAVE: Simulation 2 IWAVE.ipynb
Algorithm Stan: 2PL.stan

# Third simulation

Microdata from ENEM: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem
Read data and sample: Read ENEM.R
Algorithm IRT Autoencoders (more traits): S1 - ENEM.ipynb
Clean Data and Results: ENEM.R
