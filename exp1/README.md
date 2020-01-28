# Document matching

Based on the (paper)[https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-356.pdf]

TODO: document relevance based on terms in the paper 
> Relevance weights


## Datasets
Unzip by using `for i in *.gz; do gunzip $i; done`

Remove french ones `for i in *.f; do rm $i; done`

To cut down a csv size `head -n 50 file.csv > smaller.csv` 

## Speeds

Iteration 1 (Working state)

```
3.csv
[1][Prepare] Time elapsed 9.000000
[2][Corpus freqs] Time elapsed 10.000000
[3][Idfs] Time elapsed 10.000000
[4][Vectors] Time elapsed 15.000000
[5][Distance matrix] Time elapsed 56.000000
[6][Clusters] Time elapsed 56.000000
```

