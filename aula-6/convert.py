
# Script Python para converter arquivo do Livro no formato a ser lido
# pelo R

for c in critics:
    for b in critics[c]:
        print c, "\t", b, "\t", critics[c][b]
        
