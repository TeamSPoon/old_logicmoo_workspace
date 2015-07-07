RES=.
geniserver -m lu-small-grammar.geni -l lexicon.geni --morphlexicon morph.mph --rootfeat='[cat:Nom|NomPropre|Sen|Prix|Pro]' --partial &
echo 'geni server launched'
