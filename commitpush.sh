git pull
git --git-dir=games/.git --work-tree=games pull

#% hg pull
#% hg commit -m "$*"
git commit -a -m "$*"
git --git-dir=games/.git --work-tree=games commit
git push
#% hg push

