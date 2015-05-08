git pull
git --git-dir=games/.git --work-tree=games pull

#% hg pull
#% hg commit -m "$*"
git commit -a -m "$*"
git push
git --git-dir=games/.git --work-tree=games commit
git --git-dir=games/.git --work-tree=games push
#% hg push

