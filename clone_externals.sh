git clone https://github.com/TeamSPoon/hMUD externals/hMUD
git clone https://github.com/TeamSPoon/ClioPatria externals/ClioPatria
git clone https://github.com/TeamSPoon/swish externals/swish
git clone https://github.com/TeamSPoon/logicmoo externals/logicmoo
git clone https://github.com/TeamSPoon/MUD_Examples examples/
git clone https://github.com/TeamSPoon/MUD_PDDL externals/MUD_PDDL
git clone https://github.com/TeamSPoon/MUD_KnowRob externals/MUD_KnowRob
git clone -b indigo-devel --single-branch https://github.com/TeamSPoon/knowrob_addons.git externals/MUD_KnowRob/knowrob_addons
git clone -b indigo-devel --single-branch https://github.com/TeamSPoon/knowrob.git externals/MUD_KnowRob/knowrob

git clone -b indigo-devel --single-branch https://github.com/TeamSPoon/knowrob.git externals/MUD_KnowRob/knowrob

git clone https://github.com/TeamSPoon/iai_maps.git externals/MUD_KnowRob/iai_maps
git clone https://github.com/TeamSPoon/XperiMental.git externals/XperiMental

git clone --recursive -b indigo-devel --single-branch https://github.com/TeamSPoon/knowrob.git knowrob

git clone https://github.com/TeamSPoon/MUD_WebTHEA externals/MUD_WebTHEA

git clone https://github.com/TeamSPoon/MUD_ScriptEngines externals/MUD_ScriptEngines

git clone https://github.com/TeamSPoon/MUD_DeepParsing externals/MUD_DeepParsing

git clone https://github.com/TeamSPoon/MUD_XperiMental MUD_XperiMental


 git clone -b indigo-devel --single-branch https://github.com/TeamSPoon/knowrob_addons.git knowrob_addons

 git clone -b indigo-devel --single-branch https://github.com/TeamSPoon/knowrob.git knowrob
 
git clone -b indigo-devel --single-branch --recursive https://github.com/TeamSPoon/knowrob.git knowrob
git merge upstream/indigo-devel
git checkout indigo-devel

 
 git clone https://github.com/TeamSPoon/iai_maps.git 

cd knowrob
git pull https://github.com/knowrob/knowrob.git indigo-devel
git pull https://github.com/hatguy/knowrob.git indigo-devel
git pull https://github.com/zyfang/knowrob.git indigo-devel
git pull https://github.com/TeamSPoon/knowrob.git indigo-devel

cd ../knowrob_addons
git pull https://github.com/knowrob/knowrob_addons.git indigo-devel
git pull https://github.com/hatguy/knowrob_addons.git indigo-devel
git pull https://github.com/zyfang/knowrob_addons.git indigo-devel

svn co http://roboticssrv.wtb.tue.nl/svn/ros/user/loyvanbeek/

