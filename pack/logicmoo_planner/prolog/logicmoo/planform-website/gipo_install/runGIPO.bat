SET GIPO=T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\logicmoo_base\prolog\logicmoo\planner\planform\gipo_install
SET PATH=%PATH%;%GIPO%\sp311\bin;
javaw -classpath %GIPO%\gipo.jar;%GIPO%\java_cup.jar;%GIPO%\jgraph.jar;%GIPO%\jgraphaddons.jar;%GIPO%\sp311\bin\jasper.jar -Docled.path=%GIPO%\jplan -Docled.codebase=%GIPO% -Dsicstus.path=%GIPO%\sp311\bin  jplan.top.OclEd

