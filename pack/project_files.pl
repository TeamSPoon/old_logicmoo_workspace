
:- maplist([W]>>
 format(user_output,'
  	    <Folder Name="~w">
			<F  N="~w/*.*"
			Recurse="1"
			Refilter="1"
			Excludes="pldata/;*.qlf;*.class;.git/;~~*;*~~;~~*/;*~~/;.svn/;*.*r;*.*o;*.dll;*.exe"/>
		</Folder>
',[W,W]),[
%ClioPatria,
clause_attvars,
dictoo,
each_call_cleanup,
eggdrop,
file_scope,
gvar_syntax,
hilog,
hook_hybrid,
instant_prolog_docs,
logicmoo_base,
%logicmoo_experimental,
%logicmoo_nlu,
%logicmoo_packages,
%logicmoo_planner,
%logtalk,
loop_check,
multimodal_dcg,
multivar,
must_trace,
no_repeats,
predicate_streams,
prologmud,
ref_unused,
reindent,
s_expression,
slack_prolog,
subclause_expansion,
%swicli,
%swish,
with_open_options,
with_thread_local,
xlisting]).


 maplist(pack_upgrade,[clause_attvars,
dictoo,
each_call_cleanup,
eggdrop,
file_scope,
gvar_syntax,
hilog,
hook_hybrid,
instant_prolog_docs,
logicmoo_base,
%logicmoo_experimental,
%logicmoo_nlu,
%logicmoo_packages,
%logicmoo_planner,
%logtalk,
loop_check,
multimodal_dcg,
multivar,
must_trace,
no_repeats,
predicate_streams,
prologmud,
ref_unused,
reindent,
s_expression,
slack_prolog,
subclause_expansion,
%swicli,
%swish,
with_open_options,
with_thread_local,
xlisting]).




:-
 asserta((refesh_pack(P):- !, format(atom(URL),'https://github.com/TeamSPoon/~w.git',[P]),
  pack_install(URL,[upgrade(true),git(true),interactive(false)]))).

:-
  maplist(refesh_pack,[logicmoo_utils,predicate_streams]).

:- maplist(refesh_pack,[gvar_syntax,dictoo,clause_attvars]).

maplist(refesh_pack,[each_call_cleanup,
eggdrop,
file_scope,
% hilog,
hook_hybrid,
instant_prolog_docs,
%logicmoo_experimental,
%logicmoo_nlu,
%logicmoo_packages,
%logicmoo_planner,
%logtalk,
loop_check,
multimodal_dcg,
must_trace,
no_repeats,
subclause_expansion]).

maplist(refesh_pack,[
s_expression,
with_open_options,
with_thread_local,
xlisting,xlisting_web]).

logicmoo_base,
maplist(refesh_pack,[prologmud,prologmud_samples]).

pfc,
slack_prolog,

]).

 git submodule foreach "git commit -am "v1.1.115" && git push || :"
 git commit -am "v1.1.115" && git push

