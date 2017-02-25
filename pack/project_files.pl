
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

