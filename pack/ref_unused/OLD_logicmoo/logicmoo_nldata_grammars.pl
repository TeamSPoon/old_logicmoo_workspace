:- module(grammars,
	  [sample_grammar/2]).

:-op(900,xfx,::).
:-op(900,xfx,--->).

% sample_grammar(+Name,-Grammar)
%  Grammar is a grammar by the name of Name.

sample_grammar(english_fragment,
	       [start_symbol(s),

		preterminals([conj,pn,det,n_indef,iv,tv,prep]),

		s ---> [np,vp], 
		s ---> [s,conj,s], 
		
		np ---> [pn], 
		np ---> [det,n_bar],
		np ---> [np,conj,np],
		
		n_bar --->[n_indef],
		n_bar --->[n_bar,pp],
		
		vp ---> [iv], 
		vp ---> [tv,np],
		vp ---> [vtvpo,np,pp],
		vp ---> [vp,conj,vp],
		
		pp ---> [prep,np],
		
		det::a,
		det::the,
		n_indef::bird,
		n_indef::cat,
		n_indef::dog,
		n_indef::box,
		n_indef::table,
		pn::john,
		pn::mary,
		iv::slept,
		tv::watched,
		vtvpo::put,
		conj::and,
		prep::in,
		prep::on]).

