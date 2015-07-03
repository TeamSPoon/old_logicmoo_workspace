package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_parse_file46pl_15_3.java
 * @procedure $dummy_parse_file.pl_15/3 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_parse_file46pl_15_3 extends Predicate {
    static Predicate $dummy_parse_file46pl_15_3_1 = new PRED_$dummy_parse_file46pl_15_3_1();
    static Predicate $dummy_parse_file46pl_15_3_2 = new PRED_$dummy_parse_file46pl_15_3_2();
    static Predicate $dummy_parse_file46pl_15_3_sub_1 = new PRED_$dummy_parse_file46pl_15_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_$dummy_parse_file46pl_15_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_$dummy_parse_file46pl_15_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_parse_file46pl_15_3_1, $dummy_parse_file46pl_15_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "$dummy_parse_file.pl_15(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_$dummy_parse_file46pl_15_3_sub_1 extends PRED_$dummy_parse_file46pl_15_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_parse_file46pl_15_3_2);
    }
}

class PRED_$dummy_parse_file46pl_15_3_1 extends PRED_$dummy_parse_file46pl_15_3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        p1 = new PRED_$unify_2(a3, a1, cont);
        return new PRED_$greater_or_equal_2(a1, a2, p1);
    }
}

class PRED_$dummy_parse_file46pl_15_3_2 extends PRED_$dummy_parse_file46pl_15_3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        p1 = new PRED_$unify_2(a3, a2, cont);
        return new PRED_$less_than_2(a1, a2, p1);
    }
}
