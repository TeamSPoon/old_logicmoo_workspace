(define (problem schedule-25-5)
(:domain schedule)
(:objects
    B1
    A1
    Z0
    W0
    V0
    U0
    S0
    R0
    P0
    Q0
    O0
    N0
    M0
    L0
    K0
    J0
    I0
    H0
    G0
    F0
    E0
    D0
    C0
    CIRCULAR
    TWO
    THREE
    BLUE
    YELLOW
    BACK
    RED
    B0
    FRONT
    ONE
    BLACK
    OBLONG
    A0
)
(:init
    (idle punch) (idle drill-press) (idle lathe) (idle roller) (idle polisher)
    (idle immersion-painter) (idle spray-painter) (idle grinder) (ru unwantedargs)
    (SHAPE A0 OBLONG)
    (SURFACE-CONDITION A0 SMOOTH)
    (PAINTED A0 BLUE)
    (HAS-HOLE A0 ONE FRONT) (lasthole A0 ONE FRONT) (linked A0 nowidth noorient ONE FRONT)
    (TEMPERATURE A0 COLD)
    (SHAPE B0 CIRCULAR)
    (SURFACE-CONDITION B0 SMOOTH)
    (PAINTED B0 BLACK)
    (HAS-HOLE B0 THREE FRONT) (lasthole B0 THREE FRONT) (linked B0 nowidth noorient THREE FRONT)
    (TEMPERATURE B0 COLD)
    (SHAPE C0 CIRCULAR)
    (SURFACE-CONDITION C0 SMOOTH)
    (PAINTED C0 YELLOW)
    (HAS-HOLE C0 ONE BACK) (lasthole C0 ONE BACK) (linked C0 nowidth noorient ONE BACK)
    (TEMPERATURE C0 COLD)
    (SHAPE D0 CIRCULAR)
    (SURFACE-CONDITION D0 SMOOTH)
    (PAINTED D0 YELLOW)
    (HAS-HOLE D0 TWO BACK) (lasthole D0 TWO BACK) (linked D0 nowidth noorient TWO BACK)
    (TEMPERATURE D0 COLD)
    (SHAPE E0 CYLINDRICAL)
    (SURFACE-CONDITION E0 ROUGH)
    (PAINTED E0 YELLOW)
    (HAS-HOLE E0 ONE FRONT) (lasthole E0 ONE FRONT) (linked E0 nowidth noorient ONE FRONT)
    (TEMPERATURE E0 COLD)
    (SHAPE F0 CIRCULAR)
    (SURFACE-CONDITION F0 POLISHED)
    (PAINTED F0 BLACK)
    (HAS-HOLE F0 ONE FRONT) (lasthole F0 ONE FRONT) (linked F0 nowidth noorient ONE FRONT)
    (TEMPERATURE F0 COLD)
    (SHAPE G0 CIRCULAR)
    (SURFACE-CONDITION G0 POLISHED)
    (PAINTED G0 BLUE)
    (HAS-HOLE G0 TWO FRONT) (lasthole G0 TWO FRONT) (linked G0 nowidth noorient TWO FRONT)
    (TEMPERATURE G0 COLD)
    (SHAPE H0 OBLONG)
    (SURFACE-CONDITION H0 POLISHED)
    (PAINTED H0 YELLOW)
    (HAS-HOLE H0 ONE FRONT) (lasthole H0 ONE FRONT) (linked H0 nowidth noorient ONE FRONT)
    (TEMPERATURE H0 COLD)
    (SHAPE I0 CYLINDRICAL)
    (SURFACE-CONDITION I0 ROUGH)
    (PAINTED I0 RED)
    (HAS-HOLE I0 ONE BACK) (lasthole I0 ONE BACK) (linked I0 nowidth noorient ONE BACK)
    (TEMPERATURE I0 COLD)
    (SHAPE J0 CIRCULAR)
    (SURFACE-CONDITION J0 POLISHED)
    (PAINTED J0 YELLOW)
    (HAS-HOLE J0 TWO FRONT) (lasthole J0 TWO FRONT) (linked J0 nowidth noorient TWO FRONT)
    (TEMPERATURE J0 COLD)
    (SHAPE K0 OBLONG)
    (SURFACE-CONDITION K0 SMOOTH)
    (PAINTED K0 RED)
    (HAS-HOLE K0 THREE FRONT) (lasthole K0 THREE FRONT) (linked K0 nowidth noorient THREE FRONT)
    (TEMPERATURE K0 COLD)
    (SHAPE L0 CYLINDRICAL)
    (SURFACE-CONDITION L0 ROUGH)
    (PAINTED L0 BLUE)
    (HAS-HOLE L0 THREE BACK) (lasthole L0 THREE BACK) (linked L0 nowidth noorient THREE BACK)
    (TEMPERATURE L0 COLD)
    (SHAPE M0 OBLONG)
    (SURFACE-CONDITION M0 ROUGH)
    (PAINTED M0 BLACK)
    (HAS-HOLE M0 THREE FRONT) (lasthole M0 THREE FRONT) (linked M0 nowidth noorient THREE FRONT)
    (TEMPERATURE M0 COLD)
    (SHAPE N0 CYLINDRICAL)
    (SURFACE-CONDITION N0 ROUGH)
    (PAINTED N0 BLUE)
    (HAS-HOLE N0 THREE FRONT) (lasthole N0 THREE FRONT) (linked N0 nowidth noorient THREE FRONT)
    (TEMPERATURE N0 COLD)
    (SHAPE O0 CIRCULAR)
    (SURFACE-CONDITION O0 SMOOTH)
    (PAINTED O0 BLUE)
    (HAS-HOLE O0 TWO BACK) (lasthole O0 TWO BACK) (linked O0 nowidth noorient TWO BACK)
    (TEMPERATURE O0 COLD)
    (SHAPE Q0 CYLINDRICAL)
    (SURFACE-CONDITION Q0 POLISHED)
    (PAINTED Q0 YELLOW)
    (HAS-HOLE Q0 TWO BACK) (lasthole Q0 TWO BACK) (linked Q0 nowidth noorient TWO BACK)
    (TEMPERATURE Q0 COLD)
    (SHAPE P0 CYLINDRICAL)
    (SURFACE-CONDITION P0 POLISHED)
    (PAINTED P0 YELLOW)
    (HAS-HOLE P0 THREE FRONT) (lasthole P0 THREE FRONT) (linked P0 nowidth noorient THREE FRONT)
    (TEMPERATURE P0 COLD)
    (SHAPE R0 CIRCULAR)
    (SURFACE-CONDITION R0 POLISHED)
    (PAINTED R0 YELLOW)
    (HAS-HOLE R0 ONE FRONT) (lasthole R0 ONE FRONT) (linked R0 nowidth noorient ONE FRONT)
    (TEMPERATURE R0 COLD)
    (SHAPE S0 CIRCULAR)
    (SURFACE-CONDITION S0 SMOOTH)
    (PAINTED S0 BLUE)
    (HAS-HOLE S0 ONE BACK) (lasthole S0 ONE BACK) (linked S0 nowidth noorient ONE BACK)
    (TEMPERATURE S0 COLD)
    (SHAPE U0 CYLINDRICAL)
    (SURFACE-CONDITION U0 ROUGH)
    (PAINTED U0 RED)
    (HAS-HOLE U0 THREE BACK) (lasthole U0 THREE BACK) (linked U0 nowidth noorient THREE BACK)
    (TEMPERATURE U0 COLD)
    (SHAPE V0 CIRCULAR)
    (SURFACE-CONDITION V0 SMOOTH)
    (PAINTED V0 RED)
    (HAS-HOLE V0 TWO FRONT) (lasthole V0 TWO FRONT) (linked V0 nowidth noorient TWO FRONT)
    (TEMPERATURE V0 COLD)
    (SHAPE W0 CYLINDRICAL)
    (SURFACE-CONDITION W0 POLISHED)
    (PAINTED W0 YELLOW)
    (HAS-HOLE W0 ONE BACK) (lasthole W0 ONE BACK) (linked W0 nowidth noorient ONE BACK)
    (TEMPERATURE W0 COLD)
    (SHAPE Z0 OBLONG)
    (SURFACE-CONDITION Z0 SMOOTH)
    (PAINTED Z0 BLACK)
    (HAS-HOLE Z0 ONE FRONT) (lasthole Z0 ONE FRONT) (linked Z0 nowidth noorient ONE FRONT)
    (TEMPERATURE Z0 COLD)
    (SHAPE A1 OBLONG)
    (SURFACE-CONDITION A1 SMOOTH)
    (PAINTED A1 BLACK)
    (HAS-HOLE A1 ONE BACK) (lasthole A1 ONE BACK) (linked A1 nowidth noorient ONE BACK)
    (TEMPERATURE A1 COLD)
    (SHAPE B1 CYLINDRICAL)
    (SURFACE-CONDITION B1 POLISHED)
    (PAINTED B1 RED)
    (HAS-HOLE B1 TWO FRONT) (lasthole B1 TWO FRONT) (linked B1 nowidth noorient TWO FRONT)
    (TEMPERATURE B1 COLD)
    (CAN-ORIENT DRILL-PRESS BACK)
    (CAN-ORIENT PUNCH BACK)
    (CAN-ORIENT DRILL-PRESS FRONT)
    (CAN-ORIENT PUNCH FRONT)
    (HAS-PAINT IMMERSION-PAINTER YELLOW)
    (HAS-PAINT SPRAY-PAINTER YELLOW)
    (HAS-PAINT IMMERSION-PAINTER BLUE)
    (HAS-PAINT SPRAY-PAINTER BLUE)
    (HAS-PAINT IMMERSION-PAINTER BLACK)
    (HAS-PAINT SPRAY-PAINTER BLACK)
    (HAS-PAINT IMMERSION-PAINTER RED)
    (HAS-PAINT SPRAY-PAINTER RED)
    (HAS-BIT DRILL-PRESS THREE)
    (HAS-BIT PUNCH THREE)
    (HAS-BIT DRILL-PRESS TWO)
    (HAS-BIT PUNCH TWO)
    (HAS-BIT DRILL-PRESS ONE)
    (HAS-BIT PUNCH ONE)
    (PART B1) (unscheduled B1)
    (PART A1) (unscheduled A1)
    (PART Z0) (unscheduled Z0)
    (PART W0) (unscheduled W0)
    (PART V0) (unscheduled V0)
    (PART U0) (unscheduled U0)
    (PART S0) (unscheduled S0)
    (PART R0) (unscheduled R0)
    (PART P0) (unscheduled P0)
    (PART Q0) (unscheduled Q0)
    (PART O0) (unscheduled O0)
    (PART N0) (unscheduled N0)
    (PART M0) (unscheduled M0)
    (PART L0) (unscheduled L0)
    (PART K0) (unscheduled K0)
    (PART J0) (unscheduled J0)
    (PART I0) (unscheduled I0)
    (PART H0) (unscheduled H0)
    (PART G0) (unscheduled G0)
    (PART F0) (unscheduled F0)
    (PART E0) (unscheduled E0)
    (PART D0) (unscheduled D0)
    (PART C0) (unscheduled C0)
    (PART B0) (unscheduled B0)
    (PART A0) (unscheduled A0)
)
(:goal (and
    (SHAPE B0 CYLINDRICAL)
    (PAINTED G0 YELLOW)
    (PAINTED N0 RED)
    (SHAPE M0 CYLINDRICAL)
    (SURFACE-CONDITION V0 ROUGH)
    (SURFACE-CONDITION Z0 POLISHED)
    (PAINTED B0 BLUE)
    (PAINTED W0 BLACK)
    (PAINTED Z0 YELLOW)
    (SHAPE A1 CYLINDRICAL)
    (PAINTED D0 BLACK)
    (SURFACE-CONDITION B1 ROUGH)
    (SURFACE-CONDITION J0 SMOOTH)
    (SURFACE-CONDITION I0 SMOOTH)
    (PAINTED E0 BLACK)
    (SHAPE C0 CYLINDRICAL)
    (SHAPE R0 CYLINDRICAL)
    (SURFACE-CONDITION R0 ROUGH)
    (SURFACE-CONDITION A1 ROUGH)
    (SURFACE-CONDITION E0 SMOOTH)
    (SURFACE-CONDITION A0 ROUGH)
    (SHAPE A0 CYLINDRICAL)
    (PAINTED J0 BLACK)
    (SURFACE-CONDITION D0 ROUGH)
    (SURFACE-CONDITION G0 ROUGH)
)))