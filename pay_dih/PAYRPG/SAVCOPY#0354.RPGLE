000100010920     D prno            s              6  0
000200010921     D coname          s             30
000300010920     D run_type        s              1
000400010921     D year            s              4  0
000500010921     D last_year       s                   like(year)
000600010921     D end_date        s              8  0
000700010921     D last_date       s              8  0
000800010921     D result          s             11  2
000900010920     D intrst          s             11  2
001000010920     D comcon          s             11  2
001100010920     D netsav          s             11  2
001200010920     D totbfw          s             11  2
001300010920     D totint          s             11  2
001400010920     D totcom          s             11  2
001500010920     D totwit          s             11  2
001600010920     D totemp          s             11  2
001700010920     D totsav          s             11  2
001800010920     D yremp           s              4  0
001900040222     D*status          s              1
002000010914     D name#           s             40
002100010921     D emp_name        s            -14    like(name#)
002200010920     D prd_ded         s             11  2
002300010920     D withdr          s             11  2
002400021125     D pay_type        s              2  0
002500021125     D lib_type        s              2  0
002600010920     D cmth            s              2  0
002700010920     D pmth            s              2  0
002800010920     D wth             s             13  2 dim(12)
002900010920     D sav             s             13  2 dim(12)
003000021125     D int_type        s              1    inz('I')
003100021125     D con_type        s              1    inz('C')
