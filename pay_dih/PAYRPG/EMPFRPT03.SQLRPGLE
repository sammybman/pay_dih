000100950605     H DECEDIT('.') DATEDIT(*DMY/)
000200170216     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR') alwnull(*usrctl)
000300950605     F**
000400010907     F** EMPLOYEES' Monthly Deductions
000500010907     F** PROGRAMMER------S.Britton  & C.Williams
000600950605     F**
000700010907     F* Employee Master File
000800010907     Fempmast   if   e           k disk
000900010907     F* Liability Master File
001000980406     Fliabm     if   e           k disk
001100010907     F* Payroll Setup Deduction File
001200060214     Fpaysetup  uf a e             disk    commit
001300021006     Fpaycycd   if   e           k disk
001400021005     Fqsysprt   o    f  132        printer oflind(*inof) usropn
001500020207     ** Payroll/general PROTOTYPES
001600111013      /copy QCPYSRC,PAYPR
001700111013      /copy QCPYSRC,genpr
001800111013      /copy QCPYSRC,emppr
001900980406     **
002000980406     D la              s             11  2 dim(12)
002100010907     D mth             s              2  0
002200010907     D prd             s              6  0
002300021005     D prvprd          s              6  0
002400021005     D prendd          s              8  0
002500010907     D deduction       s             11  2
002600021005     D total_ded       s             11  2
002700021005     D total_lia       s             11  2
002800020207     D coname          s             30
002900050514     D countr          s              5  0
003000021005     D year            s              4  0
003100021005     D month           s              9
003200021005     D upd_flag        s              1
003300021006     D prtcyc          s              2
003400021006     D cycdesc#        s            + 2    like(cycdesc)
003500111013     D esysdedcyc      s              2
003600111013     D esysdedcyh      s              2
003700980409     **
003800980409     D  in             c                   const('abcdefghijklmnopqrstuvwxyz')
003900980409     D  out            c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
004000980406     **
004100980407     D                sds
004200980407     D  @pgm             *proc
004300111013     D  @user                358    367
004400980407     D  @sdate               276    281  0
004500980407     D  @stime               282    287  0
004600980406     **
004700980406     Ilifmt
004800980406     I              liamt01                     la(01)
004900980406     I              liamt02                     la(02)
005000980406     I              liamt03                     la(03)
005100980406     I              liamt04                     la(04)
005200980406     I              liamt05                     la(05)
005300980406     I              liamt06                     la(06)
005400980406     I              liamt07                     la(07)
005500980406     I              liamt08                     la(08)
005600980406     I              liamt09                     la(09)
005700980406     I              liamt10                     la(10)
005800980406     I              liamt11                     la(11)
005900980406     I              liamt12                     la(12)
006000021005     **
006100021005     C     trn_key       klist
006200021005     C                   kfld                    emseltyp
006300021005     C                   kfld                    styp              3 0
006400021005     C                   kfld                    emp
006500111013
006600111013      /free
006700111013        exec sql
006800111013           select esysdedcyc, esysdedcyh into :esysdedcyc, :esysdedcyh
006900111013                  from empcon;
007000050514          countr = *zeros;
007100050514          dow not get_ltype_info('RD':-1:ematf_struc:countr);
007200021006          emdsp = %xlate(in:out:emdsp);
007300021006      /end-free
007400010907
007500020207     C                   if        emalwpyd = 'Y'
007600010907     C                             and emautpyd <> 'Y'
007700010907     C
007800021005     C                   open      qsysprt
007900021005     C                   except    hedr
008000980407     **
008100010907     C     *loval        setll     emprec
008200010907     C                   read      emprec
008300010907     C                   dow       not(%eof)
008400980407     **  Select Regular or Executive employees
008500021005     C                   eval      *in77 = *off
008600021005     C                   eval      *in78 = *off
008700980407     **
008800020207     C                   if        get_cat(status) = 'A'
008900980407     **
009000010907     C     trn_key       chain     lifmt
009100980406     **
009200010907     C                   if        %found and la(mth) > *zeros
009300021005
009400021005     C                   select
009500021005      ** transactions to be deducted
009600021005     C                   when      licode <> 'N'
009700010907
009800010907     **
009900010907     C                   if        la(mth) >= lifix
010000010907     C                   eval      deduction = lifix
010100980406     C                   else
010200010907     C                   eval      deduction = la(mth)
010300021005     C                   eval      *in78 = *on
010400980406     C                   endif
010500050514     C                   eval      total_Lia += la(mth)
010600050514     C                   eval      total_Ded += deduction
010700010907     **  Write to deduction file
010800021005     C                   if        upd_flag = 'U'
010900010907     C                   eval      pstemp = emp
011000021005     C                   eval      psttype = emseltyp
011100111013     C                   eval      pstcyc = esysdedcyc
011200010907     C                   eval      pstamt  = deduction
011300980407     C                   write     pstfmt
011400021005     C                   endif
011500021005
011600021005      ** transactions with code not to be deducted
011700021005     C                   other
011800021005
011900021005     C                   eval      *in77 = *on
012000021005     C                   endsl
012100021005     **
012200021005     C                   if        *inof
012300021005     C                   except    hedr
012400021005     C                   endif
012500021005
012600021005     C                   except    detail
012700021005     **
012800980407     C                   endif
012900980407     C                   endif
013000021005
013100010907     C                   read      emprec
013200010907     C                   enddo
013300021005
013400021005     C                   except    total
013500021005     C                   close     qsysprt
013600010907     C                   endif
013700050514      /free
013800050514          enddo;
013900050514          *inlr = *on;
014000050514          return;
014100050514      /end-free
014200980407     **
014300980407      ** Initial Routine
014400980407     **
014500020207     C     *inzsr        begsr
014600021005     **
014700021005     C     *entry        plist
014800021005     C                   parm                    prd
014900021005     C                   parm                    upd_flag
015000021005     **
015100021005     C                   move      prd           mth
015200021005     C                   movel     prd           year
015300980409     **
015400020207     C                   eval      coname = get_coname
015500021005     C                   eval      month = get_month(mth)
015600021005     C                   eval      prvprd = get_new_prd(prd:1:'S')
015700021005     C                   callp     get_prd_info('*rng':*blanks:prvprd:
015800021005     C                                          *omit:prendd)
015900021006
016000021006     C                   select
016100021006     **
016200021006     C                   when      upd_flag  = 'U'
016300021006
016400111013     C                   if        esysdedcyc = *blanks
016500021006     C                   eval      prtcyc = '*M'
016600021006     C                   else
016700111013     C                   eval      prtcyc = esysdedcyc
016800021006     C                   endif
016900021006     **
017000021006     C                   other
017100021006
017200111013     C                   if        esysdedcyh = *blanks
017300021006     C                   eval      prtcyc = '*M'
017400021006     C                   else
017500111013     C                   eval      prtcyc = esysdedcyh
017600021006     C                   endif
017700021006     C                   endsl
017800021006     ** get Run-type cycle
017900021006     C     prtcyc        chain     cycfmt
018000021006     C                   eval      cycdesc# = '(' + %trim(cycdesc) + ')'
018100980409     **
018200980407     C                   endsr
018300980407     **
018400021005     Oqsysprt   e            hedr           2 02
018500980409     O                       @pgm                10
018600020207     O                       coname              77
018700980409     O                       @sdate        Y    130
018800021005     O          e            hedr           2
018900980409     O                       @user               10
019000021006     O                       emdsp               42
019100021006     O                                           65 'PAYROLL DEDUCTIONS -'
019200021006     O                       month               75
019300021006     O                       year                80
019400980409     O                       @stime             130 '  :  :  '
019500021005     O          e            hedr           1
019600021005     O                                            8 'EMP.'
019700021005     O                                           16 'DEPT.'
019800021005     O                                           31 'EMPLOYEE NAME'
019900021005     O                                           58 'BALANCE AT'
020000021005     O                                           90 'TOTAL'
020100021006     O                       cycdesc#           113
020200021005     O          e            hedr           2
020300021005     O                                            7 'NO.'
020400021005     O                                           15 'NO.'
020500021005     O                       prendd              58 '    /  /  '
020600021005     O                                           93 'DEDUCTIONS'
020700980407     **
020800980407     O          e            detail         2
020900980406     O                       emp           Z      8
021000980406     O                       dept          Z     14
021100980406     O                       surnam              31
021200980406     O                       christ              44
021300980406     O                       la(mth)       AB    60
021400021005     O                       deduction     1b    93
021500021005     O                  78                        2 '**'
021600021005     O                  77                        2 '??'
021700980406     **  Print final totals
021800021005     O          e            total       1  2
021900021005     O                       total_lia     Ab    60
022000021005     O                       total_ded     1b    93
022100980406     **
022200021005     O          e            total          2
022300940829     O                                           24 '?? - EMPLOYEE HAS BALANC'
022400940829     O                                           42 'E & NO DEDUCTIONS.'
022500980406     **
022600021005     O          e            total
022700940829     O                                           24 '** - EMPLOYEE DEBT WILL '
022800021005     O                                           49 'BE ERASED THIS CYCLE'
