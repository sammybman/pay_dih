000100950605     H DECEDIT('.') DATEDIT(*DMY/)
000200950605     F**
000300010907     F** EMPLOYEES' Monthly Deductions
000400010907     F** PROGRAMMER------S.Britton  & C.Williams
000500950605     F**
000600010907     F* Employee Master File
000700010907     Fempmast   if   e           k disk
000800010907     F* Liability Master File
000900980406     Fliabm     if   e           k disk
001000010907     F* Payroll Setup Deduction File
001100980407     Fpaysetup  uf a e             disk
001200050513     Fqsysprt   o    f  132        printer oflind(*inof)
001300020207     ** Payroll/general PROTOTYPES
001400040222      /copy *libl/payrpg,PAYPR
001500020207      /copy genpr
001600090619      /copy emppr
001700980406     **
001800980409     D tabmon          s             02  0 dim(12) CTDATA PERRCD(6)
001900980409     D tabmth          s             09    dim(12) ALT(TABMON)
002000980409     D tabcod          s             02  0 dim(12) CTDATA PERRCD(11)
002100980409     D tabdat          s             05    dim(12) ALT(TABCOD)
002200980406     **
002300980406     D la              s             11  2 dim(12)
002400010907     D mth             s              2  0
002500010907     D prd             s              6  0
002600010907     D deduction       s             11  2
002700020207     D coname          s             30
002800980409     **
002900980409     D  in             c                   const('abcdefghijklmnopqrstuvwxyz')
003000980409     D  out            c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003100980406     **
003200980407     D                sds
003300980407     D  @pgm             *proc
003400980407     D  @user                254    263
003500980407     D  @sdate               276    281  0
003600980407     D  @stime               282    287  0
003700980406     **
003800980406     Ilifmt
003900980406     I              liamt01                     la(01)
004000980406     I              liamt02                     la(02)
004100980406     I              liamt03                     la(03)
004200980406     I              liamt04                     la(04)
004300980406     I              liamt05                     la(05)
004400980406     I              liamt06                     la(06)
004500980406     I              liamt07                     la(07)
004600980406     I              liamt08                     la(08)
004700980406     I              liamt09                     la(09)
004800980406     I              liamt10                     la(10)
004900980406     I              liamt11                     la(11)
005000980406     I              liamt12                     la(12)
005100980406     **
005200980406     C     *entry        plist
005300010907     C                   parm                    prd
005400020207     **
005500020207     c     in:out        xlate     emdsp         emdsp
005600020207     C     emdsp         cat(p)    '-':0         hedr             24
005700010907
005800020207     C                   if        emalwpyd = 'Y'
005900010907     C                             and emautpyd <> 'Y'
006000010907     C
006100970305     **
006200010907     C                   move      prd           mth
006300980406     **
006400980406     C     trn_key       klist
006500010907     C                   kfld                    emseltyp
006600980406     C                   kfld                    styp              3 0
006700980407     C                   kfld                    emp
006800980407     **
006900980409     C                   SETOFF                                       98
007000980409     C                   SETOFF                                       789085
007100980407     **
007200010907     C     *loval        setll     emprec
007300010907     C                   read      emprec
007400010907     C                   dow       not(%eof)
007500980407     **  Select Regular or Executive employees
007600980407     **
007700020207     C                   if        get_cat(status) = 'A'
007800980407     **
007900010907     C     trn_key       chain     lifmt
008000980406     **
008100010907     C                   if        %found and la(mth) > *zeros
008200980406     C                             and licode <> 'N'
008300010907
008400010907     **
008500010907     C                   if        la(mth) >= lifix
008600010907     C                   eval      deduction = lifix
008700980406     C                   else
008800010907     C                   eval      deduction = la(mth)
008900980406     C                   endif
009000010907     **  Write to deduction file
009100010907     C                   eval      pstemp = emp
009200010907     C                   eval      psttype = emseltyp
009300010907     C                   eval      pstamt  = deduction
009400980407     C                   write     pstfmt
009500980407     **
009600980407     C                   except    detail
009700980407     C                   endif
009800980407     C                   endif
009900010907     C                   read      emprec
010000010907     C                   enddo
010100010907     C                   endif
010200980407     **
010300980407      ** Initial Routine
010400980407     **
010500020207     C     *inzsr        begsr
010600980407     **     Get report dates
010700980407     C     UMONTH        IFEQ      01
010800980407     C                   SETON                                        90
010900980407     C     UYEAR         SUB       01            UYAR              2 0
011000980407     C                   ENDIF
011100980407     **
011200980407     C     UMONTH        LOOKUP    TABCOD        TABDAT                   89
011300980407     C     UMONTH        LOOKUP    TABMON        TABMTH                   91
011400980409     **
011500020207     C                   eval      coname = get_coname
011600980409     **
011700980407     C                   endsr
011800980407     **
011900090619     Oqsysprt   h    1p                     2 02
012000980407     O         or    of
012100980409     O                       @pgm                10
012200020207     O                       coname              77
012300980409     O                       @sdate        Y    130
012400980407     O          h    1p                     2
012500980407     O         or    of
012600980409     O                       @user               10
012700980409     O                       hedr                59
012800950605     O                                           83 'DEDUCTIONS-FORTNIGHTLY-'
012900980409     O                  91   tabmth              93
013000980409     O                       *year               98
013100980409     O                       @stime             130 '  :  :  '
013200980407     O          h    1p                     1
013300980407     O         or    of
013400950605     O                                           26 'EMP.   DEPT.  EMPLOYEE'
013500950605     O                                           31 'NAME'
013600950605     O                                           72 'BALANCE AT         FIRST'
013700950605     O                                           86 'SECOND'
013800950605     O                                          100 'TOTAL'
013900980407     O          h    1p                     2
014000980407     O         or    of
014100950605     O                                           15 'NO.    NO.'
014200950605     O                     89TABDAT              54
014300950605     O                                           55 '/'
014400950605     O                    N90UYEAR               57
014500950605     O                     90UYAR                57
014600950606     O                                           86 'F/NIGHT      F/NIGHT'
014700950605     O                                          103 'DEDUCTIONS'
014800980407     **
014900980407     O          e            detail         2
015000980406     O                       emp           Z      8
015100980406     O                       dept          Z     14
015200980406     O                       surnam              31
015300980406     O                       christ              44
015400980406     O                       la(mth)       AB    60
015500020207     O*                      wekone        1b    73
015600020207     O*                      wektwo        1b    86
015700020218     O                       deduction     1b   100
015800950605     O                 N85N98                     2 '**'
015900950605     O                  85                        2 '??'
016000950605     O                  78                        2 '??'
016100980406     **  Print final totals
016200980409     O          t    lr                  1  2
016300020207     O*                      totlon        A     60
016400020207     O*                      totone        1     73
016500020207     O*                      tottwo        1     86
016600020207     O*                      finded        1    100
016700980406     **
016800980409     O          t    lr                     2
016900940829     O                                           24 '?? - EMPLOYEE HAS BALANC'
017000940829     O                                           42 'E & NO DEDUCTIONS.'
017100980406     **
017200980409     O          t    lr
017300940829     O                                           24 '** - EMPLOYEE DEBT WILL '
017400940829     O                                           45 'BE ERASED THIS MONTH.'
017500950605**
01760094082901FEBRUARY 02MARCH    03APRIL    04MAY      05JUNE     06JULY
01770094082907AUGUST   08SEPTEMBER09OCTOBER  10NOVEMBER 11DECEMBER 12JANUARY
017800940829**
0179009506050131/010228/020331/030430/040531/050630/060731/070831/080930/091031/101130/11
0180009506051231/12
