000100950418     H DATEDIT(*DMY)
000200171014     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR') alwnull(*usrctl)
000300950418     F** SAVINGS LETTERS
000400010918     F** ADJUSTMENTS------T.I.BYNOE  ** ADJ. S.BRITTON
000500011204     Fmasall    ip   e           k disk
000600011204     Fsavmast   if   e           k disk
000700021128     Fsavyear   if   e           k disk    rename(savrec:savfmt)
000800010921     Fliabtl07  if   e           k disk
000900011228     Fpayhisl11 if   e           k disk
001000010921     Fpaytranl03if   e           k disk
001100021127     Fsavconf   if   e             disk
001200021127     Fsavconf2  if   e           k disk
001300021127     Fqsysprt   o    f  132        printer oflind(*inof)
001400010921     ** Payroll PROTOTYPES
001500131030      /copy qcpysrc,PAYPR
001600131030      /copy qcpysrc,GENPR
001700010918     **
001800010921      ** Include data definition
001900131030      /copy qcpysrc,savcopy#4
002000010921
002100010921      ** Include Input specs.
002200131030      /copy qcpysrc,savcopy#2
002300010918     **
002400021128     C     year_key      klist
002500021128     C                   kfld                    emp
002600021128     C                   kfld                    year
002700951005     **
002800010921     C                   if        get_cat(status) = 'A'
002900011204
003000011204     c     emp           chain     savrec
003100011204     C                   if        %found
003200011204     C                   eval      emp_name = %trim(christ) + ' '
003300011204     C                                      + %trim(surnam)
003400021128     c     year_key      chain     savfmt
003500021128     C                   if        %found(savyear)
003600021128     **
003700021128     C                   eval      savpyear = savbfw
003800021128     C                   eval      prd_ded = empsav
003900021128     C                   eval      netsav = savbfw - withdr + intrst
004000021128     C                                    + empsav + comcon - savtax
004100021128     C                   else
004200010918
004300010921      ** Include Year-end calcs
004400131030      /copy qcpysrc,savcopy#3
004500010921
004600021128     C                   endif
004700021128     C                   eval      result = netsav + withdr + savtax
004800010918     C                   except
004900951005     **
005000010918     C                   endif
005100011204     C                   endif
005200010921     **
005300010921      ** Include Period calc sub routines
005400021127      /copy *libl/payrpg,savcopy#
005500010921     ** Init routine
005600010921     C     *inzsr        begsr
005700010921     C     *entry        plist
005800010921     C                   parm                    prno
005900010921     C                   parm                    run_type
006000010921     **
006100010921     C                   move      prno          cmth
006200010921     **
006300010921     **  Get Previous Month
006400010921     C                   eval      pmth = cmth - 1
006500010921     C                   if        pmth = 0
006600010921     C                   eval      pmth = 12
006700010921     C                   endif
006800010921     **
006900010921     C                   movel     prno          year
007000010921     ** Get period end-date
007100010921     C                   callp     get_prd_info('*rng':*blanks:prno:*omit:
007200010921     C                                          end_date)
007300021127     C                   eval      coname = get_coname
007400021127     ** read control file
007500021127     C                   read      sacfmt
007600021127     C                   eval      pay_type = sacdtype
007700021127     C                   eval      lib_type = sactype
007800021128     ** control previous file reference
007900021128     C                   if        cmth >= sacmth
008000021128     C                   eval      year = year + 1
008100021128     C                   endif
008200021128     C                   eval      last_year = year - 1
008300010921     C                   endsr
008400021127     Oqsysprt   e                           1 04
008500021127     O                       coname              57
008600010918     O          e                           2
008700021127     O****                                       52 '====================='
008800010918     O          e                        1  1
008900950418     O                                           37 'S A V I N G S'
009000950418     O                                           62 'C E R T I F I C A T E'
009100010918     O          e                           2
009200950418     O                                           37 '-------------'
009300950418     O                                           62 '---------------------'
009400010918     O          e                        1  1
009500950418     O                                           32 'FINANCIAL    YEAR'
009600950418     O                                           47 'ENDED   30TH'
009700950418     O                                           59 'SEPTEMBER'
009800010921     O                       year                67
009900010918     O          e                           2
010000950418     O                                           32 '---------    ----'
010100950418     O                                           47 '-----   ----'
010200950418     O                                           59 '---------'
010300950418     O                                           67 '----'
010400010918     O          e                        1  1
010500950418     O                                           19 'T H I S   I S   TO'
010600950418     O                                           45 'C E R T I F Y   T H A T'
010700950418     O                                           64 'AT   THE   CLOSE'
010800950418     O                                           76 'OF   THE'
010900010918     O          e                           1
011000950418     O                                           17 'FINANCIAL   YEAR'
011100950418     O                                           38 'ABOVE - MENTIONED.'
011200950418     O                                           51 'THE   NET'
011300950418     O                                           76 'BALANCE   OF   YOUR'
011400010918     O          e                           2
011500950418     O                                           19 'SAVING   WAS   THE'
011600950418     O                                           44 'LAST   AMOUNT   STATED'
011700950418     O                                           52 'BELOW'
011800010918     O          e                        1
011900950418     O                                            5 'NAME'
012000950418     O                                           54 'EMPLOYEE NO.'
012100950418     O                                           71 'DEPT NO.'
012200010918     O          e
012300010921     O                       emp_name            38
012400010921     O                       emp           z     58
012500010921     O                       dept          z     74
012600010918     O          e                        1  1
012700950418     O                                            5 '----'
012800950418     O                                           54 '-------- --'
012900950418     O                                           71 '---- ---'
013000010918     O          e                        2  2
013100950418     O                                           18 'FURTHER   DETAILS'
013200950418     O                                           43 'ARE   GIVEN   BELOW *'
013300010918     O          e                           2
013400950418     O                                           18 'TOTAL   TO   30TH'
013500001005     O                                           32 'SEPTEMBER, '
013600010921     O                       last_year           36
013700010921     O                       savpyear      b     74
013800010918     O          e                           2
013900950418     O                                           20 'YOUR   CONTRIBUTION'
014000950418     O                                           40 'FOR   THE   YEAR '
014100010921     O                       prd_Ded       2b    72
014200010918     O          e                           2
014300950418     O                                           24 'COMPANY''S  CONTRIBUTION'
014400950418     O                                           44 'FOR   THE   YEAR'
014500010921     O                       comcon        2b    72
014600010918     O          e                           2
014700950418     O                                           21 'INTEREST   FOR   THE'
014800950418     O                                           29 'YEAR '
014900010921     O                       intrst        2b    72
015000010918     O          e                           1
015100950418     O                                           74 '------------'
015200010918     O          e                           2
015300950418     O                                           58 'SUB  TOTAL'
015400010921     O                       result        bb    74
015500950418     O                                           75 '*'
015600010918     O          e                           2
015700950418     O                                           19 'LESS   WITHDRAWALS'
015800010921     O                       withdr        ab    74
015900021128     O          e                           2
016000021128     O                                           11 'LESS   TAX'
016100021128     O                       savtax        ab    74
016200010918     O          e                           1
016300950418     O                                           74 '------------'
016400010918     O          e                           1
016500950418     O                                           24 'NET   SAVINGS   AS   AT'
016600010921     O                       end_date            35 '    /  /  '
016700010921     O                       netsav        a     74
016800010918     O                                           75 '*'
016900010918     O          e                           2
017000950418     O                                           74 '------------'
017100010918     O          e                           2
017200021128     O                       coname              76
017300010918     O          e                           2
017400081204     O                                           60 'D. SEEPAUL'
017500010918     O          e
017600081204     O                                           64 'FINANCE CONTROLLER/ASST.'
017700081204     O                                           74 'SECRETARY'
017800010918     O          t    lr                       01
017900010921     O                       totbfw        2     15
018000010918     O                       totint        2     27
018100010921     O                       totemp        2     37
018200010918     O                       totcom        2     49
018300010921     O                       totwit        2     59
018400010921     O                       totsav        2     71
