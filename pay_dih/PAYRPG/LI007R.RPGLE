000100091121     H DECEDIT('.') DATEDIT(*DMY/) BNDDIR('DISDIR') ACTGRP(*caller)
000200091121     H DFTACTGRP(*NO)
000300950606     F*EMPLOYEES LOAN & C.O.E ACCOUNTS---BY EMPLOYEE NUMBER
000400980122     F*PROGRAMMER:-S. britton
000500020206     F* LIABILITY TRANSACTION FILE
000600980119     Fliabtl06  if   e           k disk
000700020206     F* LIABILITY MASTER FILE
000800980122     Fliabm     if   e           k disk
000900020206     F* EMPLOYEE MASTER FILE
001000980121     Fempmast   if   e           k disk
001100050513     Fqsysprt   o    f  132        printer oflind(*inof)
001200980121     F                                     formlen(066)
001300980121     F                                     formofl(066)
001400020206     ** Payroll/general PROTOTYPES
001500151009      /copy *libl/qcpysrc,PAYPR
001600151009      /copy qcpysrc,emppr
001700151009      /copy qcpysrc,genpr
001800970403     **
001900980120     D                sds
002000980120     D  @pgm             *proc
002100980120     D  @user                254    263
002200980119     **
002300980119     D la              s             11  2 dim(12)
002400980122     D end_date        s              8  0
002500980121     D str_date        s              8  0
002600980122     D @hdr_cnst       s             55
002700980122     D @emp_cnst       s             47
002800980122     D baltxt          c                   const('Previous Balance')
002900020206     D coname          s             30
003000020206     D prd#            s              6  0
003100020206     D month           s              9
003200020206     D prstrd          s              8  0
003300020206     D prendd          s              8  0
003400980121     **
003500980121     D                 ds
003600980121     D s_prd                   1      6  0
003700980121     D year                    1      4  0
003800980121     D smth                    5      6  0
003900980119     **
004000980119     Ilifmt
004100980119     I              liamt01                     la(01)
004200980119     I              liamt02                     la(02)
004300980119     I              liamt03                     la(03)
004400980119     I              liamt04                     la(04)
004500980119     I              liamt05                     la(05)
004600980119     I              liamt06                     la(06)
004700980119     I              liamt07                     la(07)
004800980119     I              liamt08                     la(08)
004900980119     I              liamt09                     la(09)
005000980119     I              liamt10                     la(10)
005100980119     I              liamt11                     la(11)
005200980119     I              liamt12                     la(12)
005300980120     **
005400980120     C     *like         define    emp           @emp
005500980122     **
005600980122     C     trn_key       klist
005700980122     C                   kfld                    typ
005800980122     C                   kfld                    liemp
005900980217     C                   kfld                    styp              3 0
006000980122     C                   kfld                    s_prd
006100980122     **
006200980122     C     trn_key2      klist
006300980122     C                   kfld                    typ
006400980217     C**                 kfld                    styp              3 0
006500980122     C                   kfld                    liemp
006600980119     **
006700980127     C                   except    rep_hedr
006800980119     **
006900980122     C     typ           chain     lifmt                              lr
007000980122     **
007100980122     C                   dow       not(*inlr)
007200980127     C                   if        *inof
007300980127     C                   except    rep_hedr
007400980127     C                   setoff                                       of
007500980127     C                   endif
007600980127     **
007700980122     ** Select employees if balance not equal to zero
007800980122     C                   if        la(emth) <> *zeros
007900980122     C     liemp         chain     emprec                             88
008000980127     C
008100980122     C                   add       la(smth)      finbal           10 2
008200980122     C                   add       la(emth)      finamt           10 2
008300980122     **
008400980126     C     trn_key       setgt     ltfmt
008500980126     C                   read      ltfmt                                  99
008600980126     C                   if        ltemp <> liemp
008700980126     C                   seton                                        99
008800980126     C                   endif
008900980120     **
009000980122     ** Select transactions for report period
009100980122     C                   dow       not(*in99)
009200980120     **
009300980127     C                   if        ltorg <> 'PAY'
009400980120     C     ltamt         comp      *zeros                             9897
009500980120     C                   if        *in98
009600980120     C                   add       ltamt         totdeb            9 2
009700980120     C                   else
009800980120     C                   add       ltamt         totcre            9 2
009900980120     C                   endif
010000980127     **
010100980127     C                   else
010200980127     C                   add       ltamt         totpay            9 2
010300980127     C                   endif
010400980120     **
010500980122     C     trn_key2      reade     ltfmt                                  99
010600980122     C  n99prd           comp      ltprno                               99
010700980119     C                   enddo
010800980122     **
010900980122     ** Print employee total lines
011000980122     **
011100980122     C                   add       totdeb        findeb           10 2
011200980122     C                   add       totcre        fincre           10 2
011300980127     C                   add       totpay        finpay           10 2
011400980122     C                   except    total
011500980122     C                   endif
011600980122
011700980122     C     typ           reade     lifmt                                  lr
011800980122     C                   enddo
011900980122     **
012000980122     ** Print report final totals
012100980120     C                   except    fintot
012200980120     ******************************************************
012300980120     ** Initial routine **
012400980120     ******************************************************
012500020206     C     *inzsr        begsr
012600020206     **
012700020206     C     *entry        plist
012800020206     C                   parm                    prd               6 0
012900020206     C                   parm                    typ               2 0
013000020206     **
013100020206     C                   movel     prd           rp_year           4
013200020206     C                   move      prd           emth              2 0
013300020206     C                   movel     prd           year              4 0
013400980122     ** Quarterly processing for Loans and C.O.E. accounts only
013500980122     C                   if        typ = 1 or typ = 2
013600980122     **
013700980121     C                   if        emth = 03  or emth = 06
013800980121     C                             or emth = 09 or emth = 12
013900980120     C                   seton                                        12
014000980120     C                   endif
014100980120     **
014200980120     C                   if        emth <= 3
014300980120     C                   z-add     12            smth              2 0
014400980120     C                   else
014500980120     C                   if        emth > 3 and emth <= 6
014600980120     C                   z-add     3             smth
014700980120     C                   else
014800980120     C                   if        emth > 6 and emth <= 9
014900980120     C                   z-add     6             smth
015000980120     C                   else
015100980120     C                   if        emth > 9 and emth <= 12
015200980120     C                   z-add     9             smth
015300980120     C                   endif
015400980120     C                   endif
015500980120     C                   endif
015600980120     C                   endif
015700980122     **
015800980122     C                   else
015900980122     **
016000980122     C                   eval      smth = emth - 1
016100980122     C                   if        smth = 0
016200980122     C                   z-add     12            smth
016300980122     C                   endif
016400980122     C                   endif
016500980121     **
016600050513      /free
016700050513           get_ltype_info('RD':typ:ematf_struc);
016800050513      /end-free
016900980122     C     emdsp         cat(p)    baltxt:1      @emp_cnst
017000980121     **
017100980121     ** Get proper year for start period
017200980121     C                   if        smth = 12
017300980121     C                   sub       1             year
017400980121     C                   endif
017500980121     **
017600020206     C                   eval      month = get_month(emth)
017700980122     C     emdsp         cat(p)    'as at':1     @hdr_cnst
017800020206     C                   cat       month:1       @hdr_cnst
017900980122     C                   cat       rp_year:1     @hdr_cnst
018000980121     **
018100020206     ** get period  dates
018200020206     C                   callp     get_prd_info('*rng':*blanks:prd:
018300020206     C                                          prstrd:prendd)
018400020206     C                   z-add     prendd        end_date
018500020206     C                   z-add     s_prd         prd#
018600020206     C                   callp     get_prd_info('*rng':*blanks:prd#:
018700020206     C                                          prstrd:prendd)
018800020206     C                   z-add     prendd        str_date
018900020206
019000020206     C                   eval      coname = get_coname
019100980120     C                   endsr
019200980120     **
019300980120     **
019400050513     Oqsysprt   e            rep_hedr    2  1
019500980120     O                       @pgm                10
019600020206     O                       coname              77
019700980122     O          e            rep_hedr       2
019800980120     O                       @user               10
019900980122     O          e            rep_hedr       2
020000980122     O                       @hdr_cnst           57
020100980122     O          e            rep_hedr       2
020200980122     O                                            8 'Code:-'
020300980122     O                       emacct            +  0
020400980122     **
020500980127     O          e            rep_hedr       1
020600980127     O                                           59 'BALANCE'
020700980127     O                                          103 'PAYROLL'
020800980127     O                                          117 'NET'
020900980127     O          e            rep_hedr       1
021000980127     O                                           12 'EMPLOYEE NO.'
021100980127     O                                           34 'NAME'
021200980127     O                                           58 'B/FWD.'
021300980127     O                                           74 'DEBITS'
021400980127     O                                           89 'CREDITS'
021500980127     O                                          104 'DEDUCTION'
021600980127     O                                          119 'BALANCE'
021700980127     O          e            rep_hedr       2
021800980127     O                                          109 'AT'
021900980127     O                       end_date           120 '    /  /  '
022000980120     **
022100050513     Oqsysprt   e            total          1
022200980127     O                       emp           z      7
022300980127     O                       dept          z     10
022400980127     O                       christ              32
022500980127     O                       surnam              46
022600980127     O                       la(smth)      bb    61
022700980127     O                       totdeb        bb    76
022800980127     O                       totcre        bb    91
022900980127     O                       totpay        bb   106
023000980127     O                       la(emth)      bb   121
023100980127     **
023200050513     Oqsysprt   e            fintot      2
023300980127     O                       finbal        b     61
023400980127     O                       findeb        b     76
023500980127     O                       fincre        b     91
023600980127     O                       finpay        b    106
023700980127     O                       finamt        b    121
023800980120     **
