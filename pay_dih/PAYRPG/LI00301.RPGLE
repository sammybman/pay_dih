000100170216     H DECEDIT('.') DATEDIT(*DMY/)   alwnull(*usrctl)
000200980128     F*EMPLOYEES ACCOUNTS---BY EMPLOYEE NUMBER
000300980122     F*PROGRAMMER:-S. britton
000400020206     F* LIABILITY TRANSACTION FILE
000500980119     Fliabtl06  if   e           k disk
000600050513     F* EMPLOYEE MASTER FILE
000700050513     Fempmast   if   e           k disk
000800020206     F* LIABILITY MASTER FILE
000900980122     Fliabm     if   e           k disk
001000050505     Fqsysprt   o    f  132        printer oflind(*inof)
001100980121     F                                     formlen(066)
001200980121     F                                     formofl(066)
001300020108     ** Payroll/general PROTOTYPES
001400151009      /copy *libl/qcpysrc,PAYPR
001500151009      /copy qcpysrc,genpr
001600151009      /copy qcpysrc,emppr
001700970403     **
001800980120     D                sds
001900980120     D  @pgm             *proc
002000980120     D  @user                254    263
002100980119     **
002200980119     D la              s             11  2 dim(12)
002300980122     D end_date        s              8  0
002400980121     D str_date        s              8  0
002500011126     d last_dept       s                   like(dept)
002600020111     d finbal          s             11  2
002700011126     d finamt          s                   like(finbal)
002800011126     d findeb          s                   like(finbal)
002900011126     d fincre          s                   like(finbal)
003000011126     d depamt          s                   like(finbal)
003100011126     d depbal          s                   like(finbal)
003200011126     d depdeb          s                   like(finbal)
003300011126     d depcre          s                   like(finbal)
003400980128     D cvtlen_prm      s              4b 0 inz(55)
003500980127     D @hdr_cnst       s             55
003600980122     D @emp_cnst       s             47
003700980122     D baltxt          c                   const('Previous Balance')
003800980404     D StatusTxt       s             10
003900020206     D month           s              9
004000020206     D prd             s              6  0
004100020206     D prd#            s              6  0
004200020206     D coname          s             30
004300020206     D prstrd          s              8  0
004400020206     D prendd          s              8  0
004500980121     **
004600980121     D                 ds
004700980121     D s_prd                   1      6  0
004800980121     D year                    1      4  0
004900980121     D smth                    5      6  0
005000980202     **
005100980202     D  in             c                   const('abcdefghijklmnopqrstuvwxyz')
005200980202     D  out            c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
005300980202     **
005400980119     Ilifmt
005500980119     I              liamt01                     la(01)
005600980119     I              liamt02                     la(02)
005700980119     I              liamt03                     la(03)
005800980119     I              liamt04                     la(04)
005900980119     I              liamt05                     la(05)
006000980119     I              liamt06                     la(06)
006100980119     I              liamt07                     la(07)
006200980119     I              liamt08                     la(08)
006300980119     I              liamt09                     la(09)
006400980119     I              liamt10                     la(10)
006500980119     I              liamt11                     la(11)
006600980119     I              liamt12                     la(12)
006700980120     **
006800980120     C     *like         define    emp           @emp
006900980122     **
007000980122     C     trn_key       klist
007100980122     C                   kfld                    typ
007200980122     C                   kfld                    liemp
007300980217     C                   kfld                    styp              3 0
007400980122     C                   kfld                    s_prd
007500980122     **
007600980122     C     trn_key2      klist
007700980122     C                   kfld                    typ
007800980122     C                   kfld                    liemp
007900980218     **
008000980218     C     trn_key3      klist
008100980218     C                   kfld                    typ
008200980218     C                   kfld                    styp
008300980218     C                   kfld                    emp
008400980119     **
008500980122     C                   except    rep_hedr
008600980404     **
008700020409     C
008800020409     ***************************************************************
008900020409     **  Report Generation **
009000020409     ***************************************************************
009100020409     C                   read      emprec                                 lr
009200020409     c                   eval      last_dept = dept
009300020409     **
009400020409     C                   dow       not(*inlr)
009500020409     **
009600020409     C     trn_key3      chain     lifmt                              88
009700020409     C                   if        not(*in88)
009800020409     **
009900020409     C     trn_key       setgt     ltfmt
010000020409     C                   read      ltfmt                                  99
010100020521     C*                  dsply                   prd
010200020521     C*                  dsply                   ltprno
010300020409     C                   if        ltemp <> liemp
010400020409     C                   seton                                        99
010500020409     C                   endif
010600020409     ** Perform department calcs.
010700020411     C                   if        runtyp = 3
010800020409     C                   exsr      dept_lvl_calc
010900020411     C                   endif
011000020409     **
011100020409     ** Select employees if balance not equal to zero
011200020409     ** or if there were transactions for the month
011300020409     **
011400020409     C                   if        la(emth) <> *zeros
011500020409     C                             or not(*in99)
011600020409     **
011700020409     C                   exsr      GetStatusTxt
011800020409     **
011900020409     C                   except    hedr
012000020409     **
012100020409     C                   eval      finbal = finbal + la(smth)
012200020409     C                   eval      finamt = finamt + la(emth)
012300020409     C                   eval      depbal = depbal + la(smth)
012400020409     C                   eval      depamt = depamt + la(emth)
012500020409     **
012600020409     C  n99prd           comp      ltprno                               99
012700020409     **
012800020409     ** Select transactions for report period
012900020409     C                   dow       not(*in99)
013000020409     **
013100020409     C     ltamt         comp      *zeros                             9897
013200020409     C                   if        *in98
013300020412     C                   add       ltamt         totdeb           10 2
013400020409     C                   else
013500020412     C                   add       ltamt         totcre           10 2
013600020409     C                   endif
013700020409     **
013800020409     ** Temporary block of code to get correct bill number from old Gl. database
013900020409     C                   if        ltref <> *blanks and ltorg = 'GLD'
014000020409     C                   movel     ltref         TempFld           8 0
014100020409     C                   z-add     TempFld       ltnumb
014200020409     C                   endif
014300020409     ************************  END OF TEMP. BLOCK *********************
014400020409     **
014500020409     C                   except    detail
014600020409     **
014700020409     C     trn_key2      reade     ltfmt                                  99
014800020409     C  n99prd           comp      ltprno                               99
014900020409     C                   enddo
015000020409     **
015100020409     ** Print employee total lines
015200020409     **
015300020409     C                   eval      findeb = findeb + totdeb
015400020409     C                   eval      fincre = fincre + totcre
015500020409     C                   eval      depdeb = depdeb + totdeb
015600020409     C                   eval      depcre = depcre + totcre
015700020409     C                   except    total
015800020409     C                   endif
015900020409     C                   endif
016000020409
016100020409     C                   read      emprec                                 lr
016200020409     C                   enddo
016300020409     ** Print Last department totals
016400020411     C                   if        runtyp = 3
016500020409     C                   if        depbal <> *zeros or depamt <> *zeros or
016600020409     C                             depdeb <> *zeros or depcre <> *zeros
016700020409     C                   except    deptot
016800020409     c                   endif
016900020411     c                   endif
017000980218     **
017100980218     ** Print report final totals
017200980218     C                   except    fintot
017300980218
017400980218
017500011126     *******************************************************
017600011126     **   Calculate Totals After Each Department
017700011126     ********************************************************
017800011126     C     dept_lvl_calc begsr
017900011126     C
018000011126     C                   if        last_dept <> dept
018100011126     C                   if        depbal <> *zeros or depamt <> *zeros or
018200011126     C                             depdeb <> *zeros or depcre <> *zeros
018300011126     C                   except    deptot
018400011126     c                   endif
018500011126     c                   eval      last_dept = dept
018600011126     c                   endif
018700011126     c
018800011126     C                   endsr
018900980218
019000980120     ******************************************************
019100980120     ** Initial routine **
019200980120     ******************************************************
019300020206     C     *inzsr        begsr
019400020206     **
019500020206     C     *entry        plist
019600020206     C                   parm                    prd               6 0
019700020206     C                   parm                    typ               2 0
019800020411     C                   parm                    runtyp            1 0
019900020409     **
020000020206     C                   movel     prd           rp_year           4
020100020206     C                   move      prd           emth              2 0
020200020206     C                   movel     prd           year              4 0
020300980122     ** Quarterly processing for Loans and C.O.E. accounts only
020400980122     C                   if        typ = 1 or typ = 2
020500980122     **
020600980121     C                   if        emth = 03  or emth = 06
020700980121     C                             or emth = 09 or emth = 12
020800980120     C                   seton                                        12
020900980120     C                   endif
021000980120     **
021100980120     C                   if        emth <= 3
021200980120     C                   z-add     12            smth              2 0
021300980120     C                   else
021400980120     C                   if        emth > 3 and emth <= 6
021500980120     C                   z-add     3             smth
021600980120     C                   else
021700980120     C                   if        emth > 6 and emth <= 9
021800980120     C                   z-add     6             smth
021900980120     C                   else
022000980120     C                   if        emth > 9 and emth <= 12
022100980120     C                   z-add     9             smth
022200980120     C                   endif
022300980120     C                   endif
022400980120     C                   endif
022500980120     C                   endif
022600980122     **
022700980122     C                   else
022800980122     **
022900980122     C                   eval      smth = emth - 1
023000980122     C                   if        smth = 0
023100980122     C                   z-add     12            smth
023200980122     C                   endif
023300980122     C                   endif
023400980121     **
023500050513      /free
023600050513            get_ltype_info('RD':typ:ematf_struc);
023700050513      /end-free
023800050513
023900980122     C     emdsp         cat(p)    baltxt:1      @emp_cnst
024000980121     **
024100980121     ** Get proper year for start period
024200980121     C                   if        smth = 12
024300980121     C                   sub       1             year
024400980121     C                   endif
024500980121     **
024600020206     C                   eval      month = get_month(emth)
024700980204     C     emdsp         cat(p)    'as at':1     @hdr_cnst
024800020206     C                   cat       month:1       @hdr_cnst
024900980204     C                   cat       rp_year:1     @hdr_cnst
025000980127     **
025100980204     c     in:out        xlate     @hdr_cnst     @hdr_cnst
025200980121     **
025300020206     ** get period  dates
025400020206     C                   callp     get_prd_info('*rng':*blanks:prd:
025500020206     C                                          prstrd:prendd)
025600980122     C                   z-add     prendd        end_date
025700020206     C                   z-add     s_prd         prd#
025800020206     C                   callp     get_prd_info('*rng':*blanks:prd#:
025900020206     C                                          prstrd:prendd)
026000980121     C                   z-add     prendd        str_date
026100020206
026200020206     C                   eval      coname = get_coname
026300980120     C                   endsr
026400980404     ******************************************************
026500980404     ** STATUS  routine **
026600980404     ******************************************************
026700980404     C     GetStatusTxt  begsr
026800020108     C                   if        get_cat(status) <> 'A'
026900980404     C                   move      'RESIGNED'    StatusTxt
027000980404     C                   else
027100980404     C                   move      *blanks       StatusTxt
027200980404     C                   endif
027300980404     C                   endsr
027400980120     **
027500980404     ******************************************************
027600050505     Oqsysprt   e            rep_hedr    2  1
027700020206     O                       @pgm                10
027800020206     O                       coname              77
027900980122     O          e            rep_hedr       2
028000980120     O                       @user               10
028100980122     O          e            rep_hedr       2
028200980122     O                       @hdr_cnst           57
028300980122     O          e            rep_hedr       2
028400980122     O                                            8 'Code:-'
028500980122     O                       emacct            +  0
028600980404     ** PRINT HIGHLIGHTED INFORMATION
028700011126     O          e            hedr           0
028800980404     O                       StatusTxt           33
028900980404     O                       emp           Z     91
029000980122     **
029100980120     O          e            hedr           2
029200980404     O                       StatusTxt           33
029300980120     O                       christ              63
029400980120     O                       surnam              76
029500950606     O                                           85 'EMP. #:'
029600980120     O                       emp           Z     91
029700980120     O                       dept          Z    108
029800950606     O                                          105 'DEPT. NO.:'
029900980120     O          e            hedr           1
030000950606     O                                            5 'DATE'
030100950606     O                                           33 'PARTICULARS'
030200950606     O                                           74 'BAL. B/FWD.'
030300950606     O                                           87 'DEBITS'
030400950606     O                                          103 'CREDITS'
030500950606     O                  U1                      117 '11% INTEREST'
030600950606     O                                          131 'NET BALANCE'
030700980120     O          e            hedr           2
030800980122     O                       end_date           131 '    /  /  '
030900980122     O                                          120 'AT'
031000980120     O          e            hedr           1
031100980122     O                       str_date            10 '    /  /  '
031200980122     O                       @emp_cnst           58
031300980122     O                       la(smth)      A     74
031400980120     **
031500980120     O          e            detail         1
031600980121     O                       ltdate              10 '    /  /  '
031700980121     O                       ltnumb        Z     20
031800980217     O                       ltorg               24
031900050505     O                       ltloc         z     28
032000050505     O                       lttext              60
032100980120     O                     98ltamt         1     87
032200980120     O                     97ltamt         1    103
032300980120     **
032400050505     Oqsysprt   e            total       2  2
032500980122     O                       totdeb        1b    87
032600980122     O                       totcre        1b   100
032700980122     O                       la(emth)      ab   130
032800980122     O                       la(smth)      ab    71
032900950606     O                                           73 '*'
033000950606     O                                           90 '*'
033100950606     O                                          103 '*'
033200950606     O                  U1                      115 '*'
033300950606     O                                          132 '*'
033400980120     O          e          12total          1
033500950606     O                                           24 'PLEASE CHECK THIS STATEM'
033600950606     O                                           48 'ENT WITHOUT DELAY AND RE'
033700950606     O                                           72 'TAIN FOR FUTURE REFERENC'
033800950606     O                                           74 'E.'
033900980120     O          e          12total          2
034000950606     O                                           24 '------------------------'
034100950606     O                                           48 '------------------------'
034200950606     O                                           72 '------------------------'
034300950606     O                                           73 '-'
034400980120     O          e          12total          1
034500960603     O                                           24 'THE CREDIT CONTROL DEPAR'
034600960603     O                                           48 'TMENT MUST BE NOTIFIED P'
034700960603     O                                           55 'ROMPTLY'
034800980120     O          e          12total          1
034900950606     O                                           24 'OF ANY ERROR OR OMISSION'
035000950606     O                                           48 ' WITHIN 15 DAYS, OTHERWI'
035100950606     O                                           50 'SE'
035200980120     O          e          12total          2
035300950606     O                                           24 'CORRECTNESS OF STATEMENT'
035400950606     O                                           41 ' WILL BE ASSUMED.'
035500980120     O          e            total       1  3
035600950606     O                                           24 '------------------------'
035700950606     O                                           48 '------------------------'
035800950606     O                                           72 '------------------------'
035900950606     O                                           96 '------------------------'
036000950606     O                                          120 '------------------------'
036100950606     O                                          132 '------------'
036200011126     ** Department totals
036300050505     Oqsysprt   e            deptot      1
036400011126     O                       depbal        jb    71
036500011126     O                       depdeb        1b    88
036600011126     O                       depcre        Jb   104
036700011126     O                       depamt        Jb   131
036800011126     O                                           33 'DEPARTMENT TOTAL'
036900011126     O                                           73 '*'
037000011126     O                                           90 '*'
037100011126     O                                          105 '*'
037200011126     O                                          132 '*'
037300011126     O          e            deptot      1  2
037400011126     O                                           24 '------------------------'
037500011126     O                                           48 '------------------------'
037600011126     O                                           72 '------------------------'
037700011126     O                                           96 '------------------------'
037800011126     O                                          120 '------------------------'
037900011126     O                                          132 '------------'
038000011126     ** Final report totals
038100050505     Oqsysprt   e            fintot      2
038200990721     O                       finbal        j     71
038300980120     O                       findeb        1     88
038400980120     O                       fincre        J    104
038500980120     O                       finamt        J    131
038600950606     O                                           73 '*'
038700020111     O**                                         90 '*'
038800950606     O                                          105 '*'
038900950606     O                                          132 '*'
039000980120     **
