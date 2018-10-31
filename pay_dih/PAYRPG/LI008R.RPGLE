000100091121     H DECEDIT('.') DATEDIT(*DMY/) BNDDIR('DISDIR') ACTGRP(*caller)
000200170216     H DFTACTGRP(*NO) alwnull(*usrctl)
000300950614     F**
000400021015     F*PROGRAM:-ALLOWANCES
000500980210     F*PROGRAMMER:-****  S.BRITTON
000600021015     Fli008d    cf   e             workstn
000700021015     F                                     sfile(optsfl:rrn3)
000800180315     F/IF DEFINED(WEB)
000900180315     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001000180315     F/ENDIF
001100950614     F**
001200020206     F* EMPLOYEE MASTER FILE
001300020206     Fempmast   ip   e           k disk
001400020206     F* LIABILITY TRANSACTION FILE
001500980406     Fliabtl09  if   e           k disk
001600020206     F* LIABILITY MASTER FILE
001700990506     Fliabm     if   e           k disk
001800020214     Fpaycrosl03if   e           k disk
001900050513     Fqsysprt   o    f  132        printer oflind(*inof)
002000980210     F                                     formlen(66)
002100980210     F                                     formofl(66)
002200020206     ** Payroll/general PROTOTYPES
002300150110      /copy qcpysrc,PAYPR
002400150110      /copy qcpysrc,genpr
002500150110      /copy qcpysrc,emppr
002600980210     **
002700980214     D                sds
002800980214     D  @pgm             *proc
002900990508     D  @sdate               276    281  0
003000990508     D  @stime               282    287  0
003100180315     D  @user                358    367
003200970523     **
003300980216     D arrln           s              1    dim(35)                              TOTAL AMOUNT
003400980214     D @sup_cnst       s             18
003500020214     D payrun_factor   s              7  3 inz(1)
003600020304     D payrun_qty      s              2  0 inz(1)
003700020214     D wrkhrs#         s              5  2
003800020214     D totamt          s             11  2
003900020214     D totmth          s             11  2
004000020214     D total           s             11  2
004100020214     D mthtot          s             11  2
004200020214     D ytdtot          s             11  2
004300020214     D n               s              2  0
004400020214     D xcnt            s              3  0
004500020214     D ycnt            s              3  0 inz(0)
004600020214     D styp            s              3  0
004700020214     D atxt            s             35    dim(50)
004800070508     D aly             s             11  2 dim(50)
004900020213     D ald             s              9  2 dim(50)
005000020213     D all             s              9  2 dim(50)
005100020214     D yr_all          s             11  2 dim(50)
005200020214     D mt_all          s             11  2 dim(50)
005300990506     D la              s             11  2 dim(12)
005400020206     D month           s              9
005500020206     D p_prd           s              6  0
005600020206     D prstrd          s              8  0
005700020206     D prendd          s              8  0
005800021015     D rrn3            s              2  0
005900050513     D opttyp          s              2  0
006000050513     D countr          s              5  0
006100020206     D coname          s             30
006200020214     D BeginPgm        s               n   inz(*on)
006300020214     **
006400021015     D @rep_cnst       c                   'Employee Allowances'
006500020214     D alltxt          c                   const('ALLOWANCE')
006600970523     **
006700020214     Dty_accum         ds                  occurs(50) inz
006800020214     D ty_total                      11  2
006900980216     **
007000980216     D                 ds
007100980216     D s_prd                   1      6  0
007200980216     D year                    1      4  0
007300980216     D smth                    5      6  0
007400980128     **
007500990506     Ilifmt
007600990506     I              liamt01                     la(01)
007700990506     I              liamt02                     la(02)
007800990506     I              liamt03                     la(03)
007900990506     I              liamt04                     la(04)
008000990506     I              liamt05                     la(05)
008100990506     I              liamt06                     la(06)
008200990506     I              liamt07                     la(07)
008300990506     I              liamt08                     la(08)
008400990506     I              liamt09                     la(09)
008500990506     I              liamt10                     la(10)
008600990506     I              liamt11                     la(11)
008700990506     I              liamt12                     la(12)
008800990506     **
008900981027
009000980210     **
009100980210     C     *like         define    ltstyp        #ltstyp
009200980214     C     *like         define    eadsp         eadspln
009300990506     **
009400990506     C     mas_key       klist
009500021015     C                   kfld                    opttyp
009600020213     C                   kfld                    easubtyp
009700990506     C                   kfld                    emp
009800020214     **
009900020214     c     mas_key2      klist
010000021015     c                   kfld                    opttyp
010100020214     c                   kfld                    easubtyp
010200980128     **
010300980128     C     trn_key       klist
010400020219     C                   kfld                    prd
010500021015     C                   kfld                    opttyp
010600980210     C                   kfld                    emp
010700020214     C                   kfld                    styp
010800980209     **
010900980209     C     trn_key2      klist
011000020219     C                   kfld                    prd
011100021015     C                   kfld                    opttyp
011200980217     C                   kfld                    emp
011300950907     **
011400980210     C                   movel     christ        inital            1
011500020214     **
011600020214     C                   eval      total = *zeros
011700020214     C                   eval      totamt = *zeros
011800990507     C                   clear                   aly
011900020213
012000050513      /free
012100050513           countr = *zeros;
012200050513           dow  not get_lstype_info('RD':opttyp:-1:eastf_struc:countr);
012300050513      /end-free
012400020214     **
012500020214     C                   if        BeginPgm
012600020214     C                   eval      ycnt = ycnt + 1
012700020214     C                   eval      atxt(easubtyp) = eadsp
012800020214     C                   endif
012900020214
013000020214     C     mas_key       chain     lifmt
013100020214     C                   if        %found
013200020214     C                   eval      aly(easubtyp) = la(mth)
013300990507     C                   endif
013400020214
013500950907     **
013600020214     C     'P':' '       xlate     class         grade             3
013700990220     **
013800990220     ** Accumulate amounts for all allowances
013900990220     **
014000020214     C     mas_key2      chain     pcrsfmt
014100020214     C                   if        %found
014200020214     **
014300020214     **  Total the different allowances
014400020214     C                   eval      all(easubtyp) = get_emp_pay(emp:dept:
014500020214     C                                              pcrsptype:wrkhrs#:
014600020304     C                                              payrun_factor:
014700020304     C                                              payrun_qty:status)
014800990220
014900020214     C                   endif
015000020213     **                                                                         Telephone allowance
015100990220
015200990220     **
015300990220     C                   enddo
015400020214
015500020214     C                   eval      BeginPgm  = *off
015600020214     ** Select all executives or resigned employees with balances
015700020214     C                   if        status = 'E'  or
015800020214     C                             %xfoot(aly) <> *zeros
015900020214     **
016000020214     C                   except    hedr1
016100980210     **
016200980210     C     trn_key       setgt     ltfmt
016300980210     C                   read      ltfmt                                  99
016400020214     **
016500980210     C                   if        ltemp <> emp
016600980210     C                   seton                                        99
016700980217     C                   else
016800980214     **
016900980210     C                   z-add     ltstyp        #ltstyp
017000980214     C                   except    hedr
017100980217     C                   exsr      get_subhdr
017200980217     C                   except    sub_hedr
017300980217     C                   endif
017400980210     **
017500980210     ** Select transactions for report period
017600980210     C                   dow       not(*in99)
017700980210     **
017800980210     C                   if        ltstyp <> #ltstyp
017900980214     C                   exsr      sub_calc
018000980210     C                   z-add     ltstyp        #ltstyp
018100980214     C                   exsr      get_subhdr
018200980214     C                   except    sub_hedr
018300980210     C                   endif
018400980210     **
018500000609     c     ltstyp        occur     ty_accum                             20
018600000609     C                   if        not*in20
018700020214     C                   eval      ty_total = ty_total + ltamt
018800020214     C                   eval      totamt = totamt + ltamt
018900020215     C                   eval      total = total + ltamt
019000980210     C                   except    detail
019100000609     C                   endif
019200980210     **
019300980210     C     trn_key2      reade     ltfmt                                  99
019400980210     C  n99prd           comp      ltprno                               99
019500980210     C                   enddo
019600980217     **
019700980217     C                   exsr      sub_calc
019800980214     **
019900980214      * Employee summary routine
020000980214      * Print summary information and final totals
020100020214     **
020200020214     C                   if        total <> *zeros
020300020214     C                   except    emp_tot
020400020214     C                   endif
020500020214     **
020600020214     C                   except    emp_ftoth
020700020214     **
020800020214     C                   eval      xcnt = 1
020900020214     C                   dow       xcnt <= ycnt
021000980214     C     xcnt          occur     ty_accum
021100980214     C                   add       ty_total      aly(xcnt)
021200980214     C                   add       ty_total      mt_all(xcnt)
021300980214     C     aly(xcnt)     sub       all(xcnt)     ald(xcnt)
021400980214     C                   add       aly(xcnt)     yr_all(xcnt)
021500020214     **
021600020214     C                   except    emp_ftot
021700020214     C                   eval      xcnt = xcnt + 1
021800980214     C                   enddo
021900980214     **
022000980214     C                   clear     *all          ty_accum
022100980214     **
022200020214     C                   xfoot     mt_all        mthtot
022300020214     C                   xfoot     yr_all        ytdtot
022400951002     **
022500020214     C                   z-sub     mthtot        totmth
022600990218     **
022700020214     C                   except    emp_ftotf
022800980217     C                   clear                   ald
022900990507
023000990507     *** end valid employee selection
023100020214     C                   endif
023200981027
023300020214     Clr                 exsr      finalprint
023400981027
023500981027     ***********************************************************
023600020214     **  Write to report file
023700981027     ***********************************************************
023800980214     C     sub_calc      begsr
023900020214     **
024000020214     C                   if        totamt <> *zeros
024100980214     C                   except    sub_tot
024200020214     C                   eval      totamt = *zeros
024300980214     C                   endif
024400020214     **
024500980214     C                   endsr
024600980210     **
024700980214     *****************************************************
024800980214     ** Get sub-header data **
024900980214     *****************************************************
025000980214     C     get_subhdr    begsr
025100050513      /free
025200050513           get_lstype_info('RD':opttyp:ltstyp:eastf_struc);
025300050513      /end-free
025400020214     C     ' '           checkr    eadsp         n
025500980214     C                   add       1             n
025600980214     C                   move      *all'_'       arrln
025700980214     C                   movea     *blank        arrln(n)
025800980214     C                   movea     arrln         eadspln
025900980214     C                   endsr
026000020214     ***********************************************************
026100020214     **  Write  report totals
026200020214     ***********************************************************
026300020214     C     finalprint    begsr
026400020214     **
026500020214     C                   except    fin_reph
026600020214     **
026700020214     C                   eval      xcnt = 1
026800020214     C                   dow       xcnt <= ycnt
026900020214     **
027000020214     C                   except    fin_rept
027100020214     C                   eval      xcnt = xcnt + 1
027200020214     C                   enddo
027300020214     **
027400020214     C                   except    fin_repf
027500020214     C                   endsr
027600980210     ******************************************************
027700980210     ** Initial routine **
027800980210     ******************************************************
027900020206     C     *inzsr        begsr
028000020206     **
028100020206     C     *entry        plist
028200020206     C                   parm                    prd               6 0
028300020206
028400021015     C                   exsr      loadcom
028500021015     C                   if        rrn3 > 1
028600021015     C                   dow       0 = 0
028700021015     C                   exfmt     optctl
028800021015     **
028900021015     C                   readc     optsfl                                 16
029000021015     C                   if        not(*in16) and
029100021015     C                             opt = 'X'
029200021015     c                   eval      opttyp = optcode
029300021015     C                   leave
029400021015     C                   endif
029500021015     C                   enddo
029600021015     C                   else
029700021015     c                   eval      opttyp = optcode
029800021015     C                   endif
029900020208     **                                                                         for Allow. acc. Code
030000020208     C                   move      prd           emth              2 0
030100020208     C                   movel     prd           year              4 0
030200020208     C                   movel     prd           rpt_year          4 0
030300020213     **
030400020213     C                   move      prd           s_prd
030500020208     **  get prv prd
030600020208     C                   eval      p_prd = get_new_prd(prd:1:'S')
030700020208     C                   move      p_prd         mth               2 0
030800980210     **
030900980210     C                   eval      smth = emth - 1
031000980210     C                   if        smth = 0
031100980210     C                   z-add     12            smth
031200980210     C                   endif
031300980210     **
031400980210     ** Get proper year for start period
031500980210     C                   if        smth = 12
031600980210     C                   sub       1             year
031700980210     C                   endif
031800980210     **
031900020206     C                   eval      month = get_month(emth)
032000980210     **
032100020206     ** get period  dates
032200020206     C                   callp     get_prd_info('*rng':*blanks:prd:
032300020206     C                                          prstrd:prendd)
032400020206     C                   eval      coname = get_coname
032500020213
032600020213     C                   except    hedr2
032700980210     C                   endsr
032800980210     **
032900021015     ***************************************************************************
033000021015     ***  DISPLAY Allowance Selection                                          *
033100021015     ***************************************************************************
033200021015     C     loadcom       begsr
033300021015      /free
033400021015
033500021015           *in33 = *on;
033600021015           rrn3 =  *zeros;
033700021015           write optctl;
033800021015           *in33 = *off;
033900021015
034000050513           countr = *zeros;
034100050513           dow  not get_ltype_info('RD':-1:ematf_struc:countr);
034200021015               if %subst(%char(emdept):1:1) = '2';
034300021015           // Include Allowance types for selection
034400021015                    rrn3 = rrn3 + 1;
034500021015                    opttxt = emdsp;
034600021015                    optcode = emseltyp;
034700021015                    write  optsfl;
034800021015               endif;
034900021015           enddo;
035000021015
035100021015               if rrn3 = *zeros;
035200021015                 *inlr = *on;
035300021015                 return;
035400021015               endif;
035500021015
035600021015           *in56 = rrn3 > 0;
035700021015      /end-free
035800021015     **
035900021015     C                   endsr
036000981027
036100981027
036200050513     Oqsysprt   e            hedr2          1 03
036300980214     O                       @pgm                10
036400020206     O                       coname              77
036500990508     O                       @sdate        Y    130
036600020213     O          e            hedr2          2
036700980214     O                       @user               10
036800990508     O                       @stime             130 '  :  :  '
036900020214     **
037000980214     O          e            hedr2          1
037100020214     O                                           50 'EMPLOYEES'
037200020214     O                                           68 'ALLOWANCES AS AT'
037300020214     O                       month               78
037400020214     O                       rpt_year            83
037500980214     O          e            hedr2          2
037600020214     O                                           49 '--------'
037700020214     O                                           73 '------------------------'
037800020214     O                                           83 '----------'
037900980214     O          e            hedr1          2
038000980214     O                       surnam              33
038100980214     O                       christ              50
038200950907     O                                           69 'EMP.NO.'
038300980214     O                       emp           Z     75
038400950907     O                                           89 'DEPT. NO.'
038500980214     O                       dept          Z     93
038600980214     O          e            hedr           2
038700950907     O                                           11 'DATE'
038800020214     O                                           52 'DESCRIPTION'
038900950907     O                                           95 'AMOUNT'
039000950907     O                                          110 'TOT.AMT.'
039100950907     O                                          127 'A/C CODE'
039200980214     ** print sub_type heading
039300980214     O          e            sub_hedr       1
039400980214     O                       eadsp               70
039500980214     O          e            sub_hedr       2
039600980214     O                       eadspln             70
039700980214     ** print detail transactions
039800980214     O          e            detail         1
039900980128     O                       ltdate              13 '    /  /  '
040000981027     O                       ltnumb        z     35
040100980217     O                       ltorg               39
040200980128     O                       lttext              70
040300980128     O                       ltamt         A     97
040400980214     ** print sub-total
040500980214     O          e            sub_tot        2
040600980214     O                       totamt        A    113
040700950907     O                                          116 '*'
040800980214     **
040900980214     O          e            emp_tot        2
041000980214     O                       total         A    113
041100950907     O                                          116 '**'
041200020214
041300020214     O          e            emp_ftoth      1
041400020214     O                                           50 'YEARLY ALLOWANCE'
041500020214     O                                           63 'AS AT '
041600020214     O                       prendd            +  1 '    /  /  '
041700020214     O                                           91 'DIFFERENCE'
041800020214     O          e            emp_ftoth      2
041900020214     O                                           50 '----------------'
042000020214     O                                           54 '-'
042100020214     O                                           74 '-----------------'
042200020214     O                                           77 '-'
042300020214     O                                           91 '----------'
042400980214     O          e            emp_ftot       2
042500020214     O                       atxt(xcnt)          35
042600020214     O                       all(xcnt)     A     50
042700020214     O                       aly(xcnt)     A     74
042800020214     O                       ald(xcnt)     A     91
042900020214     O          e            emp_ftotf      2
043000950907     O                                           24 '========================'
043100950907     O                                           48 '========================'
043200950907     O                                           72 '========================'
043300950907     O                                           96 '========================'
043400950907     O                                          120 '========================'
043500950907     O                                          132 '============'
043600981027     **
043700050513     Oqsysprt   e            fin_reph       3
043800020214     O                                           49 'MONTH''S TOTAL'
043900020214     O                                           67 'TO DATE TOTALS'
044000020214     O          e            fin_rept       3
044100020214     O                       atxt(xcnt)          35
044200020214     O                       mt_all(xcnt)  A     50
044300020214     O                       yr_all(xcnt)  A     67
044400020214
044500020214     O          e            fin_repf       3
044600020214     O                       mthtot        A     50
044700020214     O                       ytdtot        A     67
044800050513     Oqsysprt   t    lr
044900950614     O                                           77 '** END  OF  REPORT  **'
