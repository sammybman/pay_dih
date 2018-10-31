000100170216     H DATEDIT(*DMY) alwnull(*usrctl)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300950418     F** SAVINGS LETTERS
000400010918     F** ADJUSTMENTS------T.I.BYNOE  ** ADJ. S.BRITTON
000500021127     Fsavmast   up   e           k disk    commit
000600021127     Fsavconf   if   e             disk
000700021127     Fsavconf2  if   e           k disk
000800010921     Fempmast   if   e           k disk
000900010921     Fliabtl07  if   e           k disk
001000011228     Fpayhisl11 if   e           k disk
001100010921     Fpaytranl03if   e           k disk
001200021128     Fpayeytd   uf a e           k disk    commit
001300010921     ** Payroll PROTOTYPES
001400111013      /copy QCPYSRC,PAYPR
001500111013      /copy QCPYSRC,GENPR
001600041010
001700041010     D*-----------------------------------------
001800041010     D* Prototype for procedure: Gl_calcs
001900041010     D*-----------------------------------------
002000041010     D Gl_Calcs        PR
002100041115     D gl_amount                     11  2 const
002200041010     D
002300010918     **
002400010921      ** Include data definition
002500021125      /copy *libl/payrpg,savcopy#4
002600010921
002700041010     **
002800041010     Dgl_accumd        ds                  occurs(300) inz
002900041010     D gl_prd                         6  0
003000041010     D gl_account                    25
003100041010     D gl_desc                       30
003200041010     D gl_total                      13  2
003300041010     D gl_refno                      15
003400041010     **
003500041010     D                 ds
003600041010     D gl_xxx                        31
003700041010     D gl_prno                        6  0 overlay(gl_xxx)
003800041010     D gl_out                        25    overlay(gl_xxx:7)
003900021127     **
004000121012     Dpay_rec          ds                  occurs(3) inz
004100021128     D pyy_prior                      2  0
004200021128     D pyy_type                       2  0
004300021128     D pyy_tax                        1
004400021128     D pyy_code                       1
004500021128     D pyy_hrs                        5  2
004600021128     D pyy_amt                       11  2
004700021127     **
004800021128     D tax_type_cnst   c                   61
004900021128     D grs_type_cnst   c                   00
005000021128     D pay_org_cnst    c                   'PAY'
005100021128     D emt_org_cnst    c                   'EMT'
005200021127     **
005300021125     D endmth          s              2  0
005400021127     D xcnt            s              3  0 inz(0)
005500021127     D origin          s              3    inz('EMT')
005600021128     D option          s              2  0
005700021127     D tax_gross       s             11  2
005800021127     D totduc          s             11  2
005900021128     D amount#         s             11  2
006000021127     D proll#          s              2  0 inz(0)
006100021127     D cycqty          s              2  0 inz(1)
006200021127     D cyccode         s              2    inz('*Y')
006300021128     D ptype#          s              2  0
006400021128     D taxflag         s              1
006500021128     D cnt             s              1  0
006600041010     D glarr           s             31    dim(300)                             TOTAL AMOUNT
006700010921      ** Include Input specs.
006800111013      /copy QCPYSRC,savcopy#2
006900010918     **
007000021127     ** Key for YTD
007100021127     C     ytd_key       klist
007200021128     C                   kfld                    savemp
007300021127     C                   kfld                    year
007400021127     C                   kfld                    ptype#
007500021128     C                   kfld                    option
007600021127
007700010921     c     savemp        chain     emprec
007800951005     **
007900021101      ** Include Year-end calcs
008000111013      /copy QCPYSRC,savcopy#3
008100021101
008200021127     C                   if        get_cat(status) = 'A'
008300041106     C                   exsr      Perform_ytdC
008400041010      /free
008500041115
008600041115
008700050514       //Gl_calcs(comcon + intrst);
008800050514       //Gl_calcs(withdr);
008900050514       //Gl_calcs(totduc);
009000041115
009100041010      /end-free
009200010921
009300021219     C                   callb     'EMPFRPT071'
009400010928     C                   parm                    savemp
009500010928     C                   parm                    year
009600010928     C                   parm                    savpyear
009700010928     C                   parm                    comcon
009800010928     C                   parm                    prd_ded
009900010928     C                   parm                    withdr
010000010928     C                   parm                    intrst
010100021128     C                   parm                    totduc
010200010928     **
010300021128     C                   eval      savpyear = netsav - totduc
010400031006     C                   clear                   wth
010500031006     C                   clear                   sav
010600010922     C                   update    savrec
010700951005     **
010800010926     C                   else
010900010926     C                   if        netsav = *zeros
011000010926     C                   delete    savrec
011100010918     C                   endif
011200010926     C                   endif
011300041113      /free
011400041113         // Last record processing
011500041113         if  *inlr;
011600050514         //exsr wtr_gll_totals;
011700041113         endif;
011800041113      /end-free
011900010921     **
012000021127     ***************************************
012100021127     ** Routine Handling YTD Calcs.
012200021127     ***************************************
012300021127     C     perform_ytdC  begsr
012400021127     ** clear tax related fields
012500031006      /FREE
012600031006         reset xcnt;
012700031006
012800031006      /end-free
012900031006     C                   clear     *all          pay_Rec
013000021127     C                   eval      tax_gross = *zeros
013100021127     C                   eval      totduc = *zeros
013200021127     ** if company contribution is taxable
013300021127     C                   if        sactaxcom = 'Y'
013400021127     C                   eval      tax_gross = comcon
013500021127     C                   endif
013600021127
013700021127     ** if Interest Calculated is taxable
013800021127     C                   if        sactaxintr = 'Y'
013900021127     C                   eval      tax_gross = tax_gross + intrst
014000021127     C                   endif
014100021127
014200021127     C                   select
014300021127     ** calculate Tax  based on payroll
014400021127     C                   when      sactaxpay = 'Y'
014500021127     C                   callp     calc_emp_tax(emp:frpay:tax_gross
014600021128     C                                          :totduc:cyccode
014700021127     C                                          :proll#:year
014800021127     C                                          :cycqty:status
014900041106     C                                          :%addr(pay_rec):xcnt
015000180130     C                                          :emply:*blank:*zeros)
015100021128     C                   eval      taxflag = *blanks
015200021127     ** calculate Tax  based on Percentage
015300021127     C                   when      sactaxper > *zeros
015400021128     C                   eval      totduc = tax_gross * sactaxper / 100
015500021128     C                   eval      taxflag = 'S'
015600021127     C                   endsl
015700021127
015800021127     ** WRITE YTD RECORD
015900021127     C                   select
016000021128     ** Write YTD as based on payroll
016100021127     C                   when      sacytdpay = 'Y'
016200021128     C                   eval      option = *zeros
016300021128     C                   do        2             cnt
016400021128     C                   eval      ytdamt = *zeros
016500021128     C                   select
016600021128     C                   when      cnt = 1
016700021128     C                   eval      ptype# = tax_type_cnst
016800021128     C                   eval      amount# = totduc
016900021128     C                   when      cnt = 2
017000021128     C                   eval      ptype# = grs_type_cnst
017100021128     C                   eval      amount# = tax_gross
017200021128     C                   endsl
017300021128     C                   if        amount# <> *zeros
017400021128     C     ytd_key       chain     ytdrec
017500021127
017600021128     C                   eval      ytdamt = ytdamt + amount#
017700021128     C                   if        not(%found)
017800021128     C                   eval      ytdtype = ptype#
017900021128     C                   eval      ytdorg = pay_org_cnst
018000021128     C                   eval      ytdtxflag = *blanks
018100021128     C                   eval      ytdopt = option
018200021127     C                   write     ytdrec
018300021127     C                   else
018400021127     C                   update    ytdrec
018500021127     C                   endif
018600021128     C                   endif
018700021128     C                   enddo
018800021128
018900021128     ** Write YTD as separate records for EACH Taxable amount
019000021127     C                   when      sacytdsep = 'Y'
019100021128     C                   do        3             cnt
019200021128     C                   eval      ytdamt = *zeros
019300021128     C                   select
019400021128     C                   when      cnt = 1
019500021128     C                   eval      option = 1
019600021128     C                   eval      ptype# = tax_type_cnst
019700021128     C                   eval      amount# = totduc
019800021128     C                   when      cnt = 2 and sactaxcom = 'Y'
019900021128     C                   eval      option = 1
020000021128     C                   eval      ptype# = grs_type_cnst
020100021128     C                   eval      amount# = comcon
020200021128     C                   when      cnt = 3 and sactaxintr= 'Y'
020300021128     C                   eval      option = 2
020400021128     C                   eval      ptype# = grs_type_cnst
020500021128     C                   eval      amount# = intrst
020600021128     C                   endsl
020700021128     C                   if        amount# <> *zeros
020800021128     C     ytd_key       chain     ytdrec
020900021128
021000021128     C                   eval      ytdamt = ytdamt + amount#
021100021128     C                   if        not(%found)
021200021128     C                   eval      ytdtype = ptype#
021300021128     C                   eval      ytdorg = emt_org_cnst
021400021128     C                   eval      ytdopt = option
021500021128     C                   eval      ytdtxflag = taxflag
021600021128     C                   write     ytdrec
021700021128     C                   else
021800021128     C                   update    ytdrec
021900021128     C                   endif
022000021128     C                   endif
022100021128     C                   enddo
022200021128     ** Write YTD as separate records for Taxable amounts
022300021127     C                   when      sacytdone = 'Y'
022400021128     C                   eval      option = 1
022500021128     C                   do        2             cnt
022600021128     C                   eval      ytdamt = *zeros
022700021128     C                   select
022800021128     C                   when      cnt = 1
022900021128     C                   eval      ptype# = tax_type_cnst
023000021128     C                   eval      amount# = totduc
023100021128     C                   when      cnt = 2
023200021128     C                   eval      ptype# = grs_type_cnst
023300021128     C                   eval      amount# = tax_gross
023400021128     C                   endsl
023500021128     C                   if        amount# <> *zeros
023600021128     C     ytd_key       chain     ytdrec
023700021128
023800021128     C                   eval      ytdamt = ytdamt + amount#
023900021128     C                   if        not(%found)
024000021128     C                   eval      ytdtype = ptype#
024100021128     C                   eval      ytdorg = emt_org_cnst
024200021128     C                   eval      ytdopt = option
024300021128     C                   eval      ytdtxflag = taxflag
024400021128     C                   write     ytdrec
024500021128     C                   else
024600021128     C                   update    ytdrec
024700021128     C                   endif
024800021128     C                   endif
024900021128     C                   enddo
025000021127     C                   endsl
025100021127     C                   endsr
025200021127
025300021127     *************************************
025400010921      ** Include Period calc sub routines
025500021127     *************************************
025600111013      /copy QCPYSRC,savcopy#
025700010921     ** Init routine
025800010921     C     *inzsr        begsr
025900010921     C     *entry        plist
026000010921     C                   parm                    prno
026100011006     C                   parm                    run_type
026200010921     **
026300010921     C                   move      prno          cmth
026400021125     ** read control file
026500021125     C                   read      sacfmt
026600021125     C                   eval      pay_type = sacdtype
026700021125     C                   eval      lib_type = sactype
026800021125     C                   if        (sacmth - 1) = 0
026900021125     C                   eval      endmth = 12
027000021125     C                   else
027100021125     C                   eval      endmth = sacmth - 1
027200021125     C                   endif
027300021125     ** Only for cycle year-end Update
027400021125     C                   if        cmth <> endmth
027500010922     C                   eval      *inlr = *on
027600010922     C                   return
027700010922     C                   endif
027800010921     **
027900010928     C                   movel     prno          year
028000010921     **  Get Previous Month
028100010921     C                   eval      pmth = cmth - 1
028200010921     C                   if        pmth = 0
028300010921     C                   eval      pmth = 12
028400010921     C                   endif
028500010921     **
028600010921     C                   endsr
028700041010
028800041010     **
028900041010
029000041010     P*--------------------------------------
029100041010     P* Procedure name: Gl_Calcs
029200041010     P* Purpose:  Calculate Gl entries
029300041010     P* Returns:        *none
029400041010     P* Parameter:      *none
029500041010     P*--------------------------------------
029600041010     P Gl_calcs        B
029700041010     D Gl_calcs        PI
029800041115     D gl_amount                     11  2 const
029900041010
030000041010     D xcnt            s              4  0 inz(0)  static
030100041010     D ycnt            s              4  0                                      TOTAL AMOUNT
030200041010
030300041010     **
030400050514     c*    gl_key        klist
030500050514     c*                  kfld                    ptrntcode
030600050514     c*                  kfld                    phistype#
030700050514     c*                  kfld                    phisdept
030800050514     c*                  kfld                    phisemp
030900041010     **
031000050514     c*    gl_key2       klist
031100050514     c*                  kfld                    ptrntcode
031200050514     c*                  kfld                    phistype#
031300050514     c*                  kfld                    phisdept
031400041010     **
031500041010
031600041010     ** FROM FILE PAYRTYPE ( Payroll Run Type)
031700050514     C*    phistype      chain     prtyfmt
031800041010     **
031900041010
032000041010     ** Summarising Division totals
032100050514     C*    phisdiv       chain     divfmt
032200050514     C*                  eval      gl_out = dtgl
032300050514     C*                  exsr      accum_gl_tot
032400050514     C*                  eval      gl_total -= phisnpay
032500041010     **
032600041010     ** PROCESS ALL TRANSACTIONS
032700050514     C*    trans_key     chain     ptisfmt                            88
032800050514     C*                  dow       not(*in88)
032900041010     ** Get account code
033000050514     C*                  eval      phistype# = phistype
033100050514     C*    gl_key        chain     pytfmt
033200050514     C*                  if        %found
033300050514     C*                  eval      gl_out = pytacode
033400050514     C*                  else
033500050514     C*    gl_key2       chain     pytfmt
033600050514     C*                  if        %found
033700050514     C*                  eval      gl_out = pytacode
033800050514     C*                  else
033900050514     C*                  eval      phistype# = *blank
034000050514     C*    gl_key        chain     pytfmt
034100050514     C*                  if        %found
034200050514     C*                  eval      gl_out = pytacode
034300050514     C*                  else
034400050514     C*    gl_key2       chain     pytfmt
034500050514     C*                  if        %found
034600050514     C*                  eval      gl_out = pytacode
034700050514     C*                  else
034800050514     C*    ptrntcode     chain     ptyfmt
034900050514     C*                  if        %found
035000050514     C*                  eval      gl_out = ptydglcod
035100050514     C*                  endif
035200050514     C*                  endif
035300050514     C*                  endif
035400050514     C*                  endif
035500050514     C*                  endif
035600041010
035700041010     ** accumulate totals
035800050514     C*                  exsr      accum_gl_tot
035900041113
036000050514     C*                  select
036100050514     C*                  when      ptrncode = 'E'
036200050514     C*                  eval      gl_total += ptrnamt
036300050514     C*                  other
036400041010
036500050514     C*                  eval      gl_total -= ptrnamt
036600050514     C*                  endsl
036700041010
036800050514     C*    trans_key     reade     ptisfmt                                88
036900050514     C*                  enddo
037000041106      /free
037100041010
037200041010            return;
037300041010
037400041010        //**************************************************************
037500041010        // Search for gl.code in array .if found set occurance level and add
037600041010        // total ,if not found add new array record and Data Struct. total
037700041010        //***************************************************************
037800041010          begsr accum_gl_tot;
037900041010
038000050514          //  gl_prno  = phisprno;
038100050514          //
038200050514          //  ycnt = %lookup(gl_xxx:glarr);
038300050514          //
038400050514          //  if  ycnt = *zeros;
038500050514          //  xcnt += 1;
038600050514          //  glarr(xcnt) = gl_xxx;
038700050514          //  %occur(gl_accumd) = xcnt;
038800050514          //  gl_account = gl_out;
038900050514          //  gl_prd = gl_prno;
039000050514          //
039100050514          //  gl_desc = %trim(prtydesc) +
039200050514          //    '-' + %editc(phispdate:'Y');
039300050514          //
039400050514          // else;
039500050514          //
039600050514          //  %occur(gl_accumd) = ycnt;
039700050514          //endif;
039800050514          //
039900041010          endsr;
040000041010
040100041010        //***************************************                           ************************
040200041010        //**  Writing to G.L.
040300041010        //***************************************                           ************************
040400041010          begsr wtr_gll_totals;
040500041010           // call pgm to write to g/l files
040600041010
040700050514         //  %occur(gl_accumd) = 1;
040800050514         //  write_to_gl(%addr(gl_accumd):xcnt:'EMT':errmsg);
040900050514         //
041000050514         ////  Reset data Structure and counter
041100050514         //  clear  *all  gl_accumd;
041200050514         //  reset  xcnt;
041300041010
041400041010          endsr;
041500041010      /end-free
041600041010     P gl_calcs        E
