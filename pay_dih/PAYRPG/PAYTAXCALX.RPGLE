000100161201     H dftactgrp(*no) bnddir('DISDIR')
000200090415      //* PROGRAMMER---- S.BRITTON
000300010720     Fpayeytdl01if   e           k disk
000400041018     Fpaycon    if   e           k disk    usropn
000500011119     Fpaycycd   if   e           k disk    usropn
000600011113     Fpayrtypl01if   e           k disk    usropn
000700011119     Fpayrresl01if   e           k disk    usropn
000800050509     Fpayhisl05 if   e           k disk    usropn
000900050509     Fpayhisl09 if   e           k disk    usropn  rename(phisfmt:phisl09)
001000010608     Fpaytaxctl if   e           k disk
001100010608     Fpaytaxctl2if   e           k disk
001200150824
001300011113     D                sds
001400011113     D  @prm             *parms
001500161130
001600161201      /copy QCPYSRC,PAYPR
001700161201
001800180316     D paytaxcalc      Pi                  ExtPgm('PAYTAXCALX')
001900161201     D   txemp                        6  0       options(*nopass)
002000180316     D   gross                       11  2       options(*nopass)
002100180316     D   tottax                      11  2       options(*nopass)
002200180316     D   employed                     8  0       options(*nopass)
002300180319     D   in_status                    1          options(*nopass)
002400180316     D   txfrpay       s              5  0
002500180319     D   in_cycle      s              2          inz('FN')
002600180319     D   in_cmplt      s              2  0       inz(25)
002700180316     D   prd_year      s              4  0       inz(2017)
002800180319     D   in_cycqty     s              2  0       inz(1)
002900180319     D** in_status     s              1          inz('R')
003000180316     D** employed      s              8  0       options(*nopass)
003100180316     D   proll_type    s              1          inz('R')
003200180316     D   nis_deduct    s             11  2       inz(0)
003300110427
003400090415      //-------------------------------------
003500090415      // Prototype for procedure: GetPrdComp
003600090415      //-------------------------------------
003700041026     D GetPrdComp      PR             2  0
003800041026     D
003900150824
004000121013     D**ts_struc       ds                  occurs(45) based(struc@)
004100121013     D sts_struc       ds                  occurs(45) inz
004200121012     D sts_cycle                      2
004300121012     D sts_cmpltp                     2  0
004400121012     D sts_cycqty                     2  0
004500121012     D sts_prolused                   1
004600011119
004700121013     D**s_arr          s              1    dim(45) based(arr@)
004800121013     Dsts_arr          s              1    dim(45) inz
004900150824
005000121012     D StrucMax        s              3P 0 Inz(5)
005100011119     D Memory          s              5P 0
005200011119     D Memory2         s              5P 0
005300180316     D taxable         s             11  2
005400011119     D prfpay          s             11  2
005500011119     D tdpye           s              9  0
005600011119     D tsav            s             11  2
005700011119     D txgro           s             11  2
005800041106     D proll_used      s              1
005900011119     D prd_cmpltP      s              2  0
006000011119     D prd_cmplt       s              2  0
006100011119     D pay_cycle       s              2
006200011119     D pay_cycqty      s              2  0
006300011113     D year_factor     s                   like(cycyearf)
006400011119     D xcnt            s              3  0 inz(0)
006500011119     D ycnt            s                   like(xcnt) inz(1)
006600150824     D pnis_type       s              2  0 inz(63)
006700180316     D ptax_type       s              2  0 inz(99)
006800021030     D pgrs_type       s              2  0 inz(00)
006900021030     D ytdgro          s             11  2
007000021030     D ytdtax          s             11  2
007100180130     D ytdnis          s             11  2
007200161130     D dyn_tax_ind     s               n   inz(*off)
007300150824
007400150824
007500021030     C     ytd_keyG      klist
007600161201     C                   kfld                    txemp
007700010720     C                   kfld                    prd_year
007800021030     C                   kfld                    pgrs_type
007900150824
008000021030     C     ytd_keyT      klist
008100161201     C                   kfld                    txemp
008200021030     C                   kfld                    prd_year
008300021030     C                   kfld                    ptax_type
008400180130
008500180130     C     ytd_keyN      klist
008600180130     C                   kfld                    txemp
008700180130     C                   kfld                    prd_year
008800180130     C                   kfld                    pnis_type
008900110427
009000161201       if  %parms < %parmnum(txemp);
009100110427         *inlr = *on;
009200110427         return;
009300110427       endif;
009400180319            tottax = *zero;
009500180319            if in_status = 'G';
009600180319             proll_type  = 'G';
009700180319            endif;
009800180319               if txemp >= 7000 and txemp < 10000;
009900180319             in_cycle  = 'WK';
010000180319             in_cmplt  = 51;
010100180319             in_status   = 'C';
010200180319             proll_type  = 'C';
010300180319               endif;
010400180319
010500180320        // if txemp = 426 or txemp = 920
010600180320        // or txemp = 700  or txemp = 1643 or txemp = 1759;
010700180320        //  txfrpay = 99999;
010800180320        //  else;
010900180316          txfrpay = *zeros;
011000180320        // endif;
011100110427
011200110427       // Get Correct Tax period
011300090415       exsr get_corr_prd;
011400150824
011500090415       ytdgro = *zeros;
011600090415       ytdtax = *zeros;
011700180130       ytdnis = *zeros;
011800090415       // No Calculation for negative Gross
011900090415       if gross < *zeros;
012000090415         return;
012100090415       endif;
012200150824
012300180130       // Get YTD Taxable Gross
012400090415       setll ytd_keyg ytdrec;
012500090415       reade ytd_keyg ytdrec;
012600090415       dow not(%eof);
012700090415         ytdgro += ytdamt;
012800090415         reade ytd_keyg ytdrec;
012900090415       enddo;
013000150824
013100180130       // Get YTD Taxes Gross
013200090415       setll ytd_keyt ytdrec;
013300090415       reade ytd_keyt ytdrec;
013400090415       dow not(%eof);
013500090415         ytdtax += ytdamt;
013600090415         reade ytd_keyt ytdrec;
013700090415       enddo;
013800180130
013900180131       // Get YTD NIS
014000180130       setll ytd_keyn ytdrec;
014100180130       reade ytd_keyn ytdrec;
014200180130       dow not(%eof);
014300180130         ytdnis += ytdamt;
014400180130         reade ytd_keyn ytdrec;
014500180130       enddo;
014600180130
014700110427
014800090415       chain pay_cycle ptxfmt;
014900110427
015000090415       //****************************************************
015100090415       //* TO-DATE FREE PAY & GROSS CALCULATIONS  ROUTINE ***
015200090415       //****************************************************
015300090415       //  Setup freepay for periods completed to date
015400161201       if txfrpay <> *all'9';
015500110427
015600161129
015700041018          if  pctxonprd = 'Y';
015800161130             dyn_tax_ind =
015900161201                     check_if_dynamic_tax(ptxtype1:ptxper:ptxfree:txemp:gross);
016000161130
016100161129             if  dyn_tax_ind;
016200161129               prfpay =
016300161201                   (getFpyGross(ptxtype1:txemp:gross) * ptxper/100) *
016400161129                   pay_cycqty;
016500161129              else;
016600161129               prfpay = ptxfree * pay_cycqty;
016700161129             endif;
016800161130
016900150824             // Get taxable gross
017000180130             if pcnistaxo = 'Y';
017100180130               txgro = gross - nis_deduct;
017200180130              else;
017300180130               txgro = gross;
017400180130             endif;
017500041018            else;
017600041106
017700161130             dyn_tax_ind =
017800161201                     check_if_dynamic_tax(ptxtype1:ptxper:ptxfree * prd_cmpltP
017900161201                                               :txemp:(gross+ytdgro));
018000161130
018100161129             if  dyn_tax_ind;
018200161129                prfpay =
018300161201                   (getFpyGross(ptxtype1:txemp:(gross + ytdgro))
018400161130                       * ptxper/100) * pay_cycqty;
018500161129              else;
018600041106                prfpay = ptxfree * prd_cmpltP
018700041106                       * pay_cycqty;
018800161129             endif;
018900041106
019000150824             // Get taxable gross
019100180130             if pcnistaxo = 'Y';
019200180130               txgro = gross + ytdgro - (nis_deduct + ytdnis);
019300180130              else;
019400180130               txgro = gross + ytdgro;
019500180130             endif;
019600041018          endif;
019700150824
019800090415      //****************************************************************
019900090415      //  TAX CALCULATIONS **
020000090415      //****************************************************************
020100150824
020200090415         if txgro > prfpay;
020300161129
020400161129           if  dyn_tax_ind;
020500161129               dyn_tax_calc();
020600161129             else;
020700161129               dft_tax_calc();
020800161129           endif;
020900161129
021000110427
021100090415           // Get tax to pay
021200041018          if  pctxonprd = 'Y';
021300041018            tottax = tdpye;
021400041018             else;
021500041018            tottax = tdpye - ytdtax;
021600041018          endif;
021700110427
021800090415           if tottax < *zeros;
021900090415             tottax = *zeros;
022000090415           endif;
022100150824
022200090415         endif;
022300090415       endif;
022400121012
022500090415       return;
022600180319
022700090415       //***************************
022800090415       // Initilisation routine   **
022900090415       //***************************
023000090415       begsr get_corr_prd;
023100090415         // Find correct cycle
023200090415         reset ycnt;
023300011119
023400011119     c     in_status     lookup    sts_arr(ycnt)                          99
023500161129
023600090415         if not *in99;
023700011113
023800090415           pay_cycle  = in_cycle;
023900090415           prd_cmplt = in_cmplt;
024000090415           pay_cycqty = in_cycqty;
024100090415           proll_used = proll_type;
024200121012
024300090415           open payrtypl01;
024400090415           open payrresl01;
024500090415           open paycycd;
024600121012
024700090415           chain pay_cycle cycfmt;
024800090415           year_factor = cycyearf * pay_cycqty;
024900180319           setll *loval prtyfmt;
025000180319           read prtyfmt;
025100180319           dow not(%eof);
025200180319
025300180319             if prtyprno <> *zeros and
025400180319                   prtyprno <> prtyproll#;
025500180319               chain prtyptype prrefmt;
025600180319               if %found and
025700180319                     %scan(in_status:prrelist) <> 0;
025800180319                 chain prtycycode cycfmt;
025900180319
026000180319                 if year_factor < (cycyearf * prtycycqty);
026100180319                   pay_cycle  = prtycycode;
026200180319                   proll_used = prtyptype;
026300180319                   prd_cmplt = prtyproll#;
026400180319                   pay_cycqty  = prtycycqty;
026500180319                   year_factor = cycyearf * prtycycqty;
026600180319                 endif;
026700180319
026800180319               endif;
026900180319             endif;
027000180319
027100180319             read prtyfmt;
027200180319           enddo;
027300121012
027400090415           // Set Processing Period to 1 more than Period Completed
027500090415           prd_cmpltP = prd_cmplt + 1;
027600090415           close payrtypl01;
027700090415           close payrresl01;
027800090415           close paycycd;
027900121012
028000090415           xcnt += 1;
028100090415           ycnt = xcnt;
028200090415           sts_arr(ycnt) = in_status;
028300121012
028400090415           If xcnt > StrucMax;
028500090415             StrucMax = StrucMax + 2;
028600090415             Memory = %Size(sts_struc) * StrucMax;
028700090415             Memory2 = %Size(sts_arr) * StrucMax;
028800161129
028900161129
029000090415           EndIf;
029100121012
029200161129
029300011119     C     ycnt          occur     sts_struc
029400161129
029500090415           sts_cycle = pay_cycle;
029600090415           sts_cmpltp = prd_cmpltp;
029700090415           sts_cycqty = pay_cycqty;
029800090415           sts_prolused  = proll_used;
029900121012
030000090415         else;
030100121012
030200011119     C     ycnt          occur     sts_struc
030300121013
030400090415           pay_cycle = sts_cycle;
030500090415           prd_cmpltp = sts_cmpltp;
030600121013           pay_cycqty = sts_cycqty;
030700090415           proll_used = sts_prolused;
030800121013
030900090415         endif;
031000110427
031100041106         if pctxprate = 'Y';
031200041106           prd_cmpltp = getPrdComp();
031300041106         endif;
031400110427
031500090415       endsr;
031600011119
031700180319
031800090415       begsr *inzsr;
031900090415         // Set up initial memory allocation for Struc of StrucMax (5) elements
032000090415         Memory = %Size(sts_struc) * StrucMax;
032100090415         Memory2 = %Size(sts_arr) * StrucMax;
032200161129
032300041106         open(e) paycon;
032400041018          read pconfmt;
032500041018          close(e) paycon;
032600090415       endsr;
032700041026
032800180319
032900180319
033000090415       //-------------------------------------------
033100090415       // Procedure name: getPrdComp
033200090415       // Purpose:  Get number of completed Periods
033300090415       // Returns:        1 (completed Periods)
033400090415       // Parameter:      none
033500090415       //-------------------------------------------
033600161129
033700041026     P getPrdComp      B
033800041026     D getPrdComp      PI             2  0
033900041107
034000041107     D trndate         s              8  0
034100050422     D strdate         s              8  0
034200041026
034300041107          trndate = %int(%subst(%editc(employed:'X'):5:4) +
034400041107                    %subst(%editc(employed:'X'):3:2) +
034500041107                    %subst(%editc(employed:'X'):1:2));
034600041107
034700041106          if %date(employed:*eur) >
034800041106           %date(%int(%char(prd_year) + '0101'):*iso);
034900050422
035000050422            strdate = %int(%char(prd_year) + '0101');
035100041106
035200050509            open(e) payhisl05;
035300161201            setll (txemp:strdate) phisfmt;
035400161201          //setll (txemp:proll_used:strdate) phisfmt;
035500161201            reade txemp phisfmt;
035600161201          //reade (txemp:proll_used) phisfmt;
035700041106            if %eof;
035800050509              close(e) payhisl05;
035900041106              return 1;
036000041106             else;
036100050509              close(e) payhisl05;
036200050509              if phistype = proll_used;
036300050509                return (prd_cmpltp - phisfnight + 1);
036400180319                // get the closest of this payroll for number L9
036500050509               else;
036600050509                open payhisl09;
036700050509                setll (proll_used:phisndate) phisl09;
036800050509                reade (proll_used) phisl09;
036900050509                close(e) payhisl09;
037000050509                return (prd_cmpltp - phisfnight + 1);
037100050509              endif;
037200041106            endif;
037300041106
037400041106          endif;
037500041106
037600041106            return prd_cmpltp;
037700041026
037800041026
037900041026     P GetPrdComp      E
038000161129
038100161129       //-----------------------------------------------
038200161129       // Procedure name: getFpyGross
038300161129       // Purpose:  Get correct Gross to Calc. Free Pay
038400161129       // Returns:        Actual Gross
038500161129       // Parameter:      type,emp,taxgross
038600161129       //----------------------------------------------
038700161129
038800161129     P getFpyGross     B
038900161129     D getFPyGross     PI            11  2
039000161129     D taxtype                        1  0 const
039100161129     D taxemp                         6  0 const
039200161129     D taxgross                      11  2 const
039300161207
039400161207     D grs_cmpltP      s              2  0
039500161129
039600161207          if pctxonprd = 'Y'; //calc. based on period only
039700161207             grs_cmpltP = 1;
039800161207            else;
039900161207             grs_cmpltP = prd_cmpltP;
040000161207          endif;
040100161207
040200161129          select;
040300161129             when taxtype = 1;
040400161129                return taxgross;
040500161129             when taxtype = 2;
040600161201                return  get_basic_wage(taxemp) /
040700161207                          year_factor * grs_cmpltP * pay_cycqty;
040800161129          endsl;
040900161129
041000161129     P GetFPyGross     E
041100161129
041200161129
041300161129       //----------------------------------------------------
041400161129       // Procedure name: dft_tax_calc
041500161129       // Purpose:  Calc. tax based on dft. (basic) settings
041600161129       // Returns:        Actual Gross
041700161129       // Parameter:      type,emp,taxgross
041800161129       //----------------------------------------------------
041900161129
042000161129     P dft_tax_calc    B
042100161129     D dft_tax_calc    PI            11  2
042200161207
042300161207     D dft_cmpltP      s              2  0
042400161129
042500161130           setll ('1':pay_cycle) ptx2fmt;
042600161130           reade ('1':pay_cycle) ptx2fmt;
042700161207
042800161129           // calculation on defaults
042900161130           if (%eof);
043000161129             tdpye = (txgro - prfpay) * ptxdftper /100;
043100161129
043200161129           else;
043300161129             // calculation on range values
043400161129             taxable = txgro - prfpay;
043500161129             tdpye  = *zeros;
043600161129
043700161130             dow not(%eof);
043800161129               // Taxable above Limit of range record
043900161207               if pctxonprd = 'Y'; //calc. based on period only
044000161207                 dft_cmpltP = 1;
044100161207                  else;
044200161207                 dft_cmpltP = prd_cmpltP;
044300161207               endif;
044400161207
044500161129               if taxable >
044600161207                     ((ptx2qtyu - ptx2qtyl) * dft_cmpltP);
044700161129
044800161207                 taxable = taxable -
044900161207                     (ptx2qtyu - ptx2qtyl) * dft_cmpltP;
045000161207                 tdpye = tdpye +
045100161207                     (ptx2qtyu - ptx2qtyl) * dft_cmpltP *
045200161207                         ptx2per / 100;
045300161129               else;
045400161129                 tdpye = tdpye + (taxable *
045500161201                     ptx2per / 100);
045600161129                 taxable = *zeros;
045700161129                 leave;
045800161129               endif;
045900161129
046000161130               reade ('1':pay_cycle) ptx2fmt;
046100161129             enddo;
046200161129
046300161129             // Trap any remaining taxable amount
046400161129             tdpye = tdpye + (taxable *
046500161129                 ptx2per / 100);
046600161129           endif;
046700161130
046800161130           return tdpye;
046900161129
047000161129     P dft_tax_calc    E
047100161129
047200161129
047300161129       //----------------------------------------------------
047400161129       // Procedure name: dyn_tax_calc
047500161129       // Purpose:  Calc. tax based on dynamic  settings
047600161129       // Returns:        Actual Gross
047700161129       // Parameter:      type,emp,taxgross
047800161129       //----------------------------------------------------
047900161129
048000161129     P dyn_tax_calc    B
048100161129     D dyn_tax_calc    PI            11  2
048200161207
048300161207     D dyn_cmpltP      s              2  0
048400161129
048500161129
048600161130           setll ('2':pay_cycle) ptx2fmt;
048700161130           reade ('2':pay_cycle) ptx2fmt;
048800161130
048900161206           select;
049000161206
049100161207             when ptxdrngtyp = 1; // calc. method  using amounts
049200161206              // calculation on defaults
049300161206              if (%eof);
049400161206                tdpye = (txgro - prfpay) * ptxdftper /100;
049500161206
049600161206              else;
049700161206                // calculation on range values
049800161206                taxable = txgro - prfpay;
049900161206                tdpye  = *zeros;
050000161206
050100161206                dow not(%eof);
050200161206                  // Taxable above Limit of range record
050300161207                 if pctxonprd = 'Y'; //calc. based on period only
050400161207                   dyn_cmpltP = 1;
050500161207                    else;
050600161207                   dyn_cmpltP = prd_cmpltP;
050700161207                 endif;
050800161207
050900161206                  if taxable >
051000161207                        ((ptx2qtyu - ptx2qtyl) * dyn_cmpltP);
051100161206
051200161207                    taxable = taxable -
051300161207                        (ptx2qtyu - ptx2qtyl) * dyn_cmpltP;
051400161207                    tdpye = tdpye +
051500161207                        (ptx2qtyu - ptx2qtyl) * dyn_cmpltP *
051600161207                             ptx2per / 100;
051700161206                  else;
051800161206                    tdpye = tdpye + (taxable *
051900161206                        ptx2per / 100);
052000161206                    taxable = *zeros;
052100161206                    leave;
052200161206                  endif;
052300161206
052400161206                  reade ('2':pay_cycle) ptx2fmt;
052500161206                enddo;
052600161206
052700161206                // Trap any remaining taxable amount
052800161206                tdpye = tdpye + (taxable *
052900161206                    ptx2per / 100);
053000161206              endif;
053100161206
053200161207             when ptxdrngtyp = 2; //calcultion method using percentages
053300161206              // calculation on defaults
053400161206              if (%eof);
053500161206                tdpye = (txgro - prfpay) * ptxtaxper /100;
053600161206
053700161206              else;
053800161206                // calculation on range values
053900161206                taxable = txgro - prfpay;
054000161206                tdpye  = *zeros;
054100161206
054200161206                dow not(%eof);
054300161206
054400161206                  // Tax calculated on various percentages of
054500161206                  // taxable amount
054600161206                    tdpye = tdpye +
054700161206                        (taxable * (ptx2qtyu - ptx2qtyl) / 100 *
054800161206                        ptx2per / 100);
054900161206
055000161206                  reade ('2':pay_cycle) ptx2fmt;
055100161206                enddo;
055200161206
055300161206              endif;
055400161206
055500161206           endsl;
055600161129
055700161130           return tdpye;
055800161129
055900161129     P dyn_tax_calc    E
056000161130
056100161130
056200161130       //----------------------------------------------------
056300161130       // Procedure name: Check_if_dynamic_tax
056400161130       // Purpose:  Check if tax calc. is dynamic
056500161130       // Returns:        indicator
056600161130       // Parameter:      type,emp,taxgross
056700161130       //----------------------------------------------------
056800161130
056900161130     P check_if_dynamic_tax...
057000161130     P                 B
057100161130     D check_if_dynamic_tax...
057200161130     D                 PI              n
057300161130     D taxtype                        1  0 const
057400161130     D taxper                         5  3 const
057500161130     D taxfree                       11  2 const
057600161130     D taxemp                         6  0 const
057700161130     D taxgross                      11  2 const
057800161130
057900161201          if taxper <> *zeros
058000161130               and (getFpyGross(taxtype:taxemp:taxgross) * taxper/100) >
058100161130                taxfree;
058200161130               return  *on;
058300161130             else;
058400161130               return  *off;
058500161130          endif;
058600161130
058700161130     P check_if_dynamic_tax...
058800161130     P                 E
058900161130
059000161130
