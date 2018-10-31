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
001800110427     D paytaxcalc      Pi                  ExtPgm('PAYTAXCALC')
001900161201     D   txemp                        6  0       options(*nopass)
002000161201     D   txfrpay                      5  0       options(*nopass)
002100110427     D   gross                       11  2       options(*nopass)
002200110427     D   tottax                      11  2       options(*nopass)
002300110427     D   in_cycle                     2          options(*nopass)
002400110427     D   in_cmplt                     2  0       options(*nopass)
002500110427     D   prd_year                     4  0       options(*nopass)
002600110427     D   in_cycqty                    2  0       options(*nopass)
002700110427     D   in_status                    1          options(*nopass)
002800110427     D   employed                     8  0       options(*nopass)
002900110427     D   proll_type                   1          options(*nopass)
003000180130     D   nis_deduct                  11  2       options(*nopass)
003100110427
003200090415      //-------------------------------------
003300090415      // Prototype for procedure: GetPrdComp
003400090415      //-------------------------------------
003500041026     D GetPrdComp      PR             2  0
003600041026     D
003700150824
003800121013     D**ts_struc       ds                  occurs(45) based(struc@)
003900121013     D sts_struc       ds                  occurs(45) inz
004000121012     D sts_cycle                      2
004100121012     D sts_cmpltp                     2  0
004200121012     D sts_cycqty                     2  0
004300121012     D sts_prolused                   1
004400011119
004500121013     D**s_arr          s              1    dim(45) based(arr@)
004600121013     Dsts_arr          s              1    dim(45) inz
004700150824
004800121012     D StrucMax        s              3P 0 Inz(5)
004900011119     D Memory          s              5P 0
005000011119     D Memory2         s              5P 0
005100011119     D taxable         s             11  2
005200011119     D prfpay          s             11  2
005300011119     D tdpye           s              9  0
005400011119     D tsav            s             11  2
005500011119     D txgro           s             11  2
005600041106     D proll_used      s              1
005700011119     D prd_cmpltP      s              2  0
005800011119     D prd_cmplt       s              2  0
005900011119     D pay_cycle       s              2
006000011119     D pay_cycqty      s              2  0
006100011113     D year_factor     s                   like(cycyearf)
006200011119     D xcnt            s              3  0 inz(0)
006300011119     D ycnt            s                   like(xcnt) inz(1)
006400150824     D pnis_type       s              2  0 inz(63)
006500021030     D ptax_type       s              2  0 inz(61)
006600021030     D pgrs_type       s              2  0 inz(00)
006700021030     D ytdgro          s             11  2
006800021030     D ytdtax          s             11  2
006900180130     D ytdnis          s             11  2
007000161130     D dyn_tax_ind     s               n   inz(*off)
007100150824
007200150824
007300021030     C     ytd_keyG      klist
007400161201     C                   kfld                    txemp
007500010720     C                   kfld                    prd_year
007600021030     C                   kfld                    pgrs_type
007700150824
007800021030     C     ytd_keyT      klist
007900161201     C                   kfld                    txemp
008000021030     C                   kfld                    prd_year
008100021030     C                   kfld                    ptax_type
008200180130
008300180130     C     ytd_keyN      klist
008400180130     C                   kfld                    txemp
008500180130     C                   kfld                    prd_year
008600180130     C                   kfld                    pnis_type
008700110427
008800161201       if  %parms < %parmnum(txemp);
008900110427         *inlr = *on;
009000110427         return;
009100110427       endif;
009200110427
009300110427       // Get Correct Tax period
009400090415       exsr get_corr_prd;
009500150824
009600090415       ytdgro = *zeros;
009700090415       ytdtax = *zeros;
009800180130       ytdnis = *zeros;
009900090415       // No Calculation for negative Gross
010000090415       if gross < *zeros;
010100090415         return;
010200090415       endif;
010300150824
010400180130       // Get YTD Taxable Gross
010500090415       setll ytd_keyg ytdrec;
010600090415       reade ytd_keyg ytdrec;
010700090415       dow not(%eof);
010800090415         ytdgro += ytdamt;
010900090415         reade ytd_keyg ytdrec;
011000090415       enddo;
011100150824
011200180130       // Get YTD Taxes Gross
011300090415       setll ytd_keyt ytdrec;
011400090415       reade ytd_keyt ytdrec;
011500090415       dow not(%eof);
011600090415         ytdtax += ytdamt;
011700090415         reade ytd_keyt ytdrec;
011800090415       enddo;
011900180130
012000180131       // Get YTD NIS
012100180130       setll ytd_keyn ytdrec;
012200180130       reade ytd_keyn ytdrec;
012300180130       dow not(%eof);
012400180130         ytdnis += ytdamt;
012500180130         reade ytd_keyn ytdrec;
012600180130       enddo;
012700180130
012800110427
012900090415       chain pay_cycle ptxfmt;
013000110427
013100090415       //****************************************************
013200090415       //* TO-DATE FREE PAY & GROSS CALCULATIONS  ROUTINE ***
013300090415       //****************************************************
013400090415       //  Setup freepay for periods completed to date
013500161201       if txfrpay <> *all'9';
013600110427
013700161129
013800041018          if  pctxonprd = 'Y';
013900161130             dyn_tax_ind =
014000161201                     check_if_dynamic_tax(ptxtype1:ptxper:ptxfree:txemp:gross);
014100161130
014200161129             if  dyn_tax_ind;
014300161129               prfpay =
014400161201                   (getFpyGross(ptxtype1:txemp:gross) * ptxper/100) *
014500161129                   pay_cycqty;
014600161129              else;
014700161129               prfpay = ptxfree * pay_cycqty;
014800161129             endif;
014900161130
015000150824             // Get taxable gross
015100180130             if pcnistaxo = 'Y';
015200180130               txgro = gross - nis_deduct;
015300180130              else;
015400180130               txgro = gross;
015500180130             endif;
015600041018            else;
015700041106
015800161130             dyn_tax_ind =
015900161201                     check_if_dynamic_tax(ptxtype1:ptxper:ptxfree * prd_cmpltP
016000161201                                               :txemp:(gross+ytdgro));
016100161130
016200161129             if  dyn_tax_ind;
016300161129                prfpay =
016400161201                   (getFpyGross(ptxtype1:txemp:(gross + ytdgro))
016500161130                       * ptxper/100) * pay_cycqty;
016600161129              else;
016700041106                prfpay = ptxfree * prd_cmpltP
016800041106                       * pay_cycqty;
016900161129             endif;
017000041106
017100150824             // Get taxable gross
017200180130             if pcnistaxo = 'Y';
017300180130               txgro = gross + ytdgro - (nis_deduct + ytdnis);
017400180130              else;
017500180130               txgro = gross + ytdgro;
017600180130             endif;
017700041018          endif;
017800150824
017900090415      //****************************************************************
018000090415      //  TAX CALCULATIONS **
018100090415      //****************************************************************
018200150824
018300090415         if txgro > prfpay;
018400161129
018500161129           if  dyn_tax_ind;
018600161129               dyn_tax_calc();
018700161129             else;
018800161129               dft_tax_calc();
018900161129           endif;
019000161129
019100110427
019200090415           // Get tax to pay
019300041018          if  pctxonprd = 'Y';
019400041018            tottax = tdpye;
019500041018             else;
019600041018            tottax = tdpye - ytdtax;
019700041018          endif;
019800110427
019900090415           if tottax < *zeros;
020000090415             tottax = *zeros;
020100090415           endif;
020200150824
020300090415         endif;
020400090415       endif;
020500121012
020600090415       return;
020700090415       //***************************
020800090415       // Initilisation routine   **
020900090415       //***************************
021000090415       begsr get_corr_prd;
021100090415         // Find correct cycle
021200090415         reset ycnt;
021300011119
021400011119     c     in_status     lookup    sts_arr(ycnt)                          99
021500161129
021600090415         if not *in99;
021700011113
021800090415           pay_cycle  = in_cycle;
021900090415           prd_cmplt = in_cmplt;
022000090415           pay_cycqty = in_cycqty;
022100090415           proll_used = proll_type;
022200121012
022300090415           open payrtypl01;
022400090415           open payrresl01;
022500090415           open paycycd;
022600121012
022700090415           chain pay_cycle cycfmt;
022800090415           year_factor = cycyearf * pay_cycqty;
022900090415           setll *loval prtyfmt;
023000090415           read prtyfmt;
023100090415           dow not(%eof);
023200121012
023300090415             if prtyprno <> *zeros and
023400090415                   prtyprno <> prtyproll#;
023500090415               chain prtyptype prrefmt;
023600090415               if %found and
023700090415                     %scan(in_status:prrelist) <> 0;
023800090415                 chain prtycycode cycfmt;
023900121012
024000090415                 if year_factor < (cycyearf * prtycycqty);
024100090415                   pay_cycle  = prtycycode;
024200090415                   proll_used = prtyptype;
024300090415                   prd_cmplt = prtyproll#;
024400090415                   pay_cycqty  = prtycycqty;
024500090415                   year_factor = cycyearf * prtycycqty;
024600090415                 endif;
024700011119
024800090415               endif;
024900090415             endif;
025000121012
025100090415             read prtyfmt;
025200090415           enddo;
025300121012
025400090415           // Set Processing Period to 1 more than Period Completed
025500090415           prd_cmpltP = prd_cmplt + 1;
025600090415           close payrtypl01;
025700090415           close payrresl01;
025800090415           close paycycd;
025900121012
026000090415           xcnt += 1;
026100090415           ycnt = xcnt;
026200090415           sts_arr(ycnt) = in_status;
026300121012
026400090415           If xcnt > StrucMax;
026500090415             StrucMax = StrucMax + 2;
026600090415             Memory = %Size(sts_struc) * StrucMax;
026700090415             Memory2 = %Size(sts_arr) * StrucMax;
026800161129
026900161129
027000090415           EndIf;
027100121012
027200161129
027300011119     C     ycnt          occur     sts_struc
027400161129
027500090415           sts_cycle = pay_cycle;
027600090415           sts_cmpltp = prd_cmpltp;
027700090415           sts_cycqty = pay_cycqty;
027800090415           sts_prolused  = proll_used;
027900121012
028000090415         else;
028100121012
028200011119     C     ycnt          occur     sts_struc
028300121013
028400090415           pay_cycle = sts_cycle;
028500090415           prd_cmpltp = sts_cmpltp;
028600121013           pay_cycqty = sts_cycqty;
028700090415           proll_used = sts_prolused;
028800121013
028900090415         endif;
029000110427
029100041106         if pctxprate = 'Y';
029200041106           prd_cmpltp = getPrdComp();
029300041106         endif;
029400110427
029500090415       endsr;
029600011119
029700090415       begsr *inzsr;
029800090415         // Set up initial memory allocation for Struc of StrucMax (5) elements
029900090415         Memory = %Size(sts_struc) * StrucMax;
030000090415         Memory2 = %Size(sts_arr) * StrucMax;
030100161129
030200041106         open(e) paycon;
030300041018          read pconfmt;
030400041018          close(e) paycon;
030500090415       endsr;
030600041026
030700090415       //-------------------------------------------
030800090415       // Procedure name: getPrdComp
030900090415       // Purpose:  Get number of completed Periods
031000090415       // Returns:        1 (completed Periods)
031100090415       // Parameter:      none
031200090415       //-------------------------------------------
031300161129
031400041026     P getPrdComp      B
031500041026     D getPrdComp      PI             2  0
031600041107
031700041107     D trndate         s              8  0
031800050422     D strdate         s              8  0
031900041026
032000041107          trndate = %int(%subst(%editc(employed:'X'):5:4) +
032100041107                    %subst(%editc(employed:'X'):3:2) +
032200041107                    %subst(%editc(employed:'X'):1:2));
032300041107
032400041106          if %date(employed:*eur) >
032500041106           %date(%int(%char(prd_year) + '0101'):*iso);
032600050422
032700050422            strdate = %int(%char(prd_year) + '0101');
032800041106
032900050509            open(e) payhisl05;
033000161201            setll (txemp:strdate) phisfmt;
033100161201          //setll (txemp:proll_used:strdate) phisfmt;
033200161201            reade txemp phisfmt;
033300161201          //reade (txemp:proll_used) phisfmt;
033400041106            if %eof;
033500050509              close(e) payhisl05;
033600041106              return 1;
033700041106             else;
033800050509              close(e) payhisl05;
033900050509              if phistype = proll_used;
034000050509                return (prd_cmpltp - phisfnight + 1);
034100050509              // get the closest of this payroll for number L9
034200050509               else;
034300050509                open payhisl09;
034400050509                setll (proll_used:phisndate) phisl09;
034500050509                reade (proll_used) phisl09;
034600050509                close(e) payhisl09;
034700050509                return (prd_cmpltp - phisfnight + 1);
034800050509              endif;
034900041106            endif;
035000041106
035100041106          endif;
035200041106
035300041106            return prd_cmpltp;
035400041026
035500041026
035600041026     P GetPrdComp      E
035700161129
035800161129       //-----------------------------------------------
035900161129       // Procedure name: getFpyGross
036000161129       // Purpose:  Get correct Gross to Calc. Free Pay
036100161129       // Returns:        Actual Gross
036200161129       // Parameter:      type,emp,taxgross
036300161129       //----------------------------------------------
036400161129
036500161129     P getFpyGross     B
036600161129     D getFPyGross     PI            11  2
036700161129     D taxtype                        1  0 const
036800161129     D taxemp                         6  0 const
036900161129     D taxgross                      11  2 const
037000161207
037100161207     D grs_cmpltP      s              2  0
037200161129
037300161207          if pctxonprd = 'Y'; //calc. based on period only
037400161207             grs_cmpltP = 1;
037500161207            else;
037600161207             grs_cmpltP = prd_cmpltP;
037700161207          endif;
037800161207
037900161129          select;
038000161129             when taxtype = 1;
038100161129                return taxgross;
038200161129             when taxtype = 2;
038300161201                return  get_basic_wage(taxemp) /
038400161207                          year_factor * grs_cmpltP * pay_cycqty;
038500161129          endsl;
038600161129
038700161129     P GetFPyGross     E
038800161129
038900161129
039000161129       //----------------------------------------------------
039100161129       // Procedure name: dft_tax_calc
039200161129       // Purpose:  Calc. tax based on dft. (basic) settings
039300161129       // Returns:        Actual Gross
039400161129       // Parameter:      type,emp,taxgross
039500161129       //----------------------------------------------------
039600161129
039700161129     P dft_tax_calc    B
039800161129     D dft_tax_calc    PI            11  2
039900161207
040000161207     D dft_cmpltP      s              2  0
040100161129
040200161130           setll ('1':pay_cycle) ptx2fmt;
040300161130           reade ('1':pay_cycle) ptx2fmt;
040400161207
040500161129           // calculation on defaults
040600161130           if (%eof);
040700161129             tdpye = (txgro - prfpay) * ptxdftper /100;
040800161129
040900161129           else;
041000161129             // calculation on range values
041100161129             taxable = txgro - prfpay;
041200161129             tdpye  = *zeros;
041300161129
041400161130             dow not(%eof);
041500161129               // Taxable above Limit of range record
041600161207               if pctxonprd = 'Y'; //calc. based on period only
041700161207                 dft_cmpltP = 1;
041800161207                  else;
041900161207                 dft_cmpltP = prd_cmpltP;
042000161207               endif;
042100161207
042200161129               if taxable >
042300161207                     ((ptx2qtyu - ptx2qtyl) * dft_cmpltP);
042400161129
042500161207                 taxable = taxable -
042600161207                     (ptx2qtyu - ptx2qtyl) * dft_cmpltP;
042700161207                 tdpye = tdpye +
042800161207                     (ptx2qtyu - ptx2qtyl) * dft_cmpltP *
042900161207                         ptx2per / 100;
043000161129               else;
043100161129                 tdpye = tdpye + (taxable *
043200161201                     ptx2per / 100);
043300161129                 taxable = *zeros;
043400161129                 leave;
043500161129               endif;
043600161129
043700161130               reade ('1':pay_cycle) ptx2fmt;
043800161129             enddo;
043900161129
044000161129             // Trap any remaining taxable amount
044100161129             tdpye = tdpye + (taxable *
044200161129                 ptx2per / 100);
044300161129           endif;
044400161130
044500161130           return tdpye;
044600161129
044700161129     P dft_tax_calc    E
044800161129
044900161129
045000161129       //----------------------------------------------------
045100161129       // Procedure name: dyn_tax_calc
045200161129       // Purpose:  Calc. tax based on dynamic  settings
045300161129       // Returns:        Actual Gross
045400161129       // Parameter:      type,emp,taxgross
045500161129       //----------------------------------------------------
045600161129
045700161129     P dyn_tax_calc    B
045800161129     D dyn_tax_calc    PI            11  2
045900161207
046000161207     D dyn_cmpltP      s              2  0
046100161129
046200161129
046300161130           setll ('2':pay_cycle) ptx2fmt;
046400161130           reade ('2':pay_cycle) ptx2fmt;
046500161130
046600161206           select;
046700161206
046800161207             when ptxdrngtyp = 1; // calc. method  using amounts
046900161206              // calculation on defaults
047000161206              if (%eof);
047100161206                tdpye = (txgro - prfpay) * ptxdftper /100;
047200161206
047300161206              else;
047400161206                // calculation on range values
047500161206                taxable = txgro - prfpay;
047600161206                tdpye  = *zeros;
047700161206
047800161206                dow not(%eof);
047900161206                  // Taxable above Limit of range record
048000161207                 if pctxonprd = 'Y'; //calc. based on period only
048100161207                   dyn_cmpltP = 1;
048200161207                    else;
048300161207                   dyn_cmpltP = prd_cmpltP;
048400161207                 endif;
048500161207
048600161206                  if taxable >
048700161207                        ((ptx2qtyu - ptx2qtyl) * dyn_cmpltP);
048800161206
048900161207                    taxable = taxable -
049000161207                        (ptx2qtyu - ptx2qtyl) * dyn_cmpltP;
049100161207                    tdpye = tdpye +
049200161207                        (ptx2qtyu - ptx2qtyl) * dyn_cmpltP *
049300161207                             ptx2per / 100;
049400161206                  else;
049500161206                    tdpye = tdpye + (taxable *
049600161206                        ptx2per / 100);
049700161206                    taxable = *zeros;
049800161206                    leave;
049900161206                  endif;
050000161206
050100161206                  reade ('2':pay_cycle) ptx2fmt;
050200161206                enddo;
050300161206
050400161206                // Trap any remaining taxable amount
050500161206                tdpye = tdpye + (taxable *
050600161206                    ptx2per / 100);
050700161206              endif;
050800161206
050900161207             when ptxdrngtyp = 2; //calcultion method using percentages
051000161206              // calculation on defaults
051100161206              if (%eof);
051200161206                tdpye = (txgro - prfpay) * ptxtaxper /100;
051300161206
051400161206              else;
051500161206                // calculation on range values
051600161206                taxable = txgro - prfpay;
051700161206                tdpye  = *zeros;
051800161206
051900161206                dow not(%eof);
052000161206
052100161206                  // Tax calculated on various percentages of
052200161206                  // taxable amount
052300161206                    tdpye = tdpye +
052400161206                        (taxable * (ptx2qtyu - ptx2qtyl) / 100 *
052500161206                        ptx2per / 100);
052600161206
052700161206                  reade ('2':pay_cycle) ptx2fmt;
052800161206                enddo;
052900161206
053000161206              endif;
053100161206
053200161206           endsl;
053300161129
053400161130           return tdpye;
053500161129
053600161129     P dyn_tax_calc    E
053700161130
053800161130
053900161130       //----------------------------------------------------
054000161130       // Procedure name: Check_if_dynamic_tax
054100161130       // Purpose:  Check if tax calc. is dynamic
054200161130       // Returns:        indicator
054300161130       // Parameter:      type,emp,taxgross
054400161130       //----------------------------------------------------
054500161130
054600161130     P check_if_dynamic_tax...
054700161130     P                 B
054800161130     D check_if_dynamic_tax...
054900161130     D                 PI              n
055000161130     D taxtype                        1  0 const
055100161130     D taxper                         5  3 const
055200161130     D taxfree                       11  2 const
055300161130     D taxemp                         6  0 const
055400161130     D taxgross                      11  2 const
055500161130
055600161201          if taxper <> *zeros
055700161130               and (getFpyGross(taxtype:taxemp:taxgross) * taxper/100) >
055800161130                taxfree;
055900161130               return  *on;
056000161130             else;
056100161130               return  *off;
056200161130          endif;
056300161130
056400161130     P check_if_dynamic_tax...
056500161130     P                 E
056600161130
056700161130
