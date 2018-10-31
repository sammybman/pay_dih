000100060911     H expropts(*RESDECPOS) dftactgrp(*NO) actgrp(*CALLER) bnddir('DISDIR')
000200060304     H DATEDIT(*YMD)
000300000000      //*********************************************************
000400000000      //              B A N K S    D I H    L T D.              *
000500151015      //                   PAYROLL System                       *
000600000000      //                                                        *
000700000000      //                         BY                             *
000800000000      //                                                        *
000900000000      //       M.I.S. Department, Thirst Park Bank DIH Ltd      *
001000000000      //             P.O BOX 10501, GEORGETOWN,GUYANA           *
001100000000      //                                                        *
001200000000      //                                                        *
001300151015      //       RPG PROGRAM NAME..: PAYPICALC                    *
001400000000      //                                                        *
001500040423      //       PROGRAMMER NAME: Samuel Britton                  *
001600151015      //       DESIGN DATE....: 2015/10/15                      *
001700000000      //                                                        *
001800151015      //  PROGRAM DESCRIPTION:  Performance Incentive Calcs.    *
001900000000      //                                                        *
002000151015      //  THIS PROGRAM IS DESIGNED TO calculate the Performance *
002100151015      //  Incentive for non-sales employees                     *
002200000000      //                                                        *
002300000000      //*********************************************************
002400060109
002500151015     Fpicommcm  if   e           k disk    usropn
002600151015     Fpicommgm  if   e           k disk    usropn
002700151021     Fpicommitm if   e           k disk    usropn
002800151015     Fpicommrl01if   e           k disk    usropn
002900151011     Fpicommwrk2uf a e           k disk    usropn
003000151011     Fpicommwrk3uf a e           k disk    usropn
003100151021     FQsysprt   o    f  165        printer oflind(*inof) usropn
003200040323
003300000000     D                SDS
003400000000     D  @Prm             *Parms
003500000000     D  @PGM             *PROC
003600130503     D  @USER                358    367
003700060318
003800060112     D@date            s             10
003900060112     Dconame           s             30
004000060109     Dstrdate          s              8  0
004100060109     Denddate          s              8  0
004200040426     Ddate1            s              8  0
004300060112     Dtime1            s              6  0
004400060424     D @cmd            s             80
004500060323     D name            s             30
004600060425     D percentage      s              5  2
004700060323     D description     s             40
004800060323     D idesc           s             30
004900151028     D*emp_tar         s             13  2
005000151028     D*emp_unit        s             13  2
005100151028     D*emp_sub         s             13  2
005200151028     D*dept_tar        s             13  2
005300151028     D*dept_unit       s             13  2
005400151028     D*dept_sub        s             13  2
005500151028     D typ_tar         s             13  2
005600151028     D typ_unit        s             13  2
005700151028     D typ_sub         s             13  2
005800151028     D fin_sub         s             13  2
005900060112     D cmd             s            200
006000151016     D qryselect       s            512    varying
006100151016     D Quote           c                   const('''')
006200151016     D WareHouseList   s            256    varying
006300151016     D TrnCodeList     s            256    varying
006400151103     D StatusList      s            256    varying
006500060111     D prno#           s              6  0
006600060403     D dollarIND       s               n
006700060403     D out_qty         s             14  4
006800060323     D rates           s                    dim(3) based(rateptr)
006900060110     D                                       like(cmrrate#1)
007000060510     D rateptr         s               *    inz(%addr(cmrrate#1))
007100060509     D payrun_factor   s              7  3 inz(1)
007200060509     D payrun_qty      s              2  0 inz(1)
007300060509     D hrs_dummy       s              5  2
007400060509     D pay_amt         s             11  2
007500060509     D no_prd          s              6  0
007600160412     D Sep             s              1    varying
007700151027
007800151027       Dcl-S arrValues     VarChar(512) dim(30);
007900160418       Dcl-S srh_amount    Packed(13:2) dim(300);
008000160418       Dcl-S srharr        Char(108) dim(300);
008100160418       Dcl-S currentIdx    Packed(3);
008200160416       Dcl-S Gcnt          Packed(3);
008300040426
008400151028     D prm_rec       e ds                   extname(gen_plist)
008500151028     D rate_rec      e ds                   extname(picommrm)
008600160412
008700160418     D*Srh_data        ds                  qualified dim(300)
008800160418     D*srh_code                      25
008900160418     D*srh_amount                    13  2
009000160412
009100160416     D                 ds
009200160416     D srh_xxx                      108
009300160416     D srh_org                        3    overlay(srh_xxx)
009400160416     D srh_loc                       75    overlay(srh_xxx:4)
009500160416     D srh_trns                      30    overlay(srh_xxx:79)
009600160412
009700040423
009800151011     D  main_pgm       pr                  extpgm('PAYPICALC')
009900060131     D   fprno                        6  0 const
010000060131     D   tprno                        6  0 const
010100151027     D   origin                       3    const  options(*nopass)
010200040608
010300040323     D  main_pgm       pi
010400060131     D   fprno                        6  0 const
010500060131     D   tprno                        6  0 const
010600151027     D   origin                       3    const  options(*nopass)
010700160409
010800160427     D*DspLngTxt       Pr                  ExtPgm( 'QUILNGTX' )
010900160427     D* LngTxt                    32767a   Const  Options( *VarSize )
011000160427     D* LngTxtLen                    10i 0 Const
011100160427     D* MsgId                         7a   Const
011200160427     D* MsgF                         20a   Const
011300160427     D* Error                     32767a   Const  Options( *VarSize )
011400160409
011500160427     D*ERRC0100        Ds                  Qualified
011600160427     D* BytPrv                       10i 0 Inz( %Size( ERRC0100 ))
011700160427     D* BytAvl                       10i 0
011800160427     D* MsgId                         7a
011900160427     D*                               1a
012000160427     D* MsgDta                      128a
012100160409
012200060109
012300151016       Dcl-Ds piDeptmDs   ExtName('PIDEPTM') End-Ds;
012400151016       Dcl-Ds piCodemDs   ExtName('PICODEM') End-Ds;
012500151016
012600151028       Dcl-DS  dataDS  qualified inz;
012700151028           date          Packed(8);
012800151028           location      Packed(3);
012900151028           amount        Packed(11:2);
013000151028           ProductCode   Char(6);
013100151028           Quantity      Packed(7);
013200151028           QuantitySin   Packed(7);
013300151028       End-DS;
013400151028
013500160413
013600060112     DcritDS           ds                  qualified  inz
013700151015     D dept#                          3  0
013800060109     D product                        6
013900040429
014000060113     D CLEAR_CMD       c                   'CLRPFM FILE(&file)'
014100060224     D PAYROLL         c                   'PAY'
014200060403     D YES             c                   'Y'
014300060510     D YEAR_CONST      c                   12
014400060112
014500060112     D Line_command    PR                  extpgm('QCMDEXC')
014600060112     D cmd                          100    options(*varsize) const
014700060112     D cmdlen                        15p 5 const
014800060109
014900060109     D write_foundDS   PR
015000151015     D dept#                          3  0 const
015100060109     D product                        6    const
015200151015     D*location                       3  0 const
015300040323     **
015400060109     DAccrue_data      pr
015500151015     D type                           2  0 const
015600151028     D dept#                          3  0 const
015700151015     D quantity                      13  2 const
015800040423
015900060109     DAccrue_empdta    pr
016000151015     D type                           2  0 const
016100151021     D dept#                          3  0 const
016200160413     D*quantity                      13  2 const
016300060403
016400060403     D  Uomval         pr              n
016500060403     D out_qty                       14  4
016600060403     D conuom                         2    const
016700060403     D frmuom                         2    const
016800151015     D frmqty                        13  2 const
016900060110
017000060110     D Search_for_rng  PR              n
017100151015     D type                           2  0 const
017200151015     D dept#                          3  0 const
017300060110     D product                        6    const
017400060323     D startInd                        n   const
017500060109
017600060113     D*-----------------------------------------
017700060113     D* Prototype for procedure: clear_workfile
017800060113     D*-----------------------------------------
017900060113     D clear_workfile  PR
018000060110
018100060110     D*-----------------------------------------
018200060110     D* Prototype for procedure: perform_output
018300060110     D*-----------------------------------------
018400060110     D perform_output  PR
018500060224
018600060224     D*-----------------------------------------
018700060224     D* Prototype for procedure: payroll_output
018800060224     D*-----------------------------------------
018900060224     D payroll_output  PR
019000060110
019100060109     D*----------------------------------------
019200060109     D* Prototype for procedure: comm_criteria
019300060109     D*----------------------------------------
019400060112     Dcomm_criteria    pr
019500151015     D type                           2  0 const
019600151015     D dept#                          3  0 const
019700060109     D product                        6    const
019800151015     D quantity                      13  2
019900060105
020000060105     D*--------------------------------------
020100060105     D* Prototype for procedure: open_file
020200060105     D*--------------------------------------
020300060105     D open_file       PR
020400151016     D read_fno                       1  0 const
020500060105
020600060105     D*--------------------------------------
020700060105     D* Prototype for procedure: read_file
020800060105     D*--------------------------------------
020900060105     D read_file       PR              n
021000060427     D read_fno                       1  0 const
021100060105
021200060105     D*--------------------------------------
021300060105     D* Prototype for procedure: process_rec
021400060105     D*--------------------------------------
021500060105     D process_rec     PR
021600060110
021700060110     D*--------------------------------------
021800060110     D* Prototype for procedure: apply_rates
021900060110     D*--------------------------------------
022000060110     D apply_rates     PR
022100060105
022200060105     D*--------------------------------------
022300060105     D* Prototype for procedure: close_file
022400060105     D*--------------------------------------
022500060105     D close_file      PR
022600151016     D read_fno                       1  0 const
022700060105
022800040323
022900130503      /copy qcpysrc,genPR
023000130503      /copy qcpysrc,salPR
023100130503      /copy qcpysrc,PAYPR
023200040423
023300040423
023400060105           // Get system Date
023500060112              @DATE = get_sys_date(date1:time1);
023600060112           // Get company name
023700060112              coname = get_coname();
023800060131
023900060110           // Get period start and end dates
024000060131              prno# = get_new_prd(fprno:1:'A');
024100060131              get_prd_info('*rng':*blanks:prno#:strdate);
024200060131              prno# = tprno;
024300060131              get_prd_info('*rng':*blanks:prno#:*omit:enddate);
024400060424
024500060509           // get number of periods
024600060509              no_prd = diff_prd(tprno:tprno);
024700060509
024800060424           // Prepare printer override
024900151028              @cmd = ' pagesize(*N 165)' +
025000060424                   ' cpi(12)';
025100130503              @cmd = %trim(@cmd) + ' HOLD(*YES)';
025200130503
025300151011              set_print_cntrl('ST':'PAY':@pgm:
025400060424              'QSYSPRT ':@user:@cmd:*omit);
025500060111
025600060112           // Clear work files
025700060113              clear_workfile();
025800060112
025900060111           // open data files for processing
026000151016              open_file(1);
026100060105
026200151015           // Process  Sales Records for Incentive payment
026300060105              Process_rec();
026400060110
026500151015           // Apply rates and values for Incentive payments
026600060110              apply_rates();
026700051230
026800060110           // Perform Printer Output
026900151016              perform_output();
027000060111
027100060224           // Output special payroll file for processing
027200151027              if  %parms >= %parmnum(origin)
027300151027               and origin = PAYROLL;
027400151015                  payroll_output();
027500060224              endif;
027600060224
027700060111           // close data files for processing
027800151016              close_file(1);
027900051230
028000040423           // Terminate Process
028100040413            *inlr = *on;
028200040413            return;
028300040323
028400151015
028500060112     OQSYSPRT   E            HDR            1 01
028600060112     O                       @PGM                10
028700060112     O                       coname              68
028800060112     O                                          126 'Page:'
028900060112     O                       PAGE          Z    132
029000060112     OQSYSPRT   E            HDR            2
029100060112     O                       @USER               10
029200151011     O                                           50 'Performance Incentive List'
029300060112     O                                         +  1 'For the Period'
029400060112     O                       strdate       Y   +  2
029500060112     O                                         +  1 'To'
029600060112     O                       enddate       Y   +  1
029700060112     O                                          120 'Date:'
029800060112     O                       date1         Y    132
029900060112
030000060112     OQSYSPRT   E            HDR2        2  2
030100060112
030200060112      // Details for report**
030300060112     OQSYSPRT   E            HDRD           2
030400060112      //
030500060112      // Position for heading
030600060114     O                                            6 'Emp.'
030700060114     O                                           12 'NAME'
030800060424     O                                           51 'Description'
030900060425     O                                           96 'Target (100%)'
031000160418     O                                          112 'Actual'
031100151028     O                                          125 ' % Achieved'
031200151028     O                                          140 'Rate'
031300151028     O                                          160 'Incentive'
031400060112      //
031500060112      // Fields in the file that would have the information  DET etc.
031600060112     OQSYSPRT   E            DET            1
031700060304     O                       cmw2emp       z      6
031800060323     O                       name           b    38
031900060424     O                       description    b    +2
032000060424     O                       cmctarget     1     +2
032100060424     O                       cmw2qty       1     +2
032200060424     O                       percentage    1     +2
032300060424     O                       cmw2rate      1     +2
032400060424     O                       cmw2value     1     +2
032500060114
032600151028      // Department Header  record
032700151028     OQSYSPRT   E            dept_tot       1
032800151028     O****                   name           b    38
032900151028     O                       description    b    80
033000151028     O                       cmctarget     1     +2
033100151028     O                       cmw2qty       1     +2
033200151028     O                       percentage    1     +2
033300151028     O                       cmw2rate      1     +2
033400151028     O****                   cmw2value     1     +2
033500151028      // summary of the incentive  to employee
033600060114     OQSYSPRT   e            emp_tot        2
033700060114      //
033800070316     O                                           80 'Employee total ->'
033900151028     O*                      emp_tar       1b    96
034000151028     O*                      emp_unit      1b   112
034100151028     O*                      emp_sub       1b   151
034200060114
034300060114      // summary of the commission to be given by type
034400060113     OQSYSPRT   e            sub_tot        2
034500060112      //
034600151028     O**                                         80 'SUB-TOTALS ->'
034700151028     O                       description    b    80
034800151028     O                       typ_tar       1b   100
034900151028     O                       typ_unit      1b   118
035000151028     O                       typ_sub       1b   160
035100151028      // summary of the incentive to be given
035200060112     OQSYSPRT   e            sum_tot        2
035300060112      //
035400151028     O                                          135 'TOTAL INCENTIVE PAID'
035500151028     O                       fin_sub       1b   160
035600060112      //
035700060112     OQSYSPRT   e            sum_tot        2
035800060424     O                                          100 '***'
035900040426
036000060105     P*--------------------------------------
036100060105     P* Procedure name: open_file
036200060105     P* Purpose:        open database file
036300060105     P* Returns:        *none
036400060105     P* Parameter:      *none
036500060105     P*--------------------------------------
036600060105     P open_file       B
036700060105     D open_file       PI
036800151016     D read_fno                       1  0 const
036900060105
037000151016        if  read_fno = 1;
037100151016           Exec Sql
037200151016             DECLARE PIDFILE CURSOR for SELECT * from pideptm
037300151028               ORDER BY pidpicode, piddept FOR READ ONLY;
037400151016
037500151016           Exec Sql
037600151016             OPEN PIDFILE;
037700151016
037800151016              open(e) picommcm;
037900151016              open(e) picommgm;
038000151016              open(e) picommrl01;
038100151016              open(e) picommwrk2;
038200151016              open(e) picommwrk3;
038300151016              open(e) picommitm;
038400151016              open(e) qsysprt;
038500151016
038600151028        //elseif read_fno = 2;
038700151028          else;
038800151028           select;
038900151028            when read_fno = 2; //Warehouse/Branch
039000151028             qryselect = 'select dtdate, dtrec, dtamt, dtpcod, ' +
039100151103           ' dtqty, dtqtys FROM WHDET WHERE dtsts in (' + Quote + 'A' + Quote +
039200160416                             ',' + Quote + 'P' + Quote + ')'  +
039300151027           ' and dtdate >= ' + %char(STRDATE) +
039400151027           ' and dtdate <= ' + %char(ENDDATE) +
039500151016           ' and dtorg = ' + Quote + 'WHS' + Quote +
039600151016           ' and dtrec in ' + WareHouseList +
039700151016           ' and dttcod in ' + TrnCodeList +
039800151016           ' order by dtdate For Read ONLY';
039900151028
040000151028            when read_fno = 3;   // Production Plant
040100151028             qryselect = 'select dtdate, dtrec, dtamt, dtpcod, ' +
040200160412           ' dtqty, dtqtys FROM WHDET WHERE dtsts = ' + Quote + 'P' + Quote +
040300160412        //                   ',' + Quote + 'A' + Quote + ')'  +
040400151028           ' and dtdate >= ' + %char(STRDATE) +
040500151028           ' and dtdate <= ' + %char(ENDDATE) +
040600151029           ' and dtorg = ' + Quote + 'WHS' + Quote +
040700151028           ' and dtrec in ' + WareHouseList +
040800151028           ' and dttcod in ' + TrnCodeList +
040900151028           ' order by dtdate For Read ONLY';
041000151028
041100151028            when read_fno = 4;   // Hotel
041200151029             qryselect = 'select htmpost, htmprcode, htmamt          ' +
041300160412           ' FROM HOTTRANSM WHERE htmsts = ' + Quote + 'A' + Quote +
041400151029           ' and htmpost >= ' + %char(STRDATE) +
041500151029           ' and htmpost <= ' + %char(ENDDATE) +
041600160413           ' and htmprcode in ' + WareHouseList +
041700151029           ' order by htmpost For Read ONLY';
041800151028
041900151028            when read_fno = 5;   // Ultimate catering
042000151029             qryselect = 'select copost, coloc, cooval    ' +
042100160412           ' FROM UCORDM WHERE costs in ' + '(' + Quote + 'P' + Quote +
042200160412             sep + Quote + ' ' + Quote +  ')' +
042300160412        // ' FROM UCORDM WHERE costs = ' + Quote + 'P' + Quote +
042400151029           ' and copost >= ' + %char(STRDATE) +
042500151029           ' and copost <= ' + %char(ENDDATE) +
042600160413           ' and coloc in ' + WareHouseList +
042700151029           ' order by copost For Read ONLY';
042800151028
042900151028            when read_fno = 6;   // Restaurants
043000160412             qryselect = 'select rsttdate, rstidcode, rstamt  ' +
043100160412           ' FROM RESTRANSM WHERE rststs = ' + Quote + 'A' + Quote +
043200160412           ' and rsttdate >= ' + %char(STRDATE) +
043300160412           ' and rsttdate <= ' + %char(ENDDATE) +
043400160413           ' and rstidcode in ' + WareHouseList +
043500160412           ' order by rsttdate For Read ONLY';
043600151028           endsl;
043700151016
043800160413       // DspLngTxt( qryselect + %char(piddept) : %Len( qryselect ) + 10
043900160413       //                : *Blanks: *Blanks: ERRC0100 );
044000160409
044100151016           Exec Sql
044200151027              declare PIfile cursor for DYNSTMT;
044300151016
044400151016           Exec Sql
044500151027              prepare DYNSTMT from :qryselect;
044600151016
044700151016           Exec Sql
044800151016              OPEN PIFILE;
044900151028
045000151016
045100151016        endif;
045200151016
045300060911
045400060105     P open_file       E
045500060105
045600060105     P*--------------------------------------
045700060105     P* Procedure name: process_rec
045800060105     P* Purpose:        get records for processing
045900060105     P* Returns:        *none
046000060105     P* Parameter:      *none
046100060105     P*--------------------------------------
046200060105     P process_rec     B
046300060105     D process_rec     PI
046400060109
046500151029     D quantity        s             13  2
046600151029     D totalQuantity   s             13  2
046700151028     D prv_picode      s              2  0
046800151028     D systemNo        s              2  0
046900060105
047000060105
047100060224             // read global info
047200060428              read  cmgfmt;
047300151016
047400151016             // read Department info
047500151016              dow not read_file(1);
047600060105
047700151028                // Retrieve PI code data
047800151028                if prv_picode <> pidpicode;
047900151028                   exec sql
048000151028                      select * into :picodemDS from picodem
048100151028                         where piccode = :pidpicode;
048200151028                   prv_picode = pidpicode;
048300151028                endif;
048400151028
048500151029                // Clear quantity/amount fields (Data Stucture)
048600151029                quantity = *zeros;
048700151029                clear dataDS;
048800151029
048900160428                // Build warehouse/Location List
049000151016                BuildWareHouseList();
049100151016
049200160428                // Build warehouse/Location Transaction Code List
049300151016                BuildTrnCodeList();
049400151016
049500151028                // Application Origin
049600151028                select;
049700160420                  when pidorg = 'WHS';     //Warehouse
049800151028                    systemNo = 2;
049900160420                  when pidorg = 'PRO';     //Production
050000151028                    systemNo = 3;
050100160420                  when pidorg = 'HOT';     //Hotel
050200151028                    systemNo = 4;
050300160420                  when pidorg = 'ULT';     //Ultimate Catering
050400151028                    systemNo = 5;
050500160420                  when pidorg = 'RES';     //Restaurants
050600151028                    systemNo = 6;
050700151028                  other;
050800151028                    systemNo = 2;
050900151028                endsl;
051000151028
051100160418                // DO Not read files if criteria is same as already
051200160418                // found during processing.. use  array/struc  data
051300160418                // to hold all info pertaining to found criteria
051400160418
051500160418              //if criteria_found(pidorg:WareHouseList:trnCodeList);
051600160418                if criteria_found(pidorg);
051700160418
051800160418                    accrue_data(pidpicode: piddept:srh_amount(currentIdx));
051900160418                  else;
052000160411
052100151016                // Read Sales Records to end of file
052200151028                open_file(systemNo);
052300151028                dow not read_file(systemNo);
052400151028
052500151028                 if systemNo <= 3; // to use origin (warehouse/product areas)
052600151028
052700151028                   if not get_prod_info('RD':dataDS.ProductCode:prod_struc)
052800151028                         and pdpfix = 'P';
052900151016
053000151028                      // Move to  field for change
053100160412                      quantity = - (dataDS.quantity
053200151028                                        + (dataDS.quantitySin/pdsqty)) ;
053300160412                      if systemNo = 3;  // Production
053400160412                        quantity = - quantity;
053500151029                      endif;
053600151016
053700151028                      // perform search for valid incentive criteria
053800151028                      comm_criteria(pidpicode:piddept:dataDS.ProductCode:
053900151020                                           quantity);
054000151016
054100151016
054200151028                     // work file to hold criteria found
054300151028                     // and accrue data on same
054400151028
054500151028                     // Calculating using Dollar Value
054600151028                     if not dollarIND;
054700151028                       dollarIND = picrevflg = YES;
054800151028                       if not dollarIND;
054900151028                         dollarIND = pidrevflg = YES;
055000151028                       endif;
055100151028                     endif;
055200151028
055300151028                     // using dollar value to calculate P.I.
055400151028                     if  dollarIND;
055500160413                       if quantity > *zeros;
055600151028                         accrue_data(pidpicode: piddept: dataDS.amount);
055700160418                         srh_amount(currentIdx) += dataDS.amount;
055800151028                         else;
055900151028                         accrue_data(pidpicode: piddept: -dataDS.amount);
056000160418                         srh_amount(currentIdx) += dataDS.amount;
056100151028                        endif;
056200151028                        else;
056300151028                       accrue_data(pidpicode: piddept: quantity);
056400160418                       srh_amount(currentIdx) += quantity;
056500151028                     endif;
056600060109
056700151028                   endif;
056800151028                   else; // non warehouse/production origins
056900151028                    accrue_data(pidpicode: piddept: dataDS.amount);
057000160418                    srh_amount(currentIdx) += dataDS.amount;
057100151028                 endif;
057200160418
057300160413                 quantity = *zeros;
057400160413                 clear dataDS;
057500151016                enddo;
057600151028
057700151028                close_file(systemNo);
057800160418                endif;
057900160416                accrue_empdta(pidpicode: piddept);
058000060427
058100060427              enddo;
058200060427
058300060111
058400060105
058500060105     P process_rec     E
058600060105
058700060109     P*--------------------------------------
058800060109     P* Procedure name: write_foundDS
058900060109     P* Purpose:        criteria data struct.
059000060109     P* Returns:        *none
059100060109     P* Parameter:      4
059200060109     P*--------------------------------------
059300060109     P write_foundDS   B
059400060109     D write_foundDS   PI
059500060109
059600151015     D dept#                          3  0 const
059700060109     D product                        6    const
059800151015     D*location                       3  0 const
059900060109
060000151015                critDS.dept# = dept#;
060100060112                critDS.product = product;
060200151015             // critDS.location = location;
060300060109
060400060109     P write_foundDS   E
060500060109
060600060109     ***************************************
060700151016     ** Accrue data on Incentive Criteria **
060800060109     ***************************************
060900060109     PAccrue_data      b                   export
061000060109     **
061100060109     DAccrue_data      pi
061200151015     D code                           2  0 const
061300151028     D dept#                          3  0 const
061400151015     D quantity                      13  2 const
061500060109
061600060109
061700160413
061800060109         // Get work file criteria
061900151028         chain (code:dept#:critDS.product) cmw3fmt;
062000060109
062100060109         if %found;
062200060109           cmw3qty += quantity;
062300060111           update cmw3fmt %fields(cmw3qty);
062400060111          else;
062500151015           cmw3code = code;
062600060109           cmw3qty = quantity;
062700151028           cmw3dept# = dept#;
062800060109           cmw3cpcod = critDS.product;
062900060109           write cmw3fmt;
063000060109         endif;
063100060109
063200060109     PAccrue_data      e
063300060109
063400060109     ************************************************
063500151020     ** Accrue employee data on Incentive Criteria **
063600060109     ************************************************
063700060109     PAccrue_empdta    b                   export
063800060109     **
063900060109     DAccrue_empdta    pi
064000151020     D type                           2  0 const
064100151020     D dept#                          3  0 const
064200160413     D*quantity                      13  2 const
064300060109
064400151020
064500151021     D xcnt            s              4  0
064600151020     D iRowsFetched    s              4  0
064700151020     D MaxRows         s              4  0 inz(3000)
064800151020
064900151020     D EmpDS           DS                  dim(3000) qualified
065000151020     D   EmpNumber                    6  0
065100151020     D   EmpDept                      3  0
065200151020     D   EmpClass                     3
065300151020     D   EmpStatus                    1
065400151020
065500060109
065600160413         // Get work file criteria
065700160413         setll (type:dept#) cmw3fmt;
065800160413         reade (type:dept#) cmw3fmt;
065900151020
066000160413         dow not %eof;
066100160413
066200151020             Exec Sql
066300151020                Declare EmpCsr Cursor For
066400151020                  select emp, dept, class, status  from EMPMAST
066500151020                     where dept = :dept#;
066600151020           //            and get_cat(status) = 'A';
066700151020
066800151020
066900151020                // open Employee table
067000151020               Exec Sql
067100151020                OPEN EmpCsr;
067200151020
067300151020                exec sql
067400151020                   FETCH EmpCsr
067500151020                      FOR :Maxrows ROWS
067600151020                      INTO :EmpDs;
067700151020
067800151020                // retrieve Number of Rows Returned
067900151020                if  sqlstt <> '02000';
068000151020
068100151020                   EXEC SQL
068200151020                     GET DIAGNOSTICS
068300151020                     :iRowsFetched = ROW_COUNT;
068400151020
068500151020                 endif;
068600151020
068700151020
068800151020                // close Employee table
068900151020               Exec Sql
069000151020                CLOSE EmpCsr;
069100151020
069200151020             for xcnt = 1 to iRowsFetched;
069300151020                if get_cat(EmpDs(xcnt).Empstatus) = 'A';
069400151020                  // Get work file criteria
069500151029                  chain (type:dept#:EmpDs(xcnt).Empnumber
069600151029                        :critDS.product)  cmw2fmt;
069700151020                  if %found;
069800160413                    cmw2qty += cmw3qty;
069900151020                    update cmw2fmt %fields(cmw2qty);
070000151020                   else;
070100151021                    cmw2code = type;
070200160413                    cmw2qty = cmw3qty;
070300160413                 // cmw2qty = quantity;
070400151020                    cmw2emp = EmpDS(xcnt).Empnumber;
070500151028                    cmw2dept# = dept#;
070600160413                    cmw2cpcod = cmw3cpcod;
070700160413                 // cmw2cpcod = critDS.product;
070800151020                    write cmw2fmt;
070900151020                  endif;
071000151020                endif;
071100151020             endfor;
071200160413
071300160413         reade (type:dept#) cmw3fmt;
071400160413         enddo;
071500060109
071600060109
071700060109     PAccrue_empdta    e
071800060110
071900060110     ***************************************
072000151016     ** apply rates for incentive payments
072100060110     ***************************************
072200060110     Papply_rates      b
072300060110     **
072400060110     Dapply_rates      pi
072500060113
072600060113     Dprevrate         s                   like(cmrrate#1)
072700060114     Dprevlwqty        s                   like(cmrrate#1)
072800060114     Dprevupqty        s                   like(cmrrate#1)
072900151020     D hclass          s              3
073000151020     D erate1          s              9  5
073100151020     D erate2          s              9  5
073200151020     D erate3          s              9  5
073300060111
073400060111     DSUPRTYPE         c                   2
073500060110
073600151016
073700060110
073800151016         // Get work file info for processing  (DEPT)
073900151016         //-------------------------------------------
074000060224
074100060110         setll *loval cmw3fmt;
074200060110         read  cmw3fmt;
074300060110         dow not %eof();
074400060110
074500060110        // search for correct range
074600151015            if  not  search_for_rng(cmw3code:cmw3dept#:cmw3cpcod:
074700151015                                    *on);
074800060323              dow   cmrqty <= cmw3qty;
074900060110
075000060224                cmw3rate = rates(1);
075100060510                cmw3rate2 = rates(2);
075200060510                cmw3rate3 = rates(3);
075300060110                cmw3value = cmw3rate * cmw3qty;
075400060510                cmw3value2 = cmw3rate2 * cmw3qty;
075500060510                cmw3value3 = cmw3rate3 * cmw3qty;
075600060323
075700151015                if  search_for_rng(cmw3code:cmw3dept#:cmw3cpcod:
075800151015                                   *off);
075900060323                  leave;
076000060323                endif;
076100060323
076200060323              enddo;
076300060323            endif;
076400060110
076500060510            update cmw3fmt %fields(cmw3rate:cmw3rate2:cmw3rate3:
076600060510                                   cmw3value:cmw3value2:cmw3value3);
076700060113         read  cmw3fmt;
076800060110
076900060110         enddo;
077000060110
077100060110
077200151016         // Get employee info for processing  (EMPLOYEE)
077300060111         //---------------------------------------------
077400060110         setll *loval cmw2fmt;
077500060110         read  cmw2fmt;
077600060110         dow not %eof();
077700060110
077800060224              // get employee info.
077900060224                get_emp_info2('R':cmw2emp:emp_struc);
078000151021          chain (tprno:cmw2dept#:cmw2cpcod)  cmcfmt;
078100060224
078200151020        //if  cmccalctyp = 'Y'
078300151020        //  or  cmgcalctyp = 'Y';
078400151015            chain (cmw2code:cmw2dept#:cmw2cpcod)
078500060215                       cmw3fmt;
078600060118
078700151015              // calculate achieved incentive
078800060118              if  %found;
078900060510                select;
079000060510                  when  %scan(empcat:cmgrlist#1) > *zeros;
079100060510                    cmw2rate = cmw3rate;
079200060510                  when  %scan(empcat:cmgrlist#2) > *zeros;
079300060510                    cmw2rate = cmw3rate2;
079400060510                  when  %scan(empcat:cmgrlist#3) > *zeros;
079500060510                    cmw2rate = cmw3rate3;
079600060510                  other;
079700060510                    cmw2rate = *zeros;
079800060510                endsl;
079900060118                else;
080000060118                cmw2rate = *zeros;
080100060118              endif;
080200151020
080300151020              hclass = %xlate('P':' ':class);
080400151020              exec sql
080500151020                  select esmbrate1, esmbrate2, esmbrate3 into
080600151020                         :erate1, :erate2, :erate3 from  empsmf
080700151020                           where  esmclass = :hclass;
080800151020
080900151020              select;
081000151021                when cmgmingflg = '0';
081100151020                  cmw2value = round_up(cmw2rate * cmw2qty:'A');
081200151021                when cmgmingflg = '1';
081300151020                  cmw2value = round_up(cmw2rate * erate1 * cmw2qty:'A');
081400151021                when cmgmingflg = '2';
081500151020                  cmw2value = round_up(cmw2rate * erate2 * cmw2qty:'A');
081600151021                when cmgmingflg = '3';
081700151020                  cmw2value = round_up(cmw2rate * erate3 * cmw2qty:'A');
081800151020              endsl;
081900151020            //cmw2value = round_up(cmw2rate * cmw2qty:'A');
082000060118
082100151020
082200151020            update cmw2fmt %fields(cmw2rate:cmw2value);
082300151020         read  cmw2fmt;
082400151020
082500060110         enddo;
082600060110
082700060111
082800060111
082900060110
083000060110     Papply_rates      e
083100060109
083200060110
083300060110     P*----------------------------------------------
083400060110     P* Procedure name: search_for_rng
083500060110     P* Purpose:  Search for correct commission rate
083600060110     P*           for range found
083700060110     P* Returns:        *boolean
083800060110     P* Parameter:      5
083900060110     P*----------------------------------------------
084000060110     P Search_for_rng  B
084100060110     D Search_for_rng  PI              n
084200151015     D type                           2  0 const
084300151015     D dept#                          3  0 const
084400060110     D product                        6    const
084500060323     D startInd                        n   const
084600060110
084700060110         // reset data file
084800060110         if startInd;
084900151015           setll (tprno:type:dept#:product)
085000060215                   cmrfmt;
085100060110         endif;
085200060110
085300151016         // Find incentive rate
085400151015         reade (tprno:type:dept#:product)
085500060215                  cmrfmt;
085600060113
085700151015         if  %eof(picommrl01);
085800060110             return *on;
085900060110            else;
086000060110             return *off;
086100060110         endif;
086200060110
086300060110     P Search_for_rng  E
086400060110
086500060111
086600060111     P*---------------------------------------
086700060111     P* Procedure name: perform_output
086800060111     P* Purpose:        Perform Printer output
086900060111     P* Returns:        *none
087000060111     P* Parameter:      *none
087100060111     P*---------------------------------------
087200060111     P perform_output  B
087300060111     D perform_output  PI
087400060114
087500060114     D hldemp          s                   like(cmw2emp)
087600151021     D hldcode         s                   like(cmw2code)
087700151028     D hlddept#        s                   like(cmw2dept#)
087800060114     D name#           s             40
087900060114     D err#            s              7
088000060114
088100060114     D INVALIDTEXT     c                   '******* INVALID EMPLOYEE ******'
088200060111
088300130613
088400060112         except hdr;
088500060112         except  hdr2;
088600060112         except  hdrd;
088700060112
088800060112         setll *loval  cmw2fmt;
088900060112         read  cmw2fmt;
089000060114         hldemp = cmw2emp;
089100151028         hldcode = cmw2code;
089200060323
089300060112         dow not %eof();
089400060112
089500151021           chain (tprno:cmw2dept#:cmw2cpcod) cmcfmt;
089600060424
089700060424              if %found;
089800060425               monitor;
089900151021            // if  cmccalctyp = 'Y'
090000151021            //    or  cmgcalctyp = 'Y';
090100151021                  chain (cmw2code:cmw2dept#:cmw2cpcod)
090200060424                         cmw3fmt;
090300060424                  if %found;
090400060424                   percentage = cmw3qty / cmctarget * 100;
090500060424                   else;
090600060424                   percentage = cmw2qty / cmctarget * 100;
090700060424                  endif;
090800151021            //  else;
090900060424                   percentage = cmw2qty / cmctarget * 100;
091000151021            // endif;
091100060425               on-error;
091200060425                  percentage = *zeros;
091300060425               endmon;
091400060424                else;
091500060424                  percentage = *zeros;
091600060424                  cmctarget = *zeros;
091700060424              endif;
091800060114
091900060114              if cmw2emp <> hldemp;
092000151028               // except emp_tot;
092100060114                  hldemp = cmw2emp;
092200060114              endif;
092300151028
092400060114
092500151021              if cmw2code <> hldcode;
092600151028                exec sql
092700151028                   select picdesc into :description from picodem
092800151028                      where piccode = :hldcode;
092900060114                  except sub_tot;
093000151021                  hldcode = cmw2code;
093100060114              endif;
093200060112
093300060303
093400151028           // description = %trim(description) + ' - ' + %char(cmw2dept#);
093500060323              idesc =  *blanks;
093600060823
093700060823              EXEC SQL
093800151028                 SELECT cmidesc INTO :idesc FROM picommitm WHERE cmipcod =
093900060823                 :cmw2cpcod;
094000060823
094100060323
094200060323              if  idesc <> *blanks;
094300060323              description = %trim(description) + ' - ' + idesc;
094400060323              endif;
094500060303
094600070316              // employee totals
094700151028           // emp_tar += cmctarget;
094800151028           // emp_unit += cmw2qty;
094900151028           // emp_sub += cmw2value;
095000151028
095100151028              // department totals
095200151028           // dept_tar = cmctarget;
095300151028           // dept_unit = cmw2qty;
095400151028           // dept_sub += cmw2value;
095500070316
095600070316              // sub-type totals
095700151028              typ_tar = cmctarget;
095800151028              typ_unit = cmw2qty;
095900151028              typ_sub += cmw2value;
096000151028
096100070316              // final totals
096200151028              fin_sub += cmw2value;
096300060112
096400060314             // Handling page break
096500060112              if  *inof;
096600060112                 except  hdr;
096700060112                 except  hdrd;
096800060112              endif;
096900151028
097000151028             // Print department Header
097100151028              if cmw2dept# <> hlddept#;
097200151028                 description = get_dept_info(cmw2dept#);
097300151028                 except dept_tot;
097400151028                 hlddept# = cmw2dept#;
097500151028
097600151028              endif;
097700060112
097800060114             //Validate Employee number & get employee info.
097900060114               if  get_cat(get_emp_info(cmw2emp:*omit:*blanks:name#:
098000060114                                       *omit:*omit:*omit:err#)) = 'A';
098100060114                  name = name#;
098200060114                 else;
098300060114                  name = INVALIDTEXT;
098400060114              endif;
098500060114
098600060112              except DET;
098700060112         read  cmw2fmt;
098800060112         enddo;
098900060112
099000151028         exec sql
099100151028            select picdesc into :description from picodem
099200151028                 where piccode = :cmw2code;
099300151028       //except  emp_tot;
099400060114         except  sub_tot;
099500060114         except  sum_tot;
099600060111
099700151015
099800060111     P perform_output  E
099900060110
100000060111
100100060109     *****************************
100200151015     ** Find incentive Criteria **
100300060109     *****************************
100400060109     Pcomm_criteria    b                   export
100500060109     **
100600060112     Dcomm_criteria    pi
100700151015     D type                           2  0 const
100800151015     D dept#                          3  0 const
100900060109     D product                        6    const
101000151015     D quantity                      13  2
101100060111
101200060330     D counter         s              1  0
101300060410     D count0          s              1  0
101400060410     D count1          s              1  0
101500151015     D deptno          s                   like(dept#)
101600060313     D prod            s                   like(product)
101700060215     D srchcode        s              6
101800060330     D prodIND         s               n
101900060330     D groupIND        s               n
102000060330     D typeIND         s               n
102100060410     D codeFLD         s              6
102200060215
102300060109
102400151028          deptno = dept#;
102500060410
102600151028        // Check Dept criteria
102700151028        //for count0 = 1 to 2;
102800060109
102900060109         // Get work file criteria
103000060330          for count1 = 1 to 3;
103100060314
103200060215                chain (0:product) cmifmt;
103300060330                 if  %found and not ProdIND;
103400060410                    codeFLD = cmipcod;
103500060215                  else;
103600060215                    srchcode = %char(pdgrp);
103700060215                   chain (1:srchcode) cmifmt;
103800060330                   if  %found and not GroupIND;
103900060410                     codeFLD = cmipcod;
104000060425                     count1 = 2;
104100060215                    else;
104200060328                     srchcode = %char(pdtyp);
104300060215                     chain (2:srchcode) cmifmt;
104400060330                     if  %found and not TypeIND;
104500060410                       codeFLD = cmipcod;
104600060425                       count1 = 3;
104700060215                      else;
104800060410                       codeFLD = *blanks;
104900060215                     endif;
105000060215                   endif;
105100060215                 endif;
105200060215
105300060215
105400151028          //deptno = dept#;
105500060410            prod = codeFLD;
105600060109
105700060109
105800151028            for counter = 1 to 4;
105900151028
106000151028              select;
106100151028               when counter = 2;
106200151028                  deptno = *zeros;
106300060425               when counter = 3;
106400060425                  if  count1 = 3;
106500151015                    deptno = dept#;
106600060425                    prod = *blanks;
106700060425                  else;
106800060425                    leave;
106900060425                  endif;
107000060425               when counter = 4;
107100060425                  if  count1 = 3;
107200151015                    deptno = *zeros;
107300060425                  else;
107400060425                    leave;
107500060425                  endif;
107600060109              endsl;
107700060109
107800060410
107900151015              chain (tprno:type:deptno:prod) cmrfmt;
108000060109              if  %found;
108100060109
108200060403                 // Calculating using standard volume
108300060403                if  cmiconvqty > *zeros
108400060403                  and  pdsize > *zeros;
108500060403                   // Get Product size
108600060403                 if  not get_size_info('RD':pdsize:size_struc);
108700060403                    // Get UOM conversion for calcs.
108800060403                  if not  uomval(out_qty:cmiconvuom:siuom:(siqty * pdsqty));
108900060512
109000060403                       quantity *= (out_qty / cmiconvqty);
109100060403                    elseif not  uomval(out_qty:cmiconvuom:sialtuom:
109200060403                                    (sialtqty * pdsqty));
109300060403                       quantity *= (out_qty  / cmiconvqty);
109400060403                  endif;
109500060403                 endif;
109600060403                endif;
109700060403
109800060413                 // Calculating using Dollar Value
109900160316                 //dollarIND = cmidollarf = YES;
110000060403
110100151015                write_foundDS(cmrdept#:cmrpcod);
110200060112                return;
110300060109
110400060109              endif;
110500060109
110600060109            endfor;
110700060330
110800060330            select;
110900060330               when count1 = 1;
111000060330                  prodIND = *on;
111100060330               when count1 = 2;
111200060330                  groupIND = *on;
111300060330               when count1 = 3;
111400060410                  typeIND = *on;
111500060330
111600060330            endsl;
111700060330
111800060425          endfor;
111900060410
112000151028        //deptno =  *zeros;
112100060410          prodIND = *off;
112200060410          groupIND = *off;
112300060410          typeIND = *off;
112400151028      //endfor;
112500060410
112600151015            write_foundDS(*zeros:*blanks);
112700060109
112800060112        return;
112900060109
113000060109
113100060109     Pcomm_criteria    e
113200060109
113300060105     P*--------------------------------------
113400060105     P* Procedure name: read_file
113500060105     P* Purpose:        read transaction file
113600060105     P* Returns:        error indicator
113700060105     P* Parameter:      errmsg => error message text
113800060105     P*--------------------------------------
113900060105     P read_file       B
114000060105     D read_file       PI              n
114100060427     D read_fno                       1  0 const
114200060105
114300060105      *   Read the records ...
114400151016
114500060427        if  read_fno = 1;
114600060911
114700060911         EXEC SQL
114800151016          FETCH NEXT FROM PIDFILE INTO :pideptmDS;
114900060911
115000160413          elseif read_fno >= 2;
115100060911
115200060911         EXEC SQL
115300151028          FETCH NEXT FROM PIFILE INTO :dataDS;
115400151028
115500060911
115600060427        endif;
115700160413
115800060427
115900060105          // For every record in the file ....
116000060105          // ... while a record was successfully read ...
116100060105            if SQLSTT = '02000';
116200060105               return *on;
116300060105              else;
116400060105               return *off;
116500060105            endif;
116600151011
116700060105     P read_file       E
116800060105
116900060105     P*--------------------------------------
117000060105     P* Procedure name: close_file
117100060105     P* Purpose:        close transaction file
117200060105     P* Returns:        *none
117300060105     P* Parameter:      *none
117400060105     P*--------------------------------------
117500060105     P close_file      B
117600060105     D close_file      PI
117700151016     D read_fno                       1  0 const
117800060105
117900060911
118000151016        if  read_fno = 1;
118100151016           Exec Sql
118200151016             CLOSE PIDFILE;
118300151016
118400151016              close(e) picommcm;
118500151016              close(e) picommgm;
118600151016              close(e) picommrl01;
118700151016              close(e) picommwrk2;
118800151016              close(e) picommwrk3;
118900151016              close(e) picommitm;
119000151016              close(e) qsysprt;
119100151016
119200151028          elseif read_fno >= 2;
119300151016           Exec Sql
119400151016             CLOSE PIFILE;
119500151016        endif;
119600060427
119700060911
119800060911
119900060105     P close_file      E
120000060113
120100060113     P*--------------------------------------
120200060113     P* Procedure name: clear_workfile
120300060215     P* Purpose:        clear work files
120400060113     P* Returns:        *none
120500060113     P* Parameter:      *none
120600060113     P*--------------------------------------
120700060113     P clear_workfile  B
120800060113     D clear_workfile  PI
120900060113
121000151015
121100060410
121200151015             //... clear file 2
121300060113              cmd = clear_cmd;
121400151011              cmd = %replace('PICOMMWRK2':cmd:
121500060113                      %scan('&file':cmd):5);
121600060113                callp(e) line_command(cmd:%size(cmd));
121700151015
121800151015             //... clear file 3
121900060113              cmd = clear_cmd;
122000151011              cmd = %replace('PICOMMWRK3':cmd:
122100060113                      %scan('&file':cmd):5);
122200060113                callp(e) line_command(cmd:%size(cmd));
122300060113
122400060113
122500060113     P clear_workfile  E
122600060224
122700060224
122800060224     P*---------------------------------------
122900060224     P* Procedure name: payroll_output
123000060224     P* Purpose:        Prepare payroll output
123100060224     P* Returns:        *none
123200060224     P* Parameter:      *none
123300060224     P*---------------------------------------
123400060224     P payroll_output  B
123500060224     D payroll_output  PI
123600060224
123700060510     D emp_amount      s             11  2
123800060510     D save_emp        s              6  0
123900060224
124000060224
124100060510        setll *loval  cmw2fmt;
124200060510        read  cmw2fmt;
124300060224
124400060510        if not  %eof();
124500060510         save_emp = cmw2emp;
124600060510
124700060510         dow  0 = 0;
124800060224
124900060510           if save_emp <> cmw2emp
125000060510             or %eof();
125100060510
125200060510
125300060224             //Validate Employee number & get employee info.
125400060510              status = get_emp_info(save_emp:dept);
125500060510              if get_cat(status) = 'A';
125600060509                if cmgdedpayt <> *zeros;
125700060823
125800060823                   Exec Sql
125900060823                     SELECT ALTAMT INTO :pay_amt FROM payalwtl02 WHERE ALTTYP =
126000060823                      :cmgdedpayt and ALTEMP = :save_emp;
126100060823
126200060823                   pay_amt = pay_amt / YEAR_CONST * no_prd;
126300060509
126400151028                  // adjust incentive value by paytype amount reduction
126500060510                   if  (emp_amount - pay_amt) >= *zeros;
126600060510                      emp_amount -= round_up(pay_amt:'A');
126700060510                     else;
126800060510                      emp_amount = *zeros;
126900060509                   endif;
127000060509                endif;
127100060823
127200060823               Exec Sql
127300151015                 INSERT INTO PICOMMPAY VALUES(:save_emp, ' ', :emp_amount);
127400060823
127500060224              endif;
127600060510              save_emp = cmw2emp;
127700060510              emp_amount = *zeros;
127800060510           endif;
127900060510
128000060510           if %eof();
128100060510              leave;
128200060510           endif;
128300060510
128400060510           emp_amount += cmw2value;
128500060224
128600060224         read  cmw2fmt;
128700060224         enddo;
128800060510
128900060510        endif;
129000060224
129100060224     P payroll_output  E
129200060403
129300060403     P*--------------------------------------------
129400060403     P* Procedure name: Uomval
129500060403     P* Purpose:  validate issuing Unit o' Mrasure
129600060403     P* Returns:        *boolean
129700060403     P* Parameter:      errmsg
129800060403     P*--------------------------------------------
129900060403     P Uomval          B
130000060403     D Uomval          PI              n
130100060403     D out_qty                       14  4
130200060403     D conuom                         2    const
130300060403     D frmuom                         2    const
130400151021     D frmqty                        13  2 const
130500060403     D
130600060403
130700060403           out_qty = perform_uconv(frmuom:conuom:frmqty);
130800060512
130900060403
131000060403           //if combi not found seton alarm
131100060403             if  out_qty = 0;
131200060403                return *on;
131300060403               else;
131400060403                return  *off;
131500060403             endif;
131600060403
131700060403     P Uomval          E
131800151016
131900151016
132000151016     P BuildWareHouseList...
132100151016     P                 b
132200151016
132300151016
132400151027       Dcl-S varCount      Packed(3)  inz(0);
132500151027
132600151016     D Ndx             s              3u 0
132700151016     D Quote           c                   const('''')
132800151016     D Sep             s              1    varying
132900151016     D whid            s              3  0
133000160418     D locid           s              3  0
133100151016
133200151016
133300160418         srh_loc = *blanks;
133400151016
133500151016         WareHouseList = '(';
133600151016
133700151016         if piddflg = 'Y';
133800151016             exec sql
133900151016               select  dwhid into :whid from deptm
134000151016                   where dept = :piddept;
134100151016
134200151016             WareHouseList += (Sep + Quote)
134300151016               + %char(whid) + Quote;
134400151016
134500151016           else;
134600151027             if  pidwhlist <> *blanks;
134700151027               parse_commaSeparatedList(pidwhlist:%addr(arrValues):varCount);
134800151027                else;
134900151027               parse_commaSeparatedList(picwhlst:%addr(arrValues):varCount);
135000151027             endif;
135100151028
135200151027             For Ndx = 1 to varCount;
135300151016
135400151027           // if  %subst(pidWHList:Offset+3:1) = *blanks
135500151027           //  or  pidWHList = *blanks;
135600151027           //    leave;
135700151027           // endif;
135800151016
135900151016              WareHouseList += (Sep + Quote)
136000151028               + arrValues(Ndx) + Quote;
136100151016              Sep = ',';
136200160418
136300160418              // put locations in fixed length array
136400160418              locid = %int(arrValues(Ndx));
136500160418              srh_loc = %trim(srh_loc) + %editc(locid:'X');
136600151016             endfor;
136700151016
136800151016         endif;
136900151016
137000151016         WareHouseList +=  ')';
137100151016
137200151016     P BuildWareHouseList...
137300151016     P                 e
137400151016
137500151016
137600151016     P BuildTrnCodeList...
137700151016     P                 b
137800151016
137900151016
138000151027       Dcl-S varCount      Packed(3)  inz(0);
138100151027
138200151028     D*ListSize        s              3u 0 inz(15)
138300151028     D*Offset          s              3u 0 inz(1)
138400151016     D Ndx             s              3u 0
138500151016     D Quote           c                   const('''')
138600151016     D Sep             s              1    varying
138700151016
138800151016
138900160418         srh_trns = *blanks;
139000160418
139100151016         if pidwhcdlst <> *blanks;
139200151028           parse_commaSeparatedList(pidwhcdlst:%addr(arrValues):varCount);
139300151016            else;
139400151028           parse_commaSeparatedList(picwhcdlst:%addr(arrValues):varCount);
139500151016         endif;
139600151016
139700151016         TrnCodeList = '(';
139800151016
139900151028         For Ndx = 1 to varCount;
140000151016
140100151016
140200151016          TrnCodeList += (Sep + Quote)
140300151028               + arrValues(Ndx) + Quote;
140400151016          Sep = ',';
140500151016          Offset += 2;
140600160418          srh_trns = %trim(srh_trns) + arrValues(Ndx);
140700151016
140800151016         endfor;
140900151016
141000151016
141100151016         TrnCodeList +=  ')';
141200151016
141300151016     P BuildTrnCodeList...
141400151016     P                 e
141500160412
141600160412     P*--------------------------------------
141700160412     P* Procedure name: accum_gl_tot
141800160412     P* Purpose:
141900160412     P*    Search for gl.code in array.
142000160412     P*    if found set occurance level and add
142100160412     P*    total ,if not found add new array record
142200160412     P*    and Data Struct. Total
142300160412     P* Returns:        -
142400160412     P* Parameter:      -
142500160412     P*--------------------------------------
142600160412     P criteria_found  B
142700160416     D criteria_found  PI              n
142800160416     D Origin                         3    const
142900160418     D*locations                     75    const
143000160418     D*Trans_codes                   30    const
143100160418
143200160418         Dcl-S locptr        Pointer    inz(%addr(srh_loc));
143300160418         Dcl-S trnptr        Pointer    inz(%addr(srh_trns));
143400160418         Dcl-S srh_locArr    Char(3) dim(25) based(locptr);
143500160418         Dcl-S srh_trnArr    Char(1) dim(30) based(trnPtr);
143600160412
143700160418             sorta srh_locArr;
143800160418             sorta srh_trnArr;
143900160418
144000160416             srh_org = origin;
144100160418           //srh_loc = locations;
144200160418           //srh_trns = Trans_codes;
144300160418             currentIdx = 0;
144400160418             currentIdx = %lookup(srh_xxx:srharr);
144500160418             if currentIdx = 0;
144600160416                gcnt += 1;
144700160416                srharr(gcnt) = srh_xxx;
144800160416
144900160418              //srh_data(gcnt).srh_amount = amount;
145000160418                currentIdx = gcnt;
145100160418                return *off;
145200160418               else;
145300160416
145400160418                return *on;
145500160416             endif;
145600160412
145700160412
145800160412     P criteria_found  E
145900160412
