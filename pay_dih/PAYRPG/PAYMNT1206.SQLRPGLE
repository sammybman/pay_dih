000100151026f    H dftactgrp(*no) actgrp(*new) bnddir('DISDIR')
000200120125     h/define PROFOUNDUI
000300030911      //*********************************************************
000400151009      //              D I S E R V   S Y S T E M S.              *
000500151009      //                  PAYROLL SYSTEM                        *
000600030911      //                                                        *
000700030911      //                         BY                             *
000800030911      //                                                        *
000900030911      //             Diserv systems Team                        *
001000030911      //             P.O BOX XXXXX, GEORGETOWN,GUYANA           *
001100151009      //                                                        *
001200030911      //                                                        *
001300030911      //                                                        *
001400151009      //       RPG PROGRAM NAME..: PAYMNT1206                   *
001500030911      //                                                        *
001600051229      //       PROGRAMMER NAME: Samuel Britton                  *
001700151009      //       DESIGN DATE....: 2015/10/09                      *
001800030911      //       MODULES........: *NONE                           *
001900030911      //       INDICATORS.....:                                 *
002000030911      //                                                        *
002100030911      //                                                        *
002200151206      //  THIS PROGRAM IS DESIGNED to store Incentive rates as  *
002300060124      //                           are defined.                 *
002400030911      //                                                        *
002500030911      //*********************************************************
002600151011     Fpicommrl01uf a E           K DISK    usropn
002700151011     Fpicommdl01if   E           K DISK    usropn
002800160308     F*icommil01if   E           K DISK    usropn
002900160308     Fpicommitm if   E           K DISK    usropn
003000151011     Fpicommcm  uf a E           K DISK    usropn
003100151009     Fpaymnt12d6CF   E             WORKSTN
003200030911     F                                     SFILE(sub1:RRN)
003300120125     F/IF DEFINED(WEB)
003400120125     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003500120125     F/ENDIF
003600060124
003700120125      /copy QCPYSRC,genpr
003800120125      /copy QCPYSRC,salpr
003900151015      /copy QCPYSRC,paypr
004000151109      /copy QCPYSRC,autpr
004100060220
004200151013
004300151013     D dept_enq        pr                  extpgm('HMRENQ02R')
004400151013     D dept#                          3  0
004500151013
004600060220     D commitem_enq    pr                  extpgm('WVENQ16R')
004700060220     D item                           6
004800060323
004900060323     D confrate_enq    pr                  extpgm('WVENQ18R')
005000151015     D  type#                         2  0
005100060323     D  period                        6  0
005200151015     D  dept#                         3  0
005300060323     d  prod#                         6
005400151109
005500151109     D Dup_Incentive   pr                  extpgm('PAYMNT1207')
005600151109     D  period                        6  0
005700060220
005800151009     Dmain_prog        PR                  extpgm('PAYMNT1206')
005900120125
006000060217     Dmain_prog        PI
006100060124
006200160408       Dcl-Ds piDeptmDs   ExtName('PIDEPTM') inz End-Ds;
006300160408       Dcl-Ds piCodemDs   ExtName('PICODEM') inz End-Ds;
006400160408       Dcl-Ds whTargDS    ExtName('WHTARG') inz End-Ds;
006500151013
006600160517       DCL-S associtm Char(6) ;
006700160517
006800060124     D ARR             S             40    DIM(1) CTDATA PERRCD(1)
006900060126     D date1           S              8  0
007000060126     D time1           S              6  0
007100030923     D type            s              2
007200060402     D acntr           s              5  0
007300030911     D rrn             s              4  0
007400030911     D rrx             s              4  0
007500030911     D rr              s              4  0
007600060131     D cmrqtyu@_prv    s                   like(cmrqtyu)
007700060221     D dftarr          s              5  2 dim(50)
007800060221     D xcnt            s              2  0
007900060221     D ycnt            s              2  0
008000151013     D month           s              2  0
008100151013     D year            s              4  0
008200151106     D fyear           s              4  0
008300151014      // Controlling Sub-Location
008400151014     D ctl_location    s              8
008500151014     D sel_location    s              8
008600151014     D dft_loc#        s              8
008700151015     D error           s               n
008800151104     D avgflag         s              1
008900151014
009000120125     D aptr            s               *   inz(*null)
009100151013     D tgsptr          s               *   inz(%addr(tgstrg01))
009200151013     D tgpptr          s               *   inz(%addr(tgptrg01))
009300151013      // Array to hold target limits for each month
009400151013     D targSARR        s             12  0 dim(12) based(tgsptr)
009500151013     D targPARR        s             12  0 dim(12) based(tgpptr)
009600151013
009700151009
009800151009     d VarLen          s             10I 0 Inz(%size(Var))
009900151009     d ApiErr          s             15a
010000151009     d Caller          s             50a
010100151009     d WhoCalled       s             10a
010200151009     d i               s             10I 0
010300151009
010400151013
010500151013       //  Data structure Locations
010600151013       // --------------------------
010700151027       //l-Ds whidDS        dim(15) qualified based(Locptr);
010800151027       //Loc      char(3);
010900151027       //comma    char(1);
011000151027       //d-Ds;
011100151013
011200060221     Ddft_struc        ds                  qualified dim(50)
011300060221     D per_lower                      5  2
011400060221     D per_upper                      5  2
011500060221     D per_rate1                     11  6
011600060221     D per_rate2                     11  6
011700141017     D per_rate3                     11  6
011800151013     D per_valuel                    13  2
011900151013     D per_valueu                    13  2
012000060322     D per_delopt                     1
012100060131
012200030911     D                sds
012300030911     D  @pgm             *proc
012400120125     D  @user                358    367
012500030911
012600060124      //-- Validate routine                                                  --
012700060221     Dvalidate_rtn     pr              n
012800030911
012900060220      //-- Load Defaults routine                                             --
013000060220     Dload_default     pr
013100151015     D dfttype                        2  0 const
013200151013     D dftdept#                       3  0 const
013300060221     D dftpcod                        6    const
013400060220
013500151206     DMsg1             c                   'Incentive Item code Invalid !'
013600060216     DMsg2             c                   'Invalid Route code'
013700151206     DMsg3             c                   'Incentive Type Invalid !'
013800060402     DMsg4             c                   'Must Specify a location'
013900151015     DMsg5             c                   'Invalid Department entered'
014000060419     DMsg6             c                   'Cannot Change data for Period alrea-
014100060419     D                                     dy Processed'
014200060309     DMsg7             c                   'Process terminated'
014300060325
014400060325     DTEXT1            c                   'MANAGER'
014500030911
014600141008
014700141008       // --------------------------------------------------
014800141008       // Prototype for procedure: range_update
014900141008       // --------------------------------------------------
015000141008       DCL-PR range_update IND;
015100141008       END-PR ;
015200141008
015300141008
015400141008
015500141008       // --------------------------------------------------
015600141008       // Prototype for procedure: roll_back_op
015700141008       // --------------------------------------------------
015800141008       DCL-PR roll_back_op ;
015900141008       END-PR ;
016000141008
016100141008
016200141008       // --------------------------------------------------
016300141008       // Prototype for procedure: AAINIT
016400141008       // --------------------------------------------------
016500141008       DCL-PR AAINIT ;
016600141008       END-PR ;
016700141008
016800141008
016900150128       DCL-S recCount PACKED(4:0) INZ(0);
017000151009
017100151009             FindCaller(Var:VarLen:'CSTK0100':JobIdInf
017200151009                         :'JIDF0100':ApiErr);
017300151009               For i = 1 to EntryCount;
017400151009                  Entry = %subst(Var:Offset + 1);
017500151009                  Caller = %trim(PgmLib) + '/' + %trim(PgmNam);
017600151009                  If (PgmNam <> @pgm and WhoCalled = *blanks);
017700151009                     WhoCalled = PgmNam;
017800151009                  Endif;
017900151009                  Offset = Offset + EntryLen;
018000151009               Endfor;
018100151009
018200151009            // Program CANNOT be called independently
018300151009            if WhoCalled <> 'PAYMNT12R';
018400151009              *inlr = *on;
018500151009              return;
018600151009           endif;
018700151009
018800151009
018900030911
019000141008        aainit();
019100060124
019200060123       dow 0 = 0;
019300060123
019400060123         exfmt fmt0;
019500060123
019600060123         errmsg = *blanks;
019700060123
019800060123         // end of program
019900060123         if *in03;
020000060216            rolbk;
020100060216            *inlr = *ON;
020200060216            return;
020300060123         endif;
020400060217
020500060217         // Duplicate Periods
020600060220           if *in10;
020700151109              dup_incentive(cmrprno);
020800060220              iter;
020900060220           endif;
021000060325
021100060123
021200060123        leave;
021300060123       enddo;
021400151011
021500151013         CMRDEPT# = *zeros;
021600141008         CMRPCOD = *blanks;
021700141008
021800030911       dow 0 = 0;
021900060123
022000030911         exfmt fmt1;
022100060325
022200030911         errmsg = *blanks;
022300030911
022400030911         // end of program
022500030911         if *in03;
022600030911           rolbk;
022700030911           *INlr = *ON;
022800030911           return;
022900030911         endif;
023000060325
023100060325         // go back
023200060325         if *in02;
023300060325            leave;
023400060325         endif;
023500030911
023600060323         // List configured rates
023700060323         if  *in01;
023800151013           confrate_enq(cmrtype:cmrprno:cmrdept#:
023900151013                        cmrpcod);
024000060323           iter;
024100060323         endif;
024200060217
024300060217          // Prompt for list
024400060217               if  *in04;
024500060217                   select;
024600151013                    // Dept. Enquiry
024700151013                      when fld = 'CMRDEPT#';
024800151013                        dept_ENQ(Cmrdept#);
024900060220                        iter;
025000151109                    // Incentive Code Enquiry
025100060217                      when fld = 'CMRPCOD';
025200060220                        Commitem_ENQ(Cmrpcod);
025300060220                        iter;
025400060217                    endsl;
025500060217               endif;
025600060123
025700151109         // validate Incentive items
025800060326           if cmrpcod <> *blanks;
025900160517           //chain cmrpcod cmifmt;
026000160517             exec sql
026100160517                select cmiapcod into :associtm from picommitm
026200160517                  where cmipcod = :cmrpcod;
026300160517             if sqlstt = '02000';
026400160517           //if not %found;
026500160517                errmsg = msg1;
026600160517                iter;
026700160517             endif;
026800060218           endif;
026900030911
027000060402
027100151015           get_dept_info(cmrdept#:error);
027200151015
027300151015           if error;
027400151015               errmsg = msg5;
027500151015               iter;
027600060402           endif;
027700060419
027800151104           // Validate Period entry / get Global average flag setting
027900151015           if  getPIlprno() >= cmrprno;
028000151015             errmsg = msg6;
028100151015             iter;
028200060419           endif;
028300060402
028400030911
028500141008         if range_update();
028600141008           leave;
028700141008         ENDIF;
028800030911
028900030911       enddo;
029000120125
029100120125         //*****************************
029200120125         // Session Init
029300120125         //*****************************
029400120125
029500120125          begsr *inzsr;
029600120125
029700151011              get_session_info('PAY':errmsg:@pgm);
029800120125
029900120125              if errmsg <> *blanks;
030000120125                show_msg(errmsg);
030100120125                *inlr = *on;
030200120125                return;
030300120125              endif;
030400120125
030500151106              get_sys_parm(sys_struc);
030600151106
030700151011              open(e) picommrl01;
030800151011              open(e) picommdl01;
030900160308           // open(e) picommil01;
031000160308              open(e) picommitm;
031100151011              open(e) picommcm;
031200120125          endsr;
031300120125
031400060118
031500141008
031600141008
031700060124     P*--------------------------------------
031800060131     P* Procedure name: Validate_rtn
031900060124     P* Purpose:        validate subfile data
032000060124     P* Returns:        *boolean - error ind
032100060124     P* Parameter:      *none
032200060124     P*--------------------------------------
032300060124     P validate_rtn    B
032400060124     D validate_rtn    PI              n
032500060124
032600060124     D error           s               n
032700060124
032800060210           *IN70 = *OFF;
032900060325           *IN74 = *OFF;
033000060210           *IN75 = *OFF;
033100060210           *IN76 = *OFF;
033200060126
033300060131           cmrqtyu@_prv = *zeros;
033400060131
033500060124           rrx = rrn;
033600060124
033700060124           for rr = 1 to rrx;
033800060124
033900060124           CHAIN RR Sub1;
034000060126
034100060124           // validate record not set for deletion
034200060124           if delopt <> 'X';
034300060124
034400060124             // validate Lower and Upper Range
034500060217             if cmctarget = 0;
034600060217               if cmrqtyu = 0;
034700060217                 *IN70 = *ON;
034800060217               endif;
034900060217              else;
035000060217               if cmrperu = 0;
035100060217                 *IN70 = *ON;
035200060217               endif;
035300060217
035400060217               cmrqty = cmctarget * cmrper / 100;
035500060217               cmrqtyu = cmctarget * cmrperu / 100;
035600060217             endif;
035700060124
035800151206             // Validate incentive Entries
035900060124             // Check for blanks
036000060131             if cmrrate#1 = *zeros;
036100060124               *in74 = '1';
036200060124             endif;
036300060124
036400060131            // If lower range value is higher than upper range value
036500060131
036600060131             if cmrqty > cmrqtyu;
036700060131               *in76 = '1';
036800060131             endif;
036900060131
037000060210          // If lower range value is lower than upper range value
037100060210          // of a previous record
037200060131
037300060131             IF cmrqtyu@_prv <> *zeros;
037400060124
037500060131               IF cmrqty <= cmrqtyu@_prv;
037600060124
037700060131                 if cmrrate#1 <> *zeros;
037800060124                     *in76 = '1';
037900060124                 endif;
038000060124
038100060124               endif;
038200060124
038300060124             endif;
038400060124
038500060210             If *in70 or *in74 or *in76
038600060210                   or *in75;
038700060210               *IN70 = *on;
038800060124             endif;
038900060131
039000060124             If *in70 or *in76;
039100060124               error = *on;
039200060124               update sub1;
039300060124               leave;
039400060124             endif;
039500060131
039600060124             // save previous upper range record
039700060131             cmrqtyu@_prv = cmrqtyu;
039800060124           endif;
039900060218           update sub1;
040000060124
040100060124         endfor;
040200060131
040300060124         // Reset relative record number for application
040400060124          rrn = rrx;
040500060124          return error;
040600060124
040700060124     P validate_rtn    E
040800060124
040900060220
041000060220     P*-----------------------------------------
041100060220     P* Procedure name: load_default
041200151206     P* Purpose:        Load incentve defaults
041300060220     P* Returns:        *boolean - error ind
041400060220     P* Parameter:      *none
041500060220     P*-----------------------------------------
041600060220     P load_default    b
041700060220     D load_default    pi
041800151015     D dfttype                        2  0 const
041900151011     D dftdept#                       3  0 const
042000060221     D dftpcod                        6    const
042100060220
042200060220     D*error           s
042300060322     D out_cnt         s              1  0
042400060322     D in_cnt          s              1  0
042500151015     D type            s              2  0
042600151015     D dept#           s              3  0
042700060322     D product         s              6
042800060220
042900060221
043000060309        clear dftarr;
043100060221        clear dft_struc;
043200060322        *in88 = *off;
043300151015        type = dfttype;
043400151011        dept# = dftdept#;
043500060322        product = dftpcod;
043600060221
043700060322        for  out_cnt = 1 to  2;
043800060322
043900060322          select;
044000060322            when out_cnt = 2;
044100060322             type = *zeros;
044200151011             dept# = dftdept#;
044300060322             product = dftpcod;
044400060322          endsl;
044500060322
044600060322         for  in_cnt = 1 to  4;
044700060322          select;
044800060322            when in_cnt = 2;
044900151011             dept# = *zeros;
045000060322            when in_cnt = 3;
045100151011             dept# = dftdept#;
045200060322             product = *blanks;
045300060322            when in_cnt = 4;
045400151011             dept# = *zeros;
045500060322             product = *blanks;
045600060322          endsl;
045700060221
045800151011          setll (type:dept#:
045900060322                  product) cdrfmt;
046000151011          reade (type:dept#:
046100060322                  product) cdrfmt;
046200060322          if  not %eof();
046300060322           dow not %eof();
046400060221             ycnt = 0;
046500060221             ycnt = %lookup(cdrper:dftarr);
046600060221             if ycnt = 0;
046700060322                xcnt += 1;
046800060322                dftarr(xcnt) = cdrper;
046900060221
047000060221                dft_struc(xcnt).per_lower = cdrper;
047100060225                dft_struc(xcnt).per_upper = cdrperu;
047200060225                dft_struc(xcnt).per_rate1 = cdrrate#1;
047300060225                dft_struc(xcnt).per_rate2 = cdrrate#2;
047400141017                dft_struc(xcnt).per_rate3 = cdrrate#3;
047500060221                ycnt = xcnt;
047600060221
047700060221             endif;
047800060221
047900151011            reade (type:dept#:
048000060322                  product) cdrfmt;
048100060322           enddo;
048200060322
048300060322           // protect ranges if found in defaults
048400060322           *in88 = *on;
048500060322           return;
048600060322          endif;
048700060322
048800060322         endfor;
048900060220        endfor;
049000060220
049100060220     P load_default    e
049200060419
049300060419     P*----------------------------------------
049400151015     P* Procedure name: getPIlPrno
049500151015     P* Purpose:        get P.I.    last Period
049600060419     P* Returns:        *numeric 6,0
049700060419     P* Parameter:      *none
049800060419     P*----------------------------------------
049900151015     P getPIlprno      B
050000151015     D getPIlprno      PI             6  0
050100060419
050200060419     D lastPeriod      s              6  0
050300060419
050400151015            Exec Sql
050500151104               SELECT cmglprno, cmgavgflag  INTO
050600151104                   :lastPeriod, :AvgFlag  FROM picommgm;
050700060419
050800060419            return lastperiod;
050900060419
051000060419
051100151015     P getPIlprno      E
051200060118
051300141008
051400141008       // --------------------------------------------------
051500141008       // Procedure name: range_update
051600141008       // Purpose:
051700141008       // Returns:
051800141008       // --------------------------------------------------
051900141008       DCL-PROC range_update ;
052000141008         DCL-PI *N IND;
052100141008         END-PI ;
052200141008           // Your local fields go here
052300141008           DCL-S retField IND;
052400160413
052500160413         // get target figures
052600160413            cmctarget = Get_targets();
052700160413
052800160413            *in86 =  cmctarget > 0;
052900160413
053000160413         *in33 = '1';
053100141008         WRITE subctl1;
053200141008         *in33 = '0';
053300141008         rrn = *zeros;
053400141008         xcnt = *zeros;
053500141008         retField = *off;
053600141008         *in55 = *off;
053700141008
053800151015         load_default(cmrtype:cmrdept#:cmrpcod);
053900160408
054000160408         cmrtype  = pidpicode;
054100160408         setll (cmrprno:cmrtype:cmrdept#:
054200141008                  cmrpcod) cmrfmt;
054300141008
054400160408         reade (cmrprno:cmrtype:cmrdept#:
054500141008                  cmrpcod) cmrfmt;
054600141008
054700141008
054800150128             RESET RECCOUNT;
054900141008         dow not %eof;
055000141008             ycnt = 0;
055100141008             ycnt = %lookup(cmrper:dftarr);
055200141008             if ycnt = 0;
055300141008                xcnt += 1;
055400141008                dftarr(xcnt) = cmrper;
055500160408
055600141008                dft_struc(xcnt).per_lower = cmrper;
055700141008                dft_struc(xcnt).per_upper = cmrperu;
055800141008                dft_struc(xcnt).per_rate1 = cmrrate#1;
055900141008                dft_struc(xcnt).per_rate2 = cmrrate#2;
056000141017                dft_struc(xcnt).per_rate3 = cmrrate#3;
056100141008                dft_struc(xcnt).per_valuel = cmrqty;
056200141008                dft_struc(xcnt).per_valueu = cmrqtyu;
056300141008                dft_struc(xcnt).per_delopt = 'X';
056400141008                ycnt = xcnt;
056500141008
056600141008               else;
056700160408
056800141008                dft_struc(ycnt).per_rate1 = cmrrate#1;
056900141008                dft_struc(ycnt).per_rate2 = cmrrate#2;
057000141017                dft_struc(ycnt).per_rate3 = cmrrate#3;
057100141008                dft_struc(ycnt).per_valuel = cmrqty;
057200141008                dft_struc(ycnt).per_valueu = cmrqtyu;
057300141008
057400141008             endif;
057500150128             reccount += 1;
057600160408          reade (cmrprno:cmrtype:cmrdept#:
057700141008                   cmrpcod) cmrfmt;
057800141008         enddo;
057900141008
058000141008         for ycnt = 1 to xcnt;
058100141008
058200141008
058300141008            cmrper  = dft_struc(ycnt).per_lower;
058400141008            cmrperu = dft_struc(ycnt).per_upper;
058500141008            cmrrate#1 = dft_struc(ycnt).per_rate1;
058600141008            cmrrate#2 = dft_struc(ycnt).per_rate2;
058700141017            cmrrate#3 = dft_struc(ycnt).per_rate3;
058800141008            cmrqty =  dft_struc(ycnt).per_valuel;
058900141008            cmrqtyu = dft_struc(ycnt).per_valueu;
059000141008            delopt = dft_struc(ycnt).per_delopt;
059100141008
059200141008            if   cmrqty =  *zeros;
059300141008                cmrqty = cmctarget * cmrper / 100;
059400141008                cmrqtyu = cmctarget * cmrperu / 100;
059500141008            endif;
059600141008
059700141008            // protect rates field if set in default
059800141008         // *in87 = cmrrate#1 > *zeros;
059900141008
060000141008           rrn += 1;
060100141008           write sub1;
060200141008
060300141008         endfor;
060400141008
060500141008         *in55 = *on;
060600141008         *in20 = rrn > *zeros;
060700150128         If reccount > 0;
060800141008         mode = 'UPDATE';
060900150128         else;
061000150128         mode = 'ENTRY';
061100150128         endif;
061200141008      /if not defined(WEB)
061300141008         write fmt2;
061400141008      /endif
061500141008         dow 0 = 0;
061600141008
061700141008           exfmt subctl1;
061800141008
061900141008           // end of program
062000141008           if *in03;
062100141008             *inlr = '1';
062200141008             return *on;
062300141008           endif;
062400141008
062500141008           // go back
062600141008           if *in02;
062700160413             leave;
062800141008           endif;
062900141008
063000141008           *IN70 = *OFF;
063100141008           *IN75 = *OFF;
063200141008           *IN76 = *OFF;
063300141008      /if not defined(WEB)
063400141008           read fmt2;
063500141008      /else
063600141008           read subctl1;
063700141008      /endif
063800141008
063900150128         // F20=Delete item
064000150128           if  *in22;
064100151109              delete_sr();
064200151109              retField = *on;
064300151109              commit;
064400151109              leave;
064500150128           endif;
064600150128
064700141008           if validate_rtn();
064800141008             iter;
064900141008           endif;
065000141008
065100141008
065200141008           // Add a line to Sub-file
065300141008           if  *in04;
065400141008              new_line();
065500141008           endif;
065600141008
065700151109           // Record Update
065800141008           if *in15;
065900141008              delete_sr();
066000150129              dmtrn#();
066100151026              commit;
066200151011              retField = *on;
066300141008             leave;
066400141008           endif;
066500141008
066600141008
066700141008         enddo;
066800141008
066900141008
067000141008
067100141008           return retField;
067200141008       END-PROC ;
067300141008
067400141008       // --------------------------------------------------
067500150129       // Procedure name: dmtrn#
067600141008       // Purpose:
067700141008       // Returns:
067800141008       // --------------------------------------------------
067900150129       DCL-PROC dmtrn# ;
068000141008              rrx =  rrn;
068100141008         for rr = 1 to rrx;
068200141008
068300141008           chain rr sub1;
068400141008
068500141008           // write record not set for deletion
068600141008           if delopt <> 'X';
068700141008
068800141008             cmrtime = time1;
068900141008             cmruser  = @user;
069000141008             cmrdate  = date1;
069100151028             cmrtype  = pidpicode;
069200141008
069300141017             if cmrrate#1 <> *zeros or cmrrate#2 <> *zeros
069400141017             or cmrrate#3 <> *zeros;
069500141008               write cmrfmt;
069600141008             endif;
069700141008
069800141008           endif;
069900141008
070000141008         endfor;
070100150128                  reset recCount;
070200150128                  //check if record exists.
070300150128             exec sql select count(1)into :recCount
070400151011                from picommcm
070500151011                  where cmcprno = :cmrprno and
070600151015                    cmctype = :cmrtype and
070700151011                     cmcdept# = :cmrdept# and cmcpcod = :cmrpcod;
070800151011
070900150128                if recCount > 0;
071000150128
071100150128               // if found, and clearing, delete record
071200150128           if cmctarget = 0;
071300151011                exec sql delete FROM picommcm  where cmcprno = :cmrprno and
071400151015               cmctype = :cmrtype and
071500151011               cmcdept# = :cmrdept# and cmcpcod = :cmrpcod with nc;
071600150128                else;
071700150128               // if found, and not clearing, update file
071800150128              exec sql
071900160416               update picommcm  set cmctarget = :cmctarget,
072000160416                 cmctime = :time1, cmcuser = :@user, cmcdate = :date1
072100150128                 where cmcprno = :cmrprno and
072200151015                   cmctype = :cmrtype and
072300151011                    cmcdept# = :cmrdept# and cmcpcod = :cmrpcod with nc;
072400150128              endif;
072500150128
072600150128              else;
072700150128                // if new record, write to file
072800150128             if cmctarget > 0;
072900150128              exec sql
073000151015               insert into picommcm  (cmctype,cmctarget,cmcprno,
073100160416                 cmcdept#,cmcpcod,cmctime,cmcuser,cmcdate)
073200160416                 values(:cmrtype,:cmctarget,:cmrprno,:cmrdept#,:cmrpcod,
073300160416                        :time1, :@user, :date1) with nc;
073400150128
073500150128
073600150128              ENDIF;
073700150128
073800150128           ENDIF;
073900150128
074000141008
074100141008       END-PROC ;
074200141008
074300141008       // --------------------------------------------------
074400141008       // Procedure name: delete_sr
074500141008       // Purpose:
074600141008       // Returns:
074700141008       // --------------------------------------------------
074800141008       DCL-PROC delete_sr ;
074900141008
075000151109           exec sql
075100151109              delete FROM picommrm  where cmrprno = :cmrprno and
075200151109               cmrtype = :pidpicode and
075300151109               cmrdept# = :cmrdept# and cmrpcod = :cmrpcod with nc;
075400141008
075500141008
075600141008       END-PROC ;
075700141008
075800141008       // --------------------------------------------------
075900141008       // Procedure name: new_line
076000141008       // Purpose:
076100141008       // Returns:
076200141008       // --------------------------------------------------
076300141008       DCL-PROC new_line ;
076400141008            clear sub1;
076500141008            rrn += 1;
076600141008            write sub1;
076700141008            *in20 = rrn > *zeros;
076800141008       END-PROC ;
076900141008
077000151013
077100151013       // --------------------------------------------------
077200151013       // Procedure name: get_targets
077300151013       // Purpose:
077400151013       // Returns:
077500151013       // --------------------------------------------------
077600151015       DCL-PROC get_targets ;
077700151015         Dcl-Pi get_targets  Packed(12) End-Pi;
077800151014
077900151027       Dcl-S Targets       Packed(12);
078000151027       Dcl-S Loc           Packed(3);
078100151027       Dcl-S arrValues     VarChar(512) dim(30);
078200151027       Dcl-S varCount      Packed(3)  inz(0);
078300151015
078400160414          // retrieve department record
078500160414          open_pidept(cmrdept#);
078600151027
078700160414          dow not read_pidept();
078800151027
078900151013            // retrieve Incentive code record
079000160414            exec sql
079100151013              select * into :piCodemDS from picodem
079200151013                 where piccode = :pidpicode;
079300151027
079400151027            if  pidwhlist <> *blanks;
079500151027              parse_commaSeparatedList(pidwhlist:%addr(arrValues):varCount);
079600151027              else;
079700151027              parse_commaSeparatedList(picwhlst:%addr(arrValues):varCount);
079800151027            endif;
079900160413
080000151013            // retrieve Targets
080100151013            year = %int(%subst(%char(cmrprno):1:4));
080200151013            month = %int(%subst(%char(cmrprno):5:2));
080300151013
080400151106
080500151106            if dsyfyflag = '1';
080600151106                if month <  dsyfstrmth;
080700151106                    fyear  = year - 1;
080800151106                       else;
080900151106                    fyear  = year;
081000151106                endif;
081100151106
081200151106            elseif dsyfyflag = '2';
081300151106
081400151106                if month <  dsyfstrmth;
081500151106                    fyear  = year;
081600151106                       else;
081700151106                    fyear  = year + 1;
081800151106                endif;
081900151106
082000151106            endif;
082100151107
082200151027            for xcnt = 1 to varCount;
082300151027              if arrValues(xcnt) <> *blanks;
082400151015
082500151027                 loc  = %int(arrValues(xcnt));
082600160413
082700160416                // get controlling location  - Production & W/house
082800151014                  exec sql
082900151014                   select wcctlsloc  into :ctl_location from whcon
083000151015                      where wccode = :loc;
083100151014
083200160414                // open file  (targets)
083300160414                open_filesr(year: loc : pidorg);
083400151013
083500151013
083600151013                dow not read_datasr();
083700151014
083800151106                   // Get Fiscal year targets
083900151106                   exec sql
084000151106                      select tgptrg01, tgptrg02, tgptrg03 ,tgptrg04,
084100151106                             tgptrg05, tgptrg06, tgptrg07 ,tgptrg08,
084200151106                             tgptrg09, tgptrg10, tgptrg11 ,tgptrg12
084300151106                        into :tgptrg01, :tgptrg02, :tgptrg03 , :tgptrg04,
084400151106                             :tgptrg05, :tgptrg06, :tgptrg07 , :tgptrg08,
084500151106                             :tgptrg09, :tgptrg10, :tgptrg11 , :tgptrg12
084600151106                         from whtarg where
084700151106                        //   into :targPARR from whtarg  where
084800151106                             tgarea = :loc and tgpcod = :tgpcod and
084900160414                             tgyear = :fyear and tgorg = :pidorg;
085000151106
085100151106                   // Get product type & group
085200151014                   get_prod_info('RD':tgpcod:prod_struc);
085300151117
085400151014
085500151014                   if  picrevflg = 'Y' or pidrevflg = 'Y';
085600151014
085700151014                     //.. for dollar value
085800160412                     // .. just for Hotel (HOT) , restaurants (RES)
085900160412                     //    and Ultimate (ULT) - Just $$
086000160412                     if %scan (pidorg :'RES,ULT,HOT') > *zeros;
086100160412                        if  avgflag = '1';
086200160412                          targets += %xfoot(targSARR) / 12;
086300160412                        elseif  avgflag = '2';
086400160412                          targets += %xfoot(targPARR) / 12;
086500160412                           else;
086600160412                          targets += targSARR(month);
086700160412                        endif;
086800160412
086900160412                       else;
087000160412
087100160412                        dft_loc# = get_ctl_loc(loc:tgpcod:
087200160412                        ctl_location:sel_location);
087300151014
087400160412                        // get product price
087500160412                        get_item_price(loc:tgpcod:dft_loc#:*omit:
087600151101                                     item_priceDS:date1);
087700151107
087800160412                        if  avgflag = '1';
087900160412                          targets += %xfoot(targSARR) / 12 * prucost;
088000160412                        elseif  avgflag = '2';
088100160412                          targets += %xfoot(targPARR) / 12 * prucost;
088200160412                           else;
088300160412                          targets += targSARR(month) * prucost;
088400160412                        endif;
088500160412                     endif;
088600151014                     else;
088700151014                      //...for units
088800151104
088900151106                      if  avgflag = '1';
089000160316                      //targets += %xfoot(targSARR) / 12;
089100160316                        targets += Perform_product_conversion(pidpicode:
089200160316                                         piddept:tgpcod:%xfoot(targSARR) / 12);
089300151106                      elseif  avgflag = '2';
089400160316                      //targets += %xfoot(targPARR) / 12;
089500160316                        targets += Perform_product_conversion(pidpicode:
089600160316                                         piddept:tgpcod:%xfoot(targPARR) / 12);
089700151104                         else;
089800160316                      //targets += targSARR(month);
089900160316                        targets += Perform_product_conversion(pidpicode:
090000160316                                           piddept:tgpcod:targSARR(month));
090100151104                      endif;
090200160307
090300151014                   endif;
090400151014
090500151013                enddo;
090600151013
090700151015
090800151013                // close file
090900151013                close_filesr();
091000151013
091100151013              endif;
091200151013            endfor;
091300160414
091400160414          enddo;
091500151013
091600160414          Close_pidept();
091700160414          return targets;
091800151013
091900151013       END-PROC get_targets;
092000151013
092100151013
092200151013       // Procedure to Open File //
092300151013       //........................//
092400151013       Dcl-Proc open_filesr;
092500151013         Dcl-Pi open_filesr;
092600160414            year     Packed(4)   const;
092700160414            area     Packed(3)   const;
092800160414            origin   Char(3)     const;
092900151013         End-Pi;
093000151013
093100151013             Exec Sql
093200151013                Declare TrgCsr Cursor For
093300151013                  select *
093400151013                     from WHTARG
093500160414                        where tgyear = :year and tgarea = :area
093600160414                        and tgorg = :origin;
093700151013
093800151013             // open Targets Table
093900151013             Exec Sql
094000151013                OPEN TrgCsr;
094100151013
094200151013       End-Proc open_filesr;
094300151013
094400151013
094500151013      //................................//
094600151013      //Procedure to get data from file//
094700151013      //...............................//
094800151013       Dcl-Proc read_datasr;
094900151013         Dcl-Pi read_datasr Ind End-pi;
095000151013
095100151013           exec sql
095200151013              FETCH next from TrgCsr
095300151013                 INTO :whtargDs;
095400151013
095500151013          if sqlstt <> '00000';
095600151013               return *on;
095700151013             else;
095800151013              return *off;
095900151013           endif;
096000151013
096100151013       End-Proc read_datasr;
096200151013
096300151013      //............................//
096400151013      //Procedure to close the file //
096500151013      //............................//
096600151013       Dcl-Proc close_filesr;
096700151013         Dcl-Pi close_filesr End-pi;
096800151013
096900151013             // close Targets table
097000151013             Exec Sql
097100151013                close TrgCsr;
097200151013
097300151013       End-Proc close_filesr;
097400151117
097500151117      //............................//
097600151117      //Perform Product Conversion  //
097700151117      //............................//
097800151117       Dcl-Proc Perform_product_conversion;
097900160316         Dcl-Pi Perform_product_conversion  Packed(12);
098000151117            type     Packed(2)    const;
098100151117            dept#    Packed(3)    const;
098200151117            product  Char(6)      const;
098300160316            in_quantity Packed(14:2)  const;
098400151117         End-Pi;
098500151117
098600151117
098700151117     D counter         s              1  0
098800151117     D count0          s              1  0
098900151117     D count1          s              1  0
099000151117     D deptno          s                   like(dept#)
099100151117     D prod            s                   like(product)
099200151117     D srchcode        s              6
099300151117     D prodIND         s               n
099400151117     D groupIND        s               n
099500151117     D typeIND         s               n
099600151117     D codeFLD         s              6
099700151206     D*dollarIND       s               n
099800151206     D out_qty         s             14  4
099900160316     D quantity        s             12  0
100000151206
100100151206     D*YES             c                   'Y'
100200151117
100300151117
100400151117          deptno = dept#;
100500160316          quantity = in_quantity;
100600151117
100700151117         // Get work file criteria
100800151117          for count1 = 1 to 3;
100900151117
101000151117                chain (0:product) cmifmt;
101100151117                 if  %found and not ProdIND;
101200151117                    codeFLD = cmipcod;
101300151117                  else;
101400151117                    srchcode = %char(pdgrp);
101500151117                   chain (1:srchcode) cmifmt;
101600151117                   if  %found and not GroupIND;
101700151117                     codeFLD = cmipcod;
101800151117                     count1 = 2;
101900151117                    else;
102000151117                     srchcode = %char(pdtyp);
102100151117                     chain (2:srchcode) cmifmt;
102200151117                     if  %found and not TypeIND;
102300151117                       codeFLD = cmipcod;
102400151117                       count1 = 3;
102500151117                      else;
102600151117                       codeFLD = *blanks;
102700151117                     endif;
102800151117                   endif;
102900151117                 endif;
103000151117
103100151117
103200151117            prod = codeFLD;
103300151117
103400151117
103500151117                 // Calculating using standard volume
103600151117                if  cmiconvqty > *zeros
103700151117                  and  pdsize > *zeros;
103800151117                   // Get Product size
103900151117                 if  not get_size_info('RD':pdsize:size_struc);
104000151117                    // Get UOM conversion for calcs.
104100151117                  if not  uomval(out_qty:cmiconvuom:siuom:(siqty * pdsqty));
104200151117
104300151117                       quantity *= (out_qty / cmiconvqty);
104400151117                    elseif not  uomval(out_qty:cmiconvuom:sialtuom:
104500151117                                    (sialtqty * pdsqty));
104600151117                       quantity *= (out_qty  / cmiconvqty);
104700151117                  endif;
104800151117                 endif;
104900151117                endif;
105000151117
105100151117                 // Calculating using Dollar Value
105200151206            ////// dollarIND = cmidollarf = YES;
105300160316                return quantity;
105400151117
105500151117          //  endif;
105600151117
105700151117          //endfor;
105800151117
105900151117            select;
106000151117               when count1 = 1;
106100151117                  prodIND = *on;
106200151117               when count1 = 2;
106300151117                  groupIND = *on;
106400151117               when count1 = 3;
106500151117                  typeIND = *on;
106600151117
106700151117            endsl;
106800151117
106900151117          endfor;
107000151117
107100151117          prodIND = *off;
107200151117          groupIND = *off;
107300151117          typeIND = *off;
107400151117
107500151117
107600160316        return  quantity;
107700151117
107800151117
107900151117       End-Proc Perform_product_conversion;
108000151117
108100151117
108200151117       // --------------------------------------------------
108300151117       // Procedure name: Uomval
108400151117       // Purpose:  validate issuing  Unit o' measure
108500151117       // Returns:        *boolean
108600151117       // Parameter:      qty, convUOM, fromUOM, fromQty
108700151117       // --------------------------------------------------
108800151117       Dcl-Proc Uomval;
108900151117         Dcl-Pi Uomval Ind;
109000151117            out_qty  Packed(14:4);
109100151117            conuom   Char(2)      const;
109200151117            frmuom   Char(2)      const;
109300151117            frmqty   Packed(13:2) const;
109400151117         End-Pi;
109500151117
109600151117
109700151117           out_qty = perform_uconv(frmuom:conuom:frmqty);
109800151117
109900151117
110000151117           //if combi not found seton alarm
110100151117             if  out_qty = 0;
110200151117                return *on;
110300151117               else;
110400151117                return  *off;
110500151117             endif;
110600151117
110700151117       End-Proc Uomval;
110800151117
110900151013
111000141008       // --------------------------------------------------
111100141008       // Procedure name: roll_back_op
111200141008       // Purpose:
111300141008       // Returns:
111400141008       // --------------------------------------------------
111500141008       DCL-PROC roll_back_op ;
111600141008           rolbk;
111700141008
111800141008         errmsg =  msg7;
111900141008         show_msg(errmsg);
112000151013       END-PROC roll_back_op;
112100141008
112200141008       // --------------------------------------------------
112300141008       // Procedure name: AAINIT
112400141008       // Purpose:Begin subroutine to print header         //
112500141008       // Returns:
112600141008       // --------------------------------------------------
112700141008       DCL-PROC AAINIT ;
112800141008          coname = get_coname;
112900141008         @date  = get_sys_date(date1:time1);
113000141008         @title = arr(1);
113100151013       END-PROC aainit;
113200160414
113300160414       // Procedure to Open File //
113400160414       //........................//
113500160414       Dcl-Proc open_pidept;
113600160414         Dcl-Pi open_pidept;
113700160414            area   Packed(3)   const;
113800160414         End-Pi;
113900160414
114000160414             Exec Sql
114100160414                Declare PIDeptCsr Cursor For
114200160414                  select *
114300160414                     from PIDEPTM
114400160414                        where piddept = :area;
114500160414
114600160414             // open PI Departments Table
114700160414             Exec Sql
114800160414                OPEN PIDeptCsr;
114900160414
115000160414       End-Proc open_pidept;
115100160414
115200160414
115300160414      //................................//
115400160414      //Procedure to get data from file//
115500160414      //...............................//
115600160414       Dcl-Proc read_PIDept;
115700160414         Dcl-Pi read_PIDept Ind End-pi;
115800160414
115900160414           exec sql
116000160414              FETCH next from PIDeptCsr
116100160414                 INTO :piDeptmDs;
116200160414
116300160414          if sqlstt <> '00000';
116400160414               return *on;
116500160414             else;
116600160414              return *off;
116700160414           endif;
116800160414
116900160414       End-Proc read_PIDept;
117000160414
117100160414      //............................//
117200160414      //Procedure to close the file //
117300160414      //............................//
117400160414       Dcl-Proc close_PIDept;
117500160414         Dcl-Pi close_PIDept End-pi;
117600160414
117700160414             // close PI departments table
117800160414             Exec Sql
117900160414                close PIDeptCsr;
118000160414
118100160414       End-Proc close_PIDept;
118200160414
118300151013
118400141008**   Header to be displayed
118500151109      Incentive Rates Maintenance
