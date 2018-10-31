000100151026     H dftactgrp(*no) actgrp(*new) bnddir('DISDIR')
000200120125     h/define PROFOUNDUI
000300030911      //*********************************************************
000400030911      //              D I S E R V   S Y S T E M S.              *
000500151009      //                  PAYROLL  SYSTEM                       *
000600030911      //                                                        *
000700030911      //                         BY                             *
000800030911      //                                                        *
000900030911      //             Diserv systems Team                        *
001000030911      //             P.O BOX XXXXX, GEORGETOWN,GUYANA           *
001100030911      //
001200030911      //                                                        *
001300030911      //                                                        *
001400151009      //       RPG PROGRAM NAME..: PAYMNT1204                   *
001500030911      //                                                        *
001600051229      //       PROGRAMMER NAME: Samuel Britton                  *
001700151009      //       DESIGN DATE....: 2015/10/09                      *
001800030911      //       MODULES........: *NONE                           *
001900030911      //       INDICATORS.....:                                 *
002000030911      //                                                        *
002100030911      //                                                        *
002200151009      //  THIS PROGRAM IS DESIGNED to store P.I. rates as       *
002300060124      //                           are defined.                 *
002400030911      //                                                        *
002500030911      //*********************************************************
002600151026     Fpicommil01if   E           K DISK
002700151026     Fpicommdl01uf a E           K DISK
002800151009     Fpaymnt12d4CF   E             WORKSTN
002900030911     F                                     SFILE(sub1:RRN)
003000120125     F/IF DEFINED(WEB)
003100120125     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003200120125     F/ENDIF
003300060124
003400120125      /copy QCPYSRC,genpr
003500151010      /copy QCPYSRC,paypr
003600120125      /COPY QCPYSRC,AUTPR
003700120125
003800151009     Dmain_prog        PR                  extpgm('PAYMNT1204')
003900120125
004000060217     Dmain_prog        PI
004100060217
004200151010     D dept_enq        pr                  extpgm('HMRENQ02R')
004300151010     D dept#                          3  0
004400060321
004500060321     D commitem_enq    pr                  extpgm('WVENQ16R')
004600060321     D item                           6
004700060321
004800060321     D confdft_enq     pr                  extpgm('WVENQ17R')
004900151012     D  type#                         2  0
005000151010     D  dept#                         3  0
005100060322     d  prod#                         6
005200060321
005300141007      //--------------------------------------
005400141007      // Prototype for procedure: open_file
005500141007      //--------------------------------------
005600060323     D open_file       PR
005700060323
005800141007      //--------------------------------------
005900141007      // Prototype for procedure: read_file
006000141007      //--------------------------------------
006100060323     D read_file       PR              n
006200060323
006300141007      //--------------------------------------
006400141007      // Prototype for procedure: close_file
006500141007      //--------------------------------------
006600060323     D close_file      PR
006700060124
006800060124     D ARR             S             40    DIM(1) CTDATA PERRCD(1)
006900060126     D date1           S              8  0
007000060126     D time1           S              6  0
007100030911     D rrn             s              4  0
007200030911     D rrx             s              4  0
007300030911     D rr              s              4  0
007400060219     D cdrperu@_prv    s                   like(cdrperu)
007500060322     D dftarr          s              5  2 dim(50)                              TOTAL AMOUNT
007600060322     D xcnt            s              2  0                                      TOTAL AMOUNT
007700060322     D ycnt            s              2  0                                      TOTAL AMOUNT
007800151012     D type            s              2  0
007900151010     D dept#           s              3  0
008000060323     D product         s              6
008100151010     D error           s               n
008200151009
008300151009     d VarLen          s             10I 0 Inz(%size(Var))
008400151009     d ApiErr          s             15a
008500151009     d Caller          s             50a
008600151009     d WhoCalled       s             10a
008700151009     d i               s             10I 0
008800151009
008900060323
009000151010     D CommDft_struc e ds                  extname(picommdr) inz qualified
009100060323
009200141007      //
009300060322     Ddft_struc        ds                  qualified dim(50)
009400060322     D per_lower                      5  2
009500060322     D per_upper                      5  2
009600060322     D per_rate1                     11  6
009700060322     D per_rate2                     11  6
009800141007     D per_rate3                     11  6
009900060322     D per_valuel                    11  2
010000060322     D per_valueu                    11  2
010100060322     D per_delopt                     1
010200060131
010300030911     D                sds
010400030911     D  @pgm             *proc
010500120125     D  @user                358    367
010600030911
010700060124      //-- Validate routine                                                  --
010800060124     Dvalidate_rtn     pr              n
010900030911
011000060322      //-- Load Defaults routine                                             --
011100060322     Dload_default     pr
011200151012     D dfttype                        2  0 const
011300151010     D dftdept#                       3  0 const
011400060322     D dftpcod                        6    const
011500060322
011600060216     DMsg1             c                   'Commission Item code Invalid !'
011700060216     DMsg2             c                   'Invalid Route code'
011800060219     DMsg3             c                   'Unknown Error: Try again'
011900060325
012000060325     DTEXT1            c                   'MANAGER'
012100060325     DTEXT2            c                   'SUPERVISOR'
012200060325     DTEXT3            c                   'SALESMAN'
012300030911
012400151009
012500151009             FindCaller(Var:VarLen:'CSTK0100':JobIdInf
012600151009                         :'JIDF0100':ApiErr);
012700151009               For i = 1 to EntryCount;
012800151009                  Entry = %subst(Var:Offset + 1);
012900151009                  Caller = %trim(PgmLib) + '/' + %trim(PgmNam);
013000151009                  If (PgmNam <> @pgm and WhoCalled = *blanks);
013100151009                     WhoCalled = PgmNam;
013200151009                  Endif;
013300151009                  Offset = Offset + EntryLen;
013400151009               Endfor;
013500151009
013600151009            // Program CANNOT be called independently
013700151009            if WhoCalled <> 'PAYMNT12R';
013800151009              *inlr = *on;
013900151009              return;
014000151009           endif;
014100151009
014200040714
014300030911
014400060215       exsr aainit;
014500060124
014600030911       dow 0 = 0;
014700060123
014800030911         exfmt fmt1;
014900030911         *IN26 = *OFF;
015000030911         *IN27 = *OFF;
015100030911         errmsg = *blanks;
015200030911
015300030911         // end of program
015400030911         if *in03;
015500030911           rolbk;
015600030911           *INlr = *ON;
015700030911           return;
015800030911         endif;
015900030911
016000060321         // end of program
016100060321         if  *in01;
016200151012           confdft_enq(cdrtype:cdrdept#:cdrpcod);
016300060321           iter;
016400060321         endif;
016500060217
016600060217          // Prompt for list
016700060217               if  *in04;
016800060217                   select;
016900151010                    // Department Enquiry
017000151010                      when fld = 'CDRDEPT#';
017100151010                        dept_ENQ(Cdrdept#);
017200060220                       iter;
017300060218                    // Commission Code Enquiry
017400060219                      when fld = 'CDRPCOD';
017500060321                        Commitem_ENQ(Cdrpcod);
017600060220                       iter;
017700060217                    endsl;
017800060217               endif;
017900060123
018000060218         // validate commission items
018100060327           if  cdrpcod <> *blanks;
018200060219             chain cdrpcod cmifmt;
018300060218             if not %found;
018400060218                errmsg = msg1;
018500060218                iter;
018600060218             endif;
018700060218           endif;
018800030911
018900151010         // validate dept#
019000151010           if  cdrdept# <> *zeros;
019100151010             get_dept_info(cdrdept#:error);
019200151010             if error;
019300060218                errmsg = msg2;
019400060218                iter;
019500060218             endif;
019600060218           endif;
019700060325
019800151010         //select;
019900151010         // when cdrsts = '1';
020000151010         //   comdesc = TEXT1;
020100151010         //endsl;
020200060210
020300060123         exsr range_update;
020400030911
020500030911       enddo;
020600030911
020700060118
020800060118       //****************************************
020900060124       // subroutine to update Commission ranges*
021000060118       //****************************************
021100060118       Begsr Range_update;
021200060124
021300060124         *in33 = '1';
021400060118         WRITE subctl1;
021500060124         *in33 = '0';
021600060425         rrn = *zeros;
021700060425         xcnt = *zeros;
021800060118
021900060126         *in55 = *off;
022000060126
022100151012         load_default(cdrtype:cdrdept#:cdrpcod);
022200060322
022300151012         setll (cdrtype:cdrdept#:
022400060219                  cdrpcod) cdrfmt;
022500060126
022600151012         reade (cdrtype:cdrdept#:
022700060219                  cdrpcod) cdrfmt;
022800060118
022900060216         dow not %eof;
023000060118
023100060322             ycnt = 0;
023200060322             ycnt = %lookup(cdrper:dftarr);
023300060322             if ycnt = 0;
023400060322                xcnt += 1;
023500060322                dftarr(xcnt) = cdrper;
023600060322
023700060322                dft_struc(xcnt).per_lower = cdrper;
023800060322                dft_struc(xcnt).per_upper = cdrperu;
023900060322                dft_struc(xcnt).per_rate1 = cdrrate#1;
024000060322                dft_struc(xcnt).per_rate2 = cdrrate#2;
024100141007                dft_struc(xcnt).per_rate3 = cdrrate#3;
024200060322                dft_struc(xcnt).per_valuel = cdrqty;
024300060322                dft_struc(xcnt).per_valueu = cdrqtyu;
024400060322                dft_struc(xcnt).per_delopt = 'X';
024500060322                ycnt = xcnt;
024600060322
024700060322             endif;
024800060126
024900151012           reade (cdrtype:cdrdept#:
025000060219                   cdrpcod) cdrfmt;
025100060126         enddo;
025200060126
025300060322         for ycnt = 1 to xcnt;
025400060322
025500060322
025600060322            cdrper  = dft_struc(ycnt).per_lower;
025700060322            cdrperu = dft_struc(ycnt).per_upper;
025800060322            cdrrate#1 = dft_struc(ycnt).per_rate1;
025900060322            cdrrate#2 = dft_struc(ycnt).per_rate2;
026000141007            cdrrate#3 = dft_struc(ycnt).per_rate3;
026100060322            cdrqty =  dft_struc(ycnt).per_valuel;
026200060322            cdrqtyu = dft_struc(ycnt).per_valueu;
026300060322            delopt = dft_struc(ycnt).per_delopt;
026400060322
026500060322            // protect rates field if set in default
026600060322            *in87 = cdrrate#1 > *zeros;
026700060322
026800060322           rrn += 1;
026900060322           write sub1;
027000060322
027100060322         endfor;
027200060322
027300060126         *in55 = *on;
027400060215         *in20 = rrn > *zeros;
027500060322
027600060322         mode = 'UPDATE';
027700060322
027800060322         write fmt2;
027900060118         dow 0 = 0;
028000060126
028100060118           exfmt subctl1;
028200060124
028300060124           // end of program
028400060118           if *in03;
028500060118             *inlr = '1';
028600060118             return;
028700060118           endif;
028800060118
028900060118           *IN70 = *OFF;
029000060118           *IN75 = *OFF;
029100060118           *IN76 = *OFF;
029200060118
029300060510           //read fmt2;
029400060118
029500060124           if validate_rtn();
029600060118             iter;
029700060118           endif;
029800060118
029900060118
030000060118           // Add a line to Sub-file
030100060118
030200060216           if  *in04;
030300060216              exsr new_line;
030400060118           endif;
030500060118
030600060118
030700060118           // Confirm Record Update
030800060118
030900060118           if *in15;
031000060118             exsr delete_sr;
031100060118             exsr confirm;
031101151026             commit;
031200060118           endif;
031300060425
031400060425           // F20=Delete item
031500060425           if  *in22;
031600060425               exsr delete_sr;
031700060425               commit;
031800060510               leavesr;
031900060425           endif;
032000060118
032100060118         enddo;
032200060118       endsr;
032300060118
032400060118
032500060124       //****************************************//
032600060124       // Subroutine to add new line to subfile  //
032700060124       //****************************************//
032800060118
032900060118       Begsr new_line;
033000060118
033100060124         clear sub1;
033200060124         rrn += 1;
033300060118         write sub1;
033400060210         *in20 = rrn > *zeros;
033500060118       endsr;
033600060118
033700060118
033800060124       //**************************************//
033900060124       // Subroutine to confirm changes made   //
034000060124       //**************************************//
034100060118
034200060118       begsr Confirm;
034300060118
034400060124         rrx =  rrn;
034500060124         for rr = 1 to rrx;
034600060118
034700060124           chain rr sub1;
034800060126
034900060118           // write record not set for deletion
035000060118           if delopt <> 'X';
035100060118
035200060219             cdrtime = time1;
035300060219             cdruser  = @user;
035400060219             cdrdate  = date1;
035500060118
035600060219               write cdrfmt;
035700060126
035800060118           endif;
035900060118
036000060124         endfor;
036100060218
036200060118
036300060118         *inlr = '1';
036400060118         return;
036500060118
036600060118       endsr;
036700060118
036800060118
036900060126       //****************************************************//
037000060126       // Begin subroutine to Delete records before updating //
037100060126       //****************************************************//
037200060118       BEGSR Delete_sr;
037300060118
037400151012         setll (cdrtype:cdrdept#:
037500060219                  cdrpcod) cdrfmt;
037600060126
037700151012         reade (cdrtype:cdrdept#:
037800060219                  cdrpcod) cdrfmt;
037900060126
038000060118
038100060124         dow not(%eof);
038200060219           delete cdrfmt;
038300151012           reade (cdrtype:cdrdept#:
038400060219                  cdrpcod) cdrfmt;
038500060126
038600060118         enddo;
038700060118
038800060118       endsr;
038900030911
039000060124       //*******************************************//
039100060124       // Begin subroutine to print header          //
039200060124       //*******************************************//
039300030911       BEGSR AAINIT;
039400030911         coname = get_coname;
039500060126         @date  = get_sys_date(date1:time1);
039600030911         @title = arr(1);
039700030911       ENDSR;
039800060118
039900060124       //*******************************************//
040000060124       // ROUTINE to reverse transactions in error  //
040100060124       //*******************************************//
040200030911       BEGSR roll_back_op;
040300030911         rolbk;
040400030911
040500060219         errmsg =  msg3;
040600030911
040700030911       ENDSR;
040800030911      /END-FREE
040900060124
041000141007      //--------------------------------------
041100141007      // Procedure name: Validate_rtn
041200141007      // Purpose:        validate subfile data
041300141007      // Returns:        *boolean - error ind
041400141007      // Parameter:      *none
041500141007      //--------------------------------------
041600060124     P validate_rtn    B
041700060124     D validate_rtn    PI              n
041800060124
041900060124     D error           s               n
042000060124
042100060210           *IN70 = *OFF;
042200060210           *IN75 = *OFF;
042300060210           *IN76 = *OFF;
042400060126
042500060219           cdrperu@_prv = *zeros;
042600060131
042700060124           rrx = rrn;
042800060124
042900060124           for rr = 1 to rrx;
043000060124
043100060124           CHAIN RR Sub1;
043200060126
043300060124           // validate record not set for deletion
043400060124           if delopt <> 'X';
043500060124
043600060124             // validate Lower and Upper Range
043700060219               if cdrperu = 0;
043800060217                 *IN70 = *ON;
043900060217               endif;
044000060124
044100060131            // If lower range value is higher than upper range value
044200060131
044300060219             if cdrper > cdrperu;
044400060131               *in76 = '1';
044500060131             endif;
044600060131
044700060210          // If lower range value is lower than upper range value
044800060210          // of a previous record
044900060131
045000060219             IF cdrperu@_prv <> *zeros;
045100060124
045200060219               IF cdrper <= cdrperu@_prv;
045300060124
045400060124                     *in76 = '1';
045500060124
045600060124               endif;
045700060124
045800060124             endif;
045900060124
046000060210             If *in70 or *in74 or *in76
046100060210                   or *in75;
046200060210               *IN70 = *on;
046300060124             endif;
046400060131
046500060219             If *in70 or *in76;
046600060124               error = *on;
046700060124               update sub1;
046800060124               leave;
046900060124             endif;
047000060131
047100060124             // save previous upper range record
047200060219             cdrperu@_prv = cdrperu;
047300071227             update sub1;
047400060124           endif;
047500060124
047600060124         endfor;
047700060131
047800060124         // Reset relative record number for application
047900060124          rrn = rrx;
048000060124          return error;
048100060124
048200060124      /end-free
048300060124     P validate_rtn    E
048400060124
048500060322
048600141007      //-----------------------------------------
048700141007      // Procedure name: load_default
048800141007      // Purpose:        Load commission defaults
048900141007      // Returns:        *boolean - error ind
049000141007      // Parameter:      *none
049100141007      //-----------------------------------------
049200060322     P load_default    b
049300060322     D load_default    pi
049400151012     D dfttype                        2  0 const
049500151010     D dftdept#                       3  0 const
049600060322     D dftpcod                        6    const
049700060322
049800141007      //error           s
049900060322     D out_cnt         s              1  0
050000060322     D in_cnt          s              1  0
050100060323     D found_ind       s               n
050200060322
050300060322
050400060322        clear dftarr;
050500060322        clear dft_struc;
050600060322        *in88 = *off;
050700151012        type = dfttype;
050800060323        found_ind = *off;
050900151010        dept# = dftdept#;
051000060322        product = dftpcod;
051100060322
051200060322        for  out_cnt = 1 to  2;
051300060322
051400060323
051500060322          select;
051600060322            when out_cnt = 2;
051700151012             type = *zeros;
051800151010             dept# = dftdept#;
051900060322             product = dftpcod;
052000060322          endsl;
052100060322
052200060322         for  in_cnt = 1 to  4;
052300060322          select;
052400060322            when in_cnt = 2;
052500151010             dept# = *zeros;
052600060322            when in_cnt = 3;
052700151010             dept# = dftdept#;
052800060322             product = *blanks;
052900060322            when in_cnt = 4;
053000151010             dept# = *zeros;
053100060322             product = *blanks;
053200060322          endsl;
053300060322
053400060323          // open data files for processing
053500060323          open_file();
053600060323
053700060323           dow not read_file();
053800060323             found_ind = *on;
053900060322             ycnt = 0;
054000060323             ycnt = %lookup(commdft_struc.cdrper:dftarr);
054100060322             if ycnt = 0;
054200060322                xcnt += 1;
054300060323                dftarr(xcnt) = commdft_struc.cdrper;
054400060322
054500060323                dft_struc(xcnt).per_lower = commdft_struc.cdrper;
054600060323                dft_struc(xcnt).per_upper = commdft_struc.cdrperu;
054700060323                dft_struc(xcnt).per_rate1 = commdft_struc.cdrrate#1;
054800060323                dft_struc(xcnt).per_rate2 = commdft_struc.cdrrate#2;
054900141007                dft_struc(xcnt).per_rate3 = commdft_struc.cdrrate#3;
055000060322                ycnt = xcnt;
055100060322
055200060322             endif;
055300060322
055400060322           enddo;
055500060323
055600060323           // close data files for processing
055700060323              close_file();
055800060322
055900060323          if  found_ind;
056000060322           // protect ranges if found in defaults
056100060322           *in88 = *on;
056200060322           return;
056300060322          endif;
056400060322
056500060322         endfor;
056600060322        endfor;
056700060322
056800060322      /end-free
056900060323     P load_default    e
057000060323
057100141007      //--------------------------------------
057200141007      // Procedure name: open_file
057300141007      // Purpose:        open database file
057400141007      // Returns:        *none
057500141007      // Parameter:      *none
057600141007      //--------------------------------------
057700060323     P open_file       B
057800060323     D open_file       PI
057900060323
058000141007       Exec Sql
058100151012        DECLARE COMFILE CURSOR FOR SELECT * FROM picommdr WHERE cdrtype =
058200151010        :type and cdrdept# = :cdrdept#  and
058300151012        cdrpcod = :product ORDER BY cdrtype,cdrdept#,cdrpcod
058400141007        FOR READ ONLY;
058500141007       Exec Sql
058600141007        OPEN COMFILE;
058700060323     P open_file       E
058800060323
058900141007      //--------------------------------------
059000141007      // Procedure name: read_file
059100141007      // Purpose:        read transaction file
059200141007      // Returns:        error indicator
059300141007      // Parameter:      errmsg => error message text
059400141007      //--------------------------------------
059500060323     P read_file       B
059600060323     D read_file       PI              n
059700060323
059800141007            //   Read the records ...
059900141007
060000141007       EXEC SQL
060100141007        FETCH NEXT FROM COMFILE INTO :commdft_struc;
060200060323          // For every record in the file ....
060300060323          // ... while a record was successfully read ...
060400060323            if SQLSTT = '02000';
060500060323               return *on;
060600060323              else;
060700060323               return *off;
060800060323            endif;
060900060323      /end-free
061000141007      //
061100060323     P read_file       E
061200060323
061300141007      //--------------------------------------
061400141007      // Procedure name: close_file
061500141007      // Purpose:        close transaction file
061600141007      // Returns:        *none
061700141007      // Parameter:      *none
061800141007      //--------------------------------------
061900060323     P close_file      B
062000060323     D close_file      PI
062100060323
062200141007       Exec Sql Close COMFILE;
062300060323     P close_file      E
062400060118
062500030911**   Header to be displayed
062600151010      Performance Rates Defaults
