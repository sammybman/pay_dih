000100120203     H DATEDIT (*YMD)
000200120203     H bnddir('DISDIR':'SALESDIR') dftactgrp(*no) actgrp(*caller)
000300120203     h/define PROFOUNDUI
000400050812      // ***************************************(*NO)************
000500000000      //              B A N K S    D I H    L T D.              *
000600000000      //                                                        *
000700060220      //       RPG PROGRAM NAME..: WVENQ16R                     *
000800000000      //                                                        *
000900050215      //       PROGRAMMER........: ALLISON FUNG                 *
001000060220      //       DATE..............: 20.02.2006                   *
001100000000      //                                                        *
001200060220      //  PROGRAM DESCRIPTION: Sales Commission Associated      *
001300060220      //                       Products Enquiry                 *
001400000000      //                                                        *
001500000000      //*********************************************************
001600050819     F
001700060321     Fwhprdgrp  if   e           K Disk
001800060321     Fwhprdtyp  if   e           K Disk
001900160413     Fpayenq07d cf   e             workstn sfile(winrec:rrn)
002000120203     F/IF DEFINED(WEB)
002100120203     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002200120203     F/ENDIF
002300120203
002400060220     Fqsysprt   o    f  132        printer oflind(*inof) usropn
002500040224     f
002600040224     **
002700120203      /copy QCPYSRC,glpr
002800120203      /copy QCPYSRC,genpr
002900120203      /copy QCPYSRC,autpr
003000120203      /copy QCPYSRC,salpr
003100040224     **
003200040224     D arr             S             40    DIM(1) CTDATA PERRCD(1)
003300050409     D rrn             s              4  0
003400040224     d #date           s              8  0
003500040224     d #time           s              6  0
003600050812     d loc#            s              3  0 inz(003)
003700060220     d @title          s             40
003800060220     d coname          s             30
003900060220     d @date           s             10
004000060321     d desc#           s             25
004100060321     Dworkfield        s              2  0
004200060420     D dft#            s             23
004300060420     d curr_fld2       s             20
004400060420     d hold_fld2       s             20
004500060420     d single_lin      s            132    inz(*all'-')
004600050215     **
004700060420     D CRT_TEXT        c                   '(Created on &INFO)'
004800060420     **
004900120203     D  main_pgm       pr                  extpgm('WVENQ16R')
005000060220     D                                6
005100060220     D  main_pgm       pi
005200060220     d  prod#                         6
005300060220     d
005400060321     d whcomitmSTRUC e ds                  extname(whcommil01)  inz
005500120203
005600050215     D                sds
005700050215     D  @PGM             *PROC
005800120203     D  @USER                358    367
005900050215     d
006000050811     D*--------------------------------------
006100050811     D* Prototype for procedure: PrintReport
006200050811     D*--------------------------------------
006300050811     D PrintReport     PR
006400050811
006500050409     D*--------------------------------------
006600050409     D* Prototype for procedure: load_records
006700050409     D*--------------------------------------
006800050409     D load_records    PR
006900050215
007000050409     D*--------------------------------------
007100050409     D* Prototype for procedure: clear_subfile
007200050409     D*--------------------------------------
007300050409     D clear_subfile   PR
007400050409
007500050409     D*--------------------------------------
007600050409     D* Prototype for procedure: open_file
007700050409     D*--------------------------------------
007800050409     D open_file       PR
007900050409
008000050409     D*--------------------------------------
008100050409     D* Prototype for procedure: read_file
008200050409     D*--------------------------------------
008300050409     D read_file       PR              n
008400050409
008500050409     D*--------------------------------------
008600050409     D* Prototype for procedure: bldsfl
008700050409     D*--------------------------------------
008800050409     D bldsfl          PR
008900050409     D
009000050409     D*-----------------------------------------
009100050409     D* Prototype for procedure: DisplaySubfile
009200050409     D*-----------------------------------------
009300050409     D DisplaySubfile  PR
009400050409
009500050409     D*--------------------------------------
009600050409     D* Prototype for procedure: close_file
009700050409     D*--------------------------------------
009800050409     D close_file      PR
009900050409
010000050215     *******************
010100050215     ** Program start **
010200050215     *******************
010300050215      /free
010400160413         // authority_check('PAY':@pgm);
010500060220                 dow 0=0;
010600060220                 exsr aainit;
010700060220
010800060220                 load_records();
010900060220                 leave;
011000060220                 *inlr = *on;
011100060220                 enddo;
011200050215
011300060220          //***************************************************
011400060220          //** First initilisation routine for all programs  **
011500060220          //***************************************************
011600060220                 begsr aainit;
011700060220                  CONAME = get_coname;
011800060220                   @DATE = get_sys_date(#date:#time);
011900060220                     @title = arr(1);
012000060220                 endsr;
012100050811         //***********************************
012200050811         //** header of the report          **
012300050811         //***********************************
012400160413
012500050812
012600050811     OQSYSPRT   E            HDR            1 01
012700050811     O                       @PGM                10
012800060220     O                       coname              75
012900060220     O                                          126 'PAGE:'
013000060220     O                       PAGE          Z    130
013100050811     OQSYSPRT   E            HDR            2
013200050811     O                       @USER               10
013300060220     O                       @title              80
013400050811     O
013500050822     OQSYSPRT   E            HDR            2
013600060220     O                                           20 'PRIMARY PRODUCT'
013700060323     O                                           67 'ASSOC. W/HOUSE PRODUCT'
013800050819     **
013900050811     ** Fields in the file that would have the information  DET etc.
014000050811     OQSYSPRT   E            DET            1
014100060220     O                       cmipcod             11
014200060220     O                       cmidesc            + 2
014300060220     O                       cmiapcod           + 2
014400060321     O                       desc#              + 2
014500060420     O                       dft#               + 2
014600060420     OQSYSPRT   E            det2           1
014700060420     O                       single_lin         132
014800060220     O          e            final       2
014900060220     O                                           65 '** END OF REPORT **'
015000050811     **
015100050811     P*--------------------------------------
015200050811     P* Procedure name: PrintReport
015300050811     P* Purpose:
015400050811     P*--------------------------------------
015500050811     P PrintReport     B
015600050811     D PrintReport     PI
015700050811
015800060420     D counter         s              4  0
015900160413
016000050822
016100060420             open(e) qsysprt;
016200060420             open_file();
016300050822             except  hdr;
016400050822
016500050812             // priming read of dataBase files
016600060420              dow not read_file();
016700060420
016800050824              if *inof;
016900050824                 except hdr;
017000050824              endif;
017100050811
017200060420              curr_fld2 = cmipcod + cmidesc + %trim(cmiapcod) +
017300060420                         %char(cmicode);
017400060420
017500060420                select;
017600060420                  when cmicode = 0;
017700060420                       get_prod_info('RD':cmiapcod:prod_struc);
017800060420                       desc# = pddes;
017900060420                       dft# = *blanks;
018000060420                  when cmicode = 1;
018100060420                       // validate Product group
018200060420                       workfield = %int(cmiapcod);
018300060420                       chain workfield pgrpfmt;
018400060420                       If %found;
018500060420                         desc# = pgnam;
018600060420                         dft# = CRT_TEXT;
018700060420                         dft# = %replace(
018800060420                                ('group ' + %trim(cmiapcod)):
018900060420                                dft#:%scan('&INFO':dft#):5);
019000060420                       Endif;
019100060420                  when cmicode = 2;
019200060420                        // validate Product type
019300060420                       workfield = %int(cmiapcod);
019400060420                       chain workfield ptypfmt;
019500060420                       If  %found;
019600060420                         desc# = ptynam;
019700060420                         dft# = CRT_TEXT;
019800060420                         dft# = %replace(
019900060420                                ('type ' + %trim(cmiapcod)):
020000060420                                dft#:%scan('&INFO':dft#):5);
020100060420                       endif;
020200060420                endsl;
020300060420
020400060420              if curr_fld2 <> hold_fld2;
020500060420                counter += 1;
020600060420                except det2;
020700060420                hold_fld2 = curr_fld2;
020800060420              endif;
020900060420
021000050824                except det;
021100060420           enddo;
021200050811
021300060220           except final;
021400050811           close qsysprt;
021500060420           close_file();
021600050811
021700050811      /end-free
021800050811     P PrintReport     E
021900050409     P*--------------------------------------
022000050409     P* Procedure name: load_records
022100050409     P* Purpose:        load records to subfle
022200050411     P* Returns:        *none
022300050411     P* Parameter:      *none
022400050409     P*--------------------------------------
022500050409     P load_records    B
022600050409     D load_records    PI
022700050409
022800160413
022900050822
023000050409                clear_subfile();
023100050409                open_file();
023200050409                bldsfl();
023300060420                close_file();
023400050409                displaysubfile();
023500050409
023600160413
023700050409     P load_records    E
023800050409
023900050409     P*--------------------------------------
024000050409     P* Procedure name: DisplaySubfile
024100050411     P* Purpose:        Display subfile records
024200050409     P* Returns:        *none
024300050409     P* Parameter:      *none
024400050409     P*--------------------------------------
024500050409     P DisplaySubfile  B
024600050409     D DisplaySubfile  PI
024700050409
024800160413
024900050815
025000060220             *in20 = rrn > 0;
025100060220             write footer;
025200060220             exfmt wdwctl;
025300050811
025400060220           // F3 = Program End
025500060220                   if *in03;
025600060220                      *inlr = *on;
025700060220                     return;
025800060220                   endif;
025900060220
026000050811         // print detail report
026100050812            if  *in05;
026200050812              PrintReport();
026300050812            endif;
026400060324
026500060324         // handle selection
026600060324            readc winrec;
026700060324            dow not %eof();
026800060324              if  sel# = 'X';
026900060324                 prod# = cmipcod;
027000060324                 *inlr = *on;
027100060324                 return;
027200060324              endif;
027300060324             readc winrec;
027400060324            enddo;
027500050811
027600160413
027700050409     P DisplaySubfile  E
027800050409
027900050409     P*--------------------------------------
028000050409     P* Procedure name: clear_subfile
028100050409     P* Purpose:        clear subfile
028200050411     P* Returns:        *none
028300050411     P* Parameter:      *none
028400050409     P*--------------------------------------
028500050409     P clear_subfile   B
028600050409     D clear_subfile   PI
028700050409
028800160413
028900050409
029000050409                 write window;
029100050409                 *in33 = *on;
029200050409                    rrn = *zeros;
029300050409                    write wdwctl;
029400050409                     *in33 = *off;
029500050409                     clear footer;
029600050409
029700160413
029800050409     P clear_subfile   E
029900050409
030000050409     P*--------------------------------------
030100050409     P* Procedure name: open_file
030200050409     P* Purpose:        open database file
030300050411     P* Returns:        *none
030400050411     P* Parameter:      *none
030500050409     P*--------------------------------------
030600050409     P open_file       B
030700050409     D open_file       PI
030800050815
030900050819     C/Exec Sql
031000060220     C+  Declare CMFILE Cursor For
031100060321     C+  SELECT cmipcod, cmidesc, cmiapcod, cmicode
031200060220     C+
031300160413     C+  From PICOMMitm                -- From W/H Commission File
031400060220     C+
031500060221     C*  Where cmiapcod = pdpcod       -- Primary product
031600060220     C+
031700060323     C+  Order By cmipcod              -- Sort By Primary Product
031800060220     C+
031900060220     C+  For Fetch Only                -- Read Only Cursor
032000060220     C/End-Exec
032100060220      *
032200160413         Exec Sql
032300160413            OPEN CMFILE;
032400160413
032500060220      *
032600050409     P open_file       E
032700050409
032800050409     P*--------------------------------------
032900050409     P* Procedure name: bldsfl
033000050409     P* Purpose:        build subfile
033100050411     P* Returns:        *none
033200050411     P* Parameter:      *none
033300050409     P*--------------------------------------
033400050409     P bldsfl          B
033500050409     D bldsfl          PI
033600050409
033700050409                *in55 = *off;
033800060420
033900050409           // priming read of warehouse file
034000050409          // .. and there is room in the subfile ...
034100060221                dow not read_file()  AND rrn < 9999;
034200060221
034300050409          //   Increment the subfile relative record number and
034400050409          //         add the subfile record
034500060420                select;
034600060420                  when cmicode = 0;
034700060420                       get_prod_info('RD':cmiapcod:prod_struc);
034800060420                       desc# = pddes;
034900060420                       dft# = *blanks;
035000060420                  when cmicode = 1;
035100060420                       // validate Product group
035200060420                       workfield = %int(cmiapcod);
035300060420                       chain workfield pgrpfmt;
035400060420                       If %found;
035500060420                         desc# = pgnam;
035600060420                      // dft# = CRT_TEXT;
035700060420                      // dft# = %replace(
035800060420                      //        ('group ' + %trim(cmiapcod)):
035900060420                      //        dft#:%scan('&INFO':dft#):5);
036000060420                       Endif;
036100060420                  when cmicode = 2;
036200060420                        // validate Product type
036300060420                       workfield = %int(cmiapcod);
036400060420                       chain workfield ptypfmt;
036500060420                       If  %found;
036600060420                         desc# = ptynam;
036700060420                      // dft# = CRT_TEXT;
036800060420                      // dft# = %replace(
036900060420                      //        ('type ' + %trim(cmiapcod)):
037000060420                      //        dft#:%scan('&INFO':dft#):5);
037100060420                       endif;
037200060420                endsl;
037300060321
037400050409                   rrn += 1;
037500050409                   write winrec;
037600050409
037700050409                enddo;
037800060220                *in55 = *on;
037900050409
038000050409     P bldsfl          E
038100050409
038200050409     P*--------------------------------------
038300050409     P* Procedure name: read_file
038400050409     P* Purpose:        read transaction file
038500050409     P* Returns:        error indicator
038600050409     P* Parameter:      errmsg => error message text
038700050409     P*--------------------------------------
038800050409     P read_file       B
038900050409     D read_file       PI              n
039000050409
039100050815     D*   Read the records ...
039200050818
039300050409     C/EXEC SQL
039400060220     C+ FETCH NEXT FROM CMFILE INTO :cmipcod,
039500060321     C+         :cmidesc, :cmiapcod, :cmicode
039600050812     c+
039700050409     C/END-EXEC
039800050822
039900050409          // For every record in the file ....
040000050409          // ... while a record was successfully read ...
040100050409            if SQLSTT = '02000';
040200050409               return *on;
040300050409              else;
040400050409               return *off;
040500050409            endif;
040600050815
040700050815
040800050409     P read_file       E
040900050409
041000050409     P*--------------------------------------
041100050409     P* Procedure name: close_file
041200050409     P* Purpose:        close transaction file
041300050411     P* Returns:        *none
041400050411     P* Parameter:      *none
041500050409     P*--------------------------------------
041600050409     P close_file      B
041700050409     D close_file      PI
041800050409
041900160413         Exec Sql Close CMFILE;
042000160413
042100050409     P close_file      E
042200050215
042300040224**  Program Title
042400160413Sales Performance Incentive Associated Products
