000100120125     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200120125     h/define PROFOUNDUI
000300030902      //********************************************************
000400030902      //              B A N K S    D I H    L T D.              *
000500030902      //                  M.I.S. DEPARTMENT                     *
000600030902      //                                                        *
000700030902      //                                                        *
000800151009      //       Program Name......: PAYMNT1205                   *
000900030902      //                                                        *
001000151009      //       Created By........: Samuel Britton               *
001100151009      //       Date..............: 09.10.2015                   *
001200030902      //                                                        *
001300151009      //   Description: P.I. Linked Items for Calcs.            *
001400030902      //                                                        *
001500030902      //*********************************************************
001600151014     Fpicommil01uf a E           k DISK    commit
001700060220     Fwhprdgrp  if   e           K Disk
001800060220     Fwhprdtyp  if   e           K Disk
001900060418     Fuomm      if   e           K Disk
002000151009     Fpaymnt12d5CF   E             workstn
002100060211     F                                     SFILE(sflrec:rrn)
002200120125     F/IF DEFINED(WEB)
002300120125     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002400120125     F/ENDIF
002500030902
002600120125      /copy QCPYSRC,genpr
002700120125      /copy QCPYSRC,salpr
002800120125      /copy QCPYSRC,autpr
002900120125      /copy QCPYSRC,invpr
003000030902
003100030902     Darr              s             40    dim(1) ctdata perrcd(1)
003200060213     D litem_arr       s              6    dim(75)
003300060213     D linkeditem      S              6
003400060413     D item            s              6
003500060213     D date#           S              8  0
003600060213     D time#           S              6  0
003700030902     D rrn             s              4  0
003800030902     D cnt             s              4  0
003900030927     d y               s              4  0
004000060418     d qryselect       s            200
004100151009
004200151009     d VarLen          s             10I 0 Inz(%size(Var))
004300151009     d ApiErr          s             15a
004400151009     d Caller          s             50a
004500151009     d WhoCalled       s             10a
004600151009     d i               s             10I 0
004700151009
004800060211
004900060213     d msg1            c                   'Invalid Item entered'
005000060213     d msg2            c                   'Item cannot be duplicated'
005100060325     d msg3            c                   'Item Already Linked: check '
005200060220     d msg4            c                   'Error found:please check digits'
005300060403     D msg5            c                   'Invalid UOM code'
005400060413     D msg6            c                   'Cannot perform  UOM Conversion for -
005500060413     D                                     Item '
005600060413     D msg7            c                   'Conversion Quantity cannot be zero'
005700060211
005800030902     **
005900030902     d                sds
006000030902
006100030902     D  @pgm             *proc
006200120125     D  @user                358    367
006300030902     D
006400151009     Dmain_prog        PR                  extpgm('PAYMNT1205')
006500060213     D                                3  0
006600060211     Dmain_prog        PI
006700060213     D idcode                         3  0
006800060211
006900060211     D*--------------------------------------
007000060213     D* Prototype for procedure: validlitem
007100060211     D*--------------------------------------
007200060213     D validlitem      PR              n
007300060213     D itemnumber                     6
007400060213
007500060213     D product_enq     pr                  extpgm('WENQ010R')
007600060213     D product                        6
007700060215
007800120203     D general_enq     pr                  extpgm('WENQ001R04')
007900120203     D                                6    options(*nopass)
008000120203
008100120203     D general_enq2    pr                  extpgm('WENQ001R02')
008200120203     D                                6    options(*nopass)
008300060215
008400160413     Dlst_commitems    PR                  extpgm('PAYENQ07R')
008500060321     D                                6
008600060211
008700040129     Dvalidate_rtn     PR              n
008800060324
008900060324     Ddelete_rtn       PR              n
009000060324     D                               75
009100060413
009200060413     D  Uomval         pr              n
009300060413     D out_qty                       14  4
009400060413     D conuom                         2    const
009500060413     D frmuom                         2    const
009600060413     D frmqty                        11  2 const
009700060413
009800060413     D check_UomUse    pr              n
009900060418
010000060418     D*--------------------------------------
010100060418     D* Prototype for procedure: build_query
010200060418     D*--------------------------------------
010300060418     D build_query     PR
010400060418
010500060418     D*--------------------------------------
010600060418     D* Prototype for procedure: close_file
010700060418     D*--------------------------------------
010800060418     D close_file      PR
010900060418
011000060418     D*--------------------------------------
011100060418     D* Prototype for procedure: open_file
011200060418     D*--------------------------------------
011300060418     D open_file       PR
011400060418
011500060418     D*--------------------------------------
011600060418     D* Prototype for procedure: read_file
011700060418     D*--------------------------------------
011800060418     D read_file       PR              n
011900060413
012000151009
012100151009
012200151009             FindCaller(Var:VarLen:'CSTK0100':JobIdInf
012300151009                         :'JIDF0100':ApiErr);
012400151009               For i = 1 to EntryCount;
012500151009                  Entry = %subst(Var:Offset + 1);
012600151009                  Caller = %trim(PgmLib) + '/' + %trim(PgmNam);
012700151009                  If (PgmNam <> @pgm and WhoCalled = *blanks);
012800151009                     WhoCalled = PgmNam;
012900151009                  Endif;
013000151009                  Offset = Offset + EntryLen;
013100151009               Endfor;
013200151009
013300151009            // Program CANNOT be called independently
013400151009            if WhoCalled <> 'PAYMNT12R';
013500151009              *inlr = *on;
013600151009              return;
013700151009           endif;
013800151009
013900151009
014000060211
014100060211         exsr  aainit;
014200060211
014300040120         //**************************
014400040120         //*** Start of Program   ***
014500040120         //**************************
014600060211           dow  0 = 0;
014700060211              exfmt  fmt1;
014800060211
014900060211              errmsg = *blanks;
015000060211
015100060211             // end program
015200060211               if  *in03;
015300060211                  *inlr =  *on;
015400060211                  return;
015500060211               endif;
015600060211
015700060213             // F4=Product Enquiry
015800060213               if  *in04;
015900060321                  lst_commitems(item#);
016000060211                  iter;
016100060211               endif;
016200060211
016300060213              // validate Item
016400060213                if  %subst(item#:1:1) <> '@';
016500060213                     errmsg = get_err_msg('DIH0001');
016600060213                     iter;
016700060213                endif;
016800040120
016900060211              exsr  load_records;
017000060418              build_query();
017100160412              validate_rtn();
017200060211
017300060211              leave;
017400060211           enddo;
017500030902
017600040120         //************************
017700040120         //* Body of Program    ***
017800040120         //************************
017900030927             dow 0 = 0;
018000040120               write footer;
018100060211               exfmt sflctl;
018200060211
018300060211                 errmsg = *blanks;
018400060403
018500040120           // F3=Program End
018600030902                  if *in03;
018700030902                   *inlr = *on;
018800030902                     return;
018900030902                  endif;
019000060211
019100060211           // F2= Go back
019200060213                  if  *in02;
019300060211                    leave;
019400060211                  endif;
019500031111
019600040120           // F10=Add New Line
019700060321                  if  *in10;
019800030930                    exsr add_blank_line;
019900060211                    iter;
020000030930                  endif;
020100060324
020200060324           // F20=Delete item
020300060325                  if  *in22;
020400060324                    if  delete_rtn(errmsg);
020500060324                      iter;
020600060324                    endif;
020700060324                    commit;
020800060324                    leave;
020900060324                  endif;
021000030930
021100060213           // F4= Item Enquiry
021200060321                  if  *in04;
021300060213                    select;
021400060213                     when type# = 0;
021500060220                       // Product Code
021600120203                       product_enq(litem#);
021700060213                     when type# = 1;
021800060220                       // Product Group
021900120203                       general_enq2();
022000060213                     when type# = 2;
022100060220                       // Product Type
022200120203                       general_enq();
022300060215
022400060213                    endsl;
022500060211                    iter;
022600060211                  endif;
022700030902
022800040129           // Validate routine
022900040129                  if  validate_rtn;
023000040129                    iter;
023100040129                  endif;
023200030902
023300040120           // F15=Confirm
023400030927                  if *in15;
023500060211                    exsr upd_rtn;
023600060324                    commit;
023700060211                    leave;
023800030927                  endif;
023900030902
024000040120                enddo;
024100030902
024200040120          //****************************
024300040120          //**Update Master File      **
024400040120          //****************************
024500060211               begsr upd_rtn;
024600030902
024700060211                 cnt = rrn;
024800030902
024900031111                 for  y = 1 to cnt;
025000030902
025100060211                   chain y sflrec;
025200030902
025300060220                   chain (item#:litem@) cmifmt;
025400031111
025500060213                   cmipcod = item#;
025600060213                   cmiapcod = litem#;
025700060213                   cmiuser = @user;
025800060213                   cmidate = date#;
025900060213                   cmitime = time#;
026000060220                   cmidesc = itemdesc;
026100060220                   cmicode = type#;
026200060403                   cmidollarf = dollarf#;
026300060403                   cmiconvqty = convqty#;
026400060403                   cmiconvuom = convuom#;
026500031111
026600060211                   if not(%found);
026700030902
026800060211                     // Write Record
026900060213                     if  litem# <> *blanks;
027000060213                       write cmifmt;
027100060211                     endif;
027200060211                    else;
027300060211                    select;
027400060213                     when litem# = *blanks;
027500060213                       delete cmifmt;
027600060211                     other;
027700060211                    // Update Record
027800060213                       update cmifmt;
027900060211                    endsl;
028000060211                   endif;
028100030902
028200030927             endfor;
028300030927           endsr;
028400030902
028500040120         //***************************************
028600040120         //** Sub-routine to add a blank record
028700040120         //***************************************
028800040120             begsr add_blank_line;
028900030927
029000060211               clear sflrec;
029100060211               rrn += 1;
029200060211               write sflrec;
029300030927
029400030927           endsr;
029500060413
029600060211         //************************************************                 ***
029700060211         //** subroutine to retrieve header information
029800060211         //************************************************                 ***
029900060211           begsr aainit;
030000060211             coname = get_coname();
030100060213             @date  = get_sys_date(date#:time#);
030200060211             @title = arr(1);
030300060211           endsr;
030400060211
030500040120         //***************************************
030600040120         //** Sub-routine to load subfile records
030700040120         //***************************************
030800030902              begsr load_records;
030900031111
031000060211                *in33 = *on;
031100060211                write sflctl;
031200060211                rrn = *zeros;
031300030902                *in33 = *off;
031400060324                *in55 = *off;
031500030902
031600060220                 setll (item#) cmifmt;
031700060324                 reade(n) (item#) cmifmt;
031800030902
031900060220                 // Select Linked Item Records
032000060220                 dow not(%eof);
032100031111
032200060220                  // write to subfile
032300060213                  litem# = cmiapcod;
032400060220                  itemdesc = cmidesc;
032500060220                  type#  = cmicode;
032600060403                  dollarf# = cmidollarf;
032700060403                  convqty#  = cmiconvqty;
032800060403                  convuom#  = cmiconvuom;
032900060403
033000060220                  rrn += 1;
033100060220                  litem_arr(rrn) = cmiapcod;
033200031111
033300060220                  // load hidden fields
033400060220                  litem@ = cmiapcod;
033500031111
033600060211                  write sflrec;
033700030902
033800060324                  reade(n) (item#) cmifmt;
033900031111
034000030927                 enddo;
034100040120
034200030927               exsr add_blank_line;
034300060211               *in20 = rrn > 0;
034400060324               *in55 = *on;
034500030902
034600030902             endsr;
034700030902
034800060413         //********************************
034900060413         //**  Validate screen entries   **
035000060413         //********************************
035100060413      /end-free
035200040129     Pvalidate_rtn     b
035300040129     **
035400040129     Dvalidate_rtn     pi              n
035500040129     **
035600040129     Derror            s               n
035700060220     Dworkfield        s              2  0
035800040129      /free
035900040129
036000060211              cnt = rrn;
036100060211              *in99 = *on;
036200060211              error = *off;
036300060213              clear  litem_arr;
036400060413
036500060413             // Ensure UOM Valid
036600060418                 if convuom# <> *blanks;
036700060418                  chain convuom# uomfmt;
036800060413                     if  not %found;
036900060413                     errmsg = msg5;
037000060413                     error = *on;
037100060413                    endif;
037200060413
037300060413                // check if item  for UOM conversion has valid Units
037400060418                  if  convqty# = *zeros;
037500060413                     errmsg = msg7;
037600060413                     error = *on;
037700060413                  endif;
037800060413
037900060413                 endif;
038000040129
038100060211              for  y = 1 to cnt;
038200040129
038300060211              // Set off Indicator used
038400060213                *in34 = *off;
038500040129
038600060211                chain y sflrec;
038700040129
038800060213              // Validation -if Item number used
038900060213                 if  litem# <> *blanks;
039000040129
039100060220                   monitor;
039200060220
039300060213                    select;
039400060213                     when type# = 0;
039500060213                       error = get_prod_info('RD':litem#:prod_struc);
039600060213                     when type# = 1;
039700060220
039800060220                       // validate Product group
039900060220                       workfield = %int(litem#);
040000060220                       chain workfield pgrpfmt;
040100060220                       If not %found;
040200060220                         error = *on;
040300060220                       Endif;
040400060418
040500060213                     when type# = 2;
040600060220                        // validate Product type
040700060220                       workfield = %int(litem#);
040800060220                       chain workfield ptypfmt;
040900060220                       If not %found;
041000060220                         error = *on;
041100060220                       endif;
041200060213                    endsl;
041300060220
041400060220                   on-error;
041500060220                     errmsg = msg4;
041600060220                     *in34 = *on;
041700060220                     error = *on;
041800060220                   endmon;
041900040129
042000060213                    if  error;
042100060211                       errmsg = msg1;
042200060213                       litemdesc = *blanks;
042300060211                       *in34 = *on;
042400060211                      else;
042500060220                      select;
042600060220                       when type# = 0;
042700060220                        litemdesc = pddes;
042800060220                       when type# = 1;
042900060220                        litemdesc = pgnam;
043000060220                       when type# = 2;
043100060220                        litemdesc = ptynam;
043200060220                      endsl;
043300060211                    endif;
043400040129
043500060213                    if  %lookup(litem#:litem_arr) > 0;
043600060211                      errmsg = msg2;
043700060211                      *in34 = *on;
043800060211                      error = *on;
043900060211                     else;
044000060213                      litem_arr(y) = litem#;
044100060211                    endif;
044200060418
044300060418                    // check if item  for UOM conversion has valid Units
044400060418                    if convuom# <> *blanks;
044500060418                       if  check_uomUse();
044600060418                         *in34 = *on;
044700060418                         errmsg = msg6 + item;
044800060418                         error = *on;
044900060418                       endif;
045000060418                    endif;
045100060418
045200060211
045300060213                    if  validlitem(linkeditem);
045400060213                      errmsg = msg3 + linkeditem;
045500060211                      *in34 = *on;
045600060211                      error = *on;
045700060211                    endif;
045800060213                   else;
045900060213                    litemdesc = *blanks;
046000060211                  endif;
046100040129
046200060211                  update sflrec;
046300060211              endfor;
046400060412
046500060211
046600040129               return error;
046700160413
046800040129     Pvalidate_rtn     e
046900060211
047000060213     P*-----------------------------------------------------
047100060213     P* Procedure name: validlitem
047200060213     P* Purpose:        validate List items for current code
047300060213     P*-----------------------------------------------------
047400060213     P validlitem      B
047500060213     D validlitem      PI              n
047600060213     D someitem                       6
047700060211
047800060211      /free
047900060213           someitem = *blanks;
048000060211      /end-free
048100060213            // read file for other linked items
048200060211     c/Exec Sql
048300060213     C+  Select cmipcod
048400060213     C+      into :someitem
048500060211     C+
048600060213     C+  From WHCOMMITM                -- From "Linked" item file
048700060211     C+
048800060213     C+  Where cmiapcod = :litem#      -- linked item
048900060213     C+  and  cmipcod <> :item#        -- Primary item
049000060325     C+  and  cmicode =  :type#        -- item "type"
049100060211     C+
049200060211     C+
049300060211     c/End-Exec
049400060211      /free
049500060213          if  someitem <> *blanks;
049600060211             return *on;
049700060211            else;
049800060211             return *off;
049900060211          endif;
050000060211      /end-free
050100060211
050200060213     P validlitem      E
050300060324
050400060324     P*----------------------------------------------
050500060324     P* Procedure name: delete_rtn
050600060324     P* Purpose:        Delete items (if not in Use)
050700060324     P*----------------------------------------------
050800060324     P delete_rtn      B
050900060324     D delete_rtn      PI              n
051000060324     D errmsg                        75
051100060324
051200060324     D someitem        s              6
051300060324
051400060324     D msg1            c                   'Cannot Delete: Item in use'
051500060324
051600060324          // read file for other linked items
051700060324     c/Exec Sql
051800060324     C+  Select cmrpcod
051900060324     C+      into :someitem
052000060324     C+
052100060324     C+  From WHCOMMRM                 -- From Commission master
052200060324     C+
052300060324     C+  Where cmrpcod  = :item#       -- linked item
052400060324     C+
052500060324     C+
052600060324     c/End-Exec
052700060324      /free
052800060324          if  someitem <> *blanks;
052900060324             errmsg = msg1;
053000060324             return *on;
053100060324          endif;
053200060324      /end-free
053300060324
053400060324     c/Exec Sql
053500060324     C+ DELETE FROM WHCOMMITM WHERE cmipcod = :item#
053600060324     c/End-Exec
053700060324
053800060324      /free
053900060324            return *off;
054000060324      /end-free
054100060324
054200060324     P delete_rtn      E
054300060413
054400060413     P*--------------------------------------------
054500060413     P* Procedure name: Uomval
054600060413     P* Purpose:  validate issuing Unit o' Mrasure
054700060413     P* Returns:        *boolean
054800060413     P* Parameter:      errmsg
054900060413     P*--------------------------------------------
055000060413     P Uomval          B
055100060413     D Uomval          PI              n
055200060413     D out_qty                       14  4
055300060413     D conuom                         2    const
055400060413     D frmuom                         2    const
055500060413     D frmqty                        11  2 const
055600060413
055700060413      /free
055800060413
055900060413           out_qty = perform_uconv(frmuom:conuom:frmqty);
056000060413
056100060418
056200060413           //if combi not found seton alarm
056300060413             if  out_qty = 0;
056400060413                return *on;
056500060413               else;
056600060413                return  *off;
056700060413             endif;
056800060413
056900060413      /end-free
057000060413     P Uomval          E
057100060413
057200060413     P*-------------------------------------------------
057300060413     P* Procedure name: Check_UomUSE
057400060413     P* Purpose:  Check if UOM can be used for all items
057500060413     P* Returns:        *boolean
057600060413     P* Parameter:      *none
057700060413     P*-------------------------------------------------
057800060418     P Check_UomUse    B
057900060418     D Check_UomUse    PI              n
058000060418
058100060418     D out_qty         s             14  4
058200060413
058300060413      /free
058400060418          open_file();
058500060418
058600060418          dow not read_file();
058700060418
058800060413                 // Calculating using standard volume
058900060413                if  pdsize > *zeros;
059000060413                   // Get Product size
059100060413                 if  not get_size_info('RD':pdsize:size_struc);
059200060413                    // Get UOM conversion for calcs.
059300060418                  if  uomval(out_qty:convuom#:siuom:(siqty * pdsqty));
059400060413
059500060418                    if   uomval(out_qty:convuom#:sialtuom:
059600060418                                               (sialtqty * pdsqty));
059700060418                       close_file();
059800060413                       return *on;
059900060413                    endif;
060000060413                  endif;
060100060413                  else;
060200060418
060300060418                  close_file();
060400060413                  return  *on;
060500060413                 endif;
060600060413                 else;
060700060418
060800060418                 close_file();
060900060413                 return  *on;
061000060413                endif;
061100060413
061200060413          enddo;
061300060418
061400060418          close_file();
061500060413          return *off;
061600060413      /end-free
061700060413     P check_UomUse    E
061800060418
061900060418     P*--------------------------------------
062000060418     P* Procedure name: build_query
062100060418     P* Purpose:        Build Query
062200060418     P* Returns:        *none
062300060418     P* Parameter:      *none
062400060418     P*--------------------------------------
062500060418     P build_query     B
062600060418     D build_query     PI
062700060418
062800060418      /free
062900060418
063000060418           Qryselect = 'SELECT pdpcod, pdsize , ' +
063100060418                      'pdsqty ' +
063200060418
063300060418                      ' From WHPDTL01';
063400060418
063500060418                  select;
063600060418                    when type# = 0;
063700060418                       qryselect = %trim(qryselect) +
063800060418                           ' where pdpcod = ?';
063900060418                    when type# = 1;
064000060418                       qryselect = %trim(qryselect) +
064100060418                           ' where pdgrp = ?';
064200060418                    when type# = 2;
064300060418                       qryselect = %trim(qryselect) +
064400060418                           ' where pdtyp = ?';
064500060418                  endsl;
064600160413
064700060418     P build_query     E
064800060418
064900060418     P*--------------------------------------
065000060418     P* Procedure name: open_file
065100060418     P* Purpose:        open database file
065200060418     P* Returns:        *none
065300060418     P* Parameter:      *none
065400060418     P*--------------------------------------
065500060418     P open_file       B
065600060418     D open_file       PI
065700060418
065800160412
065900060418     C/Exec Sql
066000060418     C+  Declare RMFILE Cursor For DYNSTMT
066100060418     C/End-Exec
066200060418      *
066300060418     C/Exec Sql
066400060418     c+    prepare DYNSTMT from :qryselect
066500060418     C/End-Exec
066600060418      *
066700060418     C/Exec Sql
066800060418     C+ OPEN RMFILE
066900060418     C+     using :litem#
067000060418     C/End-Exec
067100060418      *
067200060418     P open_file       E
067300060418
067400060418     P*--------------------------------------
067500060418     P* Procedure name: read_file
067600060418     P* Purpose:        read transaction file
067700060418     P* Returns:        error indicator
067800060418     P* Parameter:      errmsg => error message text
067900060418     P*--------------------------------------
068000060418     P read_file       B
068100060418     D read_file       PI              n
068200060418
068300060418     D*   Read the records ...
068400060418
068500060418     C/EXEC SQL
068600060418     C+ FETCH NEXT FROM RMFILE INTO :Item,
068700060418     C+          :pdsize, :pdsqty
068800060418     c+
068900060418     C/END-EXEC
069000060418      /free
069100060418
069200060418          // For every record in the file ....
069300060418          // ... while a record was successfully read ...
069400060418            if SQLSTT = '02000';
069500060418               return *on;
069600060418              else;
069700060418               return *off;
069800060418            endif;
069900060418
070000060418      /end-free
070100060418
070200060418     P read_file       E
070300060418
070400060418     P*--------------------------------------
070500060418     P* Procedure name: close_file
070600060418     P* Purpose:        close transaction file
070700060418     P* Returns:        *none
070800060418     P* Parameter:      *none
070900060418     P*--------------------------------------
071000060418     P close_file      B
071100060418     D close_file      PI
071200060418
071300060418      *    ----------
071400060418     C/Exec Sql Close RMFILE
071500060418     C/End-Exec
071600060418      *
071700060418     P close_file      E
071800030902
071900030902**  Program Title
072000060213     Commission Control - Linked Items
