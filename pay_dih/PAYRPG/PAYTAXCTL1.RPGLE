000100111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300161129      //* *******************************************************
000400161129      //                   DISERV SYSTEMS.                      *
000500161129      //                   PAYROLL SYSTEM                       *
000600161129      //                                                        *
000700161129      //                         BY                             *
000800161129      //                                                        *
000900161129      //             Banks DIH I.S. department                  *
001000161129      //             P.O BOX 10194, GEORGETOWN,GUYANA           *
001100161129      //                                                        *
001200161129      //                                                        *
001300161129      //       RPG PROGRAM NAME..: PAYTAXCTL1                   *
001400161129      //                                                        *
001500161129      //       PROGRAMMER NAME: SAMUEL BRITTON                  *
001600161129      //                        CHRISTOPHER WILLIAMS            *
001700161129      //       DESIGN DATE....: 2001/06/07                      *
001800161129      //       MODULES........: *NONE                           *
001900161129      //       INDICATORS.....:                                 *
002000161129      //                                                        *
002100161129      //                                                        *
002200161129      //  THIS PROGRAM IS DESIGNED TO MAINTAIN TAX CALCULATION  *
002300161129      //  RANGES.                                               *
002400161129      //                                                        *
002500161129      //*********************************************************
002600010607     Fpaytaxctl2uf a e           k disk    commit
002700010607     Fpaytaxctd1CF   e             workstn
002800010607     F                                     SFILE(sub1:rrn1)
002900111002     F/IF DEFINED(WEB)
003000111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003100111002     F/ENDIF
003200161129
003300111002      /copy QCPYSRC,genPR
003400010607     D ARR             s             40    DIM(1) CTDATA PERRCD(1)
003500161129     D rngFlag         s              1
003600161206     D rngType         s              1  0
003700010607     D rrn1            s              4  0
003800010607     D rrx             s                   like(rrn1)
003900010607     D xcnt            s              4  0
004000161201     D ptx2qtyu#_prv   s                   like(ptx2qtyu#)
004100010607     D error_found     s               n   inz(*off)
004200161201
004300010607     DMsg1             c                   CONST('% cannot be zeros !')
004400010607     DMsg2             c                   CONST('Lower limit is Greater than t-
004500010607     D                                     he Upper limit ')
004600010607     DMsg3             c                   CONST('Lower limit is Greater than t-
004700010607     D                                     he Upper limit of the previous recor-
004800010607     D                                     d')
004900161201     DMsg4             c                   'Limits entered must represent a -
005000161201     D                                     percentage not greater than 100'
005100161129
005200000306     D                sds
005300000306     D  @pgm             *proc
005400161129     D  @user                358    367
005500161129
005600981005     C
005700010607     C     *entry        plist
005800010607     C                   parm                    ptxcyc#           2
005900010607     C                   parm                    desc#
006000161129     C                   parm                    rngFlag
006100161206     C                   parm                    rngType
006200000606     C
006300161129
006400161129       exsr aainit;
006500161129       // Clear subfile
006600161129       exsr clear_subfile;
006700161129
006800161130       chain (rngFlag:ptxcyc#) ptx2fmt;
006900161129       *IN30 = NOT %FOUND;
007000010607
007100161129       // LOAD subfile
007200161129       if *in30;
007300161129         exsr range_entry;
007400161129       else;
007500161129         exsr range_update;
007600161129       endif;
007700161129
007800161129       //***********************************
007900161129       // Main Calculation and Validation **
008000161129       //***********************************
008100161129       dow 0 = 0;
008200161129         write fmt1;
008300161129         exfmt subctl1;
008400161129         // clear error message field
008500161129         errmsg = *blanks;
008600161129
008700161129         // End program
008800161129         if *in03;
008900161129           *inlr = *on;
009000161129           return;
009100161129         endif;
009200161129
009300161129         // validate subfile
009400161129         exsr validate_sr;
009500161129         if error_found;
009600161129           iter;
009700161129         endif;
009800161129
009900010607
010000161129         // Add a new line
010100161129         if *in04;
010200161129           exsr new_line;
010300161129           iter;
010400161129         endif;
010500161129
010600161129         // Confirm Record Entry
010700161129
010800161129         if *in15;
010900161129           exsr delete_sr;
011000161129           exsr confirm;
011100161129           *inlr = *on;
011200161129           return;
011300161129         endif;
011400161129
011500161129       enddo;
011600161129
011700161129       //***************************************
011800161129       // subroutine to enter Discount Ranges **
011900161129       //***************************************
012000161129
012100161129       Begsr range_entry;
012200161129
012300161129         // Set Mode Description
012400161129         mode = 'ENTRY';
012500161129
012600161129         // write line to subfile
012700161129
012800161129         rrn1 += 1;
012900161129         write sub1;
013000161129
013100161129       endsr;
013200161129
013300161129       //**************************************
013400161129       // Subroutine to LOAD Tax Ranges      **
013500161129       //**************************************
013600161129       Begsr Range_update;
013700161129         // Set Mode Description
013800161129         mode = 'UPDATE';
013900161129
014000161130         setll (rngFlag:ptxcyc#) ptx2fmt;
014100161130         reade (rngFlag:ptxcyc#) ptx2fmt;
014200161129
014300161130         dow not(%eof);
014400010607
014500161129             ptx2qtyl# = ptx2qtyl;
014600161129             ptx2qtyu# = ptx2qtyu;
014700161129             ptx2per# = ptx2per;
014800161129
014900161129             rrn1 += 1;
015000161129             write sub1;
015100161129
015200161130           reade (rngFlag:ptxcyc#) ptx2fmt;
015300161129           *IN30 = %EOF;
015400161129         enddo;
015500000406
015600161129       endsr;
015700161129
015800161129       //******************************************
015900161129       // Subroutine to Clear subfile   **
016000161129       //******************************************
016100161129       begsr clear_subfile;
016200161129
016300161129         *in33 = *on;
016400161129         write subctl1;
016500161129         *in33 = *off;
016600161129         rrn1 = *zeros;
016700161129         clear sub1;
016800161129
016900161129       endsr;
017000161129
017100161129       //******************************************
017200161129       // Subroutine to add new line to subfile  **
017300161129       //******************************************
017400161129
017500161129       Begsr new_line;
017600161129
017700161129         clear sub1;
017800161129         rrn1 += 1;
017900161129         write sub1;
018000161129
018100161129       endsr;
018200161129
018300161129
018400161129       //****************************************
018500161129       // Subroutine to confirm changes made   **
018600161129       //****************************************
018700161129
018800161129       begsr Confirm;
018900161129
019000161129         rrx = rrn1;
019100161129
019200161129         for xcnt = 1 to rrx;
019300161129
019400161129           chain xcnt sub1;
019500161129           // write record not set for deletion
019600161129           if delopt <> 'X';
019700161129
019800010607
019900161129             ptx2qtyl = ptx2qtyl#;
020000161129             ptx2qtyu = ptx2qtyu#;
020100161129             ptx2per = ptx2per#;
020200161129             ptx2cyc = ptxcyc#;
020300161129             ptx2line# = xcnt;
020400161129             ptx2rngcde = rngFlag;
020500161129
020600161129             if ptx2qtyu <> *zeros;
020700161129               write ptx2fmt;
020800161129             endif;
020900161129           endif;
021000161129
021100161129         endfor;
021200161129
021300161129       endsr;
021400161129
021500161129
021600161129       //***************************************************************
021700161129       // Begin subroutine to Delete records before updating
021800161129       //***************************************************************
021900161129       BEGSR Delete_sr;
022000161129
022100161130         setll (rngFlag:ptxcyc#) ptx2fmt;
022200161130         reade (rngFlag:ptxcyc#) ptx2fmt;
022300161129
022400161129         dow not(%eof);
022500161129
022600161130            delete ptx2fmt;
022700161130           reade (rngFlag:ptxcyc#) ptx2fmt;
022800161129
022900161129         enddo;
023000161129
023100161129       endsr;
023200161129
023300161129       //********************
023400161129       // Validate Subfile **
023500161129       //********************
023600161129       BEGSR Validate_sr;
023700161129
023800161129         ptx2qtyu#_prv = *zeros;
023900161129         reset error_found;
024000161129
024100161129         // save subfile RRN
024200161129         rrx = rrn1;
024300161201
024400161201         for xcnt = 1 to rrx;
024500161129           chain xcnt sub1;
024600161129
024700161129           // validate record not set for deletion
024800161129           if delopt <> 'X';
024900161129
025000161129             // Check for blanks
025100161129             if ptx2per# = *zeros and ptx2qtyu# <> *zeros;
025200000406
025300161129               errmsg = msg1;
025400161129               error_found = *on;
025500161129             endif;
025600161129
025700161129
025800161129             // If lower range value is higher than upper range value
025900161129             if ptx2qtyl# > ptx2qtyu#;
026000161129               errmsg = msg2;
026100161129               error_found = *on;
026200161129             endif;
026300161129
026400161129             // If lower range value is lower than upper range value
026500161129             // of a previous record
026600161129             if ptx2qtyu#_prv <> *zeros
026700161129                   and ptx2qtyl# <= ptx2qtyu#_prv;
026800161129               errmsg = msg3;
026900161129               error_found = *on;
027000161129
027100161129             endif;
027200161201
027300161201             // If lower range or  upper range value is greater than 100
027400161201             //  for range type = '2'
027500161206             if rngFlag = '2'
027600161206                and  rngType = 2;
027700161201               if ptx2qtyu# > 100
027800161201                   or  ptx2qtyl# > 100;
027900161201                 errmsg = msg4;
028000161201                 error_found = *on;
028100161201
028200161201               endif;
028300161201             endif;
028400000406
028500161129             if error_found;
028600161129               update sub1;
028700161129               leave;
028800161129             endif;
028900161129             // save previous upper range record
029000161129             ptx2qtyu#_prv =  ptx2qtyu#;
029100161129           endif;
029200161129
029300161201         endfor;
029400161129         // Reset relative record number for apllication
029500161129         rrn1 = rrx;
029600161129
029700161129       endsr;
029800161129
029900161129       //***************************************************************
030000161129       // Begin subroutine to print header
030100161129       //***************************************************************
030200161129       BEGSR AAINIT;
030300161129         CONAME = get_coname;
030400161129         @DATE = get_sys_date;
030500161129
030600000306     C                   TIME                    @TIME             6 0
030700000306     C                   MOVEL     ARR(1)        @TITLE
030800000306     C
030900161129
031000161129       ENDSR;
031100161129
031200161129
031300000306**   Header to be displayed
031400010607        Tax Configuration (Range)
