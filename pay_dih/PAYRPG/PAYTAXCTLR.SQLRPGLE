000100111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300161129      //*********************************************************
000400161129      //                   DISERV  SYSTEM                       *
000500161129      //                   PAYROLL SYSTEM                       *
000600161129      //                                                        *
000700161129      //                         BY                             *
000800161129      //                                                        *
000900161129      //             Banks DIH I.S. department                  *
001000161129      //             P.O BOX 10194, GEORGETOWN,GUYANA           *
001100161129      //
001200161129      //                                                        *
001300161129      //                                                        *
001400161129      //       RPG PROGRAM NAME..: PAYTAXCTLR                   *
001500161129      //                                                        *
001600161129      //       PROGRAMMER NAME: Samuel Britton                  *
001700161129      //                        Christopher Williams            *
001800161129      //       DESIGN DATE....: 2001/02/08                      *
001900161129      //       MODULES........: PAYTAXCTL1                      *
002000161129      //       INDICATORS.....:                                 *
002100161129      //                                                        *
002200161129      //                                                        *
002300161129      //  THIS PROGRAM IS DESIGNED  to configure  parameters    *
002400161129      //                            for tax calculation         *
002500161129      //                                                        *
002600161129      //*********************************************************
002700011106     Fpaytaxctl uf a e           k disk    commit
002800011102     Fpaytaxctl2uf a e           k disk    commit
002900010607     Fpaycycd   if   e           k disk
003000011106     Fpaycon    if   e             disk
003100011106     Fpaytaxctldcf   e             workstn
003200111002     F/IF DEFINED(WEB)
003300111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003400111002     F/ENDIF
003500161129      //
003600111002      /copy QCPYSRC,genPR
003700111002      /copy QCPYSRC,PAYPR
003800980319     D                sds
003900980917     D  @pgm             *proc
004000111002     D  @user                358    367
004100161129
004200161129         // Configure Tax Ranges
004300161129       Dcl-Pr Tax_Ranges extpgm('PAYTAXCTL1');
004400161129         cycle           Char(2)         const;
004500161129         desc            Char(30)        const;
004600161129         type            Char(1)         const;
004700161206         range_type      Packed(1)       const options(*nopass);
004800161129       End-Pr;
004900161129
005000161130
005100161130     DtaxRange         ds                  dim(2) qualified inz
005200161130     D range_cnt                      3  0
005300161130     D range_cde                      1
005400161130     D ranges                              dim(100) likeds(range_ds)
005500161130
005600161130     Drange_ds         ds                  template
005700011105     D range_lw                      11  2
005800011105     D range_up                      11  2
005900011105     D range_per                      5  3
006000161129
006100011105     D arr             S             40    dim(1) ctdata PERRCD(1)
006200011102     D year_factor     s                   like(cycyearf)
006300011106     D StrucMax        s              3P 0 Inz(5)
006400011105     D Memory          s              5P 0
006500011105     D xcnt            s              3  0 inz(0)
006600011105     D ycnt            s              3  0
006700161130     D zcnt            s              3  0
006800161130     D inCode          s              1
006900011107     D save_rngeUp     s             11  2
007000161129
007100011102     DMsg1             C                   'Invalid Cycle Code !'
007200161130     DMsg2             C                   'Type Must have a valid Entry'
007300161130     DMsg3             C                   'Tax Percent Invalid'
007400161130     DMsg4             C                   'Default Tax Range Found !'
007500161130     DMsg5             C                   'Dynamic Tax Range Found !'
007600161129
007700000407
007800161129      // Iniz routine
007900111002      /copy QCPYSRC,aainit
008000161129
008100161129      //*****************
008200161129      // Program Start
008300161129      //*****************
008400161129
008500161129       dow 0 = 0;
008600161130
008700161129         exfmt fmt1;
008800161129
008900161130         errmsg = *blanks;
009000161130
009100161129         // end of program
009200161129         if *in03;
009300161129           rolbk;
009400161129           *inlr = *on;
009500161129           return;
009600161129         endif;
009700161129
009800161129
009900161129         // Pay cycle code enquiry
010000161129         if *in05;
010100161129
010200011102     C                   call      'PAYENQ04R'
010300161129      //                  PARM      *blanks       type              1
010400161129           iter;
010500161129         endif;
010600161129
010700161129         // validate Pay cycle code
010800161129
010900161129         chain ptxcyc# cycfmt;
011000161129         if not %found;
011100161129           errmsg = msg1;
011200161129           iter;
011300161129         endif;
011400161129
011500161129         desc# = cycdesc;
011600161129         year_factor = cycyearf;
011700161129
011800161129         chain ptxcyc# ptxfmt;
011900161129         if not %found;
012000161129           ptxfree# = *zeros;
012100161129           ptxdftper# = *zeros;
012200161129           ptxper# = *zeros;
012300161129           ptxtaxper# = *zeros;
012400161129           ptxtype1# = *zeros;
012500161206           ptxdrngty# = *zeros;
012600161129         else;
012700161129           ptxfree# = ptxfree;
012800161129           ptxdftper# = ptxdftper;
012900161129           ptxper# = ptxper;
013000161129           ptxtaxper# = ptxtaxper;
013100161129           ptxtype1# = ptxtype1;
013200161206           ptxdrngty# = ptxdrngtyp;
013300161129         endif;
013400161129
013500161129         leave;
013600161129       enddo;
013700161129
013800161129       //*******************
013900161129       // Body of Program **
014000161129       //*******************
014100161129       dow 0 = 0;
014200161130         //setup text to indicate ranges
014300161130         if get_range_code('1') = '1';   //default tax calc.
014400161130           dftrng_txt = Msg4;
014500161130           else;
014600161130           dftrng_txt = *blanks;
014700161130         endif;
014800161130
014900161130         if get_range_code('2') = '2';   //default tax calc.
015000161130           dynrng_txt = Msg5;
015100161130           else;
015200161130           dynrng_txt = *blanks;
015300161130         endif;
015400161130
015500161130
015600161129         exfmt fmt2;
015700161129
015800161130         errmsg = *blanks;
015900161130
016000161129         // Go Back
016100161129         if *in02;
016200161129           rolbk;
016300161129           leave;
016400161129         endif;
016500161129
016600161129         // End of Program
016700161129         if *in03;
016800161129           rolbk;
016900161129           *inlr = *on;
017000161129           return;
017100161129         endif;
017200161130
017300161130         // Validate dynamic Tax settings
017400161130         if ptxper# <> *zeros;
017500161130            // 1 = taxable gross, 2 = basic wage
017600161130            if ptxtype1# <> 1
017700161130              and ptxtype1# <> 2;
017800161130              errmsg = Msg2;
017900161130              iter;
018000161130            endif;
018100161130            // ensure Tax rate configured
018200161130            if ptxtaxper# <= *zeros;
018300161130              errmsg = Msg3;
018400161130              iter;
018500161130            endif;
018600161130            else;
018700161130            rangeflg2 = *blanks;
018800161130         endif;
018900161129
019000161130         if rangeflg = 'Y'
019100161129           and rangeflg2 = 'Y';
019200161129            rangeflg2 = *blanks;
019300161129         endif;
019400161206
019500161206         if ptxdrngty# <= *zeros
019600161206           or ptxdrngty# > 2;
019700161206            ptxdrngty# = 1;
019800161206         endif;
019900161129
020000010607
020100161129         // F15 Confirm transaction
020200161129         if *in15;
020300161129           exsr confirm_rtn;
020400161129           exsr trn_upd_rtn;
020500161129           commit;
020600161129           leave;
020700161129         endif;
020800161129
020900161129         if rangeflg = 'Y';
021000161206           tax_ranges(ptxcyc#:desc#:'1');
021100161130           rangeflg = *blanks;
021200161129         endif;
021300161129
021400161129         if rangeflg2 = 'Y';
021500161206           tax_ranges(ptxcyc#:desc#:'2':ptxdrngty#);
021600161130           rangeflg2 = *blanks;
021700161129         endif;
021800161129
021900161129       enddo;
022000161129
022100161129       //***************************************************************
022200161129       // Begin subroutine to Confirm record entry/update
022300161129       //***************************************************************
022400161129       begsr Confirm_rtn;
022500161129
022600161129         ptxfree = round_up(ptxfree#:'A');
022700161129         ptxdftper = ptxdftper#;
022800161129         ptxper = ptxper#;
022900161129         ptxtaxper = ptxtaxper#;
023000161129         ptxtype1  = ptxtype1#;
023100161206         ptxdrngtyp = ptxdrngty#;
023200161129         ptxrngcde = get_range_code('1');   //default tax calc.
023300161129         ptxrngcde2 = get_range_code('2');  //dynamic tax calc.
023400161129
023500161129         if %found(paytaxctl);
023600161129           update ptxfmt;
023700161129         else;
023800161129           ptxcyc = ptxcyc#;
023900161129           write ptxfmt;
024000161129         endif;
024100161129
024200161129       endsr;
024300161129
024400161129       //***************************************************************
024500161129       // Update All transactions
024600161129       //***************************************************************
024700161129       begsr trn_upd_rtn;
024800161129
024900161129
025000161129         setll 1 paycon;
025100161129         read paycon;
025200011102
025300161129         if pcautotax = 'Y';
025400011105
025500161129           exsr get_range;
025600161129           setll *loval cycfmt;
025700161129           read cycfmt;
025800161129           dow not %eof;
025900161129             if cyccode <> ptxcyc#;
026000011105
026100161129               // Base setup  *******************
026200161129               chain cyccode ptxfmt;
026300161129               ptxfree = round_up((ptxfree# *
026400161129                   year_factor / cycyearf):'A');
026500161129               ptxdftper = ptxdftper#;
026600161129               ptxper = ptxper#;
026700161129               ptxtaxper = ptxtaxper#;
026800161129               ptxtype1  = ptxtype1#;
026900161206               ptxdrngtyp = ptxdrngty#;
027000161129               ptxrngcde = get_range_code('1');   //default tax calc.
027100161129               ptxrngcde2 = get_range_code('2');  //dynamic tax calc.
027200161129
027300161129               if %found;
027400161129                 update ptxfmt;
027500161129               else;
027600161129                 ptxcyc = cyccode;
027700161129                 write ptxfmt;
027800161129               endif;
027900011105
028000161130               // Range calcs.
028100161130               //--------------
028200161129               ycnt = *zeros;
028300161129               save_rngeUp = *zeros;
028400161129
028500161130               for zcnt =  1 to 2; //two types set( dft. n dynamic)
028600161130
028700161201
028800161201                 if taxRange(zcnt).range_cnt = *zeros;
028900161201                   setll (taxRange(zcnt).range_cde:cyccode) ptx2fmt;
029000161201                   reade (taxRange(zcnt).range_cde:cyccode) ptx2fmt;
029100161201                   dow not(%eof);
029200161201                    delete ptx2fmt;   //use sql
029300161201                    reade (taxRange(zcnt).range_cde:cyccode) ptx2fmt;
029400161201                   enddo;
029500161201                 endif;
029600161201
029700161130                 for ycnt =  1 to taxRange(zcnt).range_cnt;
029800161129
029900161130
030000161130                   chain (taxRange(zcnt).range_cde:cyccode:ycnt) ptx2fmt;
030100161129
030200161129
030300161130                   ptx2per = taxRange(zcnt).ranges(ycnt).range_per;
030400161130                   ptx2rngcde = taxRange(Zcnt).range_cde;
030500161129
030600161130                   //default Tax calcs.
030700161206                   if ptx2rngcde = '1'
030800161206                     or ptx2rngcde = '2'
030900161206                     and ptxdrngtyp = 1;
031000161130                      select;
031100161130                        when ycnt = 1;
031200161130                          ptx2qtyl = round_up((
031300161130                             taxRange(zcnt).ranges(ycnt).range_lw *
031400161130                            year_factor / cycyearf):'A');
031500161130                      other;
031600161130                          ptx2qtyl = save_rngeUp + 1;
031700161130                      endsl;
031800161129
031900161130                      if taxRange(zcnt).ranges(ycnt).range_up <> 999999999.00;
032000161130                         ptx2qtyu = round_up((
032100161130                             taxRange(zcnt).ranges(ycnt).range_up *
032200161130                            year_factor / cycyearf):'A');
032300161130                        else;
032400161130                         ptx2qtyu =
032500161130                             taxRange(zcnt).ranges(ycnt).range_up;
032600161130                      endif;
032700161130                      save_rngeUP = ptx2qtyu;
032800161129
032900161130                     else;
033000161129
033100161130                      //dynamic Tax calcs. (percent driven)
033200161130                      ptx2qtyl =
033300161130                             taxRange(zcnt).ranges(ycnt).range_lw;
033400161130                      ptx2qtyu =
033500161130                             taxRange(zcnt).ranges(ycnt).range_up;
033600161129
033700161130                   endif;
033800161129
033900161130                   if %found;
034000161130                     update ptx2fmt;
034100161130                   else;
034200161130                     ptx2cyc = cyccode;
034300161130                     ptx2line# = ycnt;
034400161130                     write ptx2fmt;
034500161130                   endif;
034600161130
034700161130                 endfor;
034800161130
034900161129               endfor;
035000161129
035100161129             endif;
035200161129             read cycfmt;
035300161129           enddo;
035400011102
035500161129         endif;
035600161129       endsr;
035700161129
035800161129       //*********************************
035900161129       // Get range for changed Record  **
036000161129       //*********************************
036100161129       begsr get_range;
036200161130
036300161129         // Set up initial memory allocation for Struc of StrucMax (5) elements
036400161130       //Memory = %Size(range_ds) * StrucMax;
036500161130     C*                  Alloc     Memory        Struc@
036600161129
036700161130         for zcnt = 1 to 2;
036800161130
036900161130           reset xcnt;
037000161130           select;
037100161130             when zcnt = 1;
037200161130              inCode =  get_range_code('1');
037300161201              taxRange(zcnt).range_cde = '1';
037400161130             when zcnt = 2;
037500161130              inCode =  get_range_code('2');
037600161201              taxRange(zcnt).range_cde = '2';
037700161130           endsl;
037800161201
037900161130
038000161130           setll (inCode:ptxcyc#) ptx2fmt;
038100161130           reade (inCode:ptxcyc#) ptx2fmt;
038200161130           dow not(%eof);
038300161130             xcnt += 1;
038400161129
038500161130             If xcnt > StrucMax;
038600161130               StrucMax += 1;
038700161130             //Memory = %Size(Range_ds) * StrucMax;
038800161130             EndIf;
038900161129
039000161130             taxRange(zcnt).ranges(xcnt).range_lw
039100161130                     =  ptx2qtyl;
039200161130             taxRange(zcnt).ranges(xcnt).range_up
039300161130                      = ptx2qtyu;
039400161130             taxRange(zcnt).ranges(xcnt).range_per
039500161130                       = ptx2per;
039600161130             taxRange(zcnt).range_cnt = xcnt;
039700161129
039800161130
039900161130             reade (inCode:ptxcyc#) ptx2fmt;
040000161130           enddo;
040100161130
040200161130         endfor;
040300161130
040400161129       endsr;
040500161129
040600161129      //................................//
040700161129      //Procedure to get Range Code    //
040800161129      //...............................//
040900161129       Dcl-Proc get_range_code;
041000161129         Dcl-Pi get_range_code Char(1);
041100161129           in_code Char(1) const;
041200161129         End-pi;
041300161129         Dcl-S  rec_count  Packed(3);
041400161129
041500161129         exec sql
041600161129            select count(*) into :rec_count from paytaxctl2
041700161129               where ptx2cyc = :ptxcyc and ptx2rngcde = :in_code;
041800161129
041900161129          if rec_count = *zeros;
042000161129               return *blanks;
042100161129             else;
042200161129              return in_code;
042300161129           endif;
042400161129
042500161129       End-Proc get_range_code;
042600161129
042700161129
042800980917**   Header to be displayed
042900010607        Tax Configuration
