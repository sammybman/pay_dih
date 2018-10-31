000100040622     H DATEDIT(*DMY)
000200111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300111002     H/define profoundui
000400020128     **********************************************************
000500020128     F*              B A N K S    D I H    L T D.              *
000600021011     F*                  M.I.S. DEPARTMENT                     *
000700020128     F*                                                        *
000800020128     F*                                                        *
000900040705     F*       Program Name......: PAYMNT0501                   *
001000020128     F*                                                        *
001100020128     F*       Created By.....: Allison Fung                    *
001200040705     F*                        S.Britton                       *
001300040621     F*       Date...........: 21.06.2004                      *
001400020128     F*                                                        *
001500020128     F*                                                        *
001600040621     F**      Description: Payroll Def. 2 Maintenance          *
001700020128     ***********************************************************
001800040621     Fpayrunt   uf a e           k disk
001900040705     Fpayrtypl01if   e           k disk    usropn
002000040622     Fpaymnt050dcf   e             workstn
002100040622     F                                     sfile(sub1:rrn)
002200111002     F/IF DEFINED(WEB)
002300111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002400111002     F/ENDIF
002500010301     **
002600040622     D err#            s             55
002700040622     D found_ind       s               n
002800040622     D rrn             s              4  0
002900110901     D Prun_type       s              1
003000040622     D xcnt            s              2  0
003100021018     **
003200040622     D msg1            c                   '% totals greater/less than 100 %'
003300040622     d
003400040622     **
003500010815     D                sds
003600040623     D  @PARM            *Parms
003700950711     D  @PGM             *PROC
003800111002     D  @USER                358    367
003900020128     d
004000111002     Dmain_prog        PR                  extpgm('PAYMNT0501')
004100110901     D                                1    const
004200040705     D                                2  0
004300040622     D                                2  0 const options(*nopass)
004400040622     **
004500040622     Dmain_prog        PI
004600110901     D runt#                          1    const
004700040705     D prno#                          2  0
004800040622     D payt#                          2  0 const options(*nopass)
004900021017     **
005000040622     D*--------------------------------------
005100040622     D* Prototype for procedure: ConfirmSr
005200040622     D*--------------------------------------
005300040622     D ConfirmSr       PR
005400040622
005500040622     D*--------------------------------------
005600040622     D* Prototype for procedure: LoadRecords
005700040622     D*--------------------------------------
005800040622     D LoadRecords     PR
005900040622
006000040622     D*--------------------------------------
006100040622     D* Prototype for procedure: CalculateTotals
006200040622     D*--------------------------------------
006300040623     DCalculateTotals  PR              n
006400040705
006500040705     D*--------------------------------------
006600040705     D* Prototype for procedure: GetRunType
006700040705     D*--------------------------------------
006800040705     DGetRunType       PR              n
006900040622
007000040622     D*--------------------------------------
007100040622     D* Prototype for procedure: DisplayRecords
007200040622     D*--------------------------------------
007300040622     D DisplayRecords  PR
007400040622
007500021017      /free
007600110901          prun_type = runt#;
007700110901
007800040705          if @parm = 2;
007900040705              exfmt win1;
008000040705
008100040705                 if confirm <> 'Y';
008200040705                    *inlr = *on;
008300040705                    return;
008400040705                 endif;
008500040705
008600040705            else;
008700040705             if runt# = *blanks;
008800040705               if getRunType();
008900040705                   *inlr = *on;
009000040705                   return;
009100040705                   else;
009200110901                    prun_type = run_type;
009300040705                    prno# = prtyprno;
009400040705               endif;
009500040705             endif;
009600040705          endif;
009700040622
009800040705              LoadRecords();
009900040622              DisplayRecords();
010000040622              return;
010100040622
010200040622      /end-free
010300040622
010400040622     P*--------------------------------------
010500040622     P* Procedure name: LoadRecords
010600040622     P* Purpose:        Load records to subfile
010700040622     P*--------------------------------------
010800040622     P LoadRecords     B
010900040622     D LoadRecords     PI
011000040622
011100040622      /free
011200040622       // Your calculation code goes here
011300040622                  *in33 = *on;
011400040622                    write subctl1;
011500040622                    rrn = 0;
011600040622                    *in33 = *off;
011700040622                    *in55 = *off;
011800040622
011900040622               for xcnt = 1 to prno#;
012000040623                if @parm = 2;
012100110901                  chain(n) (prun_type:xcnt) prunfmt;
012200040623                 elseif @parm = 3;
012300110901                  prunptype = payt#;
012400110901                  chain(n) (prun_type:xcnt:payt#) prunfmt;
012500040622                endif;
012600040622
012700040622              // Select Records
012800040623                  if %found(payrunt);
012900040622
013000040622             // write to subfile
013100040622
013200040622                     prunprno# = prunprno;
013300040622                     prunperc# = prunperc;
013400040622                     else;
013500040622                     prunprno# = xcnt;
013600040622                     prunperc# = *zeros;
013700040622                  endif;
013800040622
013900040622                   rrn += 1;
014000040622
014100040622                  write sub1;
014200040622
014300040622                  *in55 = *on;
014400040622                  *in20 = rrn > 0;
014500040622
014600040622               endfor;
014700040622
014800110901                prunrcode = prun_type;
014900040623                CalculateTotals();
015000040622
015100040622              RETURN;
015200040622      /end-free
015300040622     P LoadRecords     E
015400040622
015500040622     P*--------------------------------------
015600040622     P* Procedure name: DisplayRecords
015700040622     P* Purpose:        Display records of subfile
015800040622     P*--------------------------------------
015900040622     P DisplayRecords  B
016000040622     D DisplayRecords  PI
016100040622
016200040622      /free
016300040622       // Your calculation code goes here
016400040622                 dow 0=0;
016500040622                 exfmt subctl1;
016600040622
016700040622            // F2 = Cancel
016800040622                  if  *in02;
016900040622                   *inlr = *on;
017000040622                   return;
017100040622                  endif;
017200040623
017300040623            // calculate total / check for error
017400040623                if CalculateTotals();
017500040623                  iter;
017600040623                endif;
017700040623
017800040622            // F15 = Confirm Record
017900040622                  if *in15;
018000040622                    ConfirmSr();
018100040622                    *inlr = *on;
018200040622                    return;
018300040622                  endif;
018400040622
018500040622               enddo;
018600040622                 RETURN;
018700040622
018800040622      /end-free
018900040622     P DisplayRecords  E
019000040622
019100040622     P*--------------------------------------
019200040622     P* Procedure name: CalculateTotals
019300040622     P* Purpose:        Calculate running % totals
019400040622     P*--------------------------------------
019500040622     PCalculateTotals  B
019600040623     DCalculateTotals  PI              n
019700040622
019800040622      /free
019900040622       // Your calculation code goes here
020000040622                   clear err#;
020100040622                   totperc = *zeros;
020200040623
020300040622               for xcnt = 1 to prno#;
020400040622                  chain xcnt sub1;
020500040622                   totperc += prunperc#;
020600040622               endfor;
020700040622
020800040622                 if totperc > 0 and totperc <> 100;
020900040622                    err# = msg1;
021000040623                    RETURN *on;
021100040622                  endif;
021200040622
021300040623                  RETURN *off;
021400040622      /end-free
021500040622     PCalculateTotals  E
021600040622
021700040705
021800040705     P*--------------------------------------
021900040705     P* Procedure name: GetRunType
022000040705     P* Purpose:        Get payroll run type
022100040705     P*--------------------------------------
022200040705     PGetRunType       B
022300040705     DGetRunType       PI              n
022400040705
022500040705      /free
022600040705         dow 0 = 0;
022700040705            exfmt win2;
022800040705
022900040705         // end process
023000040705          if *in03;
023100040705             return *on;
023200040705          endif;
023300040705
023400040705          //validate run type entry;
023500040705            open(e) payrtypl01;
023600040705            chain run_type prtyfmt;
023700040705            close payrtypl01;
023800040705          if %found;
023900040705            leave;
024000040705          endif;
024100040705         enddo;
024200040705
024300040705           RETURN *off;
024400040705      /end-free
024500040705     PGetRunType       E
024600040705
024700040622     P*--------------------------------------
024800040622     P* Procedure name: ConfirmSr
024900040622     P* Purpose:        Confirm transactions
025000040622     P*--------------------------------------
025100040622     P ConfirmSr       B
025200040622     D ConfirmSr       PI
025300040622
025400040622      /free
025500040622       // Your calculation code goes here
025600021018
025700040622               for xcnt = 1 to prno#;
025800040622
025900040622                 chain xcnt sub1;
026000040622
026100040623                if @parm = 2;
026200110901                  chain (prun_type:xcnt) prunfmt;
026300040623                    elseif @parm = 3;
026400110901                  chain (prun_type:xcnt:payt#) prunfmt;
026500040622                endif;
026600040622
026700040623              if totperc = 100;
026800040622                     prunprno = prunprno#;
026900040622                     prunperc = prunperc#;
027000040621               //  if record was changed
027100040622                  if %found(payrunt);
027200040622                        update prunfmt;
027300040621               //  if a new record
027400040622                      else;
027500040622                         prunsts = 'A';
027600040622                        write prunfmt;
027700040622                   endif;
027800040623                 else;
027900040623                   if %found(payrunt);
028000040623                     delete prunfmt;
028100040623                   endif;
028200040623              endif;
028300021018
028400040622               endfor;
028500040622                RETURN;
028600040622      /end-free
028700040622     P ConfirmSr       E
028800040621
