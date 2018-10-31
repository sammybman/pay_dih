000100010424     H DATEDIT(*ymd)
000200110125      // PAYROLL POSTING TO GL.
000300110125      // PROGRAMMER:-S. Britton & C. Williams
000400010809     Fpayhisl02 up   e           k disk    commit
000500010809     Fpaytranl01uf   e           k disk    commit
000600010424     Fpaytdefl01if   e           k disk
000700010424     Fpaydivl01 if   e           k disk
000800010424     Fpayrtypl01if   e           k disk
000900110121     Fgenpysuml6uf   e           k disk    commit
001000110125      // Payroll PROTOTYPES
001100131031      /copy QCPYSRC,PAYPR
001200131031      /copy QCPYSRC,genpr
001300131031      /copy QCPYSRC,glpr
001400131031      /copy QCPYSRC,emppr
001500110125      //
001600960130     D                SDS
001700960130     D  @PGM             *PROC
001800131031     D  @USER                358    367
001900110125      //
002000010424     D glarr           s             31    dim(300)                             TOTAL AMOUNT
002100110121     D origin          s              3    inz('PAY')                           TOTAL AMOUNT
002200110121     D xcnt            s              4  0 inz(0)                               TOTAL AMOUNT
002300010424     D ycnt            s              4  0                                      TOTAL AMOUNT
002400031001     D errmsg          s             75                                         TOTAL AMOUNT
002500010828     D phistype#       s                   like(phistype)                       TOTAL AMOUNT
002600050415     D pydate          s               D   datfmt(*dmy)
002700050415     D dmydate         s              6  0
002800110125      //
002900010424     Dgl_accumd        ds                  occurs(300) inz
003000010424     D gl_prd                         6  0
003100010424     D gl_account                    25
003200010424     D gl_desc                       30
003300010424     D gl_total                      13  2
003400030703     D gl_refno                      15
003500110125      //
003600010424     D                 ds
003700010424     D gl_xxx                        31
003800010424     D gl_prno                        6  0 overlay(gl_xxx)
003900010424     D gl_out                        25    overlay(gl_xxx:7)
004000110125      //
004100010424     Iphisfmt       09
004200010424     I                                          phistype      l1
004300110125      //
004400110125      //*   *ENTRY        PLIST
004500110125      //*                 PARM                    TYPE              1
004600110125      //
004700010424     c     trans_key     klist
004800010424     c                   kfld                    phisemp
004900010424     c                   kfld                    phispdate
005000010424     c                   kfld                    phisrdate
005100010424     c                   kfld                    phistype
005200110125      //
005300990826
005400110125      // FROM FILE PAYRTYPE ( Payroll Run Type)
005500110125      /FREE
005600110125       chain phistype prtyfmt;
005700110125       // Getting payroll date in dmy format
005800110125      /END-FREE
005900050415     C     *iso          move      phispdate     pydate
006000050415     C                   move      pydate        dmydate
006100110125      //
006200990114
006300050415      /free
006400050415         // Summarising Division totals
006500110121           setll (origin:phisemp:phistype:dmydate) payfmt;
006600110121           reade (origin:phisemp:phistype:dmydate) payfmt;
006700050415           dow not %eof;
006800050415             chain paydiv divfmt;
006900050415             gl_out = dtgl;
007000050415             exsr  accum_gl_tot;
007100050415             gl_total -= payamt;
007200050415             payflag = 'G';
007300050415             update payfmt %fields(payflag);
007400110121             reade (origin:phisemp:phistype:dmydate) payfmt;
007500050415           enddo;
007600050415
007700050528
007800050528          // PROCESS ALL TRANSACTIONS
007900110125         chain trans_key ptisfmt;
008000110125         *IN88 = NOT %FOUND;
008100110125          dow not(*in88);
008200110125             // Get account code
008300050528             gl_out =  proll_gl(phisemp:phisdept:ptrntcode:
008400050528                                phistype);
008500010424
008600110125           // accumulate totals
008700110125         exsr accum_gl_tot;
008800110125         select;
008900110125         when ptrncode = 'E';
009000110125           gl_total += ptrnamt;
009100110125         other;
009200010424
009300110125           gl_total -= ptrnamt;
009400110125         endsl;
009500110125         // update Transaction  Master with Gl. Post date and Status
009600110125         ptrnsts = 'G';
009700110125         update ptisfmt;
009800010423
009900110125         reade trans_key ptisfmt;
010000110125         *IN88 = %EOF;
010100110125       enddo;
010200110125       // update History Master with Gl. Post date and Status
010300110125       phissts = 'G';
010400110125       get_sys_date(phisdatep);
010500110125       update phisfmt;
010600110125       //
010700110125      /END-FREE
010800010424     Cl1                 exsr      wtr_gll_totals
010900010424
011000110125      //*********************************************************************
011100110125      // Search for gl.code in array . if found set occurance level and add
011200110125      // total ,if not found add new array record and Data Struct. total
011300110125      //*********************************************************************
011400110125      /FREE
011500110125       BEGSR accum_gl_tot;
011600110125         //
011700110125         gl_prno  = phisprno;
011800110125         //
011900110125      /END-FREE
012000010424     C                   z-add     1             ycnt
012100010424     C     gl_xxx        lookup    glarr(ycnt)                            12
012200110125      /FREE
012300110125         if not(*in12);
012400110125      /END-FREE
012500010424     C                   add       1             xcnt
012600010424     C                   move      gl_xxx        glarr(xcnt)
012700010424     C     xcnt          occur     gl_accumd
012800110125      /FREE
012900110125           gl_account = gl_out;
013000110125           gl_prd = gl_prno;
013100110125           //
013200110125           gl_desc = %trim(prtydesc) +
013300110125               '-' + %editc(phispdate:'Y');
013400110125           //
013500110125         else;
013600110125           //
013700110125      /END-FREE
013800010424     C     ycnt          occur     gl_accumd
013900110125      /FREE
014000110125         endif;
014100010424
014200110125       ENDSR;
014300110125       //***************************************************
014400110125       //  Writing to G.L.
014500110125       //***************************************************
014600110125       begsr wtr_gll_totals;
014700110125          // call pgm to write to g/l files
014800110125          %occur(gl_accumd) = 1;
014900110125          write_to_gl(%addr(gl_accumd):xcnt:'PAY':errmsg);
015000110125          //  Reset data Structure and counter
015100110125         clear *all gl_accumd;
015200110125         reset xcnt;
015300990824
015400110125       endsr;
015500110125         //
015600110125         //
015700110125      /END-FREE
