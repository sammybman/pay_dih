000100950419     H DECEDIT('.') DATEDIT(*DMY/)
000200950419     F**
000300060129     F** APPLYING SALES COMMISSIONS
000400060129     F** PROGRAMMER--------S.BRITTON
000500950419     F**
000600151015     Fpicommpay uf   e           k disk    commit usropn
000700011010     Fpaytimfl01uf a e           k disk    commit
000800020607     Fpayrdefl03if   e           K DISK
000900020607     Fpayrresl01if   e           K DISK
001000050519     **
001100010912     ** Payroll PROTOTYPES
001200140604      /copy QCPYSRC,GENPR
001300140604      /copy QCPYSRC,PAYPR
001400140604      /copy QCPYSRC,emppr
001500140604      /copy QCPYSRC,salpr
001600060114
001700151015     D  perfPICalc     pr                  extpgm('PAYPICALC')
001800060130     D   lprno                        6  0 const
001900060130     D   nprno                        6  0 const
002000060224     D   origin                       3    const
002100060127
002500060131
002600060130     D sndescmsg       pr                  extpgm('ESCPMSG')
002700060130     D program                       10    const
002800060130     D msgid                          7    const
002900060418
003100060131
003200060131     D                sds
003300060131     D  @PGM             *PROC
003400140604     D  @USER                358    367
003401151015
003402151104       Dcl-Ds piCommGmDS  ExtName('PICOMMGM') Inz End-Ds;
003500060130
003600980414     **
003700010418     D type            s              2  0
003800020607     D date1           s              8  0
003900020607     D prd             s              6  0
004000060129     D updprd          s              6  0
004100020607     D mth             s              2  0
004200021005     Drun_type         s              1
004300060131     Dcounter          s              5  0
004400060406     Dgotrecord        s               n
004500021005
004700151015        // If Performance Incentive info not configured
004800060823        // terminate process
004900151015        if not getPIType();
005000060127           *inlr = *on;
005100060127           return;
005200060127        endif;
005300060129
005400060823
005500060129        counter = *zeros;
005600060129
005700061102        // verify all Warehouse locations Updated
005800060404        dow not get_whcon_info('RD':-1:whcon_struc:counter:'A');
005900060129           if wcudate <> *zeros;
006000060129             get_prd_info('*prd':*blanks:updprd:wcudate);
006100151015             if updprd < cmgnprno;
006200060130              rolbk;
006300060130              *inlr = *on;
006400151015              sndescmsg(@pgm:'PAY0051');
006500060129             endif;
006600060129           endif;
006700060129        enddo;
006800060129
006900060127
007000060114        chain (run_type:type)  prdffmt;
007100020607
007200060129        if  %found;
007300020607
007400060823        // Run calcs ONLY once if current period is Greater/equal to next run period
007500060823        // Prevent Double calcs.
007600151015        if prd > cmgnprno;
007700151015           perfPICalc(cmglprno:cmgnprno:'PAY');
007800060823        endif;
007900060114
008000151015         open(e) picommpay;
008100060224         setll *loval  cpayfmt;
008200060224         read  cpayfmt;
008300060114         dow not %eof();
008400060114
008500060131             // Validate Employee number / create payroll records
008600060224              if  get_cat(get_emp_info(cpayemp)) = 'A' and
008700060224                 %scan(get_emp_info(cpayemp):prrelist) > 0;
008800060129
008900060224                chain (run_type:type:cpayemp) timfmt;
009000060114                if  %found;
009100060224                    timamt += round_up(cpayvalue:'U');
009200060114                    update  timfmt  %fields(timamt);
009300060114                   else;
009400060224                    timamt = round_up(cpayvalue:'U');
009500060224                    timemp  = cpayemp;
009600060114                    timcode = 'E';
009700060114                    timtyp  = type;
009800060114                    timrtype = run_type;
009900060114                    timprior = *zeros;
010000060114                    write  timfmt;
010100060114                endif;
010200060224                  cpayrtype = run_type;
010300060224                  update cpayfmt %fields(cpayrtype);
010400060114              endif;
010500060406
010600060406              gotrecord = *on;
010700060114
010800060224         read  cpayfmt;
010900060114         enddo;
010901151015
011000151015         close(e) picommpay;
011100060406
011200060406         if not gotRecord;
011300060406           *inlr = *on;
011400060406           sndescmsg(@pgm:'PAY0047');
011500060406         endif;
011600060131
011700060823         // Terminate Updates if current period is Less than next run period
011800060823         // Prevent Double update
011900151015         if prd > cmgnprno;
012000060823
012100151028          // update next incentive run period
012200151015          UpdPIPrno();
012300151028          // delete OLD incentive records
012400060823          DelOldRec();
012500060823         endif;
012600060129
012700060114        endif;
012800060404
012900060404        *inlr = *on;
013000060404        return;
013200960612
013300021005     C     *inzsr        BEGSR
013400020607     **
013500020607     C     *entry        plist
013600021005     C                   parm                    run_type
013700020607     C
013800020607     ** get period  dates
013900020607     C                   callp     get_sys_date(date1)
014000020607     C                   callp     get_prd_info('*prd':*blanks:prd:
014100020607     C                                          date1)
014200020607     ** Set up correct period
014300020607     C                   move      prd           mth
014400020607
014500020607     C     run_type      chain     prrefmt
014600020607     C                   ENDSR
014700060127
014800060127     P*--------------------------------------
014900151015     P* Procedure name: getPIType
015000151015     P* Purpose:        getPIType
015100060127     P* Returns:        *boolean
015200060127     P* Parameter:      *none
015300060127     P*--------------------------------------
015400151015     P getPIType       B
015500151015     D getPIType       PI              n
015600060127
015800060823         Exec Sql
015900151015           SELECT cmgpitype , cmglprno , cmgnprno  INTO
016000151015           :cmgpitype ,:cmglprno ,:cmgnprno FROM picommgm;
016100060127
016101151103           if sqlstt = '02000' or
016200151103             cmgpitype = *zeros;
016300060127            return *off;
016400060127           else;
016500151015            type = cmgpitype;
016600060127            return *on;
016700060127         endif;
016800060127
017000060127
017100151015     P getPIType       E
017200060131
017300060131     P*--------------------------------------
017400151015     P* Procedure name: UpdPIPrno
017500151015     P* Purpose:        UpdPIPrno
017600060131     P* Returns:        *none
017700060131     P* Parameter:      *none
017800060131     P*--------------------------------------
017900151015     P UpdPIPrno       B
018000151015     D UpdPIPrno       PI
018100060131
018300151015         cmglprno  = cmgnprno;
018400151015         cmgnprno = get_new_prd(cmgnprno:1:'A');
018500060131
018600060823         Exec Sql
018700151015           UPDATE PICOMMGM SET cmglprno = :cmglprno, cmgnprno
018800151015           = :cmgnprno;
019000060131
019100060131
019200151015     P UpdPIPrno       E
019300060418
019400060418     P*-----------------------------------------------
019500060418     P* Procedure name: DelOldRec
019600060418     P* Purpose:        Delete old commission records
019700060418     P* Returns:        *none
019800060418     P* Parameter:      *none
019900060418     P*-----------------------------------------------
020000060418     P DelOldRec       B
020100060418     D DelOldRec       PI
020200060418
020300060418     D delprd          s              6  0
020400060418     D keepmths        s              2  0
020500060418
020700060823           Exec Sql
020800151015             SELECT cmgkeepdta INTO :keepmths FROM picommgm;
020900060418
021000060418        if keepmths > *zeros;
021100151015          delprd = get_new_prd(cmgnprno:keepmths:'S');
021200060418
021300060823          Exec Sql
021400151015            DELETE FROM PICOMMRM WHERE CMRPRNO < :delprd;
021500060418
021600060823          Exec Sql
021700151015            DELETE FROM PICOMMCM WHERE CMCPRNO < :delprd;
021800060418
021900060418        endif;
022100060418
022200060418
022300060418     P DelOldRec       E
