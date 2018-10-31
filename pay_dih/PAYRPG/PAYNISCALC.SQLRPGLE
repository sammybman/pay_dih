000100131101     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200010607     Fpaynisctl if   e             disk
000300010607     Fpaynisctl2if   e           k disk
000400010902     Fpaycycd   if   e           k disk
000500131021      // Payroll PROTOTYPES
000600131021      /copy qcpysrc,GENPR
000700131024      /copy qcpysrc,PAYPR
000800131101
000900010607     D  dob            s               d    datfmt(*eur)
001000010607     D  sysdate        s               d    datfmt(*iso)
001100010902     D  date           s              8  0
001200010902     D  amteml         s             11  2
001300010902     D  years          s              4  0
001400131101     D* birth          s              8s 0
001500131024
001600131024     D*--------------------------------------------------
001700131024     D* Procedure name: Mainpgm
001800131024     D* Purpose:
001900131024     D* Returns:
002000131024     D* Parameter:      AMT
002100131024     D* Parameter:      AMTEMP => Employee contribution
002200131024     D* Parameter:      amtcom => company contribution
002300131024     D* Parameter:      birth
002400131024     D* Parameter:      pay_cycle
002500131024     D* Parameter:      pay_cycqty => cycle convertion quantity
002600131024     D* Parameter:      emp# => Employee Number
002700131024     D*--------------------------------------------------
002800131024     D Mainpgm         PR                  EXTPGM('PAYNISCALC')
002900131024     D  AMT                          11P 2
003000131024     D  AMTEMP                       11P 2
003100131024     D  amtcom                       11P 2
003200131101     D  birthDate                     8s 0
003300131024     D  pay_cycle                     2
003400131024     D  pay_cycqty                    2P 0
003500131029     D  emp#                          6P 0
003600131029     D  cat_flag                      1
003700131024
003800131024     D Mainpgm         PI
003900131024     D  AMT                          11P 2
004000131024     D  AMTEMP                       11P 2
004100131024     D  amtcom                       11P 2
004200131101     D  birthDate                     8s 0
004300131024     D  pay_cycle                     2
004400131024     D  pay_cycqty                    2P 0
004500131029     D  emp#                          6P 0
004600131029     D  cat_flag                      1
004700131024
004800131024     D*--------------------------------------------------
004900131024     D* Procedure name: isDefault
005000131024     D* Purpose:        checking to see if the nis percentage to use is the...
005100131024     D*                           default one
005200131024     D* Returns:
005300131024     D* Parameter:      emp_no => employee whose salary is being tested
005400131024     D*--------------------------------------------------
005500131024     D isDefault       PR              N
005600131024     D  emp_no                        6P 0
005700131024      * data structure to allow use of altenate NIS config file fields
005800131024     D PN3DS         E DS                  EXTNAME(paynisctl3)
005900131029
006000131029     D*--------------------------------------------------
006100131029     D* Procedure name: openFilePrc
006200131029     D* Purpose:        set up cursor to enabling reading alternative file.
006300131029     D* Returns:
006400131029     D*--------------------------------------------------
006500131029     D openFilePrc     PR
006600131029
006700131029     D*--------------------------------------------------
006800131029     D* Procedure name: ReadFileprc
006900131029     D* Purpose:        read records from file
007000131029     D* Returns:
007100131029     D*--------------------------------------------------
007200131029     D ReadFileprc     PR              N
007300131029
007400131029     D*--------------------------------------------------
007500131029     D* Procedure name: closePrc
007600131029     D* Purpose:
007700131029     D* Returns:
007800131029     D*--------------------------------------------------
007900131101     D closeFilePrc    PR
008000131029      *
008100131029     D BASICONLY       S              2A   INZ('*B')
008200131029     D STDEARN         S              2A   INZ('*S')
008300131029
008400131101      /FREE
008500131029
008600131101       cat_flag = ' ';
008700010607
008800131101        // Get Configuration record
008900131021       setll 1 pnifmt;
009000131021       read pnifmt;
009100131101
009200131021       chain pay_cycle pni2fmt;
009300131101
009400131021       if not(%found);
009500131021         *inlr = *on;
009600131021         return;
009700131021       endif;
009800131101
009900131021       //****************************************************************
010000131021       //* Get year ***
010100131021       //****************************************************************
010200131101       if birthDate <> *zeros;                                               //OVER 60
010300131024
010400131021      /END-FREE
010500131101     C                   move      birthDate     dob
010600010607     C                   move      date          sysdate
010700010902     C     sysdate       subdur    dob           years:*y
010800131101
010900131021      /FREE
011000131021         if amt >= (pni2min * pay_cycqty);
011100131021           // Set Insurable earnings to the upper limit for employee working
011200131021           // beyond the ceiling.
011300131021           if amt > (pni2max * pay_cycqty);
011400131021             amt = (pni2max * pay_cycqty);
011500131021           endif;
011600131021           //
011700131021           if years < pnimxage;                                               //OVER 60
011800131029
011900131029             // if wage scale is default range
012000131101             if isDefault(emp#);
012100131029
012200131029              amtemp = amt * pnieper / 100;
012300131029              amtemp = round_up(amtemp:'U');
012400131101
012500131101              amteml = amt * (pnieper + pnicper) / 100;
012600131101              amteml = round_up(amteml:'U');
012700131101              amtcom = amteml - amtemp;
012800131101
012900131029              // if wage below the default nis value, amend flag accordingly
013000131024             else;
013100131029
013200131029
013300131101              amtemp = amt * pni3eper / 100;
013400131101              amtemp = round_up(amtemp:'U');
013500131101
013600140922              amteml = amt * (pni3eper + pni3cper) / 100;
013700131101              amteml = round_up(amteml:'U');
013800131101              amtcom = amteml - amtemp;
013900131101
014000131024             endif;
014100131021           else;
014200131101
014300131021             amtcom = amt * pninper / 100;
014400131021             amtcom = round_up(amtcom:'U');
014500131021             amtemp = *zeros;
014600131021           endif;
014700131101
014800131021         endif;
014900131021       endif;
015000131101
015100131021       *inlr = *on;
015200131021       return;
015300131101
015400131021       //**************************
015500131021       // Init. routine
015600131021       //**************************
015700131021       begsr *inzsr;
015800131021         get_sys_date(date);
015900131024
016000131021       endsr;
016100131021      /END-FREE
016200131024
016300131024
016400131024     P*--------------------------------------------------
016500131024     P* Procedure name: isDefault
016600131024     P* Purpose:        checking to see if the nis percentage to use is the...
016700131024     P*                           default one
016800131024     P* Returns:
016900131024     P* Parameter:      emp_no => employee whose salary is being tested
017000131024     P*--------------------------------------------------
017100131024     P isDefault       B
017200131024     D isDefault       PI              N
017300131024     D  emp_no                        6P 0
017400131101
017500131029     D  in_yearf       s              7P 3
017600131029     D  wrkyearf       s              7P 3
017700131029     D  basic_wage     S             13P 2
017800131029     D  max_wage       s             13P 2
017900131029     D  min_wage       s             13P 2
018000131024     D
018100131024
018200131024
018300131024     D* Local fields
018400131024     D retField        S               N   inz(*on)
018500131024
018600131024      /FREE
018700131024          //get year factor of the cycle code sent to program
018800131024              exec sql
018900131024                select cycyearf into :in_yearf from paycycd where
019000131024                  cyccode = :pay_cycle;
019100131029
019200131029          basic_wage = (get_basic_wage(emp_no)/in_yearf);
019300131029
019400131101          //get data from alternate NIS config file
019500131101            openFileprc();
019600131101            dow not (readFilePrc());
019700131029
019800131024          //get year factor of the cycle code in alternate NIS config file
019900131024
020000131024                exec sql
020100131024                select cycyearf into :wrkyearf from paycycd where
020200131024                  cyccode = :pni3cyc;
020300131024
020400131101                min_wage = round_up((pni3min *
020500131024                               wrkyearf/in_yearf):'A');
020600131101                max_wage = round_up((pni3Max *
020700131024                                              wrkyearf/in_yearf):'A');
020800131029
020900131029                Select;
021000131101                  when Pni3ecat = BASICONLY;
021100131029
021200131101                  when Pni3ecat = STDEARN;
021300131101                    basic_wage = amt;
021400131101                endsl;
021500131101
021600131101               //checking each record to see if the
021700131101               if basic_wage <= max_wage and
021800131101                 basic_wage >= min_wage;
021900131101                 cat_flag = pn3wcat;
022000131101                 retfield = *off;
022100131101                 leave;
022200131101               endif;
022300131101
022400131101            enddo;
022500131101
022600131101            closeFilePrc();
022700131101
022800131101          //Return calc flag
022900131024
023000131024        RETURN retField;
023100131024
023200131024      /END-FREE
023300131024     P isDefault       E
023400131024
023500131029
023600131029     P*--------------------------------------------------
023700131029     P* Procedure name: openFilePrc
023800131029     P* Purpose:        set up cursor to enabling reading alternative file.
023900131029     P* Returns:
024000131029     P*--------------------------------------------------
024100131029     P openFilePrc     B
024200131029     D openFilePrc     PI
024300131029
024400131029
024500131029      /FREE
024600131029
024700131029          exec sql
024800131029
024900131029
025000131029           Declare NISALTFILE Cursor For
025100131029             SELECT *
025200131029
025300131029            From paynisctl3
025400131029
025500131029               For Fetch Only;
025600131029
025700131029          Exec Sql
025800131029          OPEN  NISALTFILE;
025900131029
026000131029
026100131029      /END-FREE
026200131029     P openFilePrc     E
026300131029
026400131029
026500131029     P*--------------------------------------------------
026600131029     P* Procedure name: ReadFileprc
026700131029     P* Purpose:        read records from file
026800131029     P* Returns:
026900131029     P*--------------------------------------------------
027000131029     P ReadFileprc     B
027100131029     D ReadFileprc     PI              N
027200131029
027300131029
027400131029     D* Local fields
027500131029     D retField        S               N
027600131029
027700131029      /FREE
027800131101            EXEC SQL
027900131101                  FETCH NEXT FROM NISALTFILE INTO
028000131029
028100131029                   :pn3DS;
028200131101
028300131101            if sqlstt = '02000';
028400131101               retField = *on;
028500131101              else;
028600131101               retField = *off;
028700131101            endif;
028800131101
028900131029
029000131101            RETURN retField;
029100131029
029200131029      /END-FREE
029300131029     P ReadFileprc     E
029400131029
029500131029
029600131029     P*--------------------------------------------------
029700131029     P* Procedure name: closePrc
029800131029     P* Purpose:
029900131029     P* Returns:
030000131029     P*--------------------------------------------------
030100131101     P closeFilePrc    B
030200131101     D closeFilePrc    PI
030300131029
030400131029
030500131029      /FREE
030600131029
030700131101            EXEC SQL
030800131029                close NISALTFILE;
030900131029
031000131029      /END-FREE
031100131101     P closeFilePrc    E
031200131029
