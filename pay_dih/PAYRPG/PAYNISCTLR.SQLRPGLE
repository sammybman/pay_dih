000100111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300010505     Fpaynisctldcf   e             workstn
000301140922     F                                     sfile(sflrec:rrn)
000400111002     F/IF DEFINED(WEB)
000500111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000600111002     F/ENDIF
000700020806     Fpaynisctl uf a e             disk
000800011107     Fpaynisctl2uf a e           k disk
000900011107     Fpaycon    if   e             disk
001000010505     Fpaycycd   if   e           k disk
001100140922      //
001200111002      /copy QCPYSRC,genpr
001300111002      /copy QCPYSRC,PAYPR
001400010411     Darr              s             40    dim(1) ctdata perrcd(1)
001500140922      //
001600971012     D                sds
001700971012     D  @pgm             *proc
001800111002     D  @user                358    367
001900140923i     //
002000010505     D nextctl         s               n   inz(*off)
002100011107     D year_factor     s                   like(cycyearf)
002200011107     D savemin         s             11  2
002300011107     D savemax         s                   like(savemin)
002400010505     d msg1            c                   const('Invalid Cycle Code')
002500140922        //
002502140922       DCL-S rrn  PACKED(4:0) INZ(*zeros);
002505140922
002506140922       DCL-S new_Record IND INZ(*OFF);
002507140922       DCL-DS paynis3DS EXTNAME('PAYNISCTL3');
002508140922       END-DS ;
002600010411
002700140922         // Iniz routine
002800111002      /copy QCPYSRC,aainit
002802140922
002836140922
002837140922
002838140922
002863140922
002864140922
002900140922       *in = *off;
003000140922       clear nis_cycle;
003100140922           // Get Confiuration record
003200140922       setll 1 pnifmt;
003300140922       read pnifmt;
003400140922         //
003500140922         //************************
003600140922         //* Start of Program   ***
003700140922         //************************
003800140922       dow 0 = 0;
003900140922         //
004000140922         exfmt fmt1;
004100140922         clear errmsg;
004200140922         //
004300140922         if *in03;
004400140922           *inlr = *on;
004500140922           return;
004600140922         endif;
004700140922         //
004701140922         // if amending or adding Alternate configurations
004702140922          if *in07;
004703140922            showNISCOnfigsPrc();
004704140922            iter;
004705140922          ENDIF;
004800140922         *IN97 = *OFF;
004801140922         *IN98 = *OFF;
004802140922         *IN99 = *OFF;
004900140922         // Display List of Valid Run Types
005000140922         if *in01;
005100140922           //                  call      'HMRENQ02R'
005200140922           //                  parm                    run_type
005300140922           iter;
005400140922         endif;
005500140922         //
005600140922         if nis_cycle <> *blanks;
005700140922           chain nis_cycle cycfmt;
005800140922           // Invalid cycle code entered
005900140922           if not(%found);
006000140922             errmsg = msg1;
006100140922             iter;
006200140922           else;
006300140922             chain nis_cycle pni2fmt;
006301140922             *IN99 = NOT %FOUND;
006400140922             // Get Cycle code name for Screen
006500140922             cyc_name = cycdesc;
006600140922             year_factor = cycyearf;
006700140922             nextctl  = *on;
006800140922             leave;
006900140922           endif;
007000140922         endif;
007100140922         //  Confirm
007200140922         if *in15;
007300140922           exsr confirm_rtn;
007400140922           nextctl  = *off;
007500140922           leave;
007600140922         endif;
007700140922         //
007800140922       enddo;
007900140922       //************************
008000140922       //* Body of Program    ***
008100140922       //************************
008200140922       dow nextctl = *on;
008300140922         exfmt fmt2;
008400010427
008500140922         // Clear Error Message
008600140922         clear errmsg;
008700140922         //  Go back to start
008800140922         if *in02;
008900140922           leave;
009000140922         endif;
009100140922         //  End Program
009200140922         if *in03;
009300140922           *inlr = *on;
009400140922           return;
009500140922         endif;
009600010427
009700140922         // IF RECORD WAS CHANGED REDISPLAY FORMAT
009800140922         if *in88;
009900140922           iter;
010000140922         endif;
010100140922         //  Confirm
010200140922         if *in15;
010300140922           exsr confirm_rtn;
010400140922           exsr trn_upd_rtn;
010500140922           leave;
010600140922         endif;
010700140922         //
010800140922       enddo;
010900140922       //
011000140922       //****************************************************************
011100140922       //*  Update Payroll NIS Control File
011200140922       //****************************************************************
011300140922       //
011400140922       begsr confirm_rtn;
011500010427
011600140922         //
011700140922         if %found(paynisctl);
011800140922           update pnifmt;
011900140922         else;
012000140922           write pnifmt;
012100140922         endif;
012200140922         // UPdate control file #2 if valid code s entered
012300140922         if nis_cycle <> *blanks;
012400140922           //
012500140922           if %found(paynisctl2);
012600140922             update pni2fmt;
012700140922           else;
012800140922             pni2cyc = nis_cycle;
012900140922             write pni2fmt;
013000140922           endif;
013100140922           // Save min / max values for Update
013200140922           saveMin = pni2min;
013300140922           saveMax = pni2max;
013400140922           //
013500140922         endif;
013600140922         //
013700140922       endsr;
013800140922       //***************************************************************
013900140922       // Update All transactions
014000140922       //***************************************************************
014100140922       begsr trn_upd_rtn;
014200140922
014300140922         //
014400140922         setll 1 paycon;
014500140922         read paycon;
014600011107
014700140922         if pcautonis = 'Y';
014800011107
014900140922           setll *loval cycfmt;
015000140922           read cycfmt;
015100140922           dow not %eof;
015200140922             if cyccode <> nis_cycle;
015300011107
015400140922               // Base setup  *******************
015500140922               chain cyccode pni2fmt;
015600011107
015700140922               pni2min = round_up((saveMin *
015800140922                   year_factor / cycyearf):'A');
015900140922               pni2max = round_up((saveMax *
016000140922                   year_factor / cycyearf):'A');
016100140922               //
016200140922               if %found;
016300140922                 update pni2fmt;
016400140922               else;
016500140922                 pni2cyc = cyccode;
016600140922                 write pni2fmt;
016700140922               endif;
016800011107
016900140922               //
017000140922             endif;
017100140922             read cycfmt;
017200140922           enddo;
017300011107
017400140922         endif;
017500140922       endsr;
017600140922       //
017700140922
017701140922       // --------------------------------------------------
017702140922       // Procedure name: showNISCOnfigsPrc
017703140922       // Purpose:
017704140922       // Returns:
017705140922       // --------------------------------------------------
017706140922
017707140922       DCL-PROC showNISCOnfigsPrc ;
017708140922       DCL-S rrn# LIKE(rrn) INZ(0);
017717140922       DCL-S start LIKE(rrn) inz(0) ;
017718140922              ResetSubfile();
017719140922
017720140922
017721140922
017722140922              openFilePrc();
017723140922              dow not(readFilePrc());
017724140922                sel# = ' ';
017725140922                pni3desc# = pni3desc;
017726140922                pn3wcat## = pn3wcat;
017727140922              rrn+= 1;
017728140922              write sflrec;
017729140922              enddo;
017730140922              closeFilePrc();
017731140922
017732140922              exfmt sflctl;
017733140922              if *in03;
017734140922                *inlr = *on;
017735140922                return;
017736140922              ENDIF;
017737140923              rrn# = rrn;
017738140922              for start = 1 to rrn#;
017739140922              chain start sflrec;
017740140922              if sel# = 'X';
017741140922                exec sql
017742140922                  select * into :paynis3DS from paynisctl3
017743140922                    where pn3wcat = :pn3wcat##;
017744140923
017745140923
017746140922                leave;
017747140922              ENDIF;
017748140922              ENDFOR;
017749140922
017750140922              //Add new record
017751140922              if *in10;
017752140922                pn3wcat# = getNextWCat();
017753140922                pni3desc# = ' ';
017754140922                pni3cper# = 0;
017755140922                pni3eper# = 0;
017756140922                pni3min# = 0;
017757140922                pni3max# = 0;
017758140922                pni3cyc# = ' ';
017759140922                pni3ecat# = ' ';
017760140922                new_Record = *on;
017761140922
017762140922              else;
017763140922                new_Record = *off;
017764140922
017765140922                pn3wcat# = pn3wcat;
017766140922                pni3desc# = pni3desc;
017767140922                pni3cper# = pni3cper;
017768140922                pni3eper# = pni3eper;
017769140922                pni3min# = pni3min;
017770140922                pni3max# = pni3max;
017771140922                pni3cyc# = pni3cyc;
017772140922                pni3ecat# = pni3ecat;
017773140922              ENDIF;
017774140922         dow 0 = 0;
017775140922              exfmt fmt3;
017776140922              if *in03;
017777140922                *inlr = *on;
017778140922                return;
017779140922              ENDIF;
017780140922              // Confirm updates
017781140922           if *in15;
017782140922
017783140922             if new_record;
017784140922
017785140922                if  not  AddNewRecord();
017786140922                  iter;
017787140922                ENDIF;
017788140922              else;
017789140922
017790140922               if  not  AmendRecord();
017791140922                 iter;
017792140922               ENDIF;
017793140922
017794140922             endif;
017795140922          leave;
017796140922           ENDIF;
017797140922
017798140922         enddo;
017799140922       END-PROC ;
017800140922
017801140922       // --------------------------------------------------
017802140922       // Procedure name: ResetSubfile
017803140922       // Purpose:
017804140922       // Returns:
017805140922       // --------------------------------------------------
017806140922       DCL-PROC ResetSubfile ;
017807140922             *in33 = *on;
017808140922         write sflctl;
017809140922             *in33  = *off;
017810140922         // Clear entry fields
017811140922         clear PNI3DESC#;
017812140922         clear sel#;
017813140922         //   Reset all entry fields
017814140922         reset rrn;
017815140922         reset PN3WCAT#;
017816140922       END-PROC ;
017817140922
017818140922       // --------------------------------------------------
017819140922       // Procedure name: openFilePrc
017820140922       // Purpose:        set cursor for file to be opened-paynisctl3
017821140922       // Returns:
017822140922       // --------------------------------------------------
017823140922       DCL-PROC openFilePrc ;
017824140922           // Your local fields go here
017825140922           // Your calculation code goes here
017826140922           exec sql
017827140922            declare paynis3file cursor for select pn3wcat,pni3desc
017828140922              from paynisctl3 where
017829140922            pni3sts = 'A';
017830140922
017831140922            exec sql
017832140922            open paynis3file;
017833140922       END-PROC ;
017834140922
017835140922       // --------------------------------------------------
017836140922       // Procedure name: closeFilePrc
017837140922       // Purpose:        close file cursor for paynisctl3
017838140922       // Returns:
017839140922       // --------------------------------------------------
017840140922       DCL-PROC closeFilePrc ;
017841140922            exec sql
017842140922            close paynis3file;
017843140922       END-PROC ;
017844140922
017845140922       // --------------------------------------------------
017846140922       // Procedure name: readFilePrc
017847140922       // Purpose:        Read data from file into data structure
017848140922       // Returns:
017849140922       // --------------------------------------------------
017850140922       DCL-PROC readFilePrc ;
017851140922         DCL-PI *N IND;
017852140922         END-PI ;
017853140922           // Your local fields go here
017854140922           DCL-S retField IND;
017855140922
017856140922                exec sql
017857140922                 fetch next from paynis3file into :pn3wcat,:pni3desc;
017858140922                   if sqlstt <> '00000';
017859140922                      return *on;
017860140922                     else;
017861140922
017862140922                      return *off;
017863140922                   endif;
017864140922
017865140922
017866140922
017867140922
017868140922
017869140922           // Your calculation code goes here
017870140922           return retField;
017871140922       END-PROC ;
017872140922
017873140922       // --------------------------------------------------
017874140922       // Procedure name: getNextWCat
017875140922       // Purpose:        get the next available WageCategory
017876140922       // Returns:
017877140922       // --------------------------------------------------
017878140922       DCL-PROC getNextWCat ;
017879140922         DCL-PI *N CHAR(1);
017880140922         END-PI ;
017881140922
017882140922           // Your local fields go here
017883140922           DCL-S retField CHAR(1);
017884140922            DCL-S wcat CHAR(1);
017885140923            DCL-S wDESC CHAR(40);
017886140922
017887140922           closeFilePrc();
017888140922
017889140923       EXEC SQL
017890140923       DECLARE INPUT SCROLL CURSOR FOR
017891140923        SELECT PN3WCAT, PNI3DESC  FROM PAYNISCTL3
017892140922
017893140923       ORDER BY PN3WCAT DESC;
017894140922
017895140922       exec sql
017896140922        open Input;
017897140923         EXEC SQL
017898140923           FETCH FIRST FROM INPUT INTO :WCAT,:WDESC ;
017899140922
017900140923
017901140922             retField = %char(%Dec(wcat:1:0)+1);
017902140922
017903140922           return retField;
017904140922       END-PROC ;
017905140922
017906140922       // --------------------------------------------------
017907140922       // Procedure name: AddNewRecord
017908140922       // Purpose:        Add New Config record to paynisctl3
017909140922       // Returns:        on means sucessfully added record
017910140922       // --------------------------------------------------
017911140922       DCL-PROC AddNewRecord ;
017912140922         DCL-PI *N IND;
017913140922         END-PI ;
017914140922           // Your local fields go here
017915140922           DCL-S retField IND;
017916140922            exec sql
017917140922              insert into paynisctl3
017918140922              values('A',:pni3desc#,:PNI3MIN#,:PNI3MAX#,:PNI3CYC#,:PNI3EPER#,
017919140922              :PNI3ECAT#,' ',: PN3WCAT#,:PNI3CPER#);
017920140922            if sqlstt = '00000';
017921140922              retField = *on;
017922140922              else;
017923140922                show_msg('Error Inserting Record: ' +sqlstt);
017924140922              retField = *off;
017925140922            ENDIF;
017926140922
017927140922           return retField;
017928140922       END-PROC ;
017929140922
017930140922       // --------------------------------------------------
017931140922       // Procedure name: AmendRecord
017932140922       // Purpose:
017933140922       // Returns:        on mean sucessfully updated
017934140922       // --------------------------------------------------
017935140922       DCL-PROC AmendRecord ;
017936140922         DCL-PI *N IND;
017937140922         END-PI ;
017938140922           // Your local fields go here
017939140922           DCL-S retField IND;
017940140922           exec sql
017941140922             update paynisctl3 set pni3desc = :pni3desc#,
017942140922              PNI3MIN = :PNI3MIN#,PNI3MAX = :PNI3MAX#,
017943140922              PNI3CYC = :PNI3CYC#, PNI3EPER = :PNI3EPER#,
017944140922             PNI3ECAT = :PNI3ECAT#,
017945140922              PNI3CPER = :PNI3CPER#
017946140922              where PN3WCAT = :PN3WCAT#;
017947140922
017948140922              if sqlstt = '00000';
017949140922                 retField = *on;
017950140922              else;
017951140922
017952140922                retField = *off;
017953140922                show_msg('Error Updating Record: ' +sqlstt);
017954140922            ENDIF;
017955140922
017956140922
017957140922           return retField;
017958140922       END-PROC ;
017959140922**  Program Title
017960010505      Payroll NIS Configuration
