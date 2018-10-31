000100110309     H DATEDIT(*DMY) dftactgrp(*no) bnddir('DISDIR') actgrp(*new)
000200110222      //*****  PROGRAMMER - SAMUEL BRITTON
000300121105     FKRON03D   CF   E             WORKSTN usropn
000400120919     F/IF DEFINED(WEB)
000500120919     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000600120919     F/ENDIF
000700061101     FKEMPM     UF A E           K DISK    usropn
000800020304     Fdeptl01   IF   E           K DISK
000900960419     FKRONCF    IF   E           K DISK
001000960419     FKRONRF    IF   E           K DISK
001100960502     FKRONSF    IF   E           K DISK
001200110831     FKRONTF    IF   E           K DISK    prefix(t_)
001300121105
001400110222      // General/Payroll PROTOTYPES
001500110608      /copy qcpysrc,genPR
001600110608      /copy qcpysrc,PAYPR
001700110831
001800950516     D                SDS
001900960415     D  @PGM             *PROC
002000120919     D  @user                358    367
002100960528     D  @prm             *PARMS
002200121105
002300020304     D arr             S             40    dim(1) ctdata perrcd(1)
002400110222     D div             s              2  0
002500110222     D dep             s              3  0
002600020304     D typ             s              1
002700110222     D sts             s              1
002800020304     D pos             s              2  0
002900020304     D name#           s             40
003000110615     D name##          s             30
003100110222     D email           s            100
003200020304     D err#            s              7
003300110222     d resigned        s              8  0
003400110222     d dob             s              8  0
003500110222     d hiredate        s              8  0
003600110303     D iemp            s                   like(emp)
003700020304     D caccnm          s                   like(kaccnm)
003800020304     D #kbadge         s                   like(kbadge)
003900110303     D ibadge          s                   like(kbadge)
004000110831     D emtyp           S              1A
004100120919     D errmsg          S             75A
004200110831
004300110222     D*--------------------------------------------------
004400110222     D* Procedure name: getEmpData
004500110222     D* Purpose:
004600110222     D* Returns:
004700110222     D* Parameter:      employee => employee number
004800110222     D*--------------------------------------------------
004900110222     D getEmpData      PR
005000110222     D  employee                      6P 0
005100110301
005200110301     D*--------------------------------------------------
005300110301     D* Procedure name: updateProc
005400110301     D* Purpose:        replaces the update subroutine
005500110301     D* Returns:
005600110301     D*--------------------------------------------------
005700110301     D updateProc      PR
005800110301
005900110301
006000110301     D*--------------------------------------------------
006100110301     D* Procedure name: getEmpRate
006200110301     D* Purpose:        calculate employee's base rate
006300110301     D* Returns:
006400110301     D* Parameter:      employee => employee number
006500110301     D* Parameter:      deptNo => department number
006600110301     D*--------------------------------------------------
006700110301     D getEmpRate      PR             9P 4
006800110301     D  employee                      6P 0
006900110301     D  deptNo                        3P 0
007000110307      * hold the value of employee status field
007100110307     D status#         S              1A
007200110307
007300110627       // Update Kronos Employee Database
007400110614     D update_interface...
007500110614     D                 PR                  extpgm('KRON01RI')
007600110627
007700110627       // select Kronos Classes
007800110627     D kronos_classes...
007900110627     D                 PR                  extpgm('KRON05R')
008000110627     D  class                        15
008100110627
008200110627       // Select Kronos Restriction profiles
008300110627     D kronos_restriction...
008400110627     D                 PR                  extpgm('KRON09R')
008500110627     D  profile                      15
008600110627
008700110627       // Select Kronos Terminal group
008800110627     D kronos_terminals...
008900110627     D                 PR                  extpgm('KRON07R')
009000110627     D  terminal                     15
009100110614
009200110307     D*--------------------------------------------------
009300110307     D* Procedure name: popClassTermIDs
009400110307     D* Purpose:        populate the class and terminal IDs
009500110831     D* Returns:        boolean
009600110307     D*--------------------------------------------------
009700110831     DpopClassTermIDs  PR              n
009800110830
009900110830K0515D TermgDS         DS
010000110830K0515D  ktermg
010100110830K0515D  ktermg2
010200110830K0515D  ktermg3
010300110830K0515D  ktermg4
010400110830K0515D  ktermg5
010500110830K0515D  ktermg6
010600110830K0515D  ktermg7
010700110830K0515D  ktermg8
010800110830K0515D  ktermg9
010900110830K0515D  ktermg10
011000110830
011100110830K0515D TIDDS           DS
011200110830     D  kempdgrp1
011300110830     D  kempdgrp2
011400110831     D  kempdgrp3
011500110830     D  kempdgrp4
011600110830     D  kempdgrp5
011700110830     D  kempdgrp6
011800110830     D  kempdgrp7
011900110830     D  kempdgrp8
012000110830     D  kempdgrp9
012100110830     D  kempdgrp10
012200110309      *
012300110831     D YES             C                   'Y'
012400110831     D NO              C                   'N'
012500110831     D in              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
012600110301
012700110222      //
012800110222     C     *entry        plist
012900110303     C                   PARM                    iemp
013000110303     C                   parm                    ibadge
013100020304
013200110222      //generic initialisign subroutine
013300110614      /copy QCPYSRC,aainit
013400121105
013500121105
013600121105      /FREE
013700121105       #kbadge = *zeros;
013800110518
013900110222       if @prm = *zeros;
014000150302
014100150302         open(e)  kron03d;
014200020304
014300121105
014400110222         dow 0 = 0;
014500110222           *in99 = *on;
014600110222           exfmt begdsp;
014700110618
014800110222           // End Program
014900110222           if *in03;
015000110614             update_interface();
015100110222             *inlr = *on;
015200110222             return;
015300110222           endif;
015400110518
015500110222           // CHECK FOR NUMBER ENTRY
015600110222           if numsrh = *zeros;
015700110222             *in01 = *on;
015800110222             iter;
015900110222           endif;
016000110518
016100110222           //* CHECK FOR VALID EMPLOYEE NUMBER
016200110222           if numsrh <> *zeros;
016300110222             // get employee name
016400110222             get_emp_info(numsrh:dept:*blanks:
016500110222                 name#:*omit:*omit:*omit:err#);
016600020304
016700110222             if err# <> *blanks;
016800110222               *in02 = *on;
016900110222               iter;
017000110222             else;
017100110222               *in02 = *off;
017200110222             endif;
017300110222           endif;
017400121105
017500110222           // EXECUTE Program to get employee number
017600110222           if *in05;
017700121105
017800110222      /END-FREE
017900020304     C                   call      'HMRENQ07R'
018000020304     C                   parm                    numsrh
018100110222      /FREE
018200110222             iter;
018300110222           endif;
018400110618
018500110222           emp = numsrh;
018600110222           leave;
018700110222         enddo;
018800121105
018900110222          else;
019000110222         //
019100110222          emp = iemp;
019200110303          #kbadge = ibadge;
019300110222       endif;
019400110618
019500110301          updateProc();
019600110301
019700110301      /end-free
019800110222     P*--------------------------------------------------
019900110222     P* Procedure name: getEmpData
020000110222     P* Purpose:
020100110222     P* Returns:
020200110222     P* Parameter:      employee => employee number
020300110222     P*--------------------------------------------------
020400110222     P getEmpData      B
020500110222     D getEmpData      PI
020600110222     D  employee                      6P 0
020700110222
020800110222
020900110222      /FREE
021000110222
021100110222         exec sql
021200110307           select  emply, surnam, resign, empemail,divis,dept into
021300110307             :hiredate,:kempsurnm,:resigned,:email, :div, :dep
021400110222             from empmast where emp = :employee;
021500110222           //return;
021600110222      /END-FREE
021700110222     P getEmpData      E
021800110301
021900110301     P*--------------------------------------------------
022000110301     P* Procedure name: updateProc
022100110301     P* Purpose:        replaces the update subroutine
022200110301     P* Returns:
022300110301     P*--------------------------------------------------
022400110301     P updateProc      B
022500110301     D updateProc      PI
022600110301
022700110301
022800110301      /FREE
022900110301
023000110831           status = get_emp_info(emp:dept:*blanks:
023100110831           name#:*omit:*omit:*omit:err#);
023200110831           kemp = emp;
023300110831           chain dept dhrec;
023400110301
023500110301           open(e) kempm;
023600110301           chain kemp kempm;
023700110301            *IN77 = NOT %FOUND;
023800110618              caccnm = %editc(dept:'X');
023900120621
024000110301
024100110301         ktype = 11;
024200110518
024300110301         if #kbadge <> *zeros and
024400110301               #kbadge <> kbadge;
024500110301           kbadge = #kbadge;
024600110301           ksts = 'A';
024700110301         endif;
024800110618
024900110301         if caccnm <> kaccnm or *in77;
025000110307
025100110830           kclass = dkclas;
025200110830           krst = dkrst;
025300110830           ktermg = dkterm;
025400110830
025500110830           // Reserved for global defaults
025600110830           //ktermg2 = dkterm2;
025700110830           //ktermg3 = dkterm3;
025800110830
025900110830           ktermg4 = dkterm2;
026000110830           ktermg5 = dkterm3;
026100110830           ktermg6 = dkterm4;
026200110830           ktermg7 = dkterm5;
026300110830           kschde = '@';
026400110830
026500110830           ksts = 'A';
026600110301         endif;
026700120621
026800120621          //--------------------------------------------
026900120621          // TEMPORARY CODE TO GET CASUALS CORRECT CLASS
027000120621          //--------------------------------------------
027100120621          if status = 'C';
027200120621             if dlocat = 6 or dlocat = 8
027300120621                or dept = 48 or dept = 26;
027400120621                 kclass  = 'CASUALS - 6.45';
027500120621               else;
027600120621                 kclass  = 'CASUALS - 8 HRS';
027700120621             endif;
027800120621          endif;
027900120621          //--------------------------------------------
028000120621          // END OF TEMPORARY BLOCK
028100120621          //--------------------------------------------
028200120621
028300110307
028400110301          ksts = 'A';
028500110309         if get_cat(status:*omit:emtyp:typ) <> 'A';
028600110301           kreqc = 03;
028700110301         else;
028800110301           kreqc = 05;
028900110301         endif;
029000110518
029100110301      /END-FREE
029200110301     C     in            check     name#         pos                      88
029300110301      /FREE
029400110615         name## =   %trim(%subst(name#:pos))
029500110301             + ' , ' +  %trim(%subst(name#:1:pos-1));
029600110615         kename = name##;
029700110618         kaccnm = %editc(dept:'X');
029800110618
029900110301         dow *in99 = *on;
030000110301           EXFMT UPDDSP;
030100110627
030200110831           errmsg = *blanks;
030300110831
030400110627           // Prompt for list
030500110627           if *in04;
030600110627              select;
030700110627                // Kronos Class Enquiry
030800110627                when fld = 'KCLASS';
030900110627                    kronos_classes(kclass);
031000110627                // Kronos Restriction Profiles
031100110627                when fld = 'KRST';
031200110627                    kronos_restriction(krst);
031300110627                // Kronos Terminal Groups
031400110627                when fld = 'KTERMG';
031500110627                    kronos_terminals(ktermg);
031600110830                when fld = 'KTERMG2';
031700110830                    kronos_terminals(ktermg2);
031800110830                when fld = 'KTERMG3';
031900110830                    kronos_terminals(ktermg3);
032000110830                when fld = 'KTERMG4';
032100110830                    kronos_terminals(ktermg4);
032200110830                when fld = 'KTERMG5';
032300110830                    kronos_terminals(ktermg5);
032400110830                when fld = 'KTERMG6';
032500110830                    kronos_terminals(ktermg6);
032600110830                when fld = 'KTERMG7';
032700110830                    kronos_terminals(ktermg7);
032800110830                when fld = 'KTERMG8';
032900110830                    kronos_terminals(ktermg8);
033000110830                when fld = 'KTERMG9';
033100110830                    kronos_terminals(ktermg9);
033200110830                when fld = 'KTERMG10';
033300110830                    kronos_terminals(ktermg10);
033400110627              endsl;
033500110627            iter;
033600110627           endif;
033700110627
033800110301           // END PROGRAM
033900110301           if *in03;
034000110614             update_interface();
034100110301             *inlr = *on;
034200110301             return;
034300110301           endif;
034400110728
034500110831
034600110831           if  popClassTermIDs();
034700110831             iter;
034800110831           endif;
034900110301
035000110301           leave;
035100110301         enddo;
035200110307
035300110307         // if called with parameters
035400110307         if not *in99;
035500150310            popClassTermIDs();
035600110831         endif;
035700110728
035800110301         // check code
035900110301         if not(*in99) and *in98;
036000110301           ksts = 'A';
036100110301         endif;
036200110728
036300110301         // EXCLUDE PENSIONERS AND CASUAL AND BRANCH EMPLOYEES
036400110301         // FROM FILE UPDATE
036500110301
036600110301
036700110301           getEmpData(emp);
036800110301           kemphire = %dec(%date(hiredate:*eur));
036900110831           if resigned > 0;
037000110301               kempresign = %dec(%date(resigned:*eur));
037100110301              else;
037200110831               kempresign = 0;
037300110831           endif;
037400110831           if email <> ' ';
037500110831              kempeml =  %subst(%triml(email):1:50);
037600110301            else;
037700110301              kempeml = ' ';
037800110301           endif;
037900110301           kempbio = YES;
038000110309           kempdiv  = %char(dlocat);
038100110301           kempdept  = %char(dep);
038200110307           // populating the employee type(status)
038300110307           //field in the kempm file
038400110309          select;
038500110831            when  emtyp = 'H';
038600110831              kemptyp = 'H';
038700110831              kempfreq = 'B';
038800110831              krate  = getEmpRate(emp:dep);
038900110831              if status = 'C';
039000110831                kempbio = NO;
039100110309                kempfreq = 'W';
039200110831              endif;
039300110309            other;
039400110309             kemptyp = 'S';
039500110309             krate  = 0;
039600110309             kempfreq = 'B';
039700110831          endsl;
039800110831           //catering for casuals
039900110307
040000110301           //get Employee hourly rate
040100110301           // check if active employee.
040200110309            sts = get_cat(status);
040300110301           if sts = 'A';
040400110309             kempsts = sts;
040500110309            else;
040600110301             // if not active, and terminated
040700110309             if status = 'T';
040800110309               kempsts = status;
040900110301              else;
041000110301              // any other inactive employee
041100110301                kempsts = 'E';
041200110810             endif;
041300110309           endif;
041400110301
041500110303           ksts = 'A';
041600110301           if not(*in77);
041700110309              update kerec;
041800110309             else;
041900110309              write kerec;
042000110301           endif;
042100110831
042200110831           close(e) kempm;
042300110831           if *in99;
042400110301
042500110831            else;
042600110831             update_interface();
042700110831             *inlr = *on;
042800110831             return;
042900110831           endif;
043000110301
043100110301
043200110301      /END-FREE
043300110301     P updateProc      E
043400110301
043500110301     P*--------------------------------------------------
043600110301     P* Procedure name: getEmpRate
043700110301     P* Purpose:        calculate employee's base rate
043800110301     P* Returns:
043900110301     P* Parameter:      employee => employee number
044000110301     P* Parameter:      deptNo => department number
044100110301     P*--------------------------------------------------
044200110301     P getEmpRate      B
044300110301     D getEmpRate      PI             9P 4
044400110301     D  employee                      6P 0
044500110301     D  deptNo                        3P 0
044600110301     D
044700110301
044800110301
044900110301     D* Local fields
045000110301     D retField        S              9P 4
045100110301      * hold value of department hours
045200110301     D hours           S              5P 2
045300110301      * hold Employee Basic wage data for calculation
045400110301     D basicWage       S             13P 2
045500110301      * buffer Indicator
045600110301     D buffer          S               N
045700110302      * cycle to calc hours on
045800110302     D cycle           S              2A
045900110302     D FNIGHT          S              2A   INZ('FN')
046000110302     D WEEK            S              2A   INZ('WK')
046100110302     D MONTH           S              2A   INZ('MT')
046200110302     D MONTHVAL        S              2  0 INZ(12)
046300110302     D WEEKVAL         S              2  0 INZ(52)
046400110302     D FNITEVAL        S              2  0 INZ(26)
046500110302
046600110301      /FREE
046700110301
046800110302          get_dept_info(deptNo:buffer:hours:cycle);
046900110301            basicWage = get_basic_wage(employee);
047000110302            retField = basicWage/hours;
047100110302            select;
047200110302              when cycle = FNIGHT;
047300110302               retfield = retfield/ FNITEVAL;
047400110302                when cycle = WEEK;
047500110302               retfield = retfield/ WEEKVAL;
047600110302                when cycle = MONTH;
047700110302               retfield = retfield/ MONTHVAL;
047800110831            endsl;
047900110302
048000110301
048100110301        RETURN retField;
048200110301
048300110301      /END-FREE
048400110301     P getEmpRate      E
048500110307
048600110307     P*--------------------------------------------------
048700110307     P* Procedure name: popClassTermIDs
048800110307     P* Purpose:        populate the class and terminal IDs
048900110307     P* Returns:
049000110307     P*--------------------------------------------------
049100110307     PpopClassTermIDs  B
049200110831     DpopClassTermIDs  PI              n
049300110307
049400110831     D xcnt            s              2  0
049500110831     D ycnt            s              2  0
049600110830     D xfield          s             15
049700110831     D tptr            s               *   inz(%addr(TermgDS))
049800110831     D iptr            s               *   inz(%addr(TIDDS))
049900110831     D TermgARRY       S                   dim(10) like(ktermg) based(tptr)
050000110831     D TIDARRY         S                   dim(10) like(kempdgrp1) based(iptr)
050100110831
050200110831     D msg1            c                   'Invalid Class Profile'
050300110831     D msg2            c                   'Invalid Restriction Profile'
050400110831     D msg3            c                   'Invalid Terminal or Terminal Group'
050500110831     D msg4            c                   'Duplicate Terminal setup'
050600110307
050700110307      /FREE
050800110307
050900110831           // check for valid class profile
051000110307           CHAIN KCLASS KRONCF;
051100110831           if %found;
051200110831               kemppprule = Kclassid;
051300110831               kempdrule = Kclassid;
051400110831             else;
051500121116               errmsg = msg1 + %trim(kclass);
051600150310               if *in99;
051700150302                 show_msg(errmsg);
051800150302                 return *on;
051900150310               endif;
052000110831           endif;
052100110307
052200110831           // check for valid restriction Profile
052300110307           CHAIN KRST KRONRF;
052400110831           if not %found;
052500110831              errmsg = msg2;
052600150310              if *in99;
052700150302                show_msg(errmsg);
052800150302                return *on;
052900150310              endif;
053000110831           endif;
053100110830
053200110830           for xcnt = 1 to 10;
053300110830
053400110831             xfield = TermgARRY(xcnt);
053500110831
053600110831             if xfield  <> *blanks;
053700110831
053800110831               // check for duplicates
053900110831               ycnt = %lookup(xfield:TermgARRY);
054000110831               if ycnt <> *zeros and ycnt <> xcnt;
054100110831                  errmsg =  msg4 + ' - Check Terminal ' + %char(ycnt);
054200150310                  if *in99;
054300150302                    show_msg(errmsg);
054400150302                    return *on;
054500150310                  endif;
054600110831               endif;
054700110831
054800110831               // check for valid terminal or group
054900110831               chain xfield KRONTF;
055000110831               if %found();
055100110831                  TidARRY(xcnt) = t_ktermid;
055200110831                 else;
055300110831                  errmsg =  msg3 + ' - Check Terminal ' + %char(xcnt);
055400150310                  if *in99;
055500150302                    show_msg(errmsg);
055600150302                    return *on;
055700150310                 endif;
055800110831               endif;
055900110830
056000110831               if xcnt = 1;
056100110831                  TidARRY(2) = 'T*';
056200110831                  TidARRY(3) = 'HR*';
056300110831               endif;
056400110831
056500110831             endif;
056600110830
056700110830           endfor;
056800110830
056900110830
057000110831           return *off;
057100110307      /END-FREE
057200110307     PpopClassTermIDs  E
057300110307**
057400020304       Kronos Master File Maintenance
