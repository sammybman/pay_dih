000100950615     H DECEDIT('.') DATEDIT(*DMY/)
000200170216     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300170216     H alwnull(*usrctl)
000400011001     ** Employee Record Listing
000500020211     Femptypel01if   e           k disk
000600020408     Fli003d    cf   e             workstn
000700180315     F/IF DEFINED(WEB)
000800180315     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000900180315     F/ENDIF
001000011001     **
001100110627      /copy QCPYSRC,genpr
001200110627      /copy QCPYSRC,PAYPR
001300011001     **
001400011001     D                sds
001500951004     D  @PGM             *PROC
001600180315     D  @USER                358    367
001700011001     **
001800011001     D arr             s             40    dim(1) ctdata perrcd(1)
001900011003     d qry_select      s            200
002000020409     D prno            s              6  0
002100020411     D type            s              2  0
002200011001     D cnt             s              2  0
002300020411     D pos             s              1  0
002400011001     D run_list        s              1    dim(20)
002500011003     D error           s               n
002600011003     D msg1            c                   'Invalid Employee Type -'
002700011003     D msg2            c                   'Report sent to print queue'
002800011001
002900011106     **
003000020410     C     *entry        plist
003100020411     C                   parm                    prno
003200020411     C                   parm                    type
003300020411
003400011001     C                   exsr      aainit
003500020211     C                   eval      cnt  = *zeros
003600011001
003700011001     ******************************
003800011001     ** Selection Report Options **
003900011001     ******************************
004000011001     C                   dow       0 = 0
004100011001     C                   exfmt     fmt1
004200011008     C                   eval      errmsg = *blanks
004300011001
004400011001     ** End program
004500011001     C                   if        *in03
004600011001     C                   eval      *inlr = '1'
004700011001     C                   return
004800011001     C                   endif
004900011001
005000011001     ** Employee Type List
005100011001     C                   if        *in01
005200011001     C                   call      'HMRENQ08R'
005300011003     C**                 parm                    type_list#
005400011001     C                   iter
005500011001     C                   endif
005600011001
005700020211
005800011001     ** Validate List
005900020412     C                   if        list <> *blanks and list <> '*ALL'
006000020412     C                               and list <> '*allactive'
006100011001     C                   movea     list          run_list
006200011001     C                   eval      cnt  = 1
006300011001     **
006400011001     C                   dow       run_list(cnt) <> *blank
006500011003     C                   callp     get_cat(run_list(cnt):*omit:*omit:*omit
006600011003     C                                     :error)
006700011003     C                   if        error
006800011001     C                   eval      errmsg = msg1 + run_list(cnt)
006900011001     C                   leave
007000011001     C                   endif
007100011001     C                   eval      cnt  = cnt + 1
007200011001     C                   enddo
007300011001     ** display errmsg
007400011001     C                   if        errmsg <> *blanks
007500011001     C                   iter
007600011001     C                   endif
007700011001     C                   else
007800020412     C                   if        list = *blanks
007900020412     C                   eval      list = '*ALLACTIVE'
008000011001     C                   endif
008100020412     C                   endif
008200011001
008300011001     C                   leave
008400011001     C                   enddo
008500011001
008600011003     ** Move type field to array
008700020412     C                   if        list = '*ALLACTIVE'
008800020211     C     *loval        setll     empfmt
008900020211     C                   read      empfmt
009000020211     C                   dow       not(%eof)
009100020211     C                   if        empflag = 'A'
009200020211     C                   eval      cnt  = cnt + 1
009300020211     C                   eval      run_list(cnt) = emptype
009400020211     C                   endif
009500020211     C                   read      empfmt
009600020211     C                   enddo
009700020211     C                   endif
009800020211     **
009900020409     C                   select
010000020411     ** Employee Class selection
010100020411     C                   when      emp_class <> *blanks
010200020411     c                   eval      qry_select = ' class = ''' +
010300020411     C                                    emp_class + ''''
010400020411     C                   if        %scan('P':emp_class) = 0
010500020411     C     ' '           check     emp_class     pos
010600020411     C                   eval      pos = pos - 1
010700020411     C     ' ':'P'       xlate     emp_class:pos emp_class
010800020411     c                   eval      qry_select = %trim(qry_select) +
010900020411     C                                    ' *or class = ''' +
011000020411     C                                    emp_class + ''''
011100020411     C                   endif
011200020411     ** Employee type selection
011300020411     C                   when      list <> *blanks
011400020412     C                   if        list = '*ALL'
011500011003     C                   eval      qry_select = *blanks
011600011003     C                   else
011700011003     C                   eval      cnt = 1
011800020411     ** setup query select parameter for report run
011900011003     c                   eval      qry_select = ' status = ''' +
012000011003     C                                    run_list(cnt) + ''''
012100011003     C                   eval      cnt = cnt + 1
012200011003     C                   dow       run_list(cnt) <> *blank
012300020411     ** setup query select parameter for report run
012400011003     **
012500011003     c                   eval      qry_select = %trim(qry_select) +
012600011003     C                                    ' *or status = ''' +
012700011003     C                                    run_list(cnt) + ''''
012800011003     C                   eval      cnt = cnt + 1
012900011003     C                   enddo
013000011003     C                   endif
013100020409
013200020409     C                   endsl
013300011003     ** Run report
013400020408     C                   callb     'LI003C'
013500011003     C                   parm                    seq
013600011003     C                   parm                    qry_select
013700020409     C                   parm                    prno
013800020411     C                   parm                    type
013900011001
014000011003     C                   eval      errmsg = msg2
014100011003
014200011001     ***************************************************
014300011001     ** First initilisation routine for all programs  **
014400011001     ***************************************************
014500011001     C     aainit        Begsr
014600011001     C                   eval      @title = arr(1)
014700011001     C                   eval      coname = get_coname
014800020412     C                   eval      list = '*ALL'
014900011001     **
015000011001     C                   ENDSR
015100011001**
015200020408     Employee Liability Detail
