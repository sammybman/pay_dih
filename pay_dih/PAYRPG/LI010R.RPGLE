000100950615     H DECEDIT('.') DATEDIT(*DMY/)
000200091121     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300011001     ** Employee Record Listing
000400020211     Femptypel01if   e           k disk
000500021003     Fli010d    cf   e             workstn
000600180315     F/IF DEFINED(WEB)
000700180315     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800180315     F/ENDIF
000900011001     **
001000110627      /copy QCPYSRC,genpr
001100110627      /copy QCPYSRC,PAYPR
001200011001     **
001300011001     D                sds
001400951004     D  @PGM             *PROC
001500180315     D  @USER                358    367
001600011001     **
001700011001     D arr             s             40    dim(1) ctdata perrcd(1)
001800011003     d qry_select      s            200
001900020409     D prno            s              6  0
002000020411     D type            s              2  0
002100011001     D cnt             s              2  0
002200020411     D pos             s              1  0
002300011001     D run_list        s              1    dim(20)
002400011003     D error           s               n
002500011003     D msg1            c                   'Invalid Employee Type -'
002600011003     D msg2            c                   'Report sent to print queue'
002700011001
002800011106     **
002900020410     C     *entry        plist
003000020411     C                   parm                    prno
003100020411     C                   parm                    type
003200020411
003300011001     C                   exsr      aainit
003400020211     C                   eval      cnt  = *zeros
003500011001
003600011001     ******************************
003700011001     ** Selection Report Options **
003800011001     ******************************
003900011001     C                   dow       0 = 0
004000011001     C                   exfmt     fmt1
004100011008     C                   eval      errmsg = *blanks
004200011001
004300011001     ** End program
004400011001     C                   if        *in03
004500011001     C                   eval      *inlr = '1'
004600011001     C                   return
004700011001     C                   endif
004800011001
004900011001     ** Employee Type List
005000011001     C                   if        *in01
005100011001     C                   call      'HMRENQ08R'
005200011003     C**                 parm                    type_list#
005300011001     C                   iter
005400011001     C                   endif
005500011001
005600020211
005700011001     ** Validate List
005800020412     C                   if        list <> *blanks and list <> '*ALL'
005900021003     C                               and list <> '*ALLACTIVE'
006000021003     C                               and list <> '*ALLINACTIVE'
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
008700021003     C                   if        list = '*ALLACTIVE' or
008800021003     C                               list = '*ALLINACTIVE'
008900020211     C     *loval        setll     empfmt
009000020211     C                   read      empfmt
009100020211     C                   dow       not(%eof)
009200021003     C                   if        empflag = 'A' and list = '*ALLACTIVE' or
009300021003     C                             empflag = 'I' and list = '*ALLINACTIVE'
009400020211     C                   eval      cnt  = cnt + 1
009500020211     C                   eval      run_list(cnt) = emptype
009600020211     C                   endif
009700020211     C                   read      empfmt
009800020211     C                   enddo
009900020211     C                   endif
010000020211     **
010100020409     C                   select
010200020411     ** Employee Class selection
010300020411     C                   when      emp_class <> *blanks
010400020411     c                   eval      qry_select = ' class = ''' +
010500020411     C                                    emp_class + ''''
010600020411     C                   if        %scan('P':emp_class) = 0
010700020411     C     ' '           check     emp_class     pos
010800020411     C                   eval      pos = pos - 1
010900020411     C     ' ':'P'       xlate     emp_class:pos emp_class
011000020411     c                   eval      qry_select = %trim(qry_select) +
011100020411     C                                    ' *or class = ''' +
011200020411     C                                    emp_class + ''''
011300020411     C                   endif
011400020411     ** Employee type selection
011500020411     C                   when      list <> *blanks
011600020412     C                   if        list = '*ALL'
011700011003     C                   eval      qry_select = *blanks
011800011003     C                   else
011900011003     C                   eval      cnt = 1
012000020411     ** setup query select parameter for report run
012100011003     c                   eval      qry_select = ' status = ''' +
012200011003     C                                    run_list(cnt) + ''''
012300011003     C                   eval      cnt = cnt + 1
012400011003     C                   dow       run_list(cnt) <> *blank
012500020411     ** setup query select parameter for report run
012600011003     **
012700011003     c                   eval      qry_select = %trim(qry_select) +
012800011003     C                                    ' *or status = ''' +
012900011003     C                                    run_list(cnt) + ''''
013000011003     C                   eval      cnt = cnt + 1
013100011003     C                   enddo
013200011003     C                   endif
013300020409
013400020409     C                   endsl
013500011003     ** Run report
013600021003     C                   callb     'LI010C'
013700011003     C                   parm                    seq
013800011003     C                   parm                    qry_select
013900020409     C                   parm                    prno
014000021003     C                   parm                    asset_flag
014100011001
014200011003     C                   eval      errmsg = msg2
014300011003
014400011001     ***************************************************
014500011001     ** First initilisation routine for all programs  **
014600011001     ***************************************************
014700011001     C     aainit        Begsr
014800011001     C                   eval      @title = arr(1)
014900011001     C                   eval      coname = get_coname
015000021003     C                   eval      list = '*ALLACTIVE'
015100011001     **
015200011001     C                   ENDSR
015300011001**
015400020408     Employee Liability Detail
