000100950930     H DECEDIT('.') DATEDIT(*DMY/)
000200151009     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300950930     F* EMPLOYEE LOANS & C.O.E YEAR END LISTINGS (DEBITS & CREDITS)
000400981006     F* PROGRAMMER:-Samuel Britton
000500020302     F* LIABILITY MASTER FILE
000600981006     Fliabm     if   e           k disk
000700981006
000800050513     Fqsysprt   o    f  132        printer oflind(*inof)
000900020302     **
001000151009      /copy qcpysrc,genpr
001100151009      /copy qcpysrc,emppr
001200151009      /copy *libl/qcpysrc,PAYPR
001300981006     **
001400981006     D                sds
001500981006     D  @pgm             *proc
001600981006     D  @user                254    263
001700981006     D  @sdate               276    281  0
001800981006     D  @stime               282    287  0
001900981006     **
002000981006     D la              s             11  2 dim(12)
002100981006     **
002200981006     D @hdr_cnst       s             55
002300981006     D end_date        s              8  0
002400020302     D str_date        s              8  0
002500020302     D month           s              9
002600020302     D name#           s             40
002700020302     D coname          s             30
002800950930     **
002900981006     **
003000981006     Ilifmt
003100981006     I              liamt01                     la(01)
003200981006     I              liamt02                     la(02)
003300981006     I              liamt03                     la(03)
003400981006     I              liamt04                     la(04)
003500981006     I              liamt05                     la(05)
003600981006     I              liamt06                     la(06)
003700981006     I              liamt07                     la(07)
003800981006     I              liamt08                     la(08)
003900981006     I              liamt09                     la(09)
004000981006     I              liamt10                     la(10)
004100981006     I              liamt11                     la(11)
004200981006     I              liamt12                     la(12)
004300981006     **
004400981006     C                   except    rep_hedr
004500981006     **
004600981006     C     typ           chain     lifmt                              lr
004700981006     **
004800981006     C                   dow       not(*inlr)
004900981006     C                   if        *inof
005000981006     C                   except    rep_hedr
005100981006     C                   setoff                                       of
005200981006     C                   endif
005300981006     **
005400981006     ** Select employees if balance not equal to zero
005500981006     C                   if        la(emth) > *zeros and *in66
005600981006     C                             or la(emth) < *zeros and not(*in66)
005700981006
005800020302     ** get employee name
005900041003      /free
006000041003           get_emp_info(liemp:*omit:' ':name#);
006100041003      /end-free
006200981006     **
006300981006     C                   add       la(emth)      finamt           11 2
006400981006     C                   except
006500981006     C                   endif
006600981006     C
006700981006     **
006800981006     ** Print employee total lines
006900981006     **
007000981006     C*                  except    total
007100981006     C*                  endif
007200981006
007300981006     C     typ           reade     lifmt                                  lr
007400981006     C                   enddo
007500981006     **
007600981006     ** Print report final totals
007700981006     C                   except    total
007800950930     **
007900020302     C     *inzsr        begsr
008000020302     **
008100020302     C     *entry        plist
008200020302     C                   parm                    prd               6 0
008300020302     C                   parm                    typ               2 0
008400020302     C                   PARM                    typx2             1
008500020302     **
008600020302     C                   movel     prd           rp_year           4
008700020302     C                   move      prd           emth              2 0
008800020302     C                   movel     prd           year              4 0
008900981006     **
009000981006     C     typx2         comp      'D'                                    66
009100050513      /free
009200050513            get_ltype_info('RD':typ:ematf_struc);
009300050513      /end-free
009400981006     C*    emdsp         cat(p)    baltxt:1      @emp_cnst
009500981006     **
009600981006     **
009700020302     C                   eval      month = get_month(emth)
009800981006     C     emdsp         cat(p)    'as at':1     @hdr_cnst
009900020302     C                   cat       month:1       @hdr_cnst
010000981006     C                   cat       rp_year:1     @hdr_cnst
010100981006     **
010200020302     C                   eval      coname = get_coname
010300020302     ** get period  dates
010400020302     C                   callp     get_prd_info('*rng':*blanks:prd:
010500020302     C                                        str_date:end_date)
010600950930     **
010700950930     C                   ENDSR
010800981006
010900981006     ** OUTPUT REPORT
011000050513     Oqsysprt   e            rep_hedr    2  1
011100981006     O                       @pgm                10
011200020302     O                       coname              77
011300981006     O                       @sdate        Y    130
011400981006     O          e            rep_hedr       2
011500981006     O                       @user               10
011600981006     O          e            rep_hedr       2
011700981006     O                       @hdr_cnst           57
011800981006     O          e            rep_hedr       2
011900981006     O                                            8 'Code:-'
012000981006     O                       emacct            +  0
012100981006     O                       PAGE          Z    110
012200981006     O                       @stime             130 '  :  :  '
012300981006     **
012400981006     O          e            rep_hedr       1
012500981006     O                                           28 'E M P.  N O.'
012600981006     O                                           60 'E M P L O Y E E  N A M E'
012700981006     O                                           98 'B A L A N C E'
012800981006     O          e            rep_hedr       2
012900981006     O                                          109 'AT'
013000991010     O                       end_date           122 '    /  /  '
013100950930     O                     66                   110 '(DEBITS)'
013200950930     O                    N66                   111 '(CREDITS)'
013300981006     **
013400981006     O          e                           2
013500020302     O                       liemp         z     24
013600021002     O                       name#               73
013700981006     O                       la(emth)      ab    97
013800981006
013900981006     O          e            total       1       02
014000981006     O                       finamt        a     97
014100950930     O                                          100 '**'
014200981006     **
