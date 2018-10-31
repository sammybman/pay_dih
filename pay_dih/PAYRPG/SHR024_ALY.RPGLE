000100040602     H DATEDIT(*DMY/)  bnddir('DISDIR') dftactgrp(*no) actgrp('QILE')
000200950919     F**
000300020926     F** STICKERS FOR EXEC EMP --1 STICKER PER PERSON
000400950919     F** PROGRAMMER------T.I.BYNOE  ADJ. S.BRITTON
000500950919     F*
000600020926     FQSYSPRT   O    F  132        PRINTER
000700950919     D*
000800040602      /copy *libl/payrpg,paypr
000900950919     D                SDS
001000950919     D  @PGM             *PROC
001100040602     D  count#         s              5  0 inz(0)
001200040602     D  counter        s              5  0
001300040602     D  dname          s             40
001400040602     D  @dname         s                   like(dname)
001500040602     D  @christ        s                   like(christ)
001600040602     D  @surnam        s                   like(surnam)
001700040602     D  @descrp        s                   like(descrp)
001800950919     **
001900031029     C
002000040602      /free
002100040602           dow  not get_emp_info2('R':-1:emp_struc:count#);
002200040602
002300040602              IF  STATUS = 'E';
002400040602                   dname = get_dept_info(dept);
002500040602
002600040602                counter += 1;
002700040602
002800040602                 if   %rem(counter:2)  <>  0;
002900040602
003000040602                    @CHRIST = christ;
003100040602                    @SURNAM  = surnam;
003200040602                    @DESCRP  = descrp;
003300040602                    @DNAME  = dname;
003400040602                   else;
003500040602
003600040602                    EXCEPT    LABEL;
003700040602                  endif;
003800040602
003900040602              ENDIF;
004000040602      /end-free
004100040602     **********************************************************
004200040602     ** GET ADDRESS FROM EMPLOYEE MASTER FILE                 *
004300040602     **********************************************************
004400040602     ** LEFT LABEL TO EMP
004500040602     C*                  MOVEL     CHRIST        @CHRIST          20
004600040602     C*                  MOVEL     SURNAM        @SURNAM          20
004700040602     C*                  MOVEL     DESCRP        @DESCRP          29
004800040602     C*                  MOVEL     DNAME         @DNAME           40
004900040602     C*
005000040602     C*
005100040602     C*                  MOVEL     CHRIST        @CHRIST          20
005200040602     C*                  MOVEL     SURNAM        @SURNAM          20
005300040602     C*                  MOVEL     DESCRP        @DESCRP          29
005400040602     C*                  MOVEL     DNAME         @DNAME           40
005500040602     C*
005600040602      /free
005700040602         enddo;
005800040602         *inlr = *on;
005900040602         return;
006000040602      /end-free
006100040602
006200950919     *************************************************************
006300950919     *************************************************************
006400950919     ** LABEL PRINT FILE FOR ADVERTISING OR MAILING SHAREHOLDERS *
006500950919     *************************************************************
006600020926     OQSYSPRT   E            LABEL       1
006700020926     OQSYSPRT   E            LABEL       1
006800040602     O                       @CHRIST        B    21
006900040602     O                       CHRIST         B    54
007000040602     O                       @SURNAM        B    33
007100040602     O                       SURNAM         B    69
007200020926     OQSYSPRT   E            LABEL       1
007300020926     O                       @DESCRP        B    30
007400040602     O                       DESCRP         B    63
007500040603     OQSYSPRT   E            LABEL       1
007600020926     O                       @DNAME         B    41
007700040603     O                       DNAME          B    74
007800040602     OQSYSPRT   E            LABEL       2
007900040602     OQSYSPRT   E            LABEL       2
