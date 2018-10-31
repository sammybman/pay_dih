000100171024     H alwnull(*usrctl)
000200950921     FXMS006D   CF   E             WORKSTN
000300061129     FXMSWARD   UF A E           K DISK    usropn
000400021125     FEMPMASt   IF   E           K DISK
000500041002     **
000600041002     D Line_command    PR                  extpgm('QCMDEXC')
000700041002     D  command                     200    const
000800041002     d  length                       15  5 const
000900041002     **
001000041002     D CMD             c                   'CLRPFM XMSWARD'
001100041002     D CLEAN           c                   'CLEAN'
001200041002     **
001300950921     D                SDS
001400950921     D  @PGM             *PROC
001500041002     **
001600950719     C                   MOVE      '0'           *IN
001700041002     C                   dow       0 = 0
001800041002
001900041002      /free
002000041002          EXFMT  SELOPT;
002100041002
002200041002            if *in03;
002300041002              *inlr = *on;
002400041002              return;
002500041002            endif;
002600041002
002700041002           *in30 = *off;
002800041002           *in31 = *off;
002900041002
003000041002            if *in10;
003100041002              exfmt conrec;
003200041002              if conopt = CLEAN;
003300061129                close(e) xmsward;
003400041002                line_command(cmd:%size(cmd));
003500041002                *in30 = *on;
003600041002               else;
003700041002                *in31 = *on;
003800041002              endif;
003900041002              iter;
004000041002            endif;
004100041002
004200061129            open(e) xmsward;
004300041002      /end-free
004400021125     C     EMP           CHAIN     EMPrec                             01
004500041002
004600041002      /free
004700041002            if *in01;
004800041002              iter;
004900041002            endif;
005000041002      /end-free
005100041002
005200950815     C     EMP           CHAIN     XMSWARD                            02
005300041002
005400950815     C     *IN02         IFEQ      '1'
005500950815     C                   Z-ADD     *ZEROS        XAWARD
005600950815     C                   ENDIF
005700041002
005800950718     C                   EXFMT     REC
005900041002      /free
006000041002            if *in03;
006100041002              *inlr = *on;
006200041002              return;
006300041002            endif;
006400041002      /end-free
006500041002
006600950719     C     *IN04         IFEQ      '1'
006700950815     C                   DELETE    XMAREC
006800950719     C                   ELSE
006900950719     C     *IN04         IFEQ      '0'
007000950815     C     *IN02         ANDEQ     '0'
007100950815     C                   UPDATE    XMAREC
007200950719     C                   ELSE
007300950719     C     *IN04         IFEQ      '0'
007400950815     C     *IN02         ANDEQ     '1'
007500950815     C     XAWARD        ANDNE     *ZEROS
007600950815     C                   WRITE     XMAREC
007700950719     C                   ENDIF
007800950719     C                   ENDIF
007900950719     C                   ENDIF
008000041002
008100041002     C                   enddo
