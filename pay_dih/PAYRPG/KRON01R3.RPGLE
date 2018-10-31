000100110221     H DATFMT(*ISO)
000200110221      // PROGRAMMER - S.BRITTON
000300980205     FKRON01D   CF   E             WORKSTN
000400110221
000500980205     D                SDS
000600980205     D  @PGM             *PROC
000700110221     D  @user                254    263
000800110221
000900110222     D**               PI                  extpgm('KRON01R2')
001000110222     D**@msgid                        7
001100110221
001200110221
001300110221     D temp            S              4  0
001400110221     D user#           S             10
001500110222     D starttimestamp  S               Z
001600110222     D string          S             26
001700110221
001800110222     D**MD             C                   CONST('CLRPFM KINTF')
001900110221
002000110221
002100110221
002200110221      /FREE
002300110221       user# = @user;
002400110222        //string = '1960-09-29-12.34.56.000000';
002500110222        starttimestamp =  %timestamp(
002600110222               %char(%date()) + '-00.00.00.000000');
002700110222        dsply starttimestamp;
002800110222        *inlr = *on;
002900110222        return;
003000110222
