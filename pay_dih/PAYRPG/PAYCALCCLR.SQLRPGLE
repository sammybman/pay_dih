000100950405     H DECEDIT('.') DATEDIT(*DMY/)
000200090415      //*
000300090415      //* Regular time & Overtime calculations
000400090415      //* PROGRAMMER----** C.WILLIAMS  ADJ. S.BRITTON
000500090415      //*
000600090415      //
000700010426     Drun_type         s              1
000800131206     Dmim_type         s              1
000900131206     Dact_type         s              1
001000090415      //
001100090415      //-------------------------------------------------------------------------
001200010426     C     *entry        plist
001300010426     C                   parm                    run_type
001400131206     C                   parm                    mim_type
001500090415      //
001600070525      /free
001700110711           Exec Sql
001800110711           DELETE FROM PAYTIMFL01 WHERE TIMRTYPE = :run_type;
001900110711          //
002000110711           Exec Sql
002100110711           DELETE FROM WHCOMMPAY WHERE cpayrtype = :run_type;
002200110711         //
002300110711          Exec Sql
002400110711          DELETE FROM TIMECRDF WHERE TIMC_PTYP = :run_type;
002500131204
002600131204       //...............................................//
002700131204       // Reset payroll run defaults to previous value  //
002800131204       //...............................................//
002900131204
003000131206       if mim_type <> *blanks;
003100131206          act_type = mim_type;
003200131206           else;
003300131206          act_type = run_type;
003400131206       endif;
003500131206
003600131206       // exec sql
003700131206       //    update payrdef set prdfflag =
003800131206       //       (SELECT prcpvalue from payrtchglg
003900131206       //         where prcrtype = payrdef.prdfptype
004000131206       //         and prcptype = payrdef.prdftype and prcnrflag = 'Y'
004100131206       //         and prcsts = 'A' and prcrtype = :act_type)
004200131206       //       where  exists
004300131206       //       (SELECT * from payrtchglg
004400131206       //         where prcrtype = payrdef.prdfptype
004500131206       //         and prcptype = payrdef.prdftype and prcnrflag = 'Y'
004600131206       //         and prcsts = 'A' and prcrtype = :act_type) with NC;
004700131206
004800131204
004900131206          exec sql
005000131206             merge into payrdef as def
005100131206               using  payrtchglg as log
005200131213                  on def.prdfptype = log.prcrtype
005300131213                  and def.prdftype = log.prcptype and log.prcnrflag = 'Y'
005400131206                  and log.prcsts = 'A' and log.prcrtype = :act_type
005500131206               when MATCHED then
005600131206                  update  set def.prdfflag =
005700131216                     log.prcpvalue
005800140107               when not MATCHED and log.prcsts = 'A' and log.prcnrflag = 'Y'
005900140113                  and log.prcrtype = :act_type
006000140107                 THEN
006100131206                  insert values('A', log.prcrtype, log.prcptype,
006200131216                                log.prcpvalue) with NC;
006300131206
006400131206
006500131204          exec sql
006600131204             update payrtchglg set prcsts = 'D'
006700131204                where  prcsts = 'A' and prcnrflag = 'Y' and
006800131206                prcrtype = :act_type with NC;
006900131204
007000070525
007100070525           return;
007200070525      /end-free
007300060224
