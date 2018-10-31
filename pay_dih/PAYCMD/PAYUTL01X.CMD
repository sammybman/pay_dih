000100000000/*-------------------------------------------------------------------*/
000200000000/*                                                                   */
000300000000/*  Compile options:                                                 */
000400000000/*                                                                   */
000500150930/*    CrtCmd Cmd( ADJTAXGRO )                                        */
000600150930/*           Pgm( PAYUTL01R )                                        */
000700150930/*           SrcMbr( PAYUTL01X )                                     */
000800150930/*           VldCkr( PAYUTL01V )                                     */
000900150930/*           HlpPnlGrp( PAYUTL01H )                                  */
001000000000/*           HlpId( *CMD )                                           */
001100151002/*      //   PmtOvrPgm( PAYUTL01O )    //                            */
001200000000/*                                                                   */
001300000000/*-------------------------------------------------------------------*/
001400150930         Cmd       Prompt( 'Adjust Taxable Gross' )
001500000000
001600000000
001700150930         Parm      YEAR          *Dec        4             +
001800000000                   Min( 1 )                                +
001900000000                   Choice( *NONE )                         +
002000000000                   KeyParm( *YES )                         +
002100150930                   Prompt( 'Year to Adjust' )
002200000000
002300150930         Parm      PAYTYPE       *Dec        2             +
002400150930                   Min( 1 )                                +
002500150930                   Choice( *NONE )                         +
002600150930                   Prompt( 'Payroll Type' )
002700150930
002800000000
002900150930         Parm      STRDATE       *Date                     +
003000000000                   Min( 1 )                                +
003100000000                   Choice( *NONE )                         +
003200150930                   Prompt( 'Start Payroll Run Date' )
003300000000
003400150930         Parm      ENDDATE       *Date                     +
003500000000                   Min( 1 )                                +
003600000000                   Choice( *NONE )                         +
003700150930                   Prompt( 'End Payroll Run Date' )
003800000000
003900000000
004000000000
