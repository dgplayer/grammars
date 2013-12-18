grammar pl_psm;

options 
{
    language=Java;
    output=AST;
}

// ---------------------------------------------------------------------------------------------
// Database Language SQL - Procedure Language for Persistent Stored Modules                   **
// extensions to ISO/IEC CD 9075-4                                                            **
// International Standard for Database Language SQL: Persistent Stored Modules (SQL/PSM)      **
// ---------------------------------------------------------------------------------------------

/*

This software is made available under the BSD License:

Copyright (c) 2011, Whamtech Inc. by Douglas Godfrey
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this
      list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of the organization "Whamtech Inc." nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
OF THE POSSIBILITY OF SUCH DAMAGE.

*/

start_rule
	:	(	create_module_statement
	    |	create_trigger_statement
		|	create_procedure_statement
		|	create_function_statement
		|	procedure_statement_list
		)
//		EOF?
	;
	
// ---------------------------------------------------------------------------------------------
// ** Lexer Definitions                                                                       **
// ---------------------------------------------------------------------------------------------

// Case independant reserved words and Keywords

// Use 2 trailing underscores to avoid name conflicts with Std::Template libraries and Windows 

fragment A__
	:	'A' | 'a';

fragment B__ 
	:	'B' | 'b';

fragment C__ 
	:	'C' | 'c';
	
fragment D__ 
	:	'D' | 'd';
	
fragment E__
	:	'E' | 'e';

fragment F__
	:	'F' | 'f';

fragment G__
	:	'G' | 'g';

fragment H__
	:	'H' | 'h';
	
fragment I__
	:	'I' | 'i';
	
fragment J__
	:	'J' | 'j';
	
fragment K__
	:	'K' | 'k';
	
fragment L__
	:	'L' | 'l';
	
fragment M__ 
	:	'M' | 'm';
	
fragment N__
	:	'N' | 'n';
	
fragment O__
	:	'O' | 'o';
	
fragment P__
	:	'P' | 'p';
	
fragment Q__
	:	'Q' | 'q';
	
fragment R__
	:	'R' | 'r';
	
fragment S__
	:	'S' | 's';
	
fragment T__
	:	'T' | 't';
	
fragment U__
	:	'U' | 'u';
	
fragment V__
	:	'V' | 'v';
	
fragment W__
	:	'W' | 'w';
	
fragment X__
	:	'X' | 'x';
	
fragment Y__
	:	'Y' | 'y';
	
fragment Z__
	:	'Z' | 'z';
	
fragment Underscore
	:	'_';

fragment One__
	:	'1';
	
fragment Two__
	:	'2';
	
fragment Three__
	:	'3';
	
fragment Four__
	:	'4';
	
fragment Five__
	:	'5';
	
fragment Six__
	:	'6';
	
fragment Seven__
	:	'7';
	
fragment Eight__
	:	'8';
	
fragment Nine__
	:	'9';
	
fragment Zero__
	:	'0';

// ---------------------------------------------------------------------------------------------
// ** Reserved Word Definitions                                                               **
// ---------------------------------------------------------------------------------------------

R_Abs			:	(A__ B__ S__ );
R_Acos			:	(A__ C__ O__ S__ );
R_After			:	(A__ F__ T__ E__ R__ );
R_Allocate		:	(A__ L__ L__ O__ C__ A__ T__ E__ );
R_And			:	(A__ N__ D__ );
R_Array			:	(A__ R__ R__ A__ Y__ );
R_As			:	(A__ S__ );
R_Ascii			:	(A__ S__ C__ I__ I__ );
R_Asin			:	(A__ S__ I__ N__ );
R_Associate		:	(A__ S__ S__ O__ C__ I__ A__ T__ E__ );
R_Atan			:	(A__ T__ A__ N__ );
R_Atan2			:	(A__ T__ A__ N__ Two__ );
R_Avg			:	(A__ V__ G__ );
R_Begin			:	(B__ E__ G__ I__ N__ );
R_Before		:	(B__ E__ F__ O__ R__ E__ );
R_Between		:	(B__ E__ T__ W__ E__ E__ N__ );
R_Bfile			:	(B__ F__ I__ L__ E__ );
R_BigInt		:	(B__ I__ G__ I__ N__ T__ );
R_Binary		:	(B__ I__ N__ A__ R__ Y__ );
R_Bit			:	(B__ I__ T__ );
R_Bit_Length	:	(B__ I__ T__  Underscore L__ E__ N__ G__ T__ H__ );
R_Blob			:	(B__ L__ O__ B__ );
R_Boolean		:	(B__ O__ O__ L__ E__ A__ N__ );
R_Both			:	(B__ O__ T__ H__ );
R_Byte			:	(B__ Y__ T__ E__ );
R_Call			:	(C__ A__ L__ L__ );
R_Caller		:	(C__ A__ L__ L__ E__ R__ );
R_Case			:	(C__ A__ S__ E__ );
R_Cast			:	(C__ A__ S__ T__ );
R_Char_Length	:	(C__ H__ A__ R__  Underscore L__ E__ N__ G__ T__ H__ );
R_Char			:	(C__ H__ A__ R__ );
R_Character_Length:	(C__ H__ A__ R__ A__ C__ T__ E__ R__  Underscore L__ E__ N__ G__ T__ H__ );
R_Character		:	(C__ H__ A__ R__ A__ C__ T__ E__ R__ );
R_Client		:	(C__ L__ I__ E__ N__ T__ );
R_Clob			:	(C__ L__ O__ B__ );
R_Close			:	(C__ L__ O__ S__ E__ );
R_Coalesce		:	(C__ O__ A__ L__ E__ S__ C__ E__ );
R_Collate       :   (C__ O__ L__ L__ A__ T__ E__ );
R_Column_Name	:	(C__ O__ L__ U__ M__ N__  Underscore N__ A__ M__ E__ );
R_Column		:	(C__ O__ L__ U__ M__ N__ );
R_Concat		:	(C__ O__ N__ C__ A__ T__ );
R_Condition		:	(C__ O__ N__ D__ I__ T__ I__ O__ N__ );
R_Console		:	(C__ O__ N__ S__ O__ L__ E__ );
R_Contains		:	(C__ O__ N__ T__ A__ I__ N__ S__ );
R_Context		:	(C__ O__ N__ T__ E__ X__ T__ );
R_Continue		:	(C__ O__ N__ T__ I__ N__ U__ E__ );
R_Cos			:	(C__ O__ S__ );
R_Count			:	(C__ O__ U__ N__ T__ );
R_Create		:	(C__ R__ E__ A__ T__ E__ );
R_Cursor		:	(C__ U__ R__ S__ O__ R__ );
R_DateDiff		:	(D__ A__ T__ E__ D__ I__ F__ F__);
R_DateTime		:	(D__ A__ T__ E__ T__ I__ M__ E__ );
R_Date			:	(D__ A__ T__ E__ );
R_Day_To_Second	:	(D__ A__ Y__  Underscore T__ O__  Underscore S__ E__ C__ O__ N__ D__ );
R_DayOfYear		:	(D__ A__ Y__ O__ F__ Y__ E__ A__ R__);
R_Day			:	(D__ A__ Y__ );
R_Decimal		:	(D__ E__ C__ I__ M__ A__ L__ );
R_Declare		:	(D__ E__ C__ L__ A__ R__ E__ );
R_Dec			:	(D__ E__ C__ );
R_Default		:	(D__ E__ F__ A__ U__ L__ T__ );
R_Delete		:	(D__ E__ L__ E__ T__ E__ );
R_Difference 	:	(D__ I__ F__ F__ E__ R__ E__ N__ C__ E__ );
R_Diagnostics	:	(D__ I__ A__ G__ N__ O__ S__ T__ I__ C__ S__ );
R_Do			:	(D__ O__ );
R_Double		:	(D__ O__ U__ B__ L__ E__ );
R_Elseif		:	(E__ L__ S__ E__ I__ F__ );
R_Else			:	(E__ L__ S__ E__ );
R_End			:	(E__ N__ D__ );	
R_Enum			:	(E__ N__ U__ M__ );
R_Exception		:	(E__ X__ C__ E__ P__ T__ I__ O__ N__ );
R_Exists		:	(E__ X__ I__ S__ T__ S__ );
R_Exit			:	(E__ X__ I__ T__ );
R_Exp			:	(E__ X__ P__ );
R_Extract		:	(E__ X__ T__ R__ A__ C__ T__ );
R_Fetch			:	(F__ E__ T__ C__ H__ );
R_FileCopy		:	(F__ I__ L__ E__ C__ O__ P__ Y__ );
R_FileCreate	:	(F__ I__ L__ E__ C__ R__ E__ A__ T__ E__ );
R_FileDelete	:	(F__ I__ L__ E__ D__ E__ L__ E__ T__ E__ );
R_FileDiff		:	(F__ I__ L__ E__ D__ I__ F__ F__ );
R_FilePrint		:	(F__ I__ L__ E__ P__ R__ I__ N__ T__ );
R_FileTouch		:	(F__ I__ L__ E__ T__ O__ U__ C__ H__ );
R_File			:	(F__ I__ L__ E__ );
R_Fixed			:	(F__ I__ X__ E__ D__ );
R_Float			:	(F__ L__ O__ A__ T__ );
R_Format		:	(F__ O__ R__ M__ A__ T__ );
R_For			:	(F__ O__ R__ );
R_Found			:	(F__ O__ U__ N__ D__ );
R_From			:	(F__ R__ O__ M__ );
R_Function      :   (F__ U__ N__ C__ T__ I__ O__ N__ );
R_Get			:	(G__ E__ T__ );
R_Guid			:	(G__ U__ I__ D__  | U__ U__ I__ D__ );	
R_Handler		:	(H__ A__ N__ D__ L__ E__ R__ );
R_Hold			:	(H__ O__ L__ D__ );
R_Hour			:	(H__ O__ U__ R__ );
R_If			:	(I__ F__ );
R_Igscore		:	(I__ G__ S__ C__ O__ R__ E__ );
R_Inet			:	(I__ N__ E__ T__ );
R_Insert		:	(I__ N__ S__ E__ R__ T__ );
R_Instead		:	(I__ N__ S__ T__ E__ A__ D__ );
R_Integer		:	(I__ N__ T__ E__ G__ E__ R__ );
R_Interval		:	(I__ N__ T__ E__ R__ V__ A__ L__ );
R_Into			:	(I__ N__ T__ O__ );
R_Int			:	(I__ N__ T__ );
R_In			:	(I__ N__ );
R_Is			:	(I__ S__ );
R_Iterate		:	(I__ T__ E__ R__ A__ T__ E__ );
R_Key			:	(K__ E__ Y__ );
R_Lcase			:	(L__ C__ A__ S__ E__ | L__ O__ W__ E__ R__ C__ A__ S__ E__ );
R_Leading		:	(L__ E__ A__ D__ I__ N__ G__ );
R_Leave			:	(L__ E__ A__ V__ E__ );
R_Length		:	(L__ E__ N__ G__ T__ H__ );
R_Like			:	(L__ I__ K__ E__ );
R_Load			:	(L__ O__ A__ D__ );
R_Local         :   (L__ O__ C__ A__ L__ );
R_Locate		:	(L__ O__ C__ A__ T__ E__ );
R_Locators		:	(L__ O__ C__ A__ T__ O__ R__ S__ );
R_Locator		:	(L__ O__ C__ A__ T__ O__ R__ );
R_Log10			:	(L__ O__ G__ One__ Zero__ );
R_Log2			:	(L__ O__ G__ Two__ );
R_Log			:	(L__ O__ G__ );
R_Long			:	(L__ O__ N__ G__ );
R_Loop			:	(L__ O__ O__ P__ );
R_Lower 		:	(L__ O__ W__ E__ R__ );
R_Ltrim			:	(L__ T__ R__ I__ M__ );
R_Macaddr		:	(M__ A__ C__ A__ D__ D__ R__ );
R_Max 			:	(M__ A__ X__ );
R_Message_Text	:	(M__ E__ S__ S__ A__ G__ E__  Underscore T__ E__ X__ T__ );
R_Message		:	(M__ E__ S__ S__ A__ G__ E__ );
R_Microsecond	:	(M__ I__ C__ R__ O__ S__ E__ C__ O__ N__ D__);
R_Millisecond	:	(M__ I__ L__ L__ I__ S__ E__ C__ O__ N__ D__);
R_Minute		:	(M__ I__ N__ U__ T__ E__ );
R_Min 			:	(M__ I__ N__ );		
R_Module        :   (M__ O__ D__ U__ L__ E__ );
R_Mod			:	(M__ O__ D__ );
R_Money			:	(M__ O__ N__ E__ Y__ );
R_Month			:	(M__ O__ N__ T__ H__ );
R_Nanosecond	:	(N__ A__ N__ O__ S__ E__ C__ O__ N__ D__);
R_Not			:	(N__ O__ T__ );
R_Null			:	(N__ U__ L__ L__ );
R_Numeric		:	(N__ U__ M__ E__ R__ I__ C__ );
R_Nvl2			:	(N__ V__ L__ Two__ );
R_Nvl			:	(N__ V__ L__ );
R_Octet_Length	:	(O__ C__ T__ E__ T__  Underscore L__ E__ N__ G__ T__ H__ );
R_Of			:	(O__ F__ );
R_On			:	(O__ N__ );
R_Open			:	(O__ P__ E__ N__ );
R_Or			:	(O__ R__ );
R_Path          :   (P__ A__ T__ H__ );
R_Parameter		:	(P__ A__ R__ A__ M__ E__ T__ E__ R__ );
R_Position		:	(P__ O__ S__ I__ T__ I__ O__ N__ );
R_Precision		:	(P__ R__ E__ C__ I__ S__ I__ O__ N__ );
R_Procedure		:	(P__ R__ O__ C__ E__ D__ U__ R__ E__ );	
R_Proc			:	(P__ R__ O__ C__ );
R_Quarter		:	(Q__ U__ A__ R__ T__ E__ R__);
R_Range			:	(R__ A__ N__ G__ E__ );
R_Readline		:	(R__ E__ A__ D__ L__ I__ N__ E__ );
R_Real			:	(R__ E__ A__ L__ );
R_Repeat		:	(R__ E__ P__ E__ A__ T__ );
R_Replace		:	(R__ E__ P__ L__ A__ C__ E__ );
R_Resignal		:	(R__ E__ S__ I__ G__ N__ A__ L__ );
R_Result		:	(R__ E__ S__ U__ L__ T__ );
R_Return_Status	:	(R__ E__ T__ U__ R__ N__  Underscore S__ T__ A__ T__ U__ S__ );
R_Returns		:	(R__ E__ T__ U__ R__ N__ S__ );
R_Return		:	(R__ E__ T__ U__ R__ N__ );
R_Round			:	(R__ O__ U__ N__ D__ );
R_RowID			:	(R__ O__ W__ I__ D__ );
R_Rowset		:	(R__ O__ W__ S__ E__ T__ );
R_Row_Count		:	(R__ O__ W__  Underscore C__ O__ U__ N__ T__ );
R_Rtrim			:	(R__ T__ R__ I__ M__ );
R_Schema        :   (S__ C__ H__ E__ M__ A__ );
R_Second		:	(S__ E__ C__ O__ N__ D__ );
R_Select		:	(S__ E__ L__ E__ C__ T__ );
R_Send			:	(S__ E__ N__ D__ );
R_Set			:	(S__ E__ T__ );
R_Signal		:	(S__ I__ G__ N__ A__ L__ );
R_Sin			:	(S__ I__ N__ );
R_Size			:	(S__ I__ Z__ E__ );
R_SmallInt		:	(S__ M__ A__ L__ L__ I__ N__ T__ );
R_SmallMoney	:	(S__ M__ A__ L__ L__ M__ O__ N__ E__ Y__ );
R_Soundex		:	(S__ O__ U__ N__ D__ E__ X__ );
R_Space			:	(S__ P__ A__ C__ E__ );
R_Sql_Exception	:	(S__ Q__ L__  Underscore E__ X__ C__ E__ P__ T__ I__ O__ N__ );
R_SqlState		:	(S__ Q__ L__ S__ T__ A__ T__ E__ );
R_Sql_Warning	:	(S__ Q__ L__  Underscore W__ A__ R__ N__ I__ N__ G__ );
R_Sqrt			:	(S__ Q__ R__ T__ );
R_StdDev		:	(S__ T__ D__ D__ E__ V__  | S__ T__ D__  Underscore D__ E__ V__  | S__ T__ D__  Underscore D__ E__ V__ I__ A__ T__ I__ O__ N__ );
R_Structure		:	(S__ T__ R__ U__ C__ T__ U__ R__ E__ );
R_Substr		:	(S__ U__ B__ S__ T__ R__ );
R_Substring 	:	(S__ U__ B__ S__ T__ R__ I__ N__ G__ );
R_SumSquare 	:	(S__ U__ M__ S__ Q__ U__ A__ R__ E__  | S__ U__ M__ S__ Q__ U__ A__ R__ E__ S__  | S__ U__ M__ O__ F__ S__ Q__ U__ A__ R__ E__ S__ );
R_Sum 			:	(S__ U__ M__ );
R_Synonyms 		:	(S__ Y__ N__ O__ N__ Y__ M__ S__ );
R_SysDate		:	(S__ Y__ S__ D__ A__ T__ E__ );
R_SysDateTime	:	(S__ Y__ S__ D__ A__ T__ E__ T__ I__ M__ E__ );
R_SysTime		:	(S__ Y__ S__ T__ I__ M__ E__ );
R_SysTimestamp	:	(S__ Y__ S__ T__ I__ M__ E__ S__ T__ A__ M__ P__ );
R_Table_Name	:	(T__ A__ B__ L__ E__  Underscore N__ A__ M__ E__ );
R_Table			:	(T__ A__ B__ L__ E__ );
R_Tan			:	(T__ A__ N__ );
R_Temporary     :   (T__ E__ M__ P__ O__ R__ A__ R__ Y__ );
R_Text			:	(T__ E__ X__ T__ );
R_Then			:	(T__ H__ E__ N__ );
R_Timestamp		:	(T__ I__ M__ E__ S__ T__ A__ M__ P__ );
R_Timezone_Hour	:	(T__ I__ M__ E__ Z__ O__ N__ E__  Underscore H__ O__ U__ R__ );
R_Timezone_Minute :	(T__ I__ M__ E__ Z__ O__ N__ E__  Underscore M__ I__ N__ U__ T__ E__ );
R_Timezone		:	(T__ I__ M__ E__ Z__ O__ N__ E__ );
R_Time			:	(T__ I__ M__ E__ );
R_TinyInt		:	(T__ I__ N__ Y__ I__ N__ T__ );
R_To_Char		:	(T__ O__  Underscore C__ H__ A__ R__ );
R_To_DateTime	:	(T__ O__  Underscore D__ A__ T__ E__ T__ I__ M__ E__ );
R_To_Date		:	(T__ O__  Underscore D__ A__ T__ E__ );
R_To_DSInterval	:	(T__ O__  Underscore D__ S__ I__ N__ T__ E__ R__ V__ A__ L__);
R_To_Number		:	(T__ O__  Underscore N__ U__ M__ B__ E__ R__ );
R_To_Timestamp	:	(T__ O__  Underscore T__ I__ M__ E__ S__ T__ A__ M__ P__ );
R_To_Time		:	(T__ O__  Underscore T__ I__ M__ E__ );
R_To_YMInterval	:	(T__ O__  Underscore Y__ M__ I__ N__ T__ E__ R__ V__ A__ L__);
R_To			:	(T__ O__ );
R_Trace			:	(T__ R__ A__ C__ E__ );
R_Trailing		:	(T__ R__ A__ I__ L__ I__ N__ G__ );
R_Trigger		:	(T__ R__ I__ G__ G__ E__ R__ );
R_Trim 			:	(T__ R__ I__ M__ );
R_Trunc			:	(T__ R__ U__ N__ C__ );
R_UBigInt		:	(U__ B__ I__ G__ I__ N__ T__ );
R_Ucase			:	(U__ C__ A__ S__ E__ );
R_UInteger		:	(U__ I__ N__ T__ E__ G__ E__ R__ );
R_UInt			:	(U__ I__ N__ T__ );
R_Undo			:	(U__ N__ D__ O__ );
R_Until			:	(U__ N__ T__ I__ L__ );
R_Update		:	(U__ P__ D__ A__ T__ E__ );
R_Upper 		:	(U__ P__ P__ E__ R__ );
R_Url			:	(U__ R__ L__ );
R_USmallInt		:	(U__ S__ M__ A__ L__ L__ I__ N__ T__ );
R_Value			:	(V__ A__ L__ U__ E__ );
R_Var 			:	(V__ A__ R__  | V__ A__ R__ I__ A__ N__ C__ E__ );
R_VarBinary		:	(V__ A__ R__ B__ I__ N__ A__ R__ Y__ );
R_VarChar		:	(V__ A__ R__ C__ H__ A__ R__ );
R_Varying		:	(V__ A__ R__ Y__ I__ N__ G__ );
R_View			:	(V__ I__ E__ W__ );
R_Virtual		:	(V__ I__ R__ T__ U__ A__ L__ );
R_Week			:	(W__ E__ E__ K__);
R_When			:	(W__ H__ E__ N__ );
R_While			:	(W__ H__ I__ L__ E__ );
R_With			:	(W__ I__ T__ H__ );
R_Writeline		:	(W__ R__ I__ T__ E__ L__ I__ N__ E__ );
R_Xml			:	(X__ M__ L__ );
R_Year_To_Month	:	(Y__ E__ A__ R__  Underscore T__ O__  Underscore M__ O__ N__ T__ H__ );
R_Year			:	(Y__ E__ A__ R__ );

ExecSQLStatement:	(E__ X__ E__ C__ ) WhiteSpace+ (S__ Q__ L__ ) ( options {greedy=false;} : . )* (E__ N__ D__ E__ X__ E__ C__ );
ExecTQLStatement:	(E__ X__ E__ C__ ) WhiteSpace+ (T__ Q__ L__ ) ( options {greedy=false;} : . )* (E__ N__ D__ E__ X__ E__ C__ );

S_Semicolon		:	';';
S_AssignEq		:	':=';
S_LeftParen		:	'(';
S_RightParen	:	')';
S_QuestionMark	:	'?';
S_Bar			:	'|';
S_Period		:	'.';
S_Comma			:	',';
S_Colon			:	':';
S_LeftBracket	:	'[';
S_RightBracket	:	']';
S_RelOp_EQ		:	'=';
S_RelOp_NE		:	'!=' | '<>';	
S_RelOp_GE		:	'>=' | '!<';
S_RelOp_GT		:	'>';
S_RelOp_LE		:	'<=' ! '!>';
S_RelOp_LT		:	'<';
	
fragment
DoubleQuote
	:	'"'
	;
	
fragment
SingleQuote
	:	'\''
	;
	
fragment
Escape
	:	'\\'
	;
	
fragment
StringChar
	:	( Escape SingleQuote | ~(SingleQuote) )
	;
	
fragment
StringTerm
	:	SingleQuote ( StringChar )* SingleQuote 
	;
	
V_String_Constant
	:	(StringTerm)+
	;
	
fragment
DigitOrLetter
	:	Digit | Letter;
	
V_SqlState_Constant
	:	DoubleQuote d1=DigitOrLetter 
					d2=DigitOrLetter 
					{IsNotDoubleZero($d1.text->chars, $d2.text->chars) }?
					(DigitOrLetter 
					(DigitOrLetter 
					(DigitOrLetter)? )? )? 
		DoubleQuote
	;
	
V_Char_Constant
	:	DoubleQuote (Escape DoubleQuote | ~DoubleQuote ) DoubleQuote
	;
	
V_Identifier	
	:	Letter (Letter | Digit | Underscore | DollarSign | HashSign)*
	;
	
fragment
Letter
	:	('a'..'z' | 'A'..'Z')
	;
	
fragment	
DollarSign
	:	'$'
	;
	
fragment
HashSign
	:	'#'
	;

fragment
Digit  		:  '0'..'9';

fragment
OctalDigit  :  '0'..'7';

fragment
BinaryBit	:	'0'..'1';

fragment
BinaryDigit	:	BinaryBit BinaryBit BinaryBit BinaryBit;

fragment
BinaryByte	:	BinaryDigit BinaryDigit;			

fragment
BinaryPair	:	BinaryByte BinaryByte;

fragment
BinaryQuad	:	BinaryPair BinaryPair;

fragment
HexDigit   	:	( Digit | A__ | B__ | C__ | D__ | E__ | F__ );

fragment
HexPair     :   HexDigit HexDigit;

fragment
HexQuad     :   HexPair HexPair;

fragment
HexInteger
	:	X__	HexPair ( HexPair ( HexQuad (HexQuad HexQuad)? )? )?
	;

fragment	
OctalInteger
	:	O__ OctalDigit+
	;

fragment	
Binary_Integer
	:	B__ BinaryByte ( BinaryByte ( BinaryPair (BinaryQuad)? )? )?
	;
	
fragment
PointFloat
	:	DecimalPoint Digit+ Exponent?
	;

fragment
Exponent
	:	( F__ | D__ | E__ ) ( PlusSign | MinusSign )? DigitQuad
	;

fragment
DigitQuad
	:	Digit ( Digit ( Digit ( Digit )? )? )?
	;
	
fragment
SignedInteger
	:	( PlusSign | MinusSign ) Digit+ 
	;
	
fragment
UnsignedInteger
	:	( '1'..'9' ) Digit* 
	;
	
fragment
V_Unsigned_Integer_Constant:;

fragment
V_Signed_Integer_Constant:;

fragment
V_Unsigned_Float_Constant:;

fragment
V_Signed_Float_Constant :;

Numeric_Constant	:
	(	'0'
			(	HexInteger
				{ _type  =  V_Unsigned_Integer_Constant; }

			|	OctalInteger
				{ _type  =  V_Unsigned_Integer_Constant; }
				
			|	Binary_Integer 
				{ _type  =  V_Unsigned_Integer_Constant; }
				
			|	PointFloat
				{ _type  =  V_Unsigned_Float_Constant; }
				
			|	Digit*
				{ _type  =  V_Unsigned_Integer_Constant; }
			)
			  
	|	UnsignedInteger
			(	PointFloat
				{ _type  =  V_Unsigned_Float_Constant; }
				
			|	{true}?
				{ _type  =  V_Unsigned_Integer_Constant; }
			)
			
	|	SignedInteger 
			(	PointFloat
				{
					_type  =  V_Signed_Float_Constant;
				}
				
			|	{true}?
				{
					_type  =  V_Signed_Integer_Constant;
				}
			)
			
	|	PointFloat
			{
				_type  =  V_Unsigned_Float_Constant;
			}
	);

fragment
DecimalPoint
	:	'.'
	;
	
fragment
PlusSign
	:	'+'
	;
	
fragment
MinusSign
	:	'-'
	;
	
S_Plus
	:	'+'
	;
	
S_Minus
	:	'-'
	;
	
S_Mult
	:	'*'
	;

S_Divide
	:	'/'
	;
	
S_Modulo
	:	'%'
	;
	
fragment
EndOfLine
	:	( ('\r' | '\n') => '\r\n' | '\r' | '\n' )
	; 
	
EOL	:	r=	EndOfLine
		{	$channel=HIDDEN;
			INPUT->line++;
			INPUT->charPositionInLine = 0;
		}
	;

fragment
WhiteSpace
	:	( ' '|'\t'|'\v' )
	;
	
WS	:	r=	WhiteSpace
		{	$channel=HIDDEN;
		}
    ;
    
fragment
SingleLineComment
	:	( '--' ~('\n'|'\r')* '\r'? '\n' )
	;
	
SL_COMMENT
    :	r=	SingleLineComment
		{	$channel=HIDDEN;
			fprintf(stderr, "SingleLineComment[\%s]",$r->getText($r)->chars); 
		}
    ;
    
fragment
MultiLineComment
	:	( '/*' ( options {greedy=false;} : . )* '*/' )
	;
	
ML_COMMENT
    :	r=	MultiLineComment
		{	$channel=HIDDEN;
			fprintf(stderr, "MultiLineComment[\%s]",$r->getText($r)->chars); 
		}
    ;
    
	
// ---------------------------------------------------------------------------------------------
// ** Parser Definitions                                                                      **
// ---------------------------------------------------------------------------------------------


// ---------------------------------------------------------------------------------------------
// ** Keyword Definitions                                                                      **
// ---------------------------------------------------------------------------------------------

//k_at                  : { !(strcasecmp((const char*)LT(1).text->chars, "AT")) }? 				
//						r=V_Identifier 
//						{ $r->setType($r, T_RESERVED); $r->user1 = T_RESERVED; fprintf(stderr, "Keyword[\%s]\n",$r.text->chars); }  ;

// ---------------------------------------------------------------------------------------------
// ** PL/PSM Statement Definitions                                                            **
// ---------------------------------------------------------------------------------------------

create_module_statement
  :   R_Create R_Module
      (   R_Default? R_Character R_Set character_set_name
          (R_Collate collation_name)?
      )?
      (   R_Schema  (R_Default | schema_name) 
      )?
      (   R_Path    schema_name (S_Comma schema_name)*
      )?
      (   declare_table_statement
          S_Semicolon
      )*
      (   (   R_Declare (R_Proc | R_Procedure)
	          procedure_declaration
		      R_As
		      block_statement
		      
		  |   R_Declare R_Function
              procedure_declaration
              R_Returns elementary_type_name
		      R_As
		      block_statement
		  )
		  S_Semicolon
      )+
      R_End R_Module
  ;
/*
CREATE MODULE <Module name>
   [[DEFAULT] CHARACTER SET charset_name]
    [COLLATE collation_name]]
   [ SCHEMA default <Schema name>]
   [ PATH <Schema name> [ {,<Schema name>}... ] ]
   [ DECLARE TABLE statement(s) ]
   <SQL-invoked routine>; ...
   END MODULE
*/
  
declare_table_statement
@init	{char * tableName = NULL;}
    :   R_Declare R_Local R_Temporary R_Table
        tbl=new_table_name
        { tableName = $tbl.text->chars; }
        { declareTempTable(tableName); }
        S_LeftParen
        column_definition[tableName] (S_Comma column_definition[tableName])
        S_RightParen
    ;   
    
column_definition[char * tableName]
@init   {char * typeName = NULL; char * defaultExp = NULL; bool isKey = false;}
    :	col=new_column_name 
        typ=datatype_name 
		{ typeName = $typ.text->chars; }
		(   R_Key  
		    { isKey = true; }
		)?
		(   R_Default exp=typed_constant[typeName]
		    { defaultExp = $exp.text->chars; }    
		)?
		{declareColumn(tableName, $col.text->chars, typeName, isKey, defaultExp);}
    ;
/*
DECLARE LOCAL TEMPORARY TABLE [ MODULE. ]<R_Table name>
   (<table element> [ {,<table element>}... ])
   [ ON COMMIT {PRESERVE ROWS | DELETE ROWS} ]

         <table element> ::=
         <Column definition> | 
         <R_Table Constraint> | 
         LIKE <R_Table name> | 
         <Column name> WITH OPTIONS <column option list>
*/
create_trigger_statement
	:	R_Create R_Trigger
		(	sch1=schema_name S_Period 
			(	name1=new_trigger_name 
				{declareSchemaTrigger($sch1.text->chars, $name1.text->chars);}
			|	name1=new_trigger_number
				{declareSchemaTrigger($sch1.text->chars, $name1.text->chars);}
			)
		
		|	(	name1=new_trigger_name 
				{declareTrigger($name1.text->chars);}
			|	name1=new_trigger_number
				{declareTrigger($name1.text->chars);}
			)
		)
		R_On
		(	R_Table
			(	sch2=schema_name S_Period 
				name2a=table_name 
				{IsSchemaTableName($sch2.text->chars, $name2a.text->chars) }?
			
			|	name2b=table_name 
				{IsTableName($name2b.text->chars);}
			)
		
		|	R_View
			(	sch3=schema_name S_Period 
				name3a=view_name 
				{IsSchemaTableName($sch3.text->chars, $name3a.text->chars) }?
			
			|	name3b=view_name 
				{IsTableName($name3b.text->chars);}
			)
		
		|	R_Column
			(	sch4=schema_name S_Period 
				name4a=table_name 
				name4a2=column_name[$sch4.text->chars, $name4a.text->chars]
				{IsSchemaColumnName($sch4.text->chars, $name4a.text->chars, $name4a2.text->chars) }?
			
			|	name4b=table_name 
				name4b2=column_name["", $name4b.text->chars] 
				{IsColumnName($name4b.text->chars, $name4b2.text->chars);}
			)
		
		|	R_Virtual? R_Key
			(	sch5=schema_name S_Period 
				name5a=table_name 
				name5a2=key_column_name[$sch5.text->chars, $name5a.text->chars] 
				{IsSchemaKeyName($sch5.text->chars, $name5a.text->chars, $name5a2.text->chars) }?
			
			|	name5b=table_name 
				name5b2=key_column_name["", $name5b.text->chars] 
				{IsKeyName($name5b.text->chars, $name5b2.text->chars);}
			)
		)
		trigger_event
		procedure_specifier
		R_As
		(	(R_Proc | R_Procedure)
			(	sch=schema_name S_Period 
				(	name1=procedure_name 
					{declareSchemaProcedure($sch.text->chars, $name1.text->chars);}
				|	num1=procedure_number
					{declareSchemaProcedure($sch.text->chars, $num1.text->chars);}
				)
			
			|	(	name2=procedure_name 
					{declareProcedure($name2.text->chars);}
				|	num2=procedure_number
					{declareProcedure($num2.text->chars);}
				)
			)
			
		|	block_statement
		)
	;

trigger_event
	:	(R_Before | R_After | R_Instead R_Of)
		(R_Insert | R_Update | R_Delete | R_Fetch | R_Load)
	;
	
procedure_specifier
	:	R_With
		procedure_option (S_Comma procedure_option)*
	;
	
procedure_option
	:	new_parameter_name S_AssignEq
		(	R_Table_Name
		|	R_Column_Name (S_Period R_Value)?
		|	R_RowID
		|	R_Date
		|	R_Time
		|	R_Timestamp
		|	constant 
		)
	;
	
/*
CREATE TRIGGER [schema_name.] trigger_name 
ON { TABLE schema_name[.table_name] 
   | VIEW schema_name[.view_name] 
   | COLUMN schema_name[.table_name][.column_name] 
   | [VIRTUAL] KEY schema_name[.table_name][.key_name] 
   }  
{ <trigger_event> <procedure_specifier> } [ ; ] 
 
<trigger_event> ::= 
{ BEFORE | R_After | INSTEAD OF } 
{ [ INSERT ] [ , ] [ UPDATE ] [ , ] [ DELETE ] [ , ] [ FETCH ]  [ , ] [ LOAD ]} 

<procedure_specifier> ::= 
[ WITH <procedure_option> [ ,...n ] ]
AS { [schema_name.] { procedure_name | number } | <sql_statement_block> }  
 
<procedure_option> ::=
[ @parameter_name =
  { table_name
  | column_name [.$R_Value]
  | $R_RowID
  | $R_Date
  | $R_Time
  | $R_Timestamp
  | constant } ]
*/

create_procedure_statement
	:	R_Create (R_Proc | R_Procedure)
	    procedure_declaration
		R_As
		block_statement
	;
	
create_function_statement
    :   R_Create R_Function
        procedure_declaration
        R_Returns elementary_type_name
		R_As
		block_statement
    ;
            
procedure_declaration
    :
		(	sch=schema_name S_Period 
			(	name1=new_procedure_name 
				{declareSchemaProcedure($sch.text->chars, $name1.text->chars);}
			|	num1=new_procedure_number
				{declareSchemaProcedure($sch.text->chars, $num1.text->chars);}
			)
		
		|	(	name2=new_procedure_name 
				{declareProcedure($name2.text->chars);}
			|	num2=new_procedure_number
				{declareProcedure($num2.text->chars);}
			)
		)
		S_LeftParen new_parameter (S_Comma new_parameter)* S_RightParen
	;
	
new_parameter
@init	{char * typeName = NULL}
	:	param=new_variable_name 
		typ=datatype_name  
		{typeName = $typ.text->chars;}
		(	R_Default exp=typed_constant[typeName]
			{declareParameterWithDefault($param.text->chars, $typ.text->chars, $exp.text->chars);}
			
		|	{declareParameter($param.text->chars, $typ.text->chars);}
		)
	;
// CREATE { PROC | PROCEDURE } [ schema_name.] { procedure_name | number } 
//     [ { @parameter_name data_type } [ = default_value ] ] [ ,...n ] 

block_statement
	:
	R_Begin S_Semicolon
	procedure_statement_list
	R_End
	;
	
procedure_statement_list
	:
	procedure_statement
	(S_Semicolon procedure_statement)*
	S_Semicolon?
	;

procedure_statement
	:
	(	block_statement
	|	exec_SQL_statement
    |	get_diagnostics_statement 
    |	allocate_cursor_statement
    |	associate_locator_statement
    |	call_statement
    |	case_statement
    |	close_cursor_statement
    |	declare_statement
//    	DECLARE <variable>
//    	DECLARE <condition>
//    	DECLARE <condition handler>
//    	DECLARE CURSOR
	|	fetch_cursor_statament
    |	for_statement
//  |	goto_statement
	|	if_then_else_statement
	|	iterate_statement
	|	leave_statement
	|	loop_statement
	|	open_cursor_statement
	|	repeat_statement
	|	resignal_statement
	|	return_statement
//	|	release_savepoint_statement	
//	|	rollback_statement
//	|	savepoint_statement
	|	set_statement
	|	signal_statement
	|	while_statement
    )
	;

exec_SQL_statement
	:	ExecSQLStatement
	;
	
exec_TQL_statement
	:	ExecSQLStatement
	;
	
select_statement
	:	R_Select
	;
	
get_diagnostics_statement 
	:	
	R_Get R_Diagnostics variable_name S_AssignEq (R_Return_Status | R_Row_Count | R_Exception)
	;
	
allocate_cursor_statement
	:
	R_Allocate cursor_variable_name R_Cursor R_For R_Result R_Set locator_variable_name
	;
	
associate_locator_statement
	:
	R_Associate (R_Result R_Set)? (R_Locator | R_Locators) S_LeftParen locator_variable_list S_RightParen R_With (R_Proc | R_Procedure) procedure_name
	;
	
locator_variable_list
	:
	locator_variable_name
	(S_Comma locator_variable_name)*
	;
	
call_statement
	:
	R_Call procedure_name S_LeftParen procedure_parameter_list S_RightParen
	;
	
procedure_parameter_list
	:
	(	variable_or_constant (S_Comma variable_or_constant)* (S_Comma S_QuestionMark)*
	|	S_QuestionMark  (S_Comma S_QuestionMark)*
	)
	;
	
variable_or_constant
	:	variable_name
	|	constant
	;
	
typed_constant[char * type]
	:	{ IsNumericType(type) }?	numeric_constant
	|	{ IsStringType(type) }?		V_String_Constant
	|	{ IsCharType(type) }?		V_Char_Constant
	|	{ IsDateType(type) }?		date_constant
	|	{ IsTimeType(type) }?		time_constant
	|	{ IsDateTimeType(type) }?	datetime_constant
	|	{ IsTimestampType(type) }?	timestamp_constant
	;
	
constant
	:	numeric_constant
	|	V_String_Constant
	|	V_Char_Constant
	|	date_constant
	|	time_constant
	|	datetime_constant
	|	timestamp_constant
	;
	
case_statement
	:
	R_Case
	(	var=variable_name	simple_case_body[$var.text->chars]
	|	searched_case_body
	)
	;
	
simple_case_body [char * var_name]
	:
	(R_When typed_expression[var_name] R_Then procedure_statement S_Semicolon)+
	R_End R_Case
	;
	
searched_case_body
	:
	(R_When condition R_Then procedure_statement S_Semicolon)+
	R_End R_Case
	;
	
close_cursor_statement
	:
	R_Close R_Cursor? cursor_variable_name
	;
	
declare_statement
@init	{char * typeName = NULL;}
	:
	R_Declare
	(	(R_Continue | R_Exit | R_Undo) R_Handler R_For (condition_variable_name | handler_condition) procedure_statement
	|	var=new_variable_name 
		(	R_Condition R_For cond=handler_condition
			{declareCondition($var.text->chars, $cond.text->chars);}
		|	R_Cursor (R_With R_Hold)? (R_With R_Return R_To (R_Caller | R_Client)? )? R_For procedure_statement
			{declareCursor($var.text->chars, $cond.text->chars);}
		|	typ=datatype_name 
			{typeName = $typ.text->chars;}
			R_Default exp=typed_constant[typeName]
			{declareVariable($var.text->chars, typeName, $exp.text->chars);}
		)
	)
	;

datatype_name
	:	elementary_type_name
	|	R_Range 		R_Of elementary_type_name
	|	R_Array 		R_Of elementary_type_name (array_bounds)+
	|	R_Set 		R_Of elementary_type_name
	|	R_Structure 	S_LeftParen structure_field (S_Comma structure_field)* S_RightParen
	|	R_Enum 		S_LeftParen constant 		  (S_Comma constant)* 	   S_RightParen
	;
	
array_bounds
	:	S_LeftBracket V_Unsigned_Integer_Constant S_RightBracket
	;
	
structure_field
	:	new_variable_name datatype_name
	;
	
elementary_type_name
	:	R_TinyInt
	|	R_Boolean
	|	R_SmallInt
	|	R_USmallInt
	|	R_Int
	|	R_UInt
	|	R_Integer
	|	R_UInteger
	|	R_BigInt
	|	R_UBigInt
	|	(	R_Decimal	
		|	R_Dec		
		|	R_Numeric	
		|	R_Fixed	
		)
		precision_scale?
	|	R_Float
	|	R_Double	R_Precision?
	|	R_Real
	|	R_Money
	|	R_SmallMoney
	|	(	R_Char 
		|	R_Character
		|	R_Byte 
		|	R_Binary	
		|	R_Bit	
		)
		(R_Varying)? precision? 
	|	(	(R_Long)? R_VarChar
		|	(R_Long)? R_VarBinary
		|	(R_Long)? R_Blob
		|	(R_Long)? R_Clob
		|	(R_Long)? R_Text
		|	R_Bfile
		|	R_Xml
		|	R_Url
		)
		precision?
	|	R_Date
	|	R_Time		precision?
	|	R_DateTime	(R_With R_Timezone (S_LeftParen (integer_constant | timezone_name)  S_RightParen)? )?
	|	R_Timestamp	precision?	(R_With R_Timezone (S_LeftParen (integer_constant | timezone_name)  S_RightParen)? )?
	|	R_Interval	(R_Year_To_Month | R_Day_To_Second)
	|	R_Timezone	(S_LeftParen (integer_constant | timezone_name)  S_RightParen)?
	|	R_Guid
	|	R_Inet
	|	R_Macaddr
	;

precision
	:	S_LeftParen V_Unsigned_Integer_Constant S_RightParen
	;
	
precision_scale
	:	S_LeftParen V_Unsigned_Integer_Constant ( S_Comma V_Unsigned_Integer_Constant S_RightParen | S_RightParen )
	;

fetch_cursor_statament
	:	R_Fetch R_From cursor_variable_name R_Into variable_name (S_Comma variable_name)
	;
	
for_statement
	:	
	(	lab=new_label_name S_Colon
		{declareLabel($lab.text->chars);}
		R_For loop1=new_variable_name R_As 
		{declareLoop($loop1.text->chars);}
		(	cur1=new_variable_name R_Cursor (R_With R_Hold)? R_For
			{declareCursor($cur1.text->chars);}
			sel1a=select_statement
			R_Do
	   		procedure_statement_list
 			R_End (R_For)? (lab2=V_Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
			{undeclareCursor($cur1.text->chars);}
			
		|	sel1b=select_statement
			R_Do
		   	procedure_statement_list
 			R_End (R_For)? (lab2=V_Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		)
		{undeclareLoop($loop1.text->chars);}
		{undeclarelabel($lab.text->chars);}
		
	|	R_For loop2=new_variable_name R_As 
		{declareLoop($loop2.text->chars);}
		(	cur2=new_variable_name R_Cursor (R_With R_Hold)? R_For
			{declareCursor($cur2.text->chars);}
			sel2a=select_statement
			R_Do
	   		procedure_statement_list
 			R_End (R_For)? 
			{undeclareCursor($cur2.text->chars);}
			
		|	sel2b=select_statement
			R_Do
		   	procedure_statement_list
 			R_End (R_For)? 
		)
		{undeclareLoop($loop2.text->chars);}

	)
	;
	
if_then_else_statement
	:	R_If condition
		R_Then procedure_statement S_Semicolon
		(	R_Elseif condition
			R_Then procedure_statement S_Semicolon
		)*
		(	R_Else procedure_statement S_Semicolon
		)?
		R_End (R_If)?
	;
	
iterate_statement
	:	R_Iterate	label_name
	;
	
leave_statement
	:	R_Leave	label_name
	;
	
loop_statement
	:	
	(	lab=new_label_name S_Colon
		{declareLabel($lab.text->chars);}
		R_Loop
		procedure_statement_list
		R_End (R_Loop)? (lab2=V_Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		{undeclareLabel($lab.text->chars);}

	|	R_Loop
		procedure_statement_list
		R_End (R_Loop)?
	)
	;
	
open_cursor_statement
	:	R_Open R_Cursor? cursor_variable_name
	;

repeat_statement
	:	
	(	lab=new_label_name S_Colon
		{declareLabel($lab.text->chars);}
		R_Repeat
		procedure_statement_list 
		R_Until condition
		R_End (R_Repeat)? (lab2=V_Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		{undeclareLabel($lab.text->chars);}

	|	R_Repeat
		procedure_statement_list 
		R_Until condition
		R_End (R_Repeat)? 
	)
	;

resignal_statement
	:	R_Resignal
		(	R_SqlState (R_Value)? (V_SqlState_Constant | string_variable) 
		|	condition_variable_name
		)
		(R_Set R_Message_Text S_AssignEq (string_variable | V_String_Constant) )?
	//	RESIGNAL SQLSTATE [VALUE] <sqlstate> [SET MESSAGE_TEXT = <variable> or <diagnostic string constant>]
	//	RESIGNAL <condition name> [SET MESSAGE_TEXT = <variable> or <diagnostic string constant>]
	;
	
return_statement
	:	R_Return ( general_expression | R_Null 	)
	;
	
set_statement
	:	R_Set var=variable_name S_AssignEq typed_expression[$var.text->chars]
	;
	
signal_statement
	:	R_Signal
		(	R_SqlState (R_Value)? (V_SqlState_Constant | string_variable) 
		|	condition_variable_name
		)
		(R_Set R_Message_Text S_AssignEq (string_variable | V_String_Constant) )?
	;
	
while_statement
	:
	(	lab=new_label_name S_Colon
		{declareLabel($lab.text->chars);}
		R_While condition
		R_Do
		procedure_statement_list
		R_End (R_While)? (lab2=V_Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		{undeclareLabel($lab.text->chars);}

	|	R_While condition
		R_Do
		procedure_statement_list
		R_End (R_While)?
	)
	;
	
readline_statement
	:	R_Readline 		
		S_LeftParen 	
		(	(	R_File string_value 
		    	{SetStdioFile($stringValue.text->chars);} 
			|	R_Console 
		    	{SetStdioFile("console");} 
			) 
			S_Comma 
		)?
		formatString 
		S_Comma 
		argumentList 
	 	S_RightParen	
		{ readlineStmt($formatString.text->chars ); }
	;

writeline_statement
	:	R_Writeline		
		S_LeftParen 	
		(	(	R_File string_value 
			    {SetStdioFile($stringValue.text->chars);} 
			|	R_Console
				{SetStdioFile("console");} 
			|	R_Trace 
			    {SetStdioFile("trace");} 
			) 
			S_Comma 
		)?
		formatString 
		S_Comma 
		argumentList 
		S_RightParen
		{ writelineStmt($formatString.text->chars, $R_Writeline.line ); }
	;
	
send_statement
	:	R_Send	
		R_Message 						
		{InitMessageQueue();}
		R_To	(	R_Console 				
				{SendToConsole() }?
		 	|	que_name S_Period queID		
		 		{SendToQue($queName.text, $queID.text) }? 
		 	) 
		R_With 	
		R_Format
		S_LeftParen
		formatString 
		S_Comma
		argumentList 
		S_RightParen					
		{SendMessageFormat($formatString.text) }?
		R_Size 	
		S_LeftParen 
		integer_constant 
		S_RightParen	
		{ SendMessageSize($intValue.text) }?
		{ SendToMessageQue() }?
	;

argumentList
	:	{ResetArgList();} 
		( argument ( S_Comma argument )* )? 
	;
	
argument
	:	( variable_name 		{PushArgValue	($variable_name.text->chars);}
		| integer_constant		{PushArgInteger	($integer_constant.text->chars);}
		| float_constant		{PushArgFloat	($float_constant.text->chars);}
		| V_String_Constant 		{PushArgString	($V_String_Constant.text->chars);}
		| V_Char_Constant 		{PushArgChar	($V_Char_Constant.text->chars);} 
		| date_constant			{PushArgDate	($date_constant.text->chars);}
		| time_constant			{PushArgTime	($time_constant.text->chars);}
		| datetime_constant		{PushArgDateTime($datetime_constant.text->chars);}
		| timestamp_constant	{PushArgTimestamp($timestamp_constant.text->chars);}
		) 
	;
	
que_name
	:	V_Identifier
	;
	
queID
	:	V_Identifier
	;
	
formatString
	:	V_String_Constant {IsFormatString($V_String_Constant.text->chars) }?
	;

fileCreateStmt
	:	R_FileCreate	S_LeftParen	string_value S_RightParen
						{ fileCreateStmt($string_value.text->chars ); }
	;

fileDeleteStmt
	:	R_FileDelete	S_LeftParen	string_value S_RightParen
						{ fileDeleteStmt($string_value.text-> ); }
	;

fileTouchStmt
	:	R_FileTouch		S_LeftParen	string_value S_RightParen
						{ fileTouchStmt($string_value.text->chars ); }
	;
	
filePrintStmt
	:	R_FilePrint		S_LeftParen	str1=string_value S_Comma str2=string_value S_RightParen
						{ filePrintStmt($str1.text->chars, $str2.text->chars ); }
	;

fileCopyStmt
	:	R_FileCopy	 	S_LeftParen	str1=string_value S_Comma str2=string_value S_Comma str3=string_value S_RightParen
						{ fileCopyStmt($str1.text->chars, $str2.text->chars, $str3.text->chars ); }
	;
	
fileDiffStmt
	:	R_FileDiff	 	S_LeftParen	str1=string_value S_Comma str2=string_value S_Comma str3=string_value S_RightParen
						{ fileDiffStmt( $str1.text->chars, $str2.text->chars, $str3.text->chars ); }
	;
	
// ---------------------------------------------------------------------------------------------
// ** Identifiers                                                                             **
// ---------------------------------------------------------------------------------------------

variable_name
	:	{ IsVariable(LT(1).text->chars) }? 		id=V_Identifier	
	;

new_variable_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Identifier
	;

new_trigger_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Identifier
	;	
	
new_trigger_number	
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Unsigned_Integer_Constant
	;	
	
procedure_name
	:	{ IsProcedureName(LT(1).text->chars) }?	id=V_Identifier
	;
	
new_procedure_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Identifier
	;	
	
procedure_number
	:	{ IsProcedureName(LT(1).text->chars) }?	id=V_Unsigned_Integer_Constant
	;	
	
new_procedure_number	
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Unsigned_Integer_Constant
	;	

new_parameter_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Identifier
	;	
	
parameter_name
	:	{ IsParameterVar(LT(1).text->chars) }?	id=V_Identifier
	;
	
condition_variable_name
	:	{ IsConditionVar(LT(1).text->chars) }?	id=V_Identifier
	;
	
schema_name
	:	{ IsSchemaName(LT(1).text->chars) }?	id=V_Identifier
	;
	
table_name
	:	{ IsTableName(LT(1).text->chars) }?		id=V_Identifier
	;
	
new_table_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Identifier
	;
	
view_name
	:	{ IsViewName(LT(1).text->chars) }?		id=V_Identifier
	;
	
column_name[char * schemaName, char * tableName]
	:	{ IsColumnName(schemaName, tableName, LT(1).text->chars) }?		id=V_Identifier
	;
	
key_column_name[char * schemaName, char * tableName]
	:	{ IsKeyColumnName(schemaName, tableName, LT(1).text->chars) }?	id=V_Identifier
	;
	
new_column_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=V_Identifier
	;
	
new_label_name
	:	{ IsNotDeclared(LT(1).text->chars) }?		id=V_Identifier
	;
	
label_name
	:	{ IsLabelName(LT(1).text->chars) }?			id=V_Identifier
	;

locator_variable_name
	:	{ IsLocatorVarName(LT(1).text->chars) }?	id=V_Identifier
	;

cursor_variable_name
	:	{ IsCursorVarName(LT(1).text->chars) }?		id=V_Identifier
	;

timezone_name
	:	{ IsTimezoneName(LT(1).text->chars) }?		tz=V_String_Constant
	;
	
character_set_name
	:	{ IsCharacterSetName(LT(1).text->chars) }?	tz=V_String_Constant
	;
	
collation_name
	:	{ IsCharacterSetName(LT(1).text->chars) }?	tz=V_String_Constant
	;
	
handler_condition
	:
	(	R_Sql_Exception
	|	R_Sql_Warning
	|	R_Not R_Found
	|	R_SqlState V_SqlState_Constant
	)
	;
	
// ---------------------------------------------------------------------------------------------
// ** Conditions and R_Boolean Expressions                                                      **
// ---------------------------------------------------------------------------------------------

condition
	:  
	boolean_expression
	;

boolean_expression
	:  
	boolean_term (R_Or boolean_term)*
	;
	
boolean_term	
	:
	boolean_factor (R_And boolean_factor)*
	;
	
boolean_factor	
	:
	(R_Not) => R_Not
	(	(untyped_boolean_primary)	=>	untyped_boolean_primary
	|	(numeric_boolean_primary)	=>	numeric_boolean_primary
	|	(string_boolean_primary)	=>	string_boolean_primary
	|	(clob_boolean_primary)		=>	clob_boolean_primary
	|	(blob_boolean_primary)		=>	blob_boolean_primary
	|	(date_boolean_primary)		=>	date_boolean_primary
	|	(time_boolean_primary)		=>	time_boolean_primary
	|	(datetime_boolean_primary)	=>	datetime_boolean_primary
	|	(timestamp_boolean_primary)	=>	timestamp_boolean_primary
	|	S_LeftParen	
		condition	
		S_RightParen
	)
	;
	
untyped_boolean_primary
	:
	(	is_or_is_not	R_In R_Rowset 		V_Identifier
//	|	R_Not? R_Exists 	scalar_subquery
	)
	;
	
is_or_is_not
	:	R_Is	R_Not?
	|	R_Not
	;
	
relational_operator  
	:
    (	S_RelOp_EQ
    |	S_RelOp_NE
    |	S_RelOp_GE
    |	S_RelOp_GT
    |	S_RelOp_LE
    |	S_RelOp_LT
    )
    ;
    

// ---------------------------------------------------------------------------------------------
// ** Values and Expressions                                                                  **
// ---------------------------------------------------------------------------------------------

typed_expression [char * var_name]
	:	{IsBooleanVar(var_name) }?		=>	boolean_expression
	|	{IsNumericVar(var_name) }?		=>	numeric_expression
	|	{IsStringVar(var_name) }?		=>	string_expression
	|	{IsDateVar(var_name) }?			=>	date_expression
	|	{IsTimeVar(var_name) }?			=>	time_expression
	|	{IsDateTimeVar(var_name) }?		=>	datetime_expression
	|	{IsTimestampVar(var_name) }?	=>	timestamp_expression
	;
	
constant_value_list    :
    S_LeftParen
    constant_value_expression ( S_Comma constant_value_expression )*
    S_RightParen
    ;
	
constant_value_expression    :
    (	constant_value
    
//    |	( R_Trunc | R_To_Char )	
//		S_LeftParen
//		constant_value
//		(	S_Comma
//			V_String_Constant
//		)?
//		S_RightParen

//    |	( R_To_Number | R_To_Date | R_To_Time |  )    
//		S_LeftParen
//		V_String_Constant
//		(	S_Comma
//			V_String_Constant
//		)?
//		S_RightParen
    )
    ;
  
/* 
constant
	:	numeric_constant
	|	V_String_Constant
	|	V_Char_Constant
	|	date_constant
	|	time_constant
	|	datetime_constant
	|	timestamp_constant
	;
	 
*/

constant_value
	:
	(	S_Plus		numeric_constant
    |	S_Minus		numeric_constant
	|	constant
    |	parameter_name
    |   interval_constant
	)
	;
	
value_expression
	:
	(	non_arithmetic_value_expression
	|	general_expression	
	|	user_defined_function
	)
	;

non_arithmetic_value_expression
	:
    (	R_Context 		S_LeftParen V_Identifier S_Comma V_String_Constant S_RightParen
    |	R_Coalesce 		general_expression_set
    |	R_Nvl 			S_LeftParen general_expression S_Comma general_expression S_RightParen
    |	R_Nvl2 			S_LeftParen general_expression S_Comma general_expression S_Comma general_expression S_RightParen
    |	R_Igscore 		S_LeftParen numeric_constant S_Comma (S_QuestionMark | R_Parameter) S_Comma V_Identifier S_Comma numeric_constant (S_Comma numeric_constant)? S_RightParen
	)
	;

user_defined_function
	:
	{IsUserDefinedFunction(LT(1).text->chars) }?
	id=V_Identifier 				// User Defined Function
	S_LeftParen					// note: function_name() is ambiguous and so not supported
    nullable_expression	 	( S_Comma nullable_expression )*
	S_RightParen
	;

// ---------------------------------------------------------------------------------------------
// ** General Expressions                                                                     **
// ---------------------------------------------------------------------------------------------

nullable_expression	:
	(	general_expression
	|	R_Null
	)
	;
	
general_expression_value    :
    (	S_QuestionMark
    |	R_Null
    |	general_expression
    )
    ;

general_expression
	:
	(	general_value_expression
	|	case_expression
	|	R_Cast
		S_LeftParen
		general_value_expression
		R_As
		elementary_type_name
		S_RightParen
	)
	;

general_value_expression
	:	(	(numeric_expression)	=>	numeric_expression	
		|	(string_expression)		=>	string_expression	
		|	(date_expression)		=>	date_expression	
		|	(time_expression)		=>	time_expression
		|	(datetime_expression)	=>	datetime_expression	
		|	(timestamp_expression)	=>	timestamp_expression
		|	(interval_expression)	=>	interval_expression 
		|	parameter_name
		)
	;
	
case_expression
	:
	R_Case
	(	var=variable_name	simple_case_expression_body[$var.text->chars]
	|	searched_case_expression_body
	)
	;
	
simple_case_expression_body [char * var_name]
	:
	(R_When typed_expression[var_name] R_Then general_expression S_Comma)+
	R_End R_Case
	;

searched_case_expression_body
	:
	(R_When condition R_Then general_expression S_Comma)+
	R_End R_Case
	;

general_expression_set
	:	
    S_LeftParen 
    general_expression ( S_Comma general_expression )*
    S_RightParen
    ;

general_expression_value_set
	:	
//	(
	S_LeftParen 
    general_expression_value ( S_Comma general_expression_value )*   	
    S_RightParen
//    |	vector_subquery
//	)
    ;

// ---------------------------------------------------------------------------------------------
// ** R_Numeric Expressions                                                                     **
// ---------------------------------------------------------------------------------------------

numeric_boolean_primary
	:
	(	numeric_expression		
		(	relational_operator			numeric_expression
		|	numeric_boolean_predicate
		)
		
	|	numeric_set 
		numeric_boolean_predicate
	)
	;
	
numeric_boolean_predicate
	:
	(	is_or_is_not?
		(	R_In			numeric_set
		|	R_Between		numeric_expression R_And numeric_expression
		)
	|	is_or_is_not	R_Null
	)
	;	
	
numeric_expression
	:
	numeric_term		( ( S_Plus | S_Minus )		numeric_term )*
	;

numeric_term
	:
	numeric_factor		( ( S_Mult | S_Divide | S_Modulo )	numeric_factor )*
	;
	
numeric_factor
	:
	(	S_Plus	numeric_primary
		
	|	S_Minus	numeric_primary
	
	|			numeric_primary
	)
	;
	
numeric_primary
	:
	(	numeric_variable
	|	numeric_function
	|	numeric_coerced_value
	|	integer_constant
	|	float_constant
//	|	numeric_scalar_subquery
	)
	;
	
numeric_variable
	:	{IsNumericVariableName(LT(1).text->chars) }?	id=V_Identifier
	;
	
integer_constant
	:	V_Unsigned_Integer_Constant | V_Signed_Integer_Constant
	;

float_constant
	:	V_Unsigned_Float_Constant	 | V_Signed_Float_Constant
	;

numeric_constant
	:	integer_constant | float_constant
	;
	
/*	
sqlNUMERIC_SCALAR_SUBQUERY	:
	S_LeftParen
	STOK_SELECT
    sqlNUMERIC_EXPRESSION 
    (	sqlSELECT_FROM_CLAUSE
		sqlSELECT_WHERE_CLAUSE?
	)?
	S_RightParen
    ;

sqlNUMERIC_SUBQUERY	:
	S_LeftParen
	STOK_SELECT
    sqlNUMERIC_EXPRESSION 
    (	sqlSELECT_FROM_CLAUSE
		sqlSELECT_WHERE_CLAUSE?
		sqlSELECT_GROUP_CLAUSE?
		sqlSELECT_ORDER_CLAUSE?
	)?
	S_RightParen
    ;
*/
numeric_coerced_value
	:
	R_To_Number				// TO_NUMBER(string_exp {,string_exp {,string_exp} } )
	S_LeftParen	
	( string_expression	| parameter_name )
	(	S_Comma 
		string_expression 
		(	S_Comma 
			string_expression 
		)? 
	)?	
	S_RightParen
	;
	

numeric_function
	:
	(	(	R_Abs			// R_Abs(number)						// alt 1
		|	R_Acos		// R_Acos(number)		
		|	R_Asin		// ASIN(number)
		|	R_Atan		// ATAN(number)	
		|	R_Cos			// COS(number)	
		|	R_Exp			// EXP(number)		
		|	R_Log			// LOG(number)		
		|	R_Log10		// LOG10(number)
		|	R_Log2		// LOG2(number)
		|	R_Sin			// sin(number)	
		|	R_Sqrt		// sqrt(number)
		|	R_Tan			// tan(number)
		)
		S_LeftParen	
		numeric_expression	
		S_RightParen
		
	|	(	R_Atan2		// ATAN2(number[, number])			// alt 2
		|	R_Mod			// MOD(number[, number])
		|	R_Round		// ROUND(number[, decimal])
		)				
		S_LeftParen	
		numeric_expression	
		S_Comma	
		numeric_expression	
		S_RightParen

	|	(	R_Ascii				// ASCII(string_exp)
		|	R_Bit_Length			// BIT_LENGTH(string_exp)	// alt 3
		|	R_Char_Length			// CHAR_LENGTH(string_exp)
		|	R_Character_Length	// CHARACTER_LENGTH(string_exp)
		|	R_Octet_Length		// OCTET_LENGTH(string_exp)
		|	R_Length				// LENGTH(string_exp)
		)
		S_LeftParen	
		string_expression	
		S_RightParen

	|	R_Extract			// EXTRACT( date_part FROM date_expression )	// alt 4
		S_LeftParen	
		date_part	
		R_From 
		(	(date_expression)		=>	date_expression 
		|	(datetime_expression)	=>	datetime_expression
		|	(timestamp_expression)	=>	timestamp_expression
		|	interval_expression
		)	
		S_RightParen
		
	|	R_Position		// POSITION( string1 IN string2 )	// alt 5
		S_LeftParen	
		string_expression	R_In string_expression
		S_RightParen

    |	R_Count 				// COUNT()							// alt 6
		general_expression_set   
			
    |	(	R_Avg 			 									// alt 7
    	|	R_Max 			 
    	|	R_Min 			 
    	|	R_Sum 			 
    	|	R_Var 			 
    	|	R_StdDev 			 
    	|	R_SumSquare 			  
    	)
    	numeric_set
	)
	;
	
numeric_set
	:	S_LeftParen
		numeric_expression	(S_Comma numeric_expression)*
		S_RightParen
	;

numeric_value_set
	:  
	(	S_LeftParen	
		numeric_value 		(S_Comma numeric_value )*
		S_RightParen
		
//	|	numeric_vector_subquery
	)
	;
	
numeric_value 
	:
    (	S_QuestionMark
    |	R_Null
    |	numeric_expression
    )
	;
	
// ---------------------------------------------------------------------------------------------
// ** String Expressions                                                                      **
// ---------------------------------------------------------------------------------------------

string_boolean_primary
	:
	(	string_expression 
		(	relational_operator		string_expression
		|	string_boolean_predicate
		)
		
	|	string_set					
		string_boolean_predicate
	)	
	;
	
string_boolean_predicate
	:
	(	is_or_is_not?
		(	R_In			string_set
		|	R_Like		V_String_Constant
		|	R_Between		string_expression R_And string_expression
		)
	|	is_or_is_not
		R_Null
	)
	;
	
string_expression
	:
	string_primary 	( (S_Plus | S_Bar ) string_primary )*
	;

string_primary			:
	(	string_variable
	|	string_coerced_value
	|	string_function
	|	V_String_Constant
	)
	;

string_variable
	:	{IsStringVariableName(LT(1).text->chars) }?	id=V_Identifier
	;

string_coerced_value	:
	R_To_Char
	S_LeftParen
	general_expression
	S_RightParen
	;
	

string_function
	:
	(	(	R_Lcase		// LCASE(string_exp)
		|	R_Ltrim		// LTRIM(string_exp)
		|	R_Rtrim		// RTRIM(string_exp)
		|	R_Soundex		// SOUNDEX(string_exp)
		|	R_Ucase		// UCASE(string_exp)
	    |	R_Upper 		// UPPER(string_exp)
    	|	R_Lower 		// LOWER(string_exp)
		|	R_Synonyms	// SYNONYMS(string_exp)
		)
		S_LeftParen	
		string_expression	
		S_RightParen

	|	(	R_Concat		// CONCAT(string_exp1, string_exp2)
		|	R_Difference	// DIFFERENCE(string_exp1, string_exp2)
		)
		S_LeftParen	
		string_expression	S_Comma string_expression
		S_RightParen

	|	R_Replace			// REPLACE(string_exp1, string_exp2, string_exp3)
		S_LeftParen	
		string_expression	S_Comma string_expression	S_Comma string_expression
		S_RightParen

	|	R_Space			// SPACE(count)
		S_LeftParen	
		numeric_expression	
		S_RightParen

	|	R_Repeat			// REPEAT(string_exp, count)
		S_LeftParen	
		string_expression	S_Comma numeric_expression	
		S_RightParen

	|	(	R_Substring	// SUBSTRING(string_exp, start, length)
		|	R_Substr		// SUBSTR(string_exp, start, length)
		)
		S_LeftParen	
		string_expression	S_Comma numeric_expression	S_Comma numeric_expression	
		S_RightParen

	|	R_Locate			// LOCATE(string_exp1, string_exp2[, start])
		S_LeftParen	
		string_expression	S_Comma string_expression	( S_Comma numeric_expression	)?
		S_RightParen
		
	|	R_Trim 			//	trim( { [LEADING | TRAILING | BOTH] trim_char | trim_char } FROM trim_source )
	    S_LeftParen 
	    (	R_Leading 
	    	string_expression
	    
    	|	R_Trailing 
    		string_expression
    	
    	|	R_Both 
    		string_expression
    	
    	|	string_expression
    	)
		(	R_From
			string_expression
		)?
    	S_RightParen
	)
	;
	
	
string_set
	:	S_LeftParen
		string_value	(S_Comma string_value)*
		S_RightParen
	;
	
string_value
	:	string_variable | V_String_Constant
	;	
	
string_value_set
	:
	S_LeftParen
	(	string_set_value 	(S_Comma string_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	S_RightParen
	;

string_set_value
	:
    (	S_QuestionMark
    |	R_Null
    |	string_expression
    )
	;

// ---------------------------------------------------------------------------------------------
// ** CLOB Expressions                                                                        **
// ---------------------------------------------------------------------------------------------

clob_boolean_primary
	:	
	(	R_Not?
		R_Contains
		S_LeftParen
		clob_variable
		S_Comma
		string_value
		S_RightParen
			
	|	clob_variable
		is_or_is_not	
		R_Null
	)	
	;
	
clob_variable
	:	{IsClobVariable(LT(1).text->chars) }?	id=V_Identifier
	;
		
// ---------------------------------------------------------------------------------------------
// ** BLOB Expressions                                                                        **
// ---------------------------------------------------------------------------------------------

blob_boolean_primary
	:	
	blob_variable
	is_or_is_not	
	R_Null
	;
	
blob_variable
	:	{IsBlobVariable(LT(1).text->chars) }?	id=V_Identifier
	;
		
// ---------------------------------------------------------------------------------------------
// ** R_Date Expressions                                                                        **
// ---------------------------------------------------------------------------------------------

date_boolean_primary
	:
	(	date_expression 		
		(	relational_operator			date_expression		
		|	date_boolean_predicate
		)
		
	|	date_set
		date_boolean_predicate
	)	
	;
	
date_boolean_predicate	
	:
	(	is_or_is_not?
		(	R_In			date_set
		|	R_Between		date_expression R_And date_expression
		)
	|	is_or_is_not
		R_Null
	)
	;
	
date_expression
	:
	(	date_variable
	|	date_constant
	|	date_coerced_value
	|	date_function
	)
//	(	( S_Minus | S_Plus )
//		interval_expression
//	)*
	;
	
date_variable
	:	{IsDateVariableName(LT(1).text->chars) }?	id=V_Identifier
	;


date_constant
	:	R_Date V_String_Constant
	;
	
date_coerced_value
	:
	R_To_Date		// TO_DATE(string_sxp {,string_exp {,string_exp} } )	
	S_LeftParen	
	(	(datetime_expression)	=>	datetime_expression 
	|	(timestamp_expression)	=>	timestamp_expression 
	|	string_expression
	)
	(	S_Comma 
		string_expression 
		(	S_Comma 
			string_expression 
		)? 
	)?	
	S_RightParen
	;

date_function
	:
	(	(R_Trunc S_LeftParen date_expression)	=>
		R_Trunc
		S_LeftParen	
		date_expression
		(	S_Comma 
			string_expression 
		)?	
		S_RightParen

	|	R_SysDate
	)
	;

date_set
	:
	S_LeftParen
	(	date_expression 	(S_Comma date_expression )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	S_RightParen
	;

date_value_set
	:
	S_LeftParen
	(	date_set_value 		(S_Comma date_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	S_RightParen
	;

date_set_value
	:
    (	S_QuestionMark
    |	R_Null
    |	date_expression
    )
	;

// ---------------------------------------------------------------------------------------------
// ** R_Time Expressions                                                                        **
// ---------------------------------------------------------------------------------------------

time_boolean_primary
	:
	(	time_expression 		
		(	relational_operator			time_expression		
		|	time_boolean_predicate
		)
		
	|	time_set
		time_boolean_predicate
	)	
	;
	
time_boolean_predicate	
	:
	(	is_or_is_not?
		(	R_In			time_set
		|	R_Between		time_expression R_And time_expression
		)
	|	is_or_is_not
		R_Null
	)
	;
	
time_expression
	:
	(	time_variable
	|	time_constant
	|	time_coerced_value
	|	time_function
	)
	;
	
time_variable
	:	{IsTimeVariableName(LT(1).text->chars) }?	id=V_Identifier
	;
	
time_constant
	:	R_Time V_String_Constant
	;
	
time_coerced_value
	:
	R_To_Time		// TO_TIME(string_sxp {,string_exp {,string_exp} } )	
	S_LeftParen	
	( 	(datetime_expression)	=>	datetime_expression 
	|	(timestamp_expression)	=>	timestamp_expression 
	| 	string_expression 
	)
	(	S_Comma 
		string_expression 
		(	S_Comma 
			string_expression 
		)? 
	)?	
	S_RightParen
	;

time_function
	:
	(	R_Trunc
		S_LeftParen	
		time_expression
		(	S_Comma 
			string_expression 
		)?	
		S_RightParen

	|	R_SysTime
	)
	;

time_set
	:
	S_LeftParen
	(	time_expression 	(S_Comma time_expression )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	S_RightParen
	;

time_value_set
	:
	S_LeftParen
	(	time_set_value 		(S_Comma time_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	S_RightParen
	;

time_set_value
	:
    (	S_QuestionMark
    |	R_Null
    |	time_expression
    )
	;

// ---------------------------------------------------------------------------------------------
// ** R_DateTime Expressions                                                                    **
// ---------------------------------------------------------------------------------------------

datetime_boolean_primary
	:
	(	datetime_expression		
		(	relational_operator			datetime_expression			
		|	datetime_boolean_predicate
		)
		
	|	datetime_set
		datetime_boolean_predicate
	)	
	;
	
datetime_boolean_predicate	:
	(	is_or_is_not?
		(	R_In			datetime_set
		|	R_Between		datetime_expression R_And datetime_expression
		)
	|	is_or_is_not
		R_Null
	)
	;
	

datetime_expression
	:
	(	datetime_variable
	|	datetime_coerced_value
	|	datetime_function
	|	datetime_constant
	)
//	(	( S_Minus | S_Plus )
//		interval_expression
//	)*
	;

datetime_variable
	:	{IsDateTimeVariableName(LT(1).text->chars) }?	id=V_Identifier
	;

datetime_constant
	:	R_DateTime V_String_Constant
	;
	
datetime_set
	:
	S_LeftParen
	datetime_expression 	(S_Comma datetime_expression )*
	S_RightParen
	;
	
datetime_value_set
	:	
	S_LeftParen
	(	datetime_set_value 	(S_Comma datetime_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	S_RightParen
	;

datetime_set_value    :
    (	S_QuestionMark
    |	R_Null
    |	datetime_expression
    )
	;

datetime_coerced_value
	:
	R_To_DateTime			// converts char of CHAR, VARCHAR2, NCHAR, or NVARCHAR2 datatype to a value of DATETIME datatype. The fmt is a datetime model format specifying the format of char. R_If you omit fmt, then char must be in the default date format. R_If fmt is J, for Julian, then char must be an integer.
	S_LeftParen			// see http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions183.htm
	( datetime_constant | string_expression | timestamp_expression )
	(	S_Comma 
		string_expression 
		(	S_Comma 
			string_expression 
		)? 
	)?	
	S_RightParen
	;
	
datetime_function
	:
	(	(R_Trunc S_LeftParen datetime_expression)	=>
		R_Trunc			// TRUNC ( datetime_exp {, string_exp } ) 
		S_LeftParen		// returns a date truncated to a specific unit of measure 
						// http://www.techonthenet.com/oracle/functions/trunc_date.php
		datetime_expression 	
		(	S_Comma 
			string_expression 
		)?	
		S_RightParen
		
	|	R_SysDateTime		// The built-in function SYSDATE returns a DATE value containing the current date and time on your system. 
						// http://infolab.stanford.edu/~ullman/fcdb/oracle/or-time.html
	)
	;

// ---------------------------------------------------------------------------------------------
// ** R_Timestamp Expressions                                                                    **
// ---------------------------------------------------------------------------------------------

timestamp_boolean_primary
	:
	(	timestamp_expression		
		(	relational_operator			timestamp_expression			
		|	timestamp_boolean_predicate
		)
		
	|	timestamp_set
		timestamp_boolean_predicate
	)	
	;

timestamp_boolean_predicate	:
	(	is_or_is_not?
		(	R_In			timestamp_set
		|	R_Between		timestamp_expression R_And timestamp_expression
		)
	|	is_or_is_not
		R_Null
	)
	;
	
timestamp_expression
	:
	(	timestamp_variable
	|	timestamp_coerced_value
	|	timestamp_function
	|	timestamp_constant
	)
//	(	( S_Minus | S_Plus )
//		interval_expression
//	)*
	;
	
timestamp_variable
	:	{IsTimestampVariableName(LT(1).text->chars) }?	id=V_Identifier
	;

timestamp_constant
	:	R_Timestamp V_String_Constant
	;

timestamp_set
	:
	S_LeftParen
	timestamp_expression 	(S_Comma timestamp_expression )*
	S_RightParen
	;
	
timestamp_value_set
	:	
	S_LeftParen
	( timestamp_set_value 	(S_Comma timestamp_set_value )*
//	|	SelectStatement select
	)
	S_RightParen
	;

timestamp_set_value    :
    (	S_QuestionMark
    |	R_Null
    |	timestamp_expression
    )
	;

timestamp_coerced_value
	:
	R_To_Timestamp			// converts char of CHAR, VARCHAR2, NCHAR, or NVARCHAR2 datatype to a value of DATETIME datatype. The fmt is a datetime model format specifying the format of char. R_If you omit fmt, then char must be in the default date format. R_If fmt is J, for Julian, then char must be an integer.
	S_LeftParen			
	( timestamp_constant | string_expression )
	(	S_Comma 
		string_expression 
		(	S_Comma 
			string_expression 
		)? 
	)?	
	S_RightParen
	;
	
timestamp_function
	:
	(	(R_Trunc S_LeftParen timestamp_expression)	=>
		R_Trunc			// TRUNC ( datetime_exp {, string_exp } ) 
		S_LeftParen		// returns a date truncated to a specific unit of measure 
		timestamp_expression 	
		(	S_Comma 
			string_expression 
		)?	
		S_RightParen
		
	|	R_SysTimestamp		// The built-in function SYSDATE returns a DATE value containing the current date and time on your system. 
	)
	;

// ---------------------------------------------------------------------------------------------
// ** R_Interval Expressions                                                                    **
// ---------------------------------------------------------------------------------------------

interval_boolean_primary
	:
	(	interval_expression		
		(	relational_operator			interval_expression			
		|	interval_boolean_predicate
		)
		
	|	interval_set
		interval_boolean_predicate
	)	
	;

interval_boolean_predicate	:
	(	is_or_is_not?
		(	R_In			interval_set
		|	R_Between		interval_expression R_And interval_expression
		)
	|	is_or_is_not
		R_Null
	)
	;

interval_expression
	:
	(	interval_primary		( ( S_Plus | S_Minus )		interval_primary )*
		
	|	(	(date_expression 		S_Minus	date_expression)		=>	date_expression 		S_Minus	date_expression
		|	(time_expression		S_Minus	time_expression)		=>	time_expression			S_Minus	time_expression
		|	(datetime_expression	S_Minus	datetime_expression)	=>	datetime_expression		S_Minus	datetime_expression
		|	(timestamp_expression	S_Minus	timestamp_expression)	=>	timestamp_expression	S_Minus	timestamp_expression
		)
	)
	;
	
interval_primary
	:
	(	interval_variable
	|	interval_constant
	|	interval_function
	|	interval_coerced_value
	)
	;
	
interval_variable
	:	{IsIntervalVariableName(LT(1).text->chars) }?	id=V_Identifier
	;

interval_constant
	:	R_Interval 
		(	V_String_Constant
		
		|	( S_Plus | S_Minus )
    		(	date_constant
			|	time_constant
			|	datetime_constant
			|	timestamp_constant
			)
    		interval_qualifier
		)
	;
		
interval_qualifier
options{k=1;}
    :
    (	non_second_date_part precision? ( R_To ( non_second_date_part | R_Second ) precision? )?
    
    |	R_Second precision_scale?
    )
    ;
    	
interval_coerced_value
	:
	// converts char of CHAR, VARCHAR2, NCHAR, or NVARCHAR2 datatype to a value of R_Interval datatype. 
	// The fmt is a datetime model format specifying the format of char. 
	// R_If you omit fmt, then char must be in the default date format. 
	// R_If fmt is J, for Julian, then char must be an integer.
	(	R_To_YMInterval	
	|	R_To_DSInterval	
	)
	S_LeftParen		
	string_expression 
	(	S_Comma 
		string_expression 
		(	S_Comma 
			string_expression 
		)? 
	)?	
	S_RightParen
	;
	
interval_function
	:
	R_DateDiff			
	S_LeftParen			
	(	date_part
		(	(datetime_expression)	=>	datetime_expression
		|	(timestamp_expression)	=>	timestamp_expression
		)
		S_Comma
		(	(datetime_expression)	=>	datetime_expression
		|	(timestamp_expression)	=>	timestamp_expression
		)
		
	|	(	ymd_date_part
			date_expression
		)	=>
		ymd_date_part
		date_expression
		S_Comma
		date_expression
		
	|	(day_part time_expression)	=>
		day_part
		time_expression 	
		S_Comma 
		time_expression 	
	)
	S_RightParen	
	;
	
interval_set
	:
	S_LeftParen
	R_Interval V_String_Constant 	(S_Comma R_Interval V_String_Constant )*
	S_RightParen
	;
	
//interval_value_set
//	:	
//	S_LeftParen
//	( interval_set_value 	(S_Comma interval_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
//	)
//	S_RightParen
//	;

//interval_set_value 
//	:	R_Interval V_String_Constant
//	;
//	:
//  (	S_QuestionMark
//  |	R_Null
//  |	interval_constant
//  |	interval_expression
//  )
//	;
	
date_part
	:
	(	R_Year
    |	R_Month
    |	R_Day
    |	R_Hour
    |	R_Minute
   	|	R_Second
	|	R_Millisecond
	|	R_Microsecond
	|	R_Nanosecond
	|	R_Quarter
	|	R_DayOfYear
	|	R_Week
	|	R_Timezone_Hour
    |	R_Timezone_Minute
	)
	;	

ymd_date_part
	:
	(	R_Year
    |	R_Month
    |	R_Day
	|	R_Quarter
	|	R_DayOfYear
	|	R_Week
    )
    ;
	
non_second_date_part
	:	
	(	R_Year
    |	R_Month
    |	R_Day
    |	R_Hour
    |	R_Minute
	)
    ;
	
day_part
	:	
	(	R_Hour
    |	R_Minute
	|	R_Second
	|	R_Millisecond
	|	R_Microsecond
	|	R_Nanosecond
	)
	;

time_zone_field
    :
    (	R_Timezone_Hour
    |	R_Timezone_Minute
    )
    ;
