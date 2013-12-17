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
    * Neither the name of the "Mage Systems" nor the names of its contributors
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
	:	(	ceate_trigger_statement
		|	ceate_procedure_statement
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

Abs				:	(A__ B__ S__ );
Acos			:	(A__ C__ O__ S__ );
After			:	(A__ F__ T__ E__ R__ );
Allocate		:	(A__ L__ L__ O__ C__ A__ T__ E__ );
And				:	(A__ N__ D__ );
Array			:	(A__ R__ R__ A__ Y__ );
As				:	(A__ S__ );
Ascii			:	(A__ S__ C__ I__ I__ );
Asin			:	(A__ S__ I__ N__ );
Associate		:	(A__ S__ S__ O__ C__ I__ A__ T__ E__ );
Atan			:	(A__ T__ A__ N__ );
Atan2			:	(A__ T__ A__ N__ Two__ );
Avg				:	(A__ V__ G__ );
Begin			:	(B__ E__ G__ I__ N__ );
Before			:	(B__ E__ F__ O__ R__ E__ );
Between			:	(B__ E__ T__ W__ E__ E__ N__ );
Bfile			:	(B__ F__ I__ L__ E__ );
BigInt			:	(B__ I__ G__ I__ N__ T__ );
Binary			:	(B__ I__ N__ A__ R__ Y__ );
Bit				:	(B__ I__ T__ );
Bit_Length		:	(B__ I__ T__  Underscore L__ E__ N__ G__ T__ H__ );
Blob			:	(B__ L__ O__ B__ );
Boolean			:	(B__ O__ O__ L__ E__ A__ N__ );
Both			:	(B__ O__ T__ H__ );
Byte			:	(B__ Y__ T__ E__ );
Call			:	(C__ A__ L__ L__ );
Caller			:	(C__ A__ L__ L__ E__ R__ );
Case			:	(C__ A__ S__ E__ );
Cast			:	(C__ A__ S__ T__ );
Char			:	(C__ H__ A__ R__ );
Character		:	(C__ H__ A__ R__ A__ C__ T__ E__ R__ );
Character_Length:	(C__ H__ A__ R__ A__ C__ T__ E__ R__  Underscore L__ E__ N__ G__ T__ H__ );
Char_Length		:	(C__ H__ A__ R__  Underscore L__ E__ N__ G__ T__ H__ );
Client			:	(C__ L__ I__ E__ N__ T__ );
Clob			:	(C__ L__ O__ B__ );
Close			:	(C__ L__ O__ S__ E__ );
Coalesce		:	(C__ O__ A__ L__ E__ S__ C__ E__ );
Column_Name		:	(C__ O__ L__ U__ M__ N__  Underscore N__ A__ M__ E__ );
Column			:	(C__ O__ L__ U__ M__ N__ );
Concat			:	(C__ O__ N__ C__ A__ T__ );
Condition		:	(C__ O__ N__ D__ I__ T__ I__ O__ N__ );
Console			:	(C__ O__ N__ S__ O__ L__ E__ );
Contains		:	(C__ O__ N__ T__ A__ I__ N__ S__ );
Context			:	(C__ O__ N__ T__ E__ X__ T__ );
Continue		:	(C__ O__ N__ T__ I__ N__ U__ E__ );
Cos				:	(C__ O__ S__ );
Count			:	(C__ O__ U__ N__ T__ );
Create			:	(C__ R__ E__ A__ T__ E__ );
Cursor			:	(C__ U__ R__ S__ O__ R__ );
DateDiff		:	(D__ A__ T__ E__ D__ I__ F__ F__);
DateTime		:	(D__ A__ T__ E__ T__ I__ M__ E__ );
Date			:	(D__ A__ T__ E__ );
Day_To_Second	:	(D__ A__ Y__  Underscore T__ O__  Underscore S__ E__ C__ O__ N__ D__ );
DayOfYear		:	(D__ A__ Y__ O__ F__ Y__ E__ A__ R__);
Day				:	(D__ A__ Y__ );
Decimal			:	(D__ E__ C__ I__ M__ A__ L__ );
Declare			:	(D__ E__ C__ L__ A__ R__ E__ );
Dec				:	(D__ E__ C__ );
Default			:	(D__ E__ F__ A__ U__ L__ T__ );
Delete			:	(D__ E__ L__ E__ T__ E__ );
Difference 		:	(D__ I__ F__ F__ E__ R__ E__ N__ C__ E__ );
Dioagnostics	:	(D__ I__ A__ G__ N__ O__ S__ T__ I__ C__ S__ );
Do				:	(D__ O__ );
Double			:	(D__ O__ U__ B__ L__ E__ );
Else			:	(E__ L__ S__ E__ );
Elseif			:	(E__ L__ S__ E__ I__ F__ );
End				:	(E__ N__ D__ );	
Enum			:	(E__ N__ U__ M__ );
Exception		:	(E__ X__ C__ E__ P__ T__ I__ O__ N__ );
Exists			:	(E__ X__ I__ S__ T__ S__ );
Exit			:	(E__ X__ I__ T__ );
Exp				:	(E__ X__ P__ );
Extract			:	(E__ X__ T__ R__ A__ C__ T__ );
Fetch			:	(F__ E__ T__ C__ H__ );
FileCopy		:	(F__ I__ L__ E__ C__ O__ P__ Y__ );
FileCreate		:	(F__ I__ L__ E__ C__ R__ E__ A__ T__ E__ );
FileDelete		:	(F__ I__ L__ E__ D__ E__ L__ E__ T__ E__ );
FileDiff		:	(F__ I__ L__ E__ D__ I__ F__ F__ );
FilePrint		:	(F__ I__ L__ E__ P__ R__ I__ N__ T__ );
FileTouch		:	(F__ I__ L__ E__ T__ O__ U__ C__ H__ );
File			:	(F__ I__ L__ E__ );
Fixed			:	(F__ I__ X__ E__ D__ );
Float			:	(F__ L__ O__ A__ T__ );
Format			:	(F__ O__ R__ M__ A__ T__ );
For				:	(F__ O__ R__ );
Found			:	(F__ O__ U__ N__ D__ );
From			:	(F__ R__ O__ M__ );
Get				:	(G__ E__ T__ );
Guid			:	(G__ U__ I__ D__  | U__ U__ I__ D__ );	
Handler			:	(H__ A__ N__ D__ L__ E__ R__ );
Hold			:	(H__ O__ L__ D__ );
Hour			:	(H__ O__ U__ R__ );
If				:	(I__ F__ );
Igscore			:	(I__ G__ S__ C__ O__ R__ E__ );
In				:	(I__ N__ );
Inet			:	(I__ N__ E__ T__ );
Insert			:	(I__ N__ S__ E__ R__ T__ );
Instead			:	(I__ N__ S__ T__ E__ A__ D__ );
Int				:	(I__ N__ T__ );
Integer			:	(I__ N__ T__ E__ G__ E__ R__ );
Interval		:	(I__ N__ T__ E__ R__ V__ A__ L__ );
Into			:	(I__ N__ T__ O__ );
Is				:	(I__ S__ );
Iterate			:	(I__ T__ E__ R__ A__ T__ E__ );
Key				:	(K__ E__ Y__ );
Lcase			:	(L__ C__ A__ S__ E__ | L__ O__ W__ E__ R__ C__ A__ S__ E__ );
Leading			:	(L__ E__ A__ D__ I__ N__ G__ );
Leave			:	(L__ E__ A__ V__ E__ );
Length			:	(L__ E__ N__ G__ T__ H__ );
Like			:	(L__ I__ K__ E__ );
Load			:	(L__ O__ A__ D__ );
Locate			:	(L__ O__ C__ A__ T__ E__ );
Locator			:	(L__ O__ C__ A__ T__ O__ R__ );
Locators		:	(L__ O__ C__ A__ T__ O__ R__ S__ );
Log				:	(L__ O__ G__ );
Log10			:	(L__ O__ G__ One__ Zero__ );
Log2			:	(L__ O__ G__ Two__ );
Long			:	(L__ O__ N__ G__ );
Loop			:	(L__ O__ O__ P__ );
Lower 			:	(L__ O__ W__ E__ R__ );
Ltrim			:	(L__ T__ R__ I__ M__ );
Macaddr			:	(M__ A__ C__ A__ D__ D__ R__ );
Max 			:	(M__ A__ X__ );
MessageText		:	(M__ E__ S__ S__ A__ G__ E__  Underscore T__ E__ X__ T__ );
Message			:	(M__ E__ S__ S__ A__ G__ E__ );
Microsecond		:	(M__ I__ C__ R__ O__ S__ E__ C__ O__ N__ D__);
Millisecond		:	(M__ I__ L__ L__ I__ S__ E__ C__ O__ N__ D__);
Min 			:	(M__ I__ N__ );		
Minute			:	(M__ I__ N__ U__ T__ E__ );
Mod				:	(M__ O__ D__ );
Money			:	(M__ O__ N__ E__ Y__ );
Month			:	(M__ O__ N__ T__ H__ );
Nanosecond		:	(N__ A__ N__ O__ S__ E__ C__ O__ N__ D__);
Not				:	(N__ O__ T__ );
Null			:	(N__ U__ L__ L__ );
Numeric			:	(N__ U__ M__ E__ R__ I__ C__ );
Nvl				:	(N__ V__ L__ );
Nvl2			:	(N__ V__ L__ Two__ );
Octet_Length	:	(O__ C__ T__ E__ T__  Underscore L__ E__ N__ G__ T__ H__ );
Of				:	(O__ F__ );
On				:	(O__ N__ );
Open			:	(O__ P__ E__ N__ );
Or				:	(O__ R__ );
Parameter		:	(P__ A__ R__ A__ M__ E__ T__ E__ R__ );
Position		:	(P__ O__ S__ I__ T__ I__ O__ N__ );
Precision		:	(P__ R__ E__ C__ I__ S__ I__ O__ N__ );
Procedure		:	(P__ R__ O__ C__ E__ D__ U__ R__ E__ );	
Proc			:	(P__ R__ O__ C__ );
Quarter			:	(Q__ U__ A__ R__ T__ E__ R__);
Range			:	(R__ A__ N__ G__ E__ );
Readline		:	(R__ E__ A__ D__ L__ I__ N__ E__ );
Real			:	(R__ E__ A__ L__ );
Repeat			:	(R__ E__ P__ E__ A__ T__ );
Replace			:	(R__ E__ P__ L__ A__ C__ E__ );
Resignal		:	(R__ E__ S__ I__ G__ N__ A__ L__ );
Result			:	(R__ E__ S__ U__ L__ T__ );
Return			:	(R__ E__ T__ U__ R__ N__ );
Return_Status	:	(R__ E__ T__ U__ R__ N__  Underscore S__ T__ A__ T__ U__ S__ );
Round			:	(R__ O__ U__ N__ D__ );
RowID			:	(R__ O__ W__ I__ D__ );
Rowset			:	(R__ O__ W__ S__ E__ T__ );
Row_Count		:	(R__ O__ W__  Underscore C__ O__ U__ N__ T__ );
Rtrim			:	(R__ T__ R__ I__ M__ );
Second			:	(S__ E__ C__ O__ N__ D__ );
Send			:	(S__ E__ N__ D__ );
Set				:	(S__ E__ T__ );
Signal			:	(S__ I__ G__ N__ A__ L__ );
Sin				:	(S__ I__ N__ );
Size			:	(S__ I__ Z__ E__ );
SmallInt		:	(S__ M__ A__ L__ L__ I__ N__ T__ );
SmallMoney		:	(S__ M__ A__ L__ L__ M__ O__ N__ E__ Y__ );
Soundex			:	(S__ O__ U__ N__ D__ E__ X__ );
Space			:	(S__ P__ A__ C__ E__ );
SqlException	:	(S__ Q__ L__  Underscore E__ X__ C__ E__ P__ T__ I__ O__ N__ );
SqlState		:	(S__ Q__ L__ S__ T__ A__ T__ E__ );
SqlWarning		:	(S__ Q__ L__  Underscore W__ A__ R__ N__ I__ N__ G__ );
Sqrt			:	(S__ Q__ R__ T__ );
StdDev			:	(S__ T__ D__ D__ E__ V__  | S__ T__ D__  Underscore D__ E__ V__  | S__ T__ D__  Underscore D__ E__ V__ I__ A__ T__ I__ O__ N__ );
Structure		:	(S__ T__ R__ U__ C__ T__ U__ R__ E__ );
Substr			:	(S__ U__ B__ S__ T__ R__ );
Substring 		:	(S__ U__ B__ S__ T__ R__ I__ N__ G__ );
Sum 			:	(S__ U__ M__ );
SumSquare 		:	(S__ U__ M__ S__ Q__ U__ A__ R__ E__  | S__ U__ M__ S__ Q__ U__ A__ R__ E__ S__  | S__ U__ M__ O__ F__ S__ Q__ U__ A__ R__ E__ S__ );
Synonyms 		:	(S__ Y__ N__ O__ N__ Y__ M__ S__ );
SysDate			:	(S__ Y__ S__ D__ A__ T__ E__ );
SysDateTime		:	(S__ Y__ S__ D__ A__ T__ E__ T__ I__ M__ E__ );
SysTime			:	(S__ Y__ S__ T__ I__ M__ E__ );
SysTimestamp	:	(S__ Y__ S__ T__ I__ M__ E__ S__ T__ A__ M__ P__ );
Table_Name		:	(T__ A__ B__ L__ E__  Underscore N__ A__ M__ E__ );
Table			:	(T__ A__ B__ L__ E__ );
Tan				:	(T__ A__ N__ );
Text			:	(T__ E__ X__ T__ );
Then			:	(T__ H__ E__ N__ );
Time			:	(T__ I__ M__ E__ );
Timestamp		:	(T__ I__ M__ E__ S__ T__ A__ M__ P__ );
Timezone		:	(T__ I__ M__ E__ Z__ O__ N__ E__ );
Timezone_Hour 	:	(T__ I__ M__ E__ Z__ O__ N__ E__  Underscore H__ O__ U__ R__ );
Timezone_Minute	:	(T__ I__ M__ E__ Z__ O__ N__ E__  Underscore M__ I__ N__ U__ T__ E__ );
TinyInt			:	(T__ I__ N__ Y__ I__ N__ T__ );
To				:	(T__ O__ );
To_Char			:	(T__ O__  Underscore C__ H__ A__ R__ );
To_Date			:	(T__ O__  Underscore D__ A__ T__ E__ );
To_DateTime		:	(T__ O__  Underscore D__ A__ T__ E__ T__ I__ M__ E__ );
To_DSInterval	:	(T__ O__  Underscore D__ S__ I__ N__ T__ E__ R__ V__ A__ L__);
To_Number		:	(T__ O__  Underscore N__ U__ M__ B__ E__ R__ );
To_Time			:	(T__ O__  Underscore T__ I__ M__ E__ );
To_Timestamp	:	(T__ O__  Underscore T__ I__ M__ E__ S__ T__ A__ M__ P__ );
To_YMInterval	:	(T__ O__  Underscore Y__ M__ I__ N__ T__ E__ R__ V__ A__ L__);
Trace			:	(T__ R__ A__ C__ E__ );
Trailing		:	(T__ R__ A__ I__ L__ I__ N__ G__ );
Trigger			:	(T__ R__ I__ G__ G__ E__ R__ );
Trim 			:	(T__ R__ I__ M__ );
Trunc			:	(T__ R__ U__ N__ C__ );
UBigInt			:	(U__ B__ I__ G__ I__ N__ T__ );
Ucase			:	(U__ C__ A__ S__ E__ );
UInt			:	(U__ I__ N__ T__ );
UInteger		:	(U__ I__ N__ T__ E__ G__ E__ R__ );
Undo			:	(U__ N__ D__ O__ );
Until			:	(U__ N__ T__ I__ L__ );
Update			:	(U__ P__ D__ A__ T__ E__ );
Upper 			:	(U__ P__ P__ E__ R__ );
Url				:	(U__ R__ L__ );
USmallInt		:	(U__ S__ M__ A__ L__ L__ I__ N__ T__ );
Value			:	(V__ A__ L__ U__ E__ );
Var 			:	(V__ A__ R__  | V__ A__ R__ I__ A__ N__ C__ E__ );
VarBinary		:	(V__ A__ R__ B__ I__ N__ A__ R__ Y__ );
VarChar			:	(V__ A__ R__ C__ H__ A__ R__ );
Varying			:	(V__ A__ R__ Y__ I__ N__ G__ );
View			:	(V__ I__ E__ W__ );
Virtual			:	(V__ I__ R__ T__ U__ A__ L__ );
Week			:	(W__ E__ E__ K__);
When			:	(W__ H__ E__ N__ );
While			:	(W__ H__ I__ L__ E__ );
With			:	(W__ I__ T__ H__ );
Writeline		:	(W__ R__ I__ T__ E__ L__ I__ N__ E__ );
Xml				:	(X__ M__ L__ );
Year			:	(Y__ E__ A__ R__ );
Year_To_Month	:	(Y__ E__ A__ R__  Underscore T__ O__  Underscore M__ O__ N__ T__ H__ );

SelectInto		:	(S__ E__ L__ E__ C__ T__ ) WhiteSpace+ (I__ N__ T__ O__) ( options {greedy=false;} : . )* Semicolon;

Select			:	(S__ E__ L__ E__ C__ T__ ) ( options {greedy=false;} : . )* Semicolon;

Semicolon		:	';';
AssignEq		:	':=';
LeftParen		:	'(';
RightParen		:	')';
QuestionMark	:	'?';
Bar				:	'|';
Period			:	'.';
Comma			:	',';
Colon			:	':';
LeftBracket		:	'[';
RightBracket	:	']';
RelOp_EQ		:	'=';
RelOp_NE		:	'!=' | '<>';	
RelOp_GE		:	'>=' | '!<';
RelOp_GT		:	'>';
RelOp_LE		:	'<=' ! '!>';
RelOp_LT		:	'<';
	
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
	
String_Constant
	:	(StringTerm)+
	;
	
fragment
DigitOrLetter
	:	Digit | Letter;
	
SqlStateString
	:	DoubleQuote d1=DigitOrLetter 
					d2=DigitOrLetter 
					{IsNotDoubleZero($d1.text->chars, $d2.text->chars) }?
					(DigitOrLetter 
					(DigitOrLetter 
					(DigitOrLetter)? )? )? 
		DoubleQuote
	;
	
Char_Constant
	:	DoubleQuote (Escape DoubleQuote | ~DoubleQuote ) DoubleQuote
	;
	
Identifier	
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
Unsigned_Integer:;

fragment
Signed_Integer	:;

fragment
Unsigned_Float	:;

fragment
Signed_Float	:;

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
	
Numeric_Constant	:
	(	'0'
			(	HexInteger
				{ _type  =  Unsigned_Integer; }

			|	OctalInteger
				{ _type  =  Unsigned_Integer; }
				
			|	Binary_Integer 
				{ _type  =  Unsigned_Integer; }
				
			|	PointFloat
				{ _type  =  Unsigned_Float; }
				
			|	Digit*
				{ _type  =  Unsigned_Integer; }
			)
			  
	|	UnsignedInteger
			(	PointFloat
				{ _type  =  Unsigned_Float; }
				
			|	{true}?
				{ _type  =  Unsigned_Integer; }
			)
			
	|	SignedInteger 
			(	PointFloat
				{
					_type  =  Signed_Float;
				}
				
			|	{true}?
				{
					_type  =  Signed_Integer;
				}
			)
			
	|	PointFloat
			{
				_type  =  Unsigned_Float;
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
	
Plus
	:	'+'
	;
	
Minus
	:	'-'
	;
	
Mult
	:	'*'
	;

Divide	:	'/'
	;
	
Modulo	:	'%'
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
//			fprintf(stderr, "Whitespace[\%s]",$r->getText($r)->chars); 
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

ceate_trigger_statement
	:	Create Trigger
		(	sch1=schema_name Period 
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
		On
		(	Table
			(	sch2=schema_name Period 
				name2a=table_name 
				{IsSchemaTableName($sch2.text->chars, $name2a.text->chars) }?
			
			|	name2b=table_name 
				{IsTableName($name2b.text->chars);}
			)
		
		|	View
			(	sch3=schema_name Period 
				name3a=view_name 
				{IsSchemaTableName($sch3.text->chars, $name3a.text->chars) }?
			
			|	name3b=view_name 
				{IsTableName($name3b.text->chars);}
			)
		
		|	Column
			(	sch4=schema_name Period 
				name4a=table_name 
				name4a2=column_name[$sch4.text->chars, $name4a.text->chars]
				{IsSchemaColumnName($sch4.text->chars, $name4a.text->chars, $name4a2.text->chars) }?
			
			|	name4b=table_name 
				name4b2=column_name["", $name4b.text->chars] 
				{IsColumnName($name4b.text->chars, $name4b2.text->chars);}
			)
		
		|	Virtual? Key
			(	sch5=schema_name Period 
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
		As
		(	(Proc | Procedure)
			(	sch=schema_name Period 
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
		Semicolon?
	;

trigger_event
	:	(Before | After | Instead Of)
		(Insert | Update | Delete | Fetch | Load)
	;
	
procedure_specifier
	:	With
		procedure_option (Comma procedure_option)*
	;
	
procedure_option
	:	new_parameter_name AssignEq
		(	Table_Name
		|	Column_Name (Period Value)?
		|	RowID
		|	Date
		|	Time
		|	Timestamp
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
{ BEFORE | AFTER | INSTEAD OF } 
{ [ INSERT ] [ , ] [ UPDATE ] [ , ] [ DELETE ] [ , ] [ FETCH ]  [ , ] [ LOAD ]} 

<procedure_specifier> ::= 
[ WITH <procedure_option> [ ,...n ] ]
AS { [schema_name.] { procedure_name | number } | <sql_statement_block> }  
 
<procedure_option> ::=
[ @parameter_name =
  { table_name
  | column_name [.$Value]
  | $RowID
  | $Date
  | $Time
  | $Timestamp
  | constant } ]
*/

ceate_procedure_statement
	:	Create (Proc | Procedure)
		(	sch=schema_name Period 
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
		LeftParen new_parameter (Comma new_parameter)* RightParen
		As
		block_statement
		Semicolon?
	;
	
new_parameter
@init	{char * typeName = NULL}
	:	param=new_variable_name 
		typ=datatype_name  
		{typeName = $typ.text->chars;}
		(	Default exp=typed_constant[typeName]
//			{declareParameterWithDefault($param.text->chars, $typ.text->chars, $exp.value);}
			
		|	{declareParameter($param.text->chars, $typ.text->chars);}
		)
	;
// CREATE { PROC | PROCEDURE } [ schema_name.] { procedure_name | number } 
//     [ { @parameter_name data_type } [ = default_value ] ] [ ,...n ] 

block_statement
	:
	Begin Semicolon
	procedure_statement_list
	End
	;
	
procedure_statement_list
	:
	procedure_statement
	(Semicolon procedure_statement)*
	Semicolon?
	;

procedure_statement
	:
	(	block_statement
	|	select_statement
    |	select_into_statement
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

select_statement
	:
	Select
	;
	
select_into_statement
	:
	SelectInto
	;
	
scalar_subquery
	:
	LeftParen
	select_statement
	RightParen
	;
	
vector_subquery
	:	
	LeftParen
	select_statement
	RightParen
	;
	
get_diagnostics_statement 
	:	
	Get Dioagnostics variable_name AssignEq (Return_Status | Row_Count | Exception)
	;
	
allocate_cursor_statement
	:
	Allocate cursor_variable_name Cursor For Result Set locator_variable_name
	;

cursor_variable_name
	:	{IsCursorVariable(LT(1).text->chars) }?	id=Identifier
		
	;
	
associate_locator_statement
	:
	Associate (Result Set)? (Locator | Locators) LeftParen locator_variable_list RightParen With (Proc | Procedure) procedure_name
	;
	
locator_variable_list
	:
	locator_variable_name
	(Comma locator_variable_name)*
	;

locator_variable_name
	:	{IsLocatorVariable(LT(1).text->chars) }?	id=Identifier
	;
	
call_statement
	:
	Call procedure_name LeftParen procedure_parameter_list RightParen
	;
	
procedure_parameter_list
	:
	(	variable_or_constant (Comma variable_or_constant)* (Comma QuestionMark)*
	|	QuestionMark  (Comma QuestionMark)*
	)
	;
	
variable_or_constant
	:	variable_name
	|	constant
	;
	
typed_constant[char * type]
	:	{ IsNumericType(type) }?	numeric_constant
	|	{ IsStringType(type) }?		String_Constant
	|	{ IsCharType(type) }?		Char_Constant
	|	{ IsDateType(type) }?		date_constant
	|	{ IsTimeType(type) }?		time_constant
	|	{ IsDateTimeType(type) }?	datetime_constant
	|	{ IsTimestampType(type) }?	timestamp_constant
	;
	
constant
	:	numeric_constant
	|	String_Constant
	|	Char_Constant
	|	date_constant
	|	time_constant
	|	datetime_constant
	|	timestamp_constant
	;
	
case_statement
	:
	Case
	(	var=variable_name	simple_case_body[$var.text->chars]
	|	searched_case_body
	)
	;
	
simple_case_body [char * var_name]
	:
	(When typed_expression[var_name] Then procedure_statement Semicolon)+
	End Case
	;
	
searched_case_body
	:
	(When condition Then procedure_statement Semicolon)+
	End Case
	;
	
close_cursor_statement
	:
	Close Cursor? cursor_variable_name
	;
	
declare_statement
@init	{char * typeName = NULL;}
	:
	Declare
	(	(Continue | Exit | Undo) Handler For (condition_variable_name | handler_condition) procedure_statement
	|	var=new_variable_name 
		(	Condition For cond=handler_condition
			{declareCondition($var.text->chars, $cond.text->chars);}
		|	Cursor (With Hold)? (With Return To (Caller | Client)? )? For procedure_statement
			{declareCursor($var.text->chars, $cond.text->chars);}
		|	typ=datatype_name 
			{typeName = $typ.text->chars;}
			Default exp=typed_constant[typeName]
//			{declareVariable($var.text->chars, $typ.text->chars, $exp.value);}
		)
	)
	;

datatype_name
	:	elementary_type_name
	|	Range 		Of elementary_type_name
	|	Array 		Of elementary_type_name (array_bounds)+
	|	Set 		Of elementary_type_name
	|	Structure 	LeftParen structure_field (Comma structure_field)* RightParen
	|	Enum 		LeftParen constant 		  (Comma constant)* 	   RightParen
	;
	
array_bounds
	:	LeftBracket Unsigned_Integer RightBracket
	;
	
structure_field
	:	new_variable_name datatype_name
	;
	
elementary_type_name
	:	TinyInt
	|	Boolean
	|	SmallInt
	|	USmallInt
	|	Int
	|	UInt
	|	Integer
	|	UInteger
	|	BigInt
	|	UBigInt
	|	(	Decimal	
		|	Dec		
		|	Numeric	
		|	Fixed	
		)
		precision_scale?
	|	Float
	|	Double	Precision?
	|	Real
	|	Money
	|	SmallMoney
	|	(	Char 
		|	Character
		|	Byte 
		|	Binary	
		|	Bit	
		)
		(Varying)? precision? 
	|	(	(Long)? VarChar
		|	(Long)? VarBinary
		|	(Long)? Blob
		|	(Long)? Clob
		|	(Long)? Text
		|	Bfile
		|	Xml
		|	Url
		)
		precision?
	|	Date
	|	Time		precision?
	|	DateTime	(With Timezone (LeftParen (integer_constant | timezone_name)  RightParen)? )?
	|	Timestamp	precision?	(With Timezone (LeftParen (integer_constant | timezone_name)  RightParen)? )?
	|	Interval	(Year_To_Month | Day_To_Second)
	|	Timezone	(LeftParen (integer_constant | timezone_name)  RightParen)?
	|	Guid
	|	Inet
	|	Macaddr
	;

precision
options{k=1;}
	:	LeftParen Unsigned_Integer RightParen
	;
	
precision_scale
options{k=1;}
//	:	LeftParen  RightParen 
	:	LeftParen Unsigned_Integer ( Comma Unsigned_Integer RightParen | RightParen )
	;

fetch_cursor_statament
	:	Fetch From cursor_variable_name Into variable_name (Comma variable_name)
	;
	
for_statement
	:	
	(	lab=new_label_name Colon
		{declareLabel($lab.text->chars);}
		For loop1=new_variable_name As 
		{declareLoop($loop1.text->chars);}
		(	cur1=new_variable_name Cursor (With Hold)? For
			{declareCursor($cur1.text->chars);}
			sel1a=select_statement
			Do
	   		procedure_statement (Semicolon procedure_statement)* Semicolon?
 			End (For)? (lab2=Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
			{undeclareCursor($cur1.text->chars);}
			
		|	sel1b=select_statement
			Do
		   	procedure_statement (Semicolon procedure_statement)* Semicolon?
 			End (For)? (lab2=Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		)
		{undeclareLoop($loop1.text->chars);}
		{undeclarelabel($lab.text->chars);}
		
	|	For loop2=new_variable_name As 
		{declareLoop($loop2.text->chars);}
		(	cur2=new_variable_name Cursor (With Hold)? For
			{declareCursor($cur2.text->chars);}
			sel2a=select_statement
			Do
	   		procedure_statement (Semicolon procedure_statement)* Semicolon?
 			End (For)? 
			{undeclareCursor($cur2.text->chars);}
			
		|	sel2b=select_statement
			Do
		   	procedure_statement (Semicolon procedure_statement)* Semicolon?
 			End (For)? 
		)
		{undeclareLoop($loop2.text->chars);}

	)
	;
	
if_then_else_statement
	:	If condition
		Then procedure_statement Semicolon
		(	Elseif condition
			Then procedure_statement Semicolon
		)*
		(	Else procedure_statement Semicolon
		)?
		End (If)?
	;
	
iterate_statement
	:	Iterate	label_name
	;
	
leave_statement
	:	Leave	label_name
	;
	
loop_statement
	:	
	(	lab=new_label_name Colon
		{declareLabel($lab.text->chars);}
		Loop
		procedure_statement (Semicolon procedure_statement)* Semicolon?
		End (Loop)? (lab2=Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		{undeclareLabel($lab.text->chars);}

	|	Loop
		procedure_statement (Semicolon procedure_statement)* Semicolon?
		End (Loop)?
	)
	;
	
open_cursor_statement
	:	Open Cursor? cursor_variable_name
	;

repeat_statement
	:	
	(	lab=new_label_name Colon
		{declareLabel($lab.text->chars);}
		Repeat
		(procedure_statement Semicolon)+ 
		Until condition
		End (Repeat)? (lab2=Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		{undeclareLabel($lab.text->chars);}

	|	Repeat
		(procedure_statement Semicolon)+ 
		Until condition
		End (Repeat)? 
	)
	;

resignal_statement
	:	Resignal
		(	SqlState (Value)? (SqlStateString | string_variable) 
		|	condition_variable_name
		)
		(Set MessageText AssignEq (string_variable | String_Constant) )?
	//	RESIGNAL SQLSTATE [VALUE] <sqlstate> [SET MESSAGE_TEXT = <variable> or <diagnostic string constant>]
	//	RESIGNAL <condition name> [SET MESSAGE_TEXT = <variable> or <diagnostic string constant>]
	;
	
return_statement
	:	Return ( general_expression | Null 	)
	;
	
set_statement
	:	Set var=variable_name AssignEq typed_expression[$var.text->chars]
	;
	
signal_statement
	:	Signal
		(	SqlState (Value)? (SqlStateString | string_variable) 
		|	condition_variable_name
		)
		(Set MessageText AssignEq (string_variable | String_Constant) )?
	;
	
while_statement
	:
	(	lab=new_label_name Colon
		{declareLabel($lab.text->chars);}
		While condition
		Do
		procedure_statement (Semicolon procedure_statement)* Semicolon?
		End (While)? (lab2=Identifier {IsMatchingName($lab.text->chars, $lab2.text->chars) }? )?
		{undeclareLabel($lab.text->chars);}

	|	While condition
		Do
		procedure_statement (Semicolon procedure_statement)* Semicolon?
		End (While)?
	)
	;
	
readline_statement
	:	Readline 		
		LeftParen 	
		(	(	File string_value 
		    	{SetStdioFile($stringValue.text->chars);} 
			|	Console 
		    	{SetStdioFile("console");} 
			) 
			Comma 
		)?
		formatString 
		Comma 
		argumentList 
	 	RightParen	
		{ readlineStmt($formatString.text->chars ); }
	;

writeline_statement
	:	Writeline		
		LeftParen 	
		(	(	File string_value 
			    {SetStdioFile($stringValue.text->chars);} 
			|	Console
				{SetStdioFile("console");} 
			|	Trace 
			    {SetStdioFile("trace");} 
			) 
			Comma 
		)?
		formatString 
		Comma 
		argumentList 
		RightParen
		{ writelineStmt($formatString.text->chars, $Writeline.line ); }
	;
	
send_statement
	:	Send	
		Message 						
		{InitMessageQueue();}
		To	(	Console 				
				{SendToConsole() }?
		 	|	que_name Period queID		
		 		{SendToQue($queName.text, $queID.text) }? 
		 	) 
		With 	
		Format
		LeftParen
		formatString 
		Comma
		argumentList 
		RightParen					
		{SendMessageFormat($formatString.text) }?
		Size 	
		LeftParen 
		integer_constant 
		RightParen	
		{ SendMessageSize($intValue.text) }?
		{ SendToMessageQue() }?
	;

argumentList
	:	{ResetArgList();} 
		( argument ( Comma argument )* )? 
	;
	
argument
	:	( variable_name 		{PushArgValue	($variable_name.text->chars);}
		| integer_constant		{PushArgInteger	($integer_constant.text->chars);}
		| float_constant		{PushArgFloat	($float_constant.text->chars);}
		| String_Constant 		{PushArgString	($String_Constant.text->chars);}
		| Char_Constant 		{PushArgChar	($Char_Constant.text->chars);} 
		| date_constant			{PushArgDate	($date_constant.text->chars);}
		| time_constant			{PushArgTime	($time_constant.text->chars);}
		| datetime_constant		{PushArgDateTime($datetime_constant.text->chars);}
		| timestamp_constant	{PushArgTimestamp($timestamp_constant.text->chars);}
		) 
	;
	
que_name
	:	Identifier
	;
	
queID
	:	Identifier
	;
	
formatString
	:	String_Constant {IsFormatString($String_Constant.text->chars) }?
	;

fileCreateStmt
	:	FileCreate		LeftParen	string_value RightParen
						{ fileCreateStmt($string_value.text->chars ); }
	;

fileDeleteStmt
	:	FileDelete		LeftParen	string_value RightParen
						{ fileDeleteStmt($string_value.text-> ); }
	;

fileTouchStmt
	:	FileTouch		LeftParen	string_value RightParen
						{ fileTouchStmt($string_value.text->chars ); }
	;
	
filePrintStmt
	:	FilePrint		LeftParen	str1=string_value Comma str2=string_value RightParen
						{ filePrintStmt($str1.text->chars, $str2.text->chars ); }
	;

fileCopyStmt
	:	FileCopy	 	LeftParen	str1=string_value Comma str2=string_value Comma str3=string_value RightParen
						{ fileCopyStmt($str1.text->chars, $str2.text->chars, $str3.text->chars ); }
	;
	
fileDiffStmt
	:	FileDiff	 	LeftParen	str1=string_value Comma str2=string_value Comma str3=string_value RightParen
						{ fileDiffStmt( $str1.text->chars, $str2.text->chars, $str3.text->chars ); }
	;
	
// ---------------------------------------------------------------------------------------------
// ** Identifiers and Constants                                                               **
// ---------------------------------------------------------------------------------------------

variable_name
	:	{ IsVariable(LT(1).text->chars) }? 		id=Identifier	
	;

new_variable_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Identifier
	;

new_trigger_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Identifier
	;	
	
new_trigger_number	
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Unsigned_Integer
	;	
	
procedure_name
	:	{ IsProcedureName(LT(1).text->chars) }?	id=Identifier
	;
	
new_procedure_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Identifier
	;	
	
procedure_number
	:	{ IsProcedureName(LT(1).text->chars) }?	id=Unsigned_Integer
	;	
	
new_procedure_number	
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Unsigned_Integer
	;	

new_parameter_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Identifier
	;	
	
parameter_name
	:	{ IsParameterVar(LT(1).text->chars) }?	id=Identifier
	;
	
condition_variable_name
	:	{ IsConditionVar(LT(1).text->chars) }?	id=Identifier
	;
	
schema_name
	:	{ IsSchemaName(LT(1).text->chars) }?	id=Identifier
	;
	
table_name
	:	{ IsTableName(LT(1).text->chars) }?		id=Identifier
	;
	
new_table_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Identifier
	;
	
view_name
	:	{ IsViewName(LT(1).text->chars) }?		id=Identifier
	;
	
column_name[char * schemaName, char * tableName]
	:	{ IsColumnName(schemaName, tableName, LT(1).text->chars) }?		id=Identifier
	;
	
key_column_name[char * schemaName, char * tableName]
	:	{ IsKeyColumnName(schemaName, tableName, LT(1).text->chars) }?	id=Identifier
	;
	
new_label_name
	:	{ IsNotDeclared(LT(1).text->chars) }?	id=Identifier
	;
	
label_name
	:	{ IsLabelName(LT(1).text->chars) }?		id=Identifier
	;

timezone_name
	:	{ IsTimezoneName(LT(1).text->chars) }?	tz=String_Constant
	;
	
handler_condition
	:
	(	SqlException
	|	SqlWarning
	|	Not Found
	|	SqlState SqlStateString
	)
	;
	
// ---------------------------------------------------------------------------------------------
// ** Conditions and Boolean Expressions                                                      **
// ---------------------------------------------------------------------------------------------

condition
	:  
	boolean_expression
	;

boolean_expression
	:  
	boolean_term (Or boolean_term)*
	;
	
boolean_term	
	:
	boolean_factor (And boolean_factor)*
	;
	
boolean_factor	
	:
	(Not) => Not
	(	(untyped_boolean_primary)	=>	untyped_boolean_primary
	|	(numeric_boolean_primary)	=>	numeric_boolean_primary
	|	(string_boolean_primary)	=>	string_boolean_primary
	|	(clob_boolean_primary)		=>	clob_boolean_primary
	|	(blob_boolean_primary)		=>	blob_boolean_primary
	|	(date_boolean_primary)		=>	date_boolean_primary
	|	(time_boolean_primary)		=>	time_boolean_primary
	|	(datetime_boolean_primary)	=>	datetime_boolean_primary
	|	(timestamp_boolean_primary)	=>	timestamp_boolean_primary
	|	LeftParen	
		condition	
		RightParen
	)
	;
	
untyped_boolean_primary
	:
	(	Exists 			scalar_subquery
    |	Not Exists 		scalar_subquery
	|	is_or_is_not	In Rowset 		Identifier
	)
	;
	
is_or_is_not
	:	Is	Not?
	|	Not
	;
	
relational_operator  
	:
    (	RelOp_EQ
    |	RelOp_NE
    |	RelOp_GE
    |	RelOp_GT
    |	RelOp_LE
    |	RelOp_LT
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
    LeftParen
    constant_value_expression ( Comma constant_value_expression )*
    RightParen
    ;
	
constant_value_expression    :
    (	constant_value
    
//    |	( Trunc | To_Char )	
//		LeftParen
//		constant_value
//		(	Comma
//			String_Constant
//		)?
//		RightParen

//    |	( To_Number | To_Date | To_Time |  )    
//		LeftParen
//		String_Constant
//		(	Comma
//			String_Constant
//		)?
//		RightParen
    )
    ;

constant_value
	:
	(	constant
    |	parameter_name
	|	QuestionMark
    |	Plus		numeric_constant
    |	Minus		numeric_constant
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
    (	Context 		LeftParen Identifier Comma String_Constant RightParen
    |	Coalesce 		general_expression_set
    |	Nvl 			LeftParen general_expression Comma general_expression RightParen
    |	Nvl2 			LeftParen general_expression Comma general_expression Comma general_expression RightParen
    |	Igscore 		LeftParen numeric_constant Comma (QuestionMark | Parameter) Comma Identifier Comma numeric_constant (Comma numeric_constant)? RightParen
	)
	;

user_defined_function
	:
	id=Identifier 				// User Defined Function
	{IsUserDefinedFunction($id.text->chars) }?
	LeftParen					// note: function_name() is ambiguous and so not supported
    nullable_expression	 	( Comma nullable_expression )*
	RightParen
	;

// ---------------------------------------------------------------------------------------------
// ** General Expressions                                                                     **
// ---------------------------------------------------------------------------------------------

nullable_expression	:
	(	general_expression
	|	Null
	)
	;
	
general_expression
	:
	(	general_value_expression
	|	case_expression
	|	Cast
		LeftParen
		general_value_expression
		As
		elementary_type_name
		RightParen
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
	Case
	(	var=variable_name	simple_case_expression_body[$var.text->chars]
	|	searched_case_expression_body
	)
	;
	
simple_case_expression_body [char * var_name]
	:
	(When typed_expression[var_name] Then general_expression Semicolon)+
	End Case
	;

searched_case_expression_body
	:
	(When condition Then general_expression Semicolon)+
	End Case
	;

general_expression_set
	:	
    LeftParen 
    general_expression ( Comma general_expression )*
    RightParen
    ;

general_expression_value_set
	:	
	(	LeftParen 
    	general_expression_value ( Comma general_expression_value )*
    	
    |	vector_subquery
    )
    RightParen
    ;

general_expression_value    :
    (	QuestionMark
    |	Null
    |	general_expression
    )
    ;

// ---------------------------------------------------------------------------------------------
// ** Numeric Expressions                                                                     **
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
		(	In			numeric_set
		|	Between		numeric_expression And numeric_expression
		)
	|	is_or_is_not	Null
	)
	;	
	
numeric_expression
	:
	numeric_term		( ( Plus | Minus )		numeric_term )*
	;

numeric_term
	:
	numeric_factor		( ( Mult | Divide | Modulo )	numeric_factor )*
	;
	
numeric_factor
	:
	(	Plus	numeric_primary
		
	|	Minus	numeric_primary
	
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
	:	{IsNumericVariableName(LT(1).text->chars) }?	id=Identifier
	;
	
integer_constant
	:	Unsigned_Integer | Signed_Integer
	;

float_constant
	:	Unsigned_Float	 | Signed_Float
	;

numeric_constant
	:	integer_constant | float_constant
	;
	
/*	
sqlNUMERIC_SCALAR_SUBQUERY	:
	LeftParen
	STOK_SELECT
    sqlNUMERIC_EXPRESSION 
    (	sqlSELECT_FROM_CLAUSE
		sqlSELECT_WHERE_CLAUSE?
	)?
	RightParen
    ;

sqlNUMERIC_SUBQUERY	:
	LeftParen
	STOK_SELECT
    sqlNUMERIC_EXPRESSION 
    (	sqlSELECT_FROM_CLAUSE
		sqlSELECT_WHERE_CLAUSE?
		sqlSELECT_GROUP_CLAUSE?
		sqlSELECT_ORDER_CLAUSE?
	)?
	RightParen
    ;
*/
numeric_coerced_value
	:
	To_Number				// TO_NUMBER(string_exp {,string_exp {,string_exp} } )
	LeftParen	
	( string_expression	| parameter_name )
	(	Comma 
		string_expression 
		(	Comma 
			string_expression 
		)? 
	)?	
	RightParen
	;
	

numeric_function
	:
	(	(	Abs			// ABS(number)						// alt 1
		|	Acos		// ACOS(number)		
		|	Asin		// ASIN(number)
		|	Atan		// ATAN(number)	
		|	Cos			// COS(number)	
		|	Exp			// EXP(number)		
		|	Log			// LOG(number)		
		|	Log10		// LOG10(number)
		|	Log2		// LOG2(number)
		|	Sin			// sin(number)	
		|	Sqrt		// sqrt(number)
		|	Tan			// tan(number)
		)
		LeftParen	
		numeric_expression	
		RightParen
		
	|	(	Atan2		// ATAN2(number[, number])			// alt 2
		|	Mod			// MOD(number[, number])
		|	Round		// ROUND(number[, decimal])
		)				
		LeftParen	
		numeric_expression	
		Comma	
		numeric_expression	
		RightParen

	|	(	Ascii				// ASCII(string_exp)
		|	Bit_Length			// BIT_LENGTH(string_exp)	// alt 3
		|	Char_Length			// CHAR_LENGTH(string_exp)
		|	Character_Length	// CHARACTER_LENGTH(string_exp)
		|	Octet_Length		// OCTET_LENGTH(string_exp)
		|	Length				// LENGTH(string_exp)
		)
		LeftParen	
		string_expression	
		RightParen

	|	Extract			// EXTRACT( date_part FROM date_expression )	// alt 4
		LeftParen	
		date_part	
		From 
		(	(date_expression)		=>	date_expression 
		|	(datetime_expression)	=>	datetime_expression
		|	(timestamp_expression)	=>	timestamp_expression
		|	interval_expression
		)	
		RightParen
		
	|	Position		// POSITION( string1 IN string2 )	// alt 5
		LeftParen	
		string_expression	In string_expression
		RightParen

    |	Count 				// COUNT()							// alt 6
		general_expression_set   
			
    |	(	Avg 			 									// alt 7
    	|	Max 			 
    	|	Min 			 
    	|	Sum 			 
    	|	Var 			 
    	|	StdDev 			 
    	|	SumSquare 			  
    	)
    	numeric_set
	)
	;
	
numeric_set
	:	LeftParen
		numeric_expression	(Comma numeric_expression)*
		RightParen
	;

numeric_value_set
	:  
	(	LeftParen	
		numeric_value 		(Comma numeric_value )*
		RightParen
		
//	|	numeric_vector_subquery
	)
	;
	
numeric_value 
	:
    (	QuestionMark
    |	Null
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
		(	In			string_set
		|	Like		String_Constant
		|	Between		string_expression And string_expression
		)
	|	is_or_is_not
		Null
	)
	;
	
string_expression
	:
	string_primary 	( (Plus | Bar ) string_primary )*
	;

string_primary			:
	(	string_variable
	|	string_coerced_value
	|	string_function
	|	String_Constant
	)
	;

string_variable
	:	{IsStringVariableName(LT(1).text->chars) }?	id=Identifier
	;

string_coerced_value	:
	To_Char
	LeftParen
	general_expression
	RightParen
	;
	

string_function
	:
	(	(	Lcase		// LCASE(string_exp)
		|	Ltrim		// LTRIM(string_exp)
		|	Rtrim		// RTRIM(string_exp)
		|	Soundex		// SOUNDEX(string_exp)
		|	Ucase		// UCASE(string_exp)
	    |	Upper 		// UPPER(string_exp)
    	|	Lower 		// LOWER(string_exp)
		|	Synonyms	// SYNONYMS(string_exp)
		)
		LeftParen	
		string_expression	
		RightParen

	|	(	Concat		// CONCAT(string_exp1, string_exp2)
		|	Difference	// DIFFERENCE(string_exp1, string_exp2)
		)
		LeftParen	
		string_expression	Comma string_expression
		RightParen

	|	Replace			// REPLACE(string_exp1, string_exp2, string_exp3)
		LeftParen	
		string_expression	Comma string_expression	Comma string_expression
		RightParen

	|	Space			// SPACE(count)
		LeftParen	
		numeric_expression	
		RightParen

	|	Repeat			// REPEAT(string_exp, count)
		LeftParen	
		string_expression	Comma numeric_expression	
		RightParen

	|	(	Substring	// SUBSTRING(string_exp, start, length)
		|	Substr		// SUBSTR(string_exp, start, length)
		)
		LeftParen	
		string_expression	Comma numeric_expression	Comma numeric_expression	
		RightParen

	|	Locate			// LOCATE(string_exp1, string_exp2[, start])
		LeftParen	
		string_expression	Comma string_expression	( Comma numeric_expression	)?
		RightParen
		
	|	Trim 			//	trim( { [LEADING | TRAILING | BOTH] trim_char | trim_char } FROM trim_source )
	    LeftParen 
	    (	Leading 
	    	string_expression
	    
    	|	Trailing 
    		string_expression
    	
    	|	Both 
    		string_expression
    	
    	|	string_expression
    	)
		(	From
			string_expression
		)?
    	RightParen
	)
	;
	
	
string_set
	:	LeftParen
		string_value	(Comma string_value)*
		RightParen
	;
	
string_value
	:	string_variable | String_Constant
	;	
	
string_value_set
	:
	LeftParen
	(	string_set_value 	(Comma string_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	RightParen
	;

string_set_value
	:
    (	QuestionMark
    |	Null
    |	string_expression
    )
	;

// ---------------------------------------------------------------------------------------------
// ** CLOB Expressions                                                                        **
// ---------------------------------------------------------------------------------------------

clob_boolean_primary
	:	
	(	Not?
		Contains
		LeftParen
		clob_variable
		Comma
		string_value
		RightParen
			
	|	clob_variable
		is_or_is_not	
		Null
	)	
	;
	
clob_variable
	:	{IsClobVariable(LT(1).text->chars) }?	id=Identifier
	;
		
// ---------------------------------------------------------------------------------------------
// ** BLOB Expressions                                                                        **
// ---------------------------------------------------------------------------------------------

blob_boolean_primary
	:	
	blob_variable
	is_or_is_not	
	Null
	;
	
blob_variable
	:	{IsBlobVariable(LT(1).text->chars) }?	id=Identifier
	;
		
// ---------------------------------------------------------------------------------------------
// ** Date Expressions                                                                        **
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
		(	In			date_set
		|	Between		date_expression And date_expression
		)
	|	is_or_is_not
		Null
	)
	;
	
date_expression
	:
	(	date_variable
	|	date_constant
	|	date_coerced_value
	|	date_function
	)
//	(	( Minus | Plus )
//		interval_expression
//	)*
	;
	
date_variable
	:	{IsDateVariableName(LT(1).text->chars) }?	id=Identifier
	;


date_constant
	:	Date String_Constant
	;
	
date_coerced_value
	:
	To_Date		// TO_DATE(string_sxp {,string_exp {,string_exp} } )	
	LeftParen	
	(	(datetime_expression)	=>	datetime_expression 
	|	(timestamp_expression)	=>	timestamp_expression 
	|	string_expression
	)
	(	Comma 
		string_expression 
		(	Comma 
			string_expression 
		)? 
	)?	
	RightParen
	;

date_function
	:
	(	(Trunc LeftParen date_expression)	=>
		Trunc
		LeftParen	
		date_expression
		(	Comma 
			string_expression 
		)?	
		RightParen

	|	SysDate
	)
	;

date_set
	:
	LeftParen
	(	date_expression 	(Comma date_expression )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	RightParen
	;

date_value_set
	:
	LeftParen
	(	date_set_value 		(Comma date_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	RightParen
	;

date_set_value
	:
    (	QuestionMark
    |	Null
    |	date_expression
    )
	;

// ---------------------------------------------------------------------------------------------
// ** Time Expressions                                                                        **
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
		(	In			time_set
		|	Between		time_expression And time_expression
		)
	|	is_or_is_not
		Null
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
	:	{IsTimeVariableName(LT(1).text->chars) }?	id=Identifier
	;
	
time_constant
	:	Time String_Constant
	;
	
time_coerced_value
	:
	To_Time		// TO_TIME(string_sxp {,string_exp {,string_exp} } )	
	LeftParen	
	( 	(datetime_expression)	=>	datetime_expression 
	|	(timestamp_expression)	=>	timestamp_expression 
	| 	string_expression 
	)
	(	Comma 
		string_expression 
		(	Comma 
			string_expression 
		)? 
	)?	
	RightParen
	;

time_function
	:
	(	Trunc
		LeftParen	
		time_expression
		(	Comma 
			string_expression 
		)?	
		RightParen

	|	SysTime
	)
	;

time_set
	:
	LeftParen
	(	time_expression 	(Comma time_expression )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	RightParen
	;

time_value_set
	:
	LeftParen
	(	time_set_value 		(Comma time_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	RightParen
	;

time_set_value
	:
    (	QuestionMark
    |	Null
    |	time_expression
    )
	;

// ---------------------------------------------------------------------------------------------
// ** DateTime Expressions                                                                    **
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
		(	In			datetime_set
		|	Between		datetime_expression And datetime_expression
		)
	|	is_or_is_not
		Null
	)
	;
	

datetime_expression
	:
	(	datetime_variable
	|	datetime_coerced_value
	|	datetime_function
	|	datetime_constant
	)
//	(	( Minus | Plus )
//		interval_expression
//	)*
	;

datetime_variable
	:	{IsDateTimeVariableName(LT(1).text->chars) }?	id=Identifier
	;

datetime_constant
	:	DateTime String_Constant
	;
	
datetime_set
	:
	LeftParen
	datetime_expression 	(Comma datetime_expression )*
	RightParen
	;
	
datetime_value_set
	:	
	LeftParen
	(	datetime_set_value 	(Comma datetime_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	RightParen
	;

datetime_set_value    :
    (	QuestionMark
    |	Null
    |	datetime_expression
    )
	;

datetime_coerced_value
	:
	To_DateTime			// converts char of CHAR, VARCHAR2, NCHAR, or NVARCHAR2 datatype to a value of DATETIME datatype. The fmt is a datetime model format specifying the format of char. If you omit fmt, then char must be in the default date format. If fmt is J, for Julian, then char must be an integer.
	LeftParen			// see http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions183.htm
	( datetime_constant | string_expression | timestamp_expression )
	(	Comma 
		string_expression 
		(	Comma 
			string_expression 
		)? 
	)?	
	RightParen
	;
	
datetime_function
	:
	(	(Trunc LeftParen datetime_expression)	=>
		Trunc			// TRUNC ( datetime_exp {, string_exp } ) 
		LeftParen		// returns a date truncated to a specific unit of measure 
						// http://www.techonthenet.com/oracle/functions/trunc_date.php
		datetime_expression 	
		(	Comma 
			string_expression 
		)?	
		RightParen
		
	|	SysDateTime		// The built-in function SYSDATE returns a DATE value containing the current date and time on your system. 
						// http://infolab.stanford.edu/~ullman/fcdb/oracle/or-time.html
	)
	;

// ---------------------------------------------------------------------------------------------
// ** Timestamp Expressions                                                                    **
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
		(	In			timestamp_set
		|	Between		timestamp_expression And timestamp_expression
		)
	|	is_or_is_not
		Null
	)
	;
	
timestamp_expression
	:
	(	timestamp_variable
	|	timestamp_coerced_value
	|	timestamp_function
	|	timestamp_constant
	)
//	(	( Minus | Plus )
//		interval_expression
//	)*
	;
	
timestamp_variable
	:	{IsTimestampVariableName(LT(1).text->chars) }?	id=Identifier
	;

timestamp_constant
	:	Timestamp String_Constant
	;

timestamp_set
	:
	LeftParen
	timestamp_expression 	(Comma timestamp_expression )*
	RightParen
	;
	
timestamp_value_set
	:	
	LeftParen
	( timestamp_set_value 	(Comma timestamp_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
	)
	RightParen
	;

timestamp_set_value    :
    (	QuestionMark
    |	Null
    |	timestamp_expression
    )
	;

timestamp_coerced_value
	:
	To_Timestamp			// converts char of CHAR, VARCHAR2, NCHAR, or NVARCHAR2 datatype to a value of DATETIME datatype. The fmt is a datetime model format specifying the format of char. If you omit fmt, then char must be in the default date format. If fmt is J, for Julian, then char must be an integer.
	LeftParen			// see http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions183.htm
	( timestamp_constant | string_expression )
	(	Comma 
		string_expression 
		(	Comma 
			string_expression 
		)? 
	)?	
	RightParen
	;
	
timestamp_function
	:
	(	(Trunc LeftParen timestamp_expression)	=>
		Trunc			// TRUNC ( datetime_exp {, string_exp } ) 
		LeftParen		// returns a date truncated to a specific unit of measure 
						// http://www.techonthenet.com/oracle/functions/trunc_date.php
		timestamp_expression 	
		(	Comma 
			string_expression 
		)?	
		RightParen
		
	|	SysTimestamp		// The built-in function SYSDATE returns a DATE value containing the current date and time on your system. 
						// http://infolab.stanford.edu/~ullman/fcdb/oracle/or-time.html
	)
	;

// ---------------------------------------------------------------------------------------------
// ** Interval Expressions                                                                    **
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
		(	In			interval_set
		|	Between		interval_expression And interval_expression
		)
	|	is_or_is_not
		Null
	)
	;

interval_expression
	:
	(	interval_primary		( ( Plus | Minus )		interval_primary )*
		
	|	(	(date_expression 		Minus	date_expression)		=>	date_expression 		Minus	date_expression
		|	(time_expression		Minus	time_expression)		=>	time_expression			Minus	time_expression
		|	(datetime_expression	Minus	datetime_expression)	=>	datetime_expression		Minus	datetime_expression
		|	(timestamp_expression	Minus	timestamp_expression)	=>	timestamp_expression	Minus	timestamp_expression
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
	:	{IsIntervalVariableName(LT(1).text->chars) }?	id=Identifier
	;

interval_constant
	:	Interval 
		(	String_Constant
		
		|	( Plus | Minus )
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
    (	non_second_date_part precision? ( To ( non_second_date_part | Second ) precision? )?
    
    |	Second precision_scale?
    )
    ;
    	
interval_coerced_value
	:
	(	To_YMInterval	// converts char of CHAR, VARCHAR2, NCHAR, or NVARCHAR2 datatype to a value of Interval datatype. The fmt is a datetime model format specifying the format of char. If you omit fmt, then char must be in the default date format. If fmt is J, for Julian, then char must be an integer.
	|	To_DSInterval	// see http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions183.htm
	)
	LeftParen		
	(	date_constant
	|	time_constant
	|	datetime_constant
	|	timestamp_constant
	| 	string_expression 
	)
	(	Comma 
		string_expression 
		(	Comma 
			string_expression 
		)? 
	)?	
	RightParen
	;
	
interval_function
	:
	DateDiff			
	LeftParen			
	(	date_part
		(	(datetime_expression)	=>	datetime_expression
		|	(timestamp_expression)	=>	timestamp_expression
		)
		Comma
		(	(datetime_expression)	=>	datetime_expression
		|	(timestamp_expression)	=>	timestamp_expression
		)
		
	|	(	ymd_date_part
			date_expression
		)	=>
		ymd_date_part
		date_expression
		Comma
		date_expression
		
	|	(day_part time_expression)	=>
		day_part
		time_expression 	
		Comma 
		time_expression 	
	)
	RightParen	
	;
	
interval_set
	:
	LeftParen
	Interval String_Constant 	(Comma Interval String_Constant )*
	RightParen
	;
	
//interval_value_set
//	:	
//	LeftParen
//	( interval_set_value 	(Comma interval_set_value )*
//	|	STOK_SELECT sqlSELECT_CLAUSE
//	)
//	RightParen
//	;

//interval_set_value 
//	:	Interval String_Constant
//	;
//	:
//  (	QuestionMark
//  |	Null
//  |	interval_constant
//  |	interval_expression
//  )
//	;
	
date_part
	:
	(	Year
    |	Month
    |	Day
    |	Hour
    |	Minute
   	|	Second
	|	Millisecond
	|	Microsecond
	|	Nanosecond
	|	Quarter
	|	DayOfYear
	|	Week
	|	Timezone_Hour
    |	Timezone_Minute
	)
	;	

ymd_date_part
	:
	(	Year
    |	Month
    |	Day
	|	Quarter
	|	DayOfYear
	|	Week
    )
    ;
	
non_second_date_part
	:	
	(	Year
    |	Month
    |	Day
    |	Hour
    |	Minute
	)
    ;
	
day_part
	:	
	(	Hour
    |	Minute
	|	Second
	|	Millisecond
	|	Microsecond
	|	Nanosecond
	)
	;

time_zone_field
    :
    (	Timezone_Hour
    |	Timezone_Minute
    )
    ;
