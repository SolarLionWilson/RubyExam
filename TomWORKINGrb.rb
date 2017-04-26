$TRUE = 1
$FALSE = 0

$MAX_TABLE = 500
$MAX_Symbol = 47
$MAX_SYM_SZ = 20


$ERROR_SEMICOLON        =    1
$ERROR_IDENT            =    2
$ERROR_UNKNOWN            =    3
$ERROR_ASSIGN            =    4
$ERROR_ASSIGN_PROC        =    5
$ERROR_PROCEDURE        =    6
$ERROR_END_SYM            =    7
$ERROR_DO_SYM            =    8
$ERROR_THEN_SYM            =    9
$ERROR_VARIABLE            =    10
$ERROR_RPAREN            =    11
$ERROR_IS_PROCEDURE        =    12
$ERROR_PROG_SIZE        =    13
$ERROR_END_PROG         =    14
$ERROR_NUMBER             =    15
$ERROR_LPAREN             =    16
$ERROR_REL                 =    17
$ERROR_NUMBER_IDENT        =    18
$ERROR_NOPROCEDURE        =    19

module Symb
    VAR = 0
    CONST = 1
    BEGINSYM = 2
    ENDSYM = 3
    PERIOD = 4
    SEMICOLON = 5
    COLON = 6
    LPAREN = 7
    RPAREN = 8
    GRTHEN = 9
    LSTHEN = 10
    GREQL = 11
    LSEQL = 12
    EQL = 13
    ASSIGN = 14
    IF = 15
    IDENT = 16
    NUM = 17
    PROC = 18
    NOTEQL = 19
    MINUS = 20
    PLUS = 21
    DIV = 22
    MULT = 23
    COMMA = 24
    ODD = 25
    CALL = 26
    THEN = 27
    WHILE = 28
    DO = 29
end

module Objtype
    NOTYPE = 0
    CONSTANT = 1
    VARIABLE = 2
    PROCEDURE= 3
end

module Intype
    ALPHA = 0
    DIGIT = 1
    EOL = 2
    NONE = 3
    PUNCT = 4
    SPACE = 5
end

$linecount = 0         #line counter
$charcount = 0         #a character counter
$number = 0

$prevsym = 0                        #holds the previous Symbol
$sym = 0                            #holds current Symbol
$tablen = [$MAX_TABLE]                 #table arrays
$tablek = [$MAX_TABLE]
$id = ""                             #an identification string
$fullline = ""                        #holds entire input line
$punc = ""                            #punction array
$symstr = [$MAX_Symbol]                  #Symbols array
$tableinx = 0                           #table size

def error(num)

	#puts("Entered : error")

    puts("")

    case num
        when $ERROR_NOPROCEDURE
            puts( "Procedure not accepted here")
        when $ERROR_NUMBER_IDENT
            puts( "number or ident expected")
        when $ERROR_ASSIGN
            puts( "Assignment operator expected")
        when $ERROR_ASSIGN_PROC
            puts( "Assignment not allowed here")
        when $ERROR_END_PROG
            puts( "Premature end of program")
        when $ERROR_DO_SYM
            puts( "DO Symbol Expected")
        when $ERROR_END_SYM
            puts( "END Symbol Expected")
        when $ERROR_IDENT
            puts( "Identifier Expected")
        when $ERROR_IS_PROCEDURE
            puts( "Assignment to PROCEDURE not allowed")
        when $ERROR_NUMBER
            puts( "A number was Expected")
        when $ERROR_PROG_SIZE
            puts( "Program size is too large...")
        when $ERROR_RPAREN
            puts( "RIGHT Parenthesis Expected")
        when $ERROR_LPAREN
            puts( "LEFT Parenthesis Expected")
        when $ERROR_SEMICOLON
            puts( "Semicolon Expected" )
        when $ERROR_THEN_SYM
            puts( "THEN Symbol Expected")
        when $ERROR_UNKNOWN
            puts( "Unknown Identifier")
        when $ERROR_VARIABLE
            puts( "Variable or Expression Expected")
        when $ERROR_REL
            puts( "Relational operator expected")
    end

    puts("")
    exit 1

end

#---------------ENTER PROCEDURE-------------------------------
def enter(kind, name)


    $tableinx += 1
    $tablen[$tableinx] = name
    $tablek[$tableinx] = kind

    if (kind == Objtype::CONSTANT)
    
        if $sym != Symb::IDENT
            error($ERROR_IDENT)
        end
        getsym()
        if $sym != Symb::EQL
            error($ERROR_ASSIGN)
        end
        getsym()
        if $sym != Symb::NUM
            error($ERROR_NUMBER)
        end
        getsym()
    elsif kind == Objtype::VARIABLE
        if $sym != Symb::IDENT
            error($ERROR_IDENT)
        end
        getsym()
    elsif kind == Objtype::PROCEDURE
        getsym()
    end
end

#--------------POSITION FUNCTION----------------------------
def position()

    i = $tableinx

    $tablen[0] = $id

    while $tablen[i] != $id do
        
        i -= 1

    end
    return i
end

#-------------BLOCK------------------------------------------------
def block()
    if $sym == Symb::CONST
    
        #// ---- CONSTANT SYM ----
        getsym()
        enter(Objtype::CONSTANT, $id)

        while $sym == Symb::COMMA do
            getsym()
            enter(Objtype::CONSTANT, $id)
        end
        if $sym != Symb::SEMICOLON
            error($ERROR_SEMICOLON)
        end
        getsym()
    end
    #// ---- VARIABLE SYM ----
    if $sym == Symb::VAR
    
        getsym()

        enter(Objtype::VARIABLE, $id)
        while $sym == Symb::COMMA do
            getsym()

            enter(Objtype::VARIABLE, $id)
        end
        if $sym != Symb::SEMICOLON
            error($ERROR_SEMICOLON)
        end
        getsym()
    end
    #// ---- PROCEDURE SYM ----
    while $sym == Symb::PROC do
    
        while $sym == Symb::PROC do
        
            getsym()
            if $sym != Symb::IDENT
                error($ERROR_IDENT)
            end
            enter(Objtype::PROCEDURE, $id)
            getsym()

            block()#//inc static link for functions inside of functions, table current pointer

            if $sym != Symb::SEMICOLON
                error($ERROR_SEMICOLON)
            end
            getsym()
        end
    end

    statement()
end

#--------------STATEMENT----------------------------------------
def statement()

    i = 0

    case $sym
        #// IDENT
        when Symb::IDENT
            i = position()
            if i == 0
                error($ERROR_UNKNOWN)
            end

            case $tablek[i]
                when Objtype::VARIABLE
                    getsym()
                    if $sym != Symb::ASSIGN
                        error($ERROR_ASSIGN)
                    end
                    getsym()
                    expression()
                else
                    error($ERROR_ASSIGN_PROC)
            end

        #// PROCEDURE CALL
        when Symb::CALL
            getsym()

            if $sym != Symb::IDENT
                error($ERROR_IDENT)
            end

            i = position()
            if i == 0
                error($ERROR_UNKNOWN)
            end

            if $tablek[i] != Objtype::PROCEDURE
                error($ERROR_PROCEDURE)
            end

            getsym()

        #// BEGIN and END block
        when Symb::BEGINSYM
            getsym()
            statement()
            while $sym == Symb::SEMICOLON do
                getsym()
                statement()
            end
            if $sym != Symb::ENDSYM
                error($ERROR_END_SYM)
            end
            getsym()

        #// WHILE Symbol
        when Symb::WHILE
            getsym()
            condition()
            if $sym != Symb::DO
                error($ERROR_DO_SYM)
            end
            getsym()
            statement()

        #// IF - THEN
        when Symb::IF
            getsym()
            condition()
            if $sym != Symb::THEN
                error($ERROR_THEN_SYM)
            end
            getsym()
            statement()
    end
end

#----------CONDITION---------------------------------------
def condition()
    #// ODD Symbol
    if $sym == Symb::ODD
        getsym()
        expression()
    else
        expression()
        if (($sym == Symb::EQL) || ($sym == Symb::NOTEQL) || ($sym == Symb::LSTHEN) || ($sym == Symb::LSEQL) || ($sym == Symb::GRTHEN) || ($sym == Symb::GREQL))
            getsym()
            expression()
        else
            error($ERROR_REL)
        end
    end
end

#------------------------------EXPRESSION----------------------------
def expression()
    if (($sym == Symb::PLUS) || ($sym == Symb::MINUS))
        getsym()
        term()
    else
        term()
    end

    while ($sym == Symb::PLUS || $sym == Symb::MINUS) do
        getsym()
        term()
    end
end

#-----------------------TERM-----------------------------
def term()
    factor()
    while (($sym == Symb::MULT) || ($sym == Symb::DIV)) do
        getsym()
        factor()
    end
end

#-----------------------FACTOR------------------------------
def factor()
    i = 0

    case $sym
        #// IDENTIFER
        when Symb::IDENT
            i = position()
            if i == 0
                error($ERROR_UNKNOWN)
            end
            if $tablek[i] == Objtype::PROCEDURE
                error($ERROR_IS_PROCEDURE)
            end
            getsym()

        #// NUMBER
        when Symb::NUM
            getsym()

        #// LEFT PARENTHESE
        when Symb::LPAREN
            getsym()
            expression()
            if $sym != Symb::RPAREN
                error($ERROR_RPAREN)
            end
            getsym()
        else
            error($ERROR_VARIABLE)
    end
end

def letter(ch)
    ch =~ /[[:alpha:]]/
end

def numeric(ch)
    ch =~ /[[:digit:]]/
end

def punct(ch)
    ch =~ /[[:punct:]]/
end

def chartype(ch)
    if (ch == '\n' || ch == '\r')
        return Intype::EOL        #// character END-OF-LINE
    end
    if ch == ' '
        return Intype::SPACE      #// character SPACE
    end
    if numeric(ch)
        return Intype::DIGIT      #// character DIGIT
    end
    if letter(ch)
        return Intype::ALPHA      #// character ALPHA
    end
    if punct(ch)
        return Intype::PUNCT      #// character PUNCTUATION
    end
    if ch == '=' || ch == '<' || ch == '>' || ch == ')' || ch == '(' || ch == '+' || ch == '-' || ch == '*' || ch == '/'
        return Intype::PUNCT
    end

    return Intype::NONE
end


#----------------------------GETCHARACTER FUNCTION----------------------------
def getchar()
    if ( $charcount == $fullline.size)
        $charcount = 0      #// zero out counters
        $fullline = $infile.gets
        if (($fullline.size == 0) && ($charcount == 0))
            error($ERROR_END_PROG)
        end
        $linecount += 1          #// count lines

    end
    ch = $fullline[$charcount]
    if chartype(ch) == Intype::ALPHA
        ch = ch.upcase
    end
    $charcount += 1                      #// count characters
    return ch
end

#--------------------------GET SYMBOL FUNCTION-------------------------------
def getsym()

	$prevsym = $sym
    ch = ""
    index = 0
    begin
        ch = getchar()
    end while (chartype(ch) == Intype::SPACE || chartype(ch) == Intype::EOL || chartype(ch) == Intype::NONE)
    if (chartype(ch) == Intype::ALPHA)

        $id = ""
    
        begin
        
            $id[index] = ch
            index += 1
            ch = getchar()
        end while ((chartype(ch) == Intype::ALPHA || chartype(ch) == Intype::DIGIT || ch  == '_') && (chartype(ch) != Intype::NONE))
        if ch != Intype::NONE
            $charcount -= 1
        end
        if $id == "BEGIN"
            $sym = Symb::BEGINSYM
        elsif $id == "CALL"
            $sym = Symb::CALL
        elsif $id == "CONST"
            $sym = Symb::CONST
        elsif $id =="DO"
            $sym = Symb::DO
        elsif $id =="END"
            $sym = Symb::ENDSYM
        elsif $id =="IF"
            $sym = Symb::IF
        elsif $id =="ODD"
            $sym = Symb::ODD
        elsif $id =="PROCEDURE"
            $sym = Symb::PROC
        elsif $id =="THEN"
            $sym = Symb::THEN
        elsif $id =="VAR"
            $sym = Symb::VAR
        elsif $id =="WHILE"
            $sym = Symb::WHILE
        else
            $sym = Symb::IDENT;
            $symstr[$sym] = $id
        end
        return
    end

    if (chartype(ch) == Intype::DIGIT)
    
        strnum = [10]
        $sym = Symb::NUM
        $number = 0
        begin
        
            strnum[index] = ch
            index += 1
            #$number = 10 * $number + (ch.to_i - 48)
            ch = getchar()
        end while (chartype(ch) == Intype::DIGIT)
        $charcount -= 1
        $number = strnum.join.to_i
        $symstr[$sym] = strnum.join.to_i

        return
    end

    if (chartype(ch) == Intype::PUNCT)

        $punc = ""
    
        $punc[index] = ch
        index += 1
        if (ch == ':' || ch == '<' || ch == '>')
            ch = getchar()
            if ((chartype(ch) == Intype::PUNCT) && ((ch == '=') || (ch == '>')))
                $punc[index] = ch
                index += 1
            else
                $charcount -= 1
            end
        end
        
        if $punc == ":="
            $sym = Symb::ASSIGN
        elsif $punc == ":"
            $sym = Symb::COLON
        elsif $punc == ","
            $sym = Symb::COMMA
        elsif $punc == "/"
            $sym = Symb::DIV
        elsif $punc == "="
            $sym = Symb::EQL
        elsif $punc == ">="
            $sym = Symb::GREQL
        elsif $punc == ">"
            $sym = Symb::GRTHEN
        elsif $punc == "("
            $sym = Symb::LPAREN
        elsif $punc == "<="
            $sym = Symb::LSEQL
        elsif $punc == "<"
            $sym = Symb::LSTHEN
        elsif $punc == "-"
            $sym = Symb::MINUS
        elsif $punc == "*"
            $sym = Symb::MULT
        elsif $punc == "<>"
            $sym = Symb::NOTEQL
        elsif $punc == "."
            $sym = Symb::PERIOD
        elsif $punc == "+"
            $sym = Symb::PLUS
        elsif $punc == ")"
            $sym = Symb::RPAREN
        elsif $punc == ";"
            $sym = Symb::SEMICOLON

        end
        

        $symstr[$sym] = $punc

        return
    end
end

#-----------------------------------Main Program----------------------------


$infile = File.new('pre_mod_test_case.txt', 'r');
$outfile = File.new('output.txt', 'w')

getsym()
        
block()

puts("Successful Compilation! Go have a beer!")