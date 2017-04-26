# THOMAS E WILSON 
# RUBY COMPILER written from

$TRUE = 1
$FALSE = 0

$MAX_TABLE = 500
$MAX_Symbol = 47
$MAX_SYM_SZ = 20


$ERROR_SEMICOLON        =1
$ERROR_IDENT            =2
$ERROR_UNKNOWN          =3
$ERROR_ASSIGN           =4
$ERROR_ASSIGN_PROC      =5
$ERROR_PROCEDURE        =6
$ERROR_END_SYM          =7
$ERROR_DO_SYM           =8
$ERROR_THEN_SYM         =9
$ERROR_VARIABLE         =10
$ERROR_RPAREN           =11
$ERROR_IS_PROCEDURE     =12
$ERROR_PROG_SIZE        =13
$ERROR_END_PROG         =14
$ERROR_NUMBER           =15
$ERROR_LPAREN           =16
$ERROR_REL              =17
$ERROR_NUMBER_IDENT     =18
$ERROR_NOPROCEDURE      =19


module Symbols
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
    ELSE = 30
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
$codeIndx0 = 0                          #Beginning Code Index Before First statement
$codeIndx = 0                           #Keeping Track of Current Index of Code Array

$table = []
$code = []  # Code Array
$stack = [] # Interpreter Stack


class TableValue
    attr_reader :name, :kind, :level, :adr, :value
    def initialize(name, kind, level, adr, value)
        @name = name
    @kind = kind
    @level = level
    @adr = adr
    @value = value
    end
end
#-------------------Commands to put in the array of assembly code------------------------------------------------
class Cmd
  attr_reader :line, :cmd, :statLinks,  :value
    def initialize(line, cmd, statLinks, value)
      @line = line
      @cmd = cmd
      @statLinks = statLinks
      @value = value
    end
end
#--------------------Function to generate assembly code---------------------------------------------
def gen(cmd, statLinks, value)
    if $codeIndx > $CXMAX
      #outfile = "Error, program is too long"
      #puts outfile
      exit(0)
    end
    x = Cmd.new($codeIndx, cmd, statLinks, value) # Code becomes something
    $code.push(x)
    $codeIndx += 1
end
#--------------------Function to change jump commands---------------------------------------------
def fixJmp(cx, jmpTo)
  $code[cx].value = jmpTo
end
#--------------------Function to print p-Code for a given block---------------------------------------------
    def printCode()
      for i in $codeIndx0..$codeIndx
        outfile.puts  "#{$code[i].line} #{$code[i].cmd} #{$code[i].statLinks} #{$code[i].value}"
        $prevIndx = $codeIndx
      end
    end
#--------------------Function to find a new Base---------------------------------------------
def Base(statLinks, base)
  @b1 = base
  while $statLinks > 0
    @b1 = $stack[@b1]
    $statLinks -= 1
  end 
end
#--------------------P-Code Interpreter---------------------------------------------
  def Interpret()
    puts $outfile = "Start PL/0"
    top = 0
    base = 1
    #B = Base. new
    pos = 0
    @stack[1] = 0
    @stack[2] = 0
    @stack[3] = 0
    begin
      instr = $code[pos]
      pos += 1
      # LIT COMMMAND
      if instr.cmd == "LIT"
        top += 1
        @stack[top] = int(instr.value) # This definitely does not work

      elsif instr.cmd == "OPR"
        if instr.value == 0     #end
          top = base - 1
          base = @stack[top+2]
          pos = @stack[top+3]
        elsif instr.value == 1   #Unary minus
          @stack[top] = -stack[top]
        elsif instr.value == 2   #Addition
          top -= 1
          @stack[top] = @stack[top] + @stack[top+1]
        elsif instr.value == 3   #Subtraction
          top -= 1
          @stack[top] = @stack[top] - @stack[top+1]
        elsif instr.value == 4   #Multiplication
          top -= 1
          @stack[top] = @stack[top] * @stack[top+1]
        elsif instr.value == 5   #Division
          top -= 1
          @stack[top] = @stack[top] / @stack[top+1]
        elsif instr.value == 6   #Logical odd function
          if @stack[top] % 2 == 0
            @stack[top] = 1
          else
            @stack[top] = 0
          end
          # Case 7 is N/A, used to debug
        elsif instr.value == 8  #Test for equality, replace if true
          top -= 1
          if @stack[top] == @stack[top+1]
            @stack[top] = 1
          else
            @stack[top] = 0
          end
        elsif instr.value == 9 #Test for inequality
          top -= 1
          if @stack[top] != @stack[top+1]
            @stack[top] = 1
          else
            @stack[top] = 0
          end
        elsif instr.value == 10 # Test for <
          top -= 1
          if @stack[top] < @stack[top+1]
            @stack[top] = 1
          else
            @stack[top] = 0
          end
        elsif instr.value == 11 #Test for >=
          top -= 1
          if @stack[top] >= @stack[top+1]
            @stack[top] = 1
          else
            @stack[top] = 0
          end
        elsif instr.value == 12 #Test for >
          top -= 1
          if @stack[top] > @stack[top+1]
            @stack[top] = 1
          else
            @stack[top] = 0
          end
        elsif instr.value == 13 #Test for <=
          top -= 1
          if @stack[top] <= @stack[top+1]
            @stack[top] = 1
          else
            @stack[top] = 0
          end
        elsif instr.value == 14 #Write/Print stack[top]
          puts @stack[top]
          top -= 1
        elsif instr.value == 15 #Write/Print a newline
          puts
        else
          puts "Error: No Match"
        end
        # LOD Command
      elsif instr.cmd == "LOD"
        top += 1
        @stack[top] = @stack[B(instr.statLinks, base) + instr.value] #Definitely a jank line here, really broken
        # STO Command
      elsif instr.cmd == "STO"
        @stack[top] = @stack[B(instr.statLinks, base) + instr.value] = @stack[top] #Another broken line here
        top -= 1
        # CAL Command
      elsif instr.cmd == "CAL"
        @stack[top+1] = B(instr.statLinks, base)
        @stack[top+2] = base
        @stack[top+3] = pos
        base = top + 1
        pos = instr.value
        # INT Command
      elsif instr.cmd == "INT"
        top = top + instr.value
        # JMP Command
      elsif instr.cmd == "JMP"
        pos = instr.value
        # JPC Command
      elsif instr.cmd == "JPC"
        if @stack[top] == instr.statLinks
          pos = instr.value
        end
        top -= 1
      end
      if pos == 0
        break
      end
    end while true
    puts "END PL/0"
end
#----------------Error Messages------------------------------------------------------


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
    
        if $sym != Symbols::IDENT
            error($ERROR_IDENT)
        end
        getsym()
        if $sym != Symbols::EQL
            error($ERROR_ASSIGN)
        end
        getsym()
        if $sym != Symbols::NUM
            error($ERROR_NUMBER)
        end
        getsym()
    elsif kind == Objtype::VARIABLE
        if $sym != Symbols::IDENT
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
def block(tableIndex, level)

    tx[0] = tableIndex
    tx0 = tableIndex
    dx = 3
    cx1 = codeIndx
    gen("JMP", 0 , 0)

    if $sym == Symbols::CONST
    
        #// ---- CONSTANT SYM ----
        getsym()
        enter(Objtype::CONSTANT, $id)

        while $sym == Symbols::COMMA do
            getsym()
            enter(Objtype::CONSTANT, $id)
        end
        if $sym != Symbols::SEMICOLON
            error($ERROR_SEMICOLON)
        end
        getsym()
    end
    #// ---- VARIABLE SYM ----
    if $sym == Symbols::VAR
    
        getsym()

        enter(Objtype::VARIABLE, $id)
        while $sym == Symbols::COMMA do
            getsym()

            enter(Objtype::VARIABLE, $id)
        end
        if $sym != Symbols::SEMICOLON
            error($ERROR_SEMICOLON)
        end
        getsym()
    end
    #// ---- PROCEDURE SYM ----
    while $sym == Symbols::PROC do
    
        while $sym == Symbols::PROC do
        
            getsym()
            if $sym != Symbols::IDENT
                error($ERROR_IDENT)
            end
            enter(Objtype::PROCEDURE, $id)
            getsym()

            block(tx[0], level+1)#//inc static link for functions inside of functions, table current pointer

            if $sym != Symbols::SEMICOLON
                error($ERROR_SEMICOLON)
            end
            getsym()
        end
    end

    fixJmp(cx1, $codeIndx)
    $codeIndx0 = $codeIndx
    gen("INT", 0, dx)
    statement(tx[0], level)
    gen("OPR", 0, 0)
    #print code for this block
    printCode()
end

#--------------STATEMENT----------------------------------------
def statement()

    i = 0

    case $sym
        #// IDENT
        when Symbols::IDENT
            i = position()
            if i == 0
                error($ERROR_UNKNOWN)
            end

            case $tablek[i]
                when Objtype::VARIABLE
                    getsym()
                    if $sym != Symbols::ASSIGN
                        error($ERROR_ASSIGN)
                    end
                    getsym()
                    expression()
                    gen("STO", level - table[i].level, table[i].adr)
                else
                    error($ERROR_ASSIGN_PROC)
            end

        #// PROCEDURE CALL
        when Symbols::CALL
            getsym()

            if $sym != Symbols::IDENT
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
            gen("CAL", level - table[i].level, table[i].adr)

        #// BEGIN and END block
        when Symbols::BEGINSYM
            getsym()
            statement()
            while $sym == Symbols::SEMICOLON do
                getsym()
                statement()
            end
            if $sym != Symbols::ENDSYM
                error($ERROR_END_SYM)
            end
            getsym()

        #// WHILE Symbol
        when Symbols::WHILE
            getsym()
            cx1 = $codeIndx
            condition()
            cx2 = $codeIndx
            gen("JPC", 0, 0)
            if $sym != Symbols::DO
                error($ERROR_DO_SYM)
            end
            getsym()
            statement()
            gen("JMP", 0, cx1)
            fixJmp(cx2, $codeIndx)

        #// IF - THEN - ELSE
        when Symbols::IF
            getsym()
            condition()
            cx1 = $codeIndx
            gen("JPC", 0, 0)
            if $sym != Symbols::THEN
                error($ERROR_THEN_SYM)
            end
            getsym()
            statement()
            if $sym == Symbols::ELSE
                cx2 = $codeIndx
                gen("JMP", 0, 0)
                fixJmp(cx1, $codeIndx)
                getsym()
                statement(tx, level)
                fixJmp(cx2, $codeIndx)
            else
                fixJmp(cx1, $codeIndx)
    end
end

#----------CONDITION---------------------------------------
def condition()
    #// ODD Symbol
    if $sym == Symbols::ODD
        getsym()
        expression()
    else
        expression()
        if (($sym == Symbols::EQL) || ($sym == Symbols::NOTEQL) || ($sym == Symbols::LSTHEN) || ($sym == Symbols::LSEQL) || ($sym == Symbols::GRTHEN) || ($sym == Symbols::GREQL))
            getsym()
            expression()
        else
            error($ERROR_REL)
        end
    end
end

#------------------------------EXPRESSION----------------------------
def expression()
    if (($sym == Symbols::PLUS) || ($sym == Symbols::MINUS))
        getsym()
        term()
    else
        term()
    end

    while ($sym == Symbols::PLUS || $sym == Symbols::MINUS) do
        getsym()
        term()
    end
end

#-----------------------TERM-----------------------------
def term()
    factor()
    while (($sym == Symbols::MULT) || ($sym == Symbols::DIV)) do
        getsym()
        factor()
    end
end

#-----------------------FACTOR------------------------------
def factor()
    i = 0

    case $sym
        #// IDENTIFER
        when Symbols::IDENT
            i = position()
            if i == 0
                error($ERROR_UNKNOWN)
            end
            if $tablek[i] == Objtype::PROCEDURE
                error($ERROR_IS_PROCEDURE)
            end
            getsym()

        #// NUMBER
        when Symbols::NUM
            getsym()

        #// LEFT PARENTHESE
        when Symbols::LPAREN
            getsym()
            expression()
            if $sym != Symbols::RPAREN
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
            $sym = Symbols::BEGINSYM
        elsif $id == "CALL"
            $sym = Symbols::CALL
        elsif $id == "CONST"
            $sym = Symbols::CONST
        elsif $id =="DO"
            $sym = Symbols::DO
        elsif $id =="END"
            $sym = Symbols::ENDSYM
        elsif $id =="IF"
            $sym = Symbols::IF
        elsif $id =="ELSE"
            $sym = Symbols::ELSE
        elsif $id =="ODD"
            $sym = Symbols::ODD
        elsif $id =="PROCEDURE"
            $sym = Symbols::PROC
        elsif $id =="THEN"
            $sym = Symbols::THEN
        elsif $id =="VAR"
            $sym = Symbols::VAR
        elsif $id =="WHILE"
            $sym = Symbols::WHILE
        else
            $sym = Symbols::IDENT;
            $symstr[$sym] = $id
        end
        return
    end

    if (chartype(ch) == Intype::DIGIT)
    
        strnum = [10]
        $sym = Symbols::NUM
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
            $sym = Symbols::ASSIGN
        elsif $punc == ":"
            $sym = Symbols::COLON
        elsif $punc == ","
            $sym = Symbols::COMMA
        elsif $punc == "/"
            $sym = Symbols::DIV
        elsif $punc == "="
            $sym = Symbols::EQL
        elsif $punc == ">="
            $sym = Symbols::GREQL
        elsif $punc == ">"
            $sym = Symbols::GRTHEN
        elsif $punc == "("
            $sym = Symbols::LPAREN
        elsif $punc == "<="
            $sym = Symbols::LSEQL
        elsif $punc == "<"
            $sym = Symbols::LSTHEN
        elsif $punc == "-"
            $sym = Symbols::MINUS
        elsif $punc == "*"
            $sym = Symbols::MULT
        elsif $punc == "<>"
            $sym = Symbols::NOTEQL
        elsif $punc == "."
            $sym = Symbols::PERIOD
        elsif $punc == "+"
            $sym = Symbols::PLUS
        elsif $punc == ")"
            $sym = Symbols::RPAREN
        elsif $punc == ";"
            $sym = Symbols::SEMICOLON

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