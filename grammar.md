Non-terminals are uppercase, tokens are lowercase, items surrounded by quotation marks represent arbitrary strings of acceptable characters (that do not coincide with the strings representing other tokens)

Statement =
    assume ( "name" : Term );
    def "name" : Term := Term;

Term =
    lambda "name" . Term
    "name"
    univ int
    fun Term Term
    ( Term )
    ( Term : Term )
    Term ( Term )