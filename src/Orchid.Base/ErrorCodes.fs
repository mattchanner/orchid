namespace Orchid.Expressions

[<AutoOpen>]
module ErrorCodes =
    [<Literal>]
    let InvalidVariableDefinition = 0x01

    [<Literal>]
    let EndOfStream = 0x02
    
    [<Literal>] 
    let UnmatchedRoundBracket = 0x03    
    
    [<Literal>] 
    let UnmatchedSquareBracket = 0x04    
    
    [<Literal>] 
    let UnmatchedCurlyBracket = 0x05
    
    [<Literal>]
    let IncorrectNumberOfArgs = 0x06
    
    [<Literal>]
    let IncorrectArgType = 0x07
    
    [<Literal>]
    let UnknownToken = 0x08
    
    [<Literal>]
    let InternalError = 0x09
    
    [<Literal>]
    let InvalidTokenSequence = 0x0A
    
    [<Literal>]
    let UnknownFunction = 0x0B
    
    [<Literal>]
    let TooManyArgsToFunction = 0x0C

    [<Literal>]
    let InvalidLetBinding = 0x0D

    [<Literal>]
    let ExpressionHasNoReturnValue = 0x0E

    [<Literal>]
    let ExpressionHasMultipleReturnValues = 0x0F

    [<Literal>]
    let FinalExpressionMustBeResult = 0x10

    [<Literal>]
    let InvalidAssignmentIdentifier = 0x11

    [<Literal>]
    let DuplicateAssignment = 0x12

