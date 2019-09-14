let Command = ../../Command/type.dhall

let Argument = ../../Argument/type.dhall

in  { cmd :
        Text
    , description :
        Text
    , condition :
        Optional (Command Argument)
    , short-option :
        Optional Text
    , long-option :
        Optional Text
    , long-option-old-style :
        Optional Text
    , argument :
        Optional Text
    , keep-order :
        Bool
    , no-files :
        Bool
    , require-parameter :
        Bool
    , wraps :
        Optional Text
    }
