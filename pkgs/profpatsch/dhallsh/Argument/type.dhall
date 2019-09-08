let Option = ../Option/type.dhall

in  < Plain : Text | Flag : Option | Option : { opt : Option, arg : Text } >
