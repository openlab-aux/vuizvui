  λ(a : Type)
→ { name :
      Text
  , description :
      Text
  , options :
      List ../Option/type.dhall
  , arguments :
      ./Arguments.dhall a
  }
