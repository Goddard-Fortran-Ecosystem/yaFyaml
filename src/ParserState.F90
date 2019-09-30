module yfy_ParserState
  implicit none

  type, abstract :: ParserState
  end type ParserState

  type, extends(ParserState) :: TextState
  end type TextState
  ...
end module yfy_ParserState
