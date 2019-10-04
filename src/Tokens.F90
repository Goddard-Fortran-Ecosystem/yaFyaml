module fy_Tokens
  implicit none
  private

  public :: AbstractToken

  type, abstract :: AbstractToken
     private
  end type AbstractToken

  type, extends(AbstractToken) :: ValueToken
  end type ValueToken

  type, extends(AbstractToken) :: KeyToken
  end type KeyToken

  type, extends(AbstractToken) :: BlockMappingStartToken
  end type BlockMappingStartToken

  type, extends(AbstractToken) :: BlockSequenceStartToken
  end type BlockSequenceStartToken

  type, extends(AbstractToken) :: BlockEndToken
  end type BlockEndToken

  type, extends(AbstractToken) :: FlowMappingStartToken
  end type FlowMappingStartToken

  type, extends(AbstractToken) :: FlowMappingStopToken
  end type FlowMappingStartToken

  type, extends(AbstractToken) :: FlowSequenceStartToken
  end type FlowSequenceStartToken

  type, extends(AbstractToken) :: FlowSequenceStartToken
  end type FlowSequenceStartToken

  
  
end module fy_Tokens
