module Analyzer
contains
   subroutine analyze(obj)
      use fy_AbstractNode
      class(*), intent(in) :: obj

      select type(obj)
      class is (AbstractNode)
         print*,'is mapping node', obj%is_mapping()
      end select
   end subroutine analyze
end module Analyzer
   
