program main
  !! Test the reading of a JSON file with a nested array of objects
  use yafyaml, only : Parser, YAML_Node, FileStream, YAFYAML_SUCCESS
  use gFTL_UnlimitedVector, only : UnlimitedVector
  implicit none

  type vertex
    integer, allocatable :: depends_on(:)
  end type

  type dag
    type(vertex), allocatable :: vertices(:)
  end type

  type(dag) :: d
  type(Parser) :: p
  type(YAML_Node) :: c
  type(YAML_Node) :: dag_vertices, dag_vertices_i_depends_on

  integer :: i, j, status

  p = Parser('core')
  c = p%load('nested-object-array.json')
  dag_vertices = c%at('dag', 'vertices', rc=status)
  if (status /= YAFYAML_SUCCESS) error stop "did not find 'dag' 'vertices'"

  allocate(d%vertices(dag_vertices%size()))

  do i=1,size(d%vertices)

     dag_vertices_i_depends_on = dag_vertices%at(i, 'depends_on', rc=status)
     if (status /= YAFYAML_SUCCESS) error stop "did not find 'depends_on'"
     
     if (dag_vertices_i_depends_on%is_sequence()) then
        
        allocate(d%vertices(i)%depends_on(dag_vertices_i_depends_on%size()))
        
        do j = 1,size(d%vertices(i)%depends_on)
           d%vertices(i)%depends_on(j) = dag_vertices_i_depends_on%of(j)
        end do
     else
        error stop "expected a sequence in dag_vertices_i_depends_on"
     end if
  end do



  if ( any(d%vertices(1)%depends_on /= [2,3]      )) error stop "dag%vertices(1) incorrect"
  if ( any(d%vertices(2)%depends_on /= [3]        )) error stop "dag%vertices(2) incorrect"
  if ( any(d%vertices(3)%depends_on /= [integer::])) error stop "dag%vertices(3) incorrect"
  
  print *,"Test passed"

end program
