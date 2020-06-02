program main
  !! Test the reading of a JSON file with a nested array of objects
  use yafyaml, only : Parser, Configuration, FileStream
  use gFTL_UnlimitedVector, only : UnlimitedVector
  implicit none

  type vertex
    integer, allocatable :: depends_on(:)
  end type

  type dag
    type(vertex), allocatable :: vertices(:)
  end type

  type(dag) d
  type(Parser) p
  type(Configuration) c, subconfig

  class(*), pointer :: dag_vertices=>null(), dag_vertices_i_depends_on=>null()
  integer :: i, j

  p = Parser('core')
  c = p%load(FileStream('nested-object-array.json'))
  call c%get_node_at_selector(dag_vertices, 'dag', 'vertices')

  select type (dag_vertices)
  class is (UnlimitedVector)

    allocate(d%vertices(dag_vertices%size()))

    do i=1,size(d%vertices)

      call c%get(subconfig, 'dag', 'vertices', i)
      call subconfig%get_node_at_selector(dag_vertices_i_depends_on, 'depends_on')

      select type (dag_vertices_i_depends_on)
      class is (UnlimitedVector)

        allocate(d%vertices(i)%depends_on(dag_vertices_i_depends_on%size()))

        do j = 1,size(d%vertices(i)%depends_on)
          d%vertices(i)%depends_on(j) = c%at('dag','vertices',i,'depends_on',j)
        end do
      class default
        error stop "unexpected dag_vertices_i_depends_on class"
      end select

    end do

  class default
    error stop "unexpected dag_vertices class"
  end select

  if ( any(d%vertices(1)%depends_on /= [2,3]      )) error stop "dag%vertices(1) incorrect"
  if ( any(d%vertices(2)%depends_on /= [3]        )) error stop "dag%vertices(2) incorrect"
  if ( any(d%vertices(3)%depends_on /= [integer::])) error stop "dag%vertices(3) incorrect"

  sync all
  if (this_image()==1) print *,"Test passed"
end program
