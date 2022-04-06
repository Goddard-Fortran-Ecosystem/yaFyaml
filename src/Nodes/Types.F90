! Types and parameters defined here are used just for
! interface disambiguation in iterator methods

module fy_Types
   implicit none

   private

   public :: YAML_STRING
   public :: YAML_BOOL
   public :: YAML_INT
   public :: YAML_FLOAT
   public :: YAML_SEQUENCE
   public :: YAML_MAPPING

   public :: string_t
   public :: bool_t
   public :: int_t
   public :: float_t
   public :: sequence_t
   public :: mapping_t

   type string_t
   end type string_t
   type(string_t), parameter :: YAML_STRING = string_T()

   type bool_t
   end type bool_t
   type(bool_t), parameter :: YAML_BOOL = bool_T()

   type int_t
   end type int_t
   type(int_t), parameter :: YAML_INT = int_t()

   type float_t
   end type float_t
   type(float_t), parameter :: YAML_FLOAT = float_t()

   type sequence_t
   end type sequence_t
   type(sequence_t), parameter :: YAML_SEQUENCE = sequence_t()

   type mapping_t
   end type mapping_t
   type(mapping_t), parameter :: YAML_MAPPING = mapping_t()

end module fy_Types
