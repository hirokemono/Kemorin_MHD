module MSB_kind_defs
  implicit none
  integer(4) :: idummy
  integer(8) :: jdummy
  integer, parameter :: SingleReal_K = KIND(1.0e0)
  integer, parameter :: DoubleReal_K = KIND(1.0d0)
  integer, parameter :: RegularInt_K = KIND(idummy)
  integer, parameter :: VeryLongInt_K = KIND(jdummy)
end module
