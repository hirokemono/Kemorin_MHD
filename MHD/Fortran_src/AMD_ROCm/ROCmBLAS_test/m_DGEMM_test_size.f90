!>@file   m_DGEMM_test_size.f90
!!@brief  module m_test_DGEMM_size
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief Array and loop sizes for DGEMM tests
!!
!!@verbatim
!!@endverbatim
      module m_DGEMM_test_size
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: nloop =        5
!
      integer(kind = kint), parameter :: num_column_C = 1024
      integer(kind = kint), parameter :: num_row_C =    512
      integer(kind = kint), parameter :: num_row_A =    2048
!
      real(kind = kreal), parameter :: alpha=1.0
      real(kind = kreal), parameter :: beta=2.0

      end module m_DGEMM_test_size
!
