!>@file   m_array_for_send_recv.f90
!!@brief      module m_array_for_send_recv
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n    Modified on Apr., 2008
!!@n    Modified on Dec., 2012
!!@n    Modified on sep., 2013
!
!>@brief Work array for data communication of FEM data
!!
!!@verbatim
!!@endverbatim
!!
      module   m_array_for_send_recv
!
      use m_precision
      use t_vector_for_solver
!
      implicit  none
!
!
      type(vectors_4_solver), save :: vect1
!
      end module   m_array_for_send_recv
