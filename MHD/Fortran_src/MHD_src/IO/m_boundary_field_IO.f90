!>@file   m_boundary_field_IO.f90
!!@brief  module m_boundary_field_IO
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Structure for boundary condition data IO
!!
      module m_boundary_field_IO
!
      use m_precision
      use t_boundary_field_IO
!
      implicit  none
!
      type(IO_boundary), save :: bc_FEM_IO1
!
      end module m_boundary_field_IO
