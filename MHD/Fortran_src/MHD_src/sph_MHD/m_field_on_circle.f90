!>@file   m_field_on_circle.f90
!!@brief  module m_field_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
      module m_field_on_circle
!
      use m_precision
!
      use t_field_on_circle
!
      implicit none
!
!
      type(circle_fld_maker), save :: cdat1
!
      end module m_field_on_circle
