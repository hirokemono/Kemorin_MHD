!>@file   m_ucd_data.f90
!!@brief  module m_ucd_data
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO
!!
!!@verbatim
!!@endverbatim
!
      module m_ucd_data
!
      use m_precision
      use m_field_file_format
      use m_file_format_switch
!
      use t_ucd_data
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: fem_ucd
!
!>      file header for original field data
      character(len=kchara) :: org_ucd_header =  "field_org/out"
!
! -----------------------------------------------------------------------
!
!      contains
!
! -----------------------------------------------------------------------
!
      end module m_ucd_data
