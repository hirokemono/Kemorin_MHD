!>@file   t_commute_filter_z.f90
!!        module t_commute_filter_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief commutive filter in vertical direction
!!
!!@verbatim
!!        subroutine allocate_z_filter_mom_params
!!        subroutine deallocate_z_filter_mom_params
!!@endverbatim
!!
      module t_commute_filter_z
!
      use m_precision
!
      implicit none
!
      type vart_fileter_moments
        character(len=kchara), allocatable :: z_filter_moment_type(:)
      end type vart_fileter_moments
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      end module t_commute_filter_z
