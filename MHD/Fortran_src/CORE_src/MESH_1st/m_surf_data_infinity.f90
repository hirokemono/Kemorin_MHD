!
!     module m_surf_data_infinity
!.......................................................................
!
!     written by H. Matsui
!
!>@file  t_surface_boundary.f90
!!       module t_surface_boundary
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Structure for inifinity boundary
!!
!!@verbatim
!!      subroutine deallocate_infty_surf_ctl
!!@endverbatim
!
      module m_surf_data_infinity
!
      use m_precision
      use t_surface_boundary
!
      implicit  none
!
!>      Structure for scalar's boundary condition on surface
      type(scalar_surf_BC_list), save :: infty_list
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_infinity
!
      call dealloc_scalar_surf_BC(infty_list)
!
      end subroutine deallocate_surf_infinity
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_infinity
