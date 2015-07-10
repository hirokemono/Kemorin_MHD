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
!!      subroutine const_bc_infinity_surf_grp(iflag_surf_infty)
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
      subroutine const_bc_infinity_surf_grp(iflag_surf_infty)
!
      use m_surface_group
      use const_bc_infinity_surf
!
      integer(kind=kint), intent(in) :: iflag_surf_infty
!
!
      call count_num_bc_infinity(iflag_surf_infty,                      &
     &    sf_grp1%num_grp, surf_name, infty_list%ngrp_sf)
!
      call alloc_scalar_surf_BC(infty_list)
!
      call set_bc_infty_id(iflag_surf_infty,                            &
     &    sf_grp1%num_grp, surf_name,                                   &
     &    infty_list%ngrp_sf, infty_list%igrp_sf)
!
      end subroutine const_bc_infinity_surf_grp
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_infinity
