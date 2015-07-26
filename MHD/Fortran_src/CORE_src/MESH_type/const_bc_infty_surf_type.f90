!const_bc_infty_surf_type.f90
!     module const_bc_infty_surf_type
!
!     written by H. Matsui on Dec., 2008
!
!
!      subroutine const_bc_infinity_surf_grp(iflag_surf_infty, group)
!      subroutine empty_infty_surf_type(group)
!        type(mesh_groups), intent(inout) :: group
!
      module const_bc_infty_surf_type
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_bc_infinity_surf_grp                             &
     &          (iflag_surf_infty, surf_grp, infty_grp)
!
      use t_group_data
      use t_surface_boundary
      use const_bc_infinity_surf
!
      integer(kind=kint), intent(in) :: iflag_surf_infty
      type(surface_group_data), intent(in) :: surf_grp
      type(scalar_surf_BC_list), intent(inout) :: infty_grp
!
!
      call count_num_bc_infinity(iflag_surf_infty,                      &
     &    surf_grp%num_grp, surf_grp%grp_name, infty_grp%ngrp_sf)
!
      call alloc_scalar_surf_BC(infty_grp)
!
      call set_bc_infty_id(iflag_surf_infty, surf_grp%num_grp,          &
     &    surf_grp%grp_name, infty_grp%ngrp_sf, infty_grp%igrp_sf)
!
      end subroutine const_bc_infinity_surf_grp
!
!-----------------------------------------------------------------------
!
      subroutine empty_infty_surf_type(group)
!
      use t_mesh_data
      use const_bc_infinity_surf
!
      type(mesh_groups), intent(inout) :: group
!
!
      group%infty_grp%ngrp_sf = 0
      call alloc_scalar_surf_BC(group%infty_grp)
!
      end subroutine empty_infty_surf_type
!
!-----------------------------------------------------------------------
!
      end module const_bc_infty_surf_type
