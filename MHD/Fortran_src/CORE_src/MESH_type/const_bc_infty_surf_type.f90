!const_bc_infty_surf_type.f90
!     module const_bc_infty_surf_type
!
!     written by H. Matsui on Dec., 2008
!
!
!      subroutine s_const_bc_infty_surf_type(group)
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
      subroutine s_const_bc_infty_surf_type(group)
!
      use t_mesh_data
      use const_bc_infinity_surf
!
      type(mesh_groups), intent(inout) :: group
!
!
      call count_num_bc_infinity(group%surf_grp%num_grp,                &
     &    group%surf_grp%grp_name, group%infty_grp%ngrp_sf)
!
      call alloc_scalar_surf_BC(group%infty_grp)
!
      call set_bc_infty_id(group%surf_grp%num_grp,                      &
     &    group%surf_grp%grp_name, group%infty_grp%ngrp_sf,             &
     &    group%infty_grp%igrp_sf)
!
      end subroutine s_const_bc_infty_surf_type
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
