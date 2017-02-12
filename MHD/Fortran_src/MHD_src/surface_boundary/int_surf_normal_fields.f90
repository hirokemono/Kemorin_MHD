!
!      module int_surf_normal_fields
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine int_surf_normal_vector                               &
!!     &         (i_field, num_int, sf_bc_wall, sf_bc_spin, sf_bc_spout,&
!!     &          node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l,       &
!!     &          rhs_tbl, fem_wk, surf_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(scaler_surf_bc_data_type), intent(in) :: sf_bc_wall
!!        type(scaler_surf_bc_data_type), intent(in) :: sf_bc_spin
!!        type(scaler_surf_bc_data_type), intent(in) :: sf_bc_spout
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_surf_normal_fields
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_surface_bc_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_vector                                 &
     &         (i_field, num_int, sf_bc_wall, sf_bc_spin, sf_bc_spout,  &
     &          node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l,         &
     &          rhs_tbl, fem_wk, surf_wk, f_l)
!
      use int_surf_poisson_walls
!
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(scaler_surf_bc_data_type), intent(in) :: sf_bc_wall
      type(scaler_surf_bc_data_type), intent(in) :: sf_bc_spin
      type(scaler_surf_bc_data_type), intent(in) :: sf_bc_spout
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (sf_bc_wall%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_wall(node, ele, surf, sf_grp,             &
     &      nod_fld, jac_sf_grp_l, rhs_tbl, num_int,                    &
     &      sf_bc_wall%ngrp_sf_dat, sf_bc_wall%id_grp_sf_dat,           &
     &      i_field, fem_wk, surf_wk, f_l)
      end if
!
      if (sf_bc_spin%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_in(node, ele, surf, sf_grp,           &
     &      nod_fld, jac_sf_grp_l, rhs_tbl, num_int,                    &
     &      sf_bc_spin%ngrp_sf_dat, sf_bc_spin%id_grp_sf_dat,           &
     &      i_field, fem_wk, surf_wk, f_l)
      end if
!
      if (sf_bc_spout%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_out(node, ele, surf, sf_grp,          &
     &      nod_fld, jac_sf_grp_l, rhs_tbl, num_int,                    &
     &      sf_bc_spout%ngrp_sf_dat, sf_bc_spout%id_grp_sf_dat,         &
     &      i_field, fem_wk, surf_wk, f_l)
      end if
!
      end subroutine int_surf_normal_vector
!
!-----------------------------------------------------------------------
!
      end module int_surf_normal_fields
