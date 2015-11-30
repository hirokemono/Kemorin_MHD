!
!      module int_surf_normal_fields
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine int_surf_normal_velocity(node, ele, surf, sf_grp,    &
!!     &          nod_fld, jac_sf_grp_l, rhs_tbl, fem_wk, f_l)
!!      subroutine int_surf_normal_vector_p (node, ele, surf, sf_grp,   &
!!     &        nod_fld, jac_sf_grp_l, rhs_tbl, fem_wk, f_l)
!!      subroutine int_surf_normal_magne(node, ele, surf, sf_grp,       &
!!     &          nod_fld, jac_sf_grp_l, rhs_tbl, fem_wk, f_l)
!
      module int_surf_normal_fields
!
      use m_precision
!
      use m_control_parameter
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
!
      use int_surf_poisson_walls
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_velocity(node, ele, surf, sf_grp,      &
     &          nod_fld, jac_sf_grp_l, rhs_tbl, fem_wk, f_l)
!
      use m_node_phys_address
      use m_surf_data_press
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (sf_bc1_wall_p%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_wall(node, ele, surf, sf_grp,             &
     &      nod_fld, jac_sf_grp_l, rhs_tbl, intg_point_poisson,         &
     &      sf_bc1_wall_p%ngrp_sf_dat, sf_bc1_wall_p%id_grp_sf_dat,     &
     &      iphys%i_velo, fem_wk, f_l)
      end if
!
      if (sf_bc1_spin_p%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_in(node, ele, surf, sf_grp,           &
     &      nod_fld, jac_sf_grp_l, rhs_tbl, intg_point_poisson,         &
     &      sf_bc1_spin_p%ngrp_sf_dat, sf_bc1_spin_p%id_grp_sf_dat,     &
     &      iphys%i_velo, fem_wk, f_l)
      end if
!
      if (sf_bc1_spout_p%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_out(node, ele, surf, sf_grp,          &
     &      nod_fld, jac_sf_grp_l, rhs_tbl, intg_point_poisson,         &
     &      sf_bc1_spout_p%ngrp_sf_dat, sf_bc1_spout_p%id_grp_sf_dat,   &
     &      iphys%i_velo, fem_wk, f_l)
      end if
!
      end subroutine int_surf_normal_velocity
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_vector_p (node, ele, surf, sf_grp,     &
     &        nod_fld, jac_sf_grp_l, rhs_tbl, fem_wk, f_l)
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if ( sf_bc1_wall_f%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_wall                                      &
     &     (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,    &
     &      intg_point_poisson,                                         &
     &      sf_bc1_wall_f%ngrp_sf_dat, sf_bc1_wall_f%id_grp_sf_dat,     &
     &      iphys%i_vecp, fem_wk, f_l)
      end if
!
      if ( sf_bc1_spin_f%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_in                                    &
     &     (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,    &
     &      intg_point_poisson,                                         &
     &      sf_bc1_spin_f%ngrp_sf_dat, sf_bc1_spin_f%id_grp_sf_dat,     &
     &      iphys%i_vecp, fem_wk, f_l)
      end if
!
      if ( sf_bc1_spout_f%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_out                                   &
     &     (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,    &
     &      intg_point_poisson,                                         &
     &      sf_bc1_spout_f%ngrp_sf_dat, sf_bc1_spout_f%id_grp_sf_dat,   &
     &      iphys%i_vecp, fem_wk, f_l)
      end if
!
      end subroutine int_surf_normal_vector_p
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_normal_magne(node, ele, surf, sf_grp,         &
     &          nod_fld, jac_sf_grp_l, rhs_tbl, fem_wk, f_l)
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if ( sf_bc1_wall_f%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_wall                                      &
     &     (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,    &
     &      intg_point_poisson,                                         &
     &      sf_bc1_wall_f%ngrp_sf_dat, sf_bc1_wall_f%id_grp_sf_dat,     &
     &      iphys%i_magne, fem_wk, f_l)
      end if
!
      if ( sf_bc1_spin_f%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_in                                    &
     &     (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,    &
     &      intg_point_poisson,                                         &
     &      sf_bc1_spin_f%ngrp_sf_dat, sf_bc1_spin_f%id_grp_sf_dat,     &
     &      iphys%i_magne, fem_wk, f_l)
      end if
!
      if ( sf_bc1_spout_f%ngrp_sf_dat .gt. 0) then
        call int_surf_poisson_sph_out                                   &
     &     (node, ele, surf, sf_grp, nod_fld, jac_sf_grp_l, rhs_tbl,    &
     &      intg_point_poisson,                                         &
     &      sf_bc1_spout_f%ngrp_sf_dat, sf_bc1_spout_f%id_grp_sf_dat,   &
     &      iphys%i_magne, fem_wk, f_l)
      end if
!
      end subroutine int_surf_normal_magne
!
!-----------------------------------------------------------------------
!
      end module int_surf_normal_fields
