!set_aiccg_free_sph.f90
!      module set_aiccg_free_sph
!
!      stress free boundary in a spherical shell
!     Written by H. Matsui on Sep. 2005
!
!!      subroutine set_aiccg_bc_free_sph_in                             &
!!     &         (ele, surf, sf_grp, jac_sf_grp, rhs_tbl, mat_tbl,      &
!!     &          num_int, fem_wk, mat33)
!!      subroutine set_aiccg_bc_free_sph_out                            &
!!     &         (ele, surf, sf_grp, jac_sf_grp, rhs_tbl, mat_tbl,      &
!!     &          num_int, fem_wk, mat33)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: mat_tbl
!!  
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX), intent(inout) :: mat33
!
      module set_aiccg_free_sph
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_solver_djds
      use m_ele_material_property
      use m_surf_data_torque
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_free_sph_in                               &
     &         (ele, surf, sf_grp, jac_sf_grp, rhs_tbl, mat_tbl,        &
     &          num_int, fem_wk, mat33)
!
      use m_int_surface_data
!
      use fem_surf_crank_free_sph
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      integer (kind = kint), intent(in) :: num_int
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer (kind = kint) :: i, igrp, k2, num
!
!
      do k2 = 1, surf%nnod_4_surf
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
        do i = 1, sf_bc1_free_sph_in%ngrp_sf_dat
          igrp = sf_bc1_free_sph_in%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
          if (num .gt. 0) then
            call fem_surf_crank_free_inside(igrp, k2, num_int,          &
     &          ele%numele, ele%nnod_4_ele,                             &
     &          surf%nnod_4_surf, surf%node_on_sf,                      &
     &          sf_grp%num_item, sf_grp%num_grp_smp,                    &
     &          sf_grp%istack_grp_smp, sf_grp%item_sf_grp,              &
     &          jac_sf_grp%ntot_int, jac_sf_grp%an_sf,                  &
     &          jac_sf_grp%xj_sf, xe_sf, ak_d_velo, fem_wk%sk6)
!
            call add_skv1_to_crs_matrix33(ele, rhs_tbl, mat_tbl,        &
     &          k2, fem_wk%sk6, mat33%num_non0, mat33%aiccg)
          end if
        end do
      end do
!
      end subroutine set_aiccg_bc_free_sph_in
!
!-----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_free_sph_out                              &
     &         (ele, surf, sf_grp, jac_sf_grp, rhs_tbl, mat_tbl,        &
     &          num_int, fem_wk, mat33)
!
      use m_int_surface_data
!
      use fem_surf_crank_free_sph
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      integer (kind = kint), intent(in) :: num_int
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer (kind = kint) :: i, igrp, k2, num
!
!
      do k2 = 1, surf%nnod_4_surf
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
        do i = 1, sf_bc1_free_sph_out%ngrp_sf_dat
          igrp = sf_bc1_free_sph_out%id_grp_sf_dat(i)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
          if (num .gt. 0) then
            call fem_surf_crank_free_outside(igrp, k2, num_int,         &
     &          ele%numele, ele%nnod_4_ele,                             &
     &          surf%nnod_4_surf, surf%node_on_sf,                      &
     &          sf_grp%num_item, sf_grp%num_grp_smp,                    &
     &          sf_grp%istack_grp_smp, sf_grp%item_sf_grp,              &
     &          jac_sf_grp%ntot_int, jac_sf_grp%an_sf,                  &
     &          jac_sf_grp%xj_sf, xe_sf, ak_d_velo, fem_wk%sk6)
!
            call add_skv1_to_crs_matrix33(ele, rhs_tbl, mat_tbl,        &
     &          k2, fem_wk%sk6, mat33%num_non0, mat33%aiccg)
          end if
        end do
      end do
!
      end subroutine set_aiccg_bc_free_sph_out
!
!-----------------------------------------------------------------------
!
      end module set_aiccg_free_sph
