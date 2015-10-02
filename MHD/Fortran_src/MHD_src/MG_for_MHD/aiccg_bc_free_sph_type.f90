!aiccg_bc_free_sph_type.f90
!      module aiccg_bc_free_sph_type
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        Merged by Kemorin on June. 2005
!        Merged by Kemorin on Oct. 2005
!
!      subroutine s_aiccg_bc_free_sph_type(mesh, surf, group,           &
!     &          torque_bc, jac_sf_grp, ak_AMG, fluid_tbl, fem_wk, mat_v)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_data), intent(in) ::  surf
!        type(mesh_groups), intent(in) :: group
!        type(velocity_surf_bc_type), intent(in) :: torque_bc
!        type(jacobians_2d), intent(in) :: jac_sf_grp
!        type(coefs_4_MHD_AMG), intent(in) ::            ak_AMG
!        type(DJDS_ordering_table), intent(in) :: fluid_tbl
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(DJDS_MATRIX), intent(inout) :: mat_v
!
      module aiccg_bc_free_sph_type
!
      use m_precision
!
      use m_control_parameter
      use t_mesh_data
      use t_surface_group_geometry
      use t_surface_bc_data
      use t_coefs_element_4_MHD
      use t_jacobians
      use set_aiccg_free_sph
!
      use t_finite_element_mat
      use t_solver_djds
!
      implicit none
!
      private :: aiccg_bc_type_free_sph_in, aiccg_bc_type_free_sph_out
      private :: cal_surf_matrix_vect_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_aiccg_bc_free_sph_type(mesh, surf, group, torque_bc, &
     &          jac_sf_grp, ak_AMG, fluid_tbl, fem_wk, mat_v)
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) ::  surf
      type(mesh_groups), intent(in) :: group
      type(velocity_surf_bc_type), intent(in) :: torque_bc
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(coefs_4_MHD_AMG), intent(in) ::    ak_AMG
!
      type(DJDS_ordering_table), intent(in) :: fluid_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat_v
!
      integer(kind = kint) :: n_int
!
!
      n_int = intg_point_poisson
!
      if (torque_bc%free_sph_in%ngrp_sf_dat .gt. 0)  then
        call aiccg_bc_type_free_sph_in(n_int, mesh, surf,               &
     &      group%surf_grp, torque_bc%free_sph_in, jac_sf_grp,          &
     &       group%surf_grp_int, ak_AMG, fluid_tbl, fem_wk, mat_v)
      end if
!
      if (torque_bc%free_sph_out%ngrp_sf_dat .gt. 0) then
        call aiccg_bc_type_free_sph_out(n_int, mesh, surf,              &
     &       group%surf_grp, torque_bc%free_sph_out, jac_sf_grp,        &
     &       group%surf_grp_int, ak_AMG, fluid_tbl, fem_wk, mat_v)
      end if
!
      end subroutine s_aiccg_bc_free_sph_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine aiccg_bc_type_free_sph_in(n_int, mesh, surf, sf_grp,   &
     &          free_sph_in, jac_sf_grp, sf_grp_int, ak_AMG, fluid_tbl, &
     &          fem_wk, mat_v)
!
      use fem_surf_crank_free_sph
      use cal_skv_to_ff_smp_type
!
      integer (kind = kint), intent(in) :: n_int
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
      type(scaler_surf_bc_data_type), intent(in) :: free_sph_in
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(surf_grp_geom_4_fem_int), intent(in) :: sf_grp_int
      type(coefs_4_MHD_AMG), intent(in) ::            ak_AMG
!
      type(DJDS_ordering_table), intent(in) :: fluid_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat_v
!
!
      integer (kind = kint) :: nd, k2
      integer (kind = kint) :: i, igrp, nsf
!
!
      do k2 = 1, surf%nnod_4_surf
        call reset_sk6_type(n_scalar, mesh%ele, fem_wk)
!
        do i = 1, free_sph_in%ngrp_sf_dat
         igrp = free_sph_in%id_grp_sf_dat(i)
!
         nsf = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
         if (nsf.gt.0) then
           call fem_surf_crank_free_inside(igrp, k2, n_int,             &
     &          mesh%ele%numele, mesh%ele%nnod_4_ele, surf%nnod_4_surf, &
     &          surf%node_on_sf, sf_grp%num_item, sf_grp%num_grp_smp,   &
     &          sf_grp%istack_grp_smp, sf_grp%item_sf_grp,              &
     &          jac_sf_grp%ntot_int, jac_sf_grp%an_sf,                  &
     &          jac_sf_grp%xj_sf, sf_grp_int%xe_sf,                     &
     &          ak_AMG%ak_d_velo, fem_wk%sk6)
!
          do nd = 1, 3
            call cal_surf_matrix_vect_type(k2, nd,                      &
     &          free_sph_in%ngrp_sf_dat, free_sph_in%id_grp_sf_dat,     &
     &          mesh, surf, sf_grp, fluid_tbl, fem_wk, mat_v)
          end do
!
         end if
        end do
      end do
!
      end subroutine aiccg_bc_type_free_sph_in
!
!-----------------------------------------------------------------------
!
      subroutine aiccg_bc_type_free_sph_out(n_int, mesh, surf, sf_grp,  &
     &         free_sph_out, jac_sf_grp, sf_grp_int, ak_AMG, fluid_tbl, &
     &         fem_wk, mat_v)
!
      use fem_surf_crank_free_sph
      use cal_skv_to_ff_smp_type
!
      integer (kind = kint), intent(in) :: n_int
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
      type(scaler_surf_bc_data_type), intent(in) :: free_sph_out
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(surf_grp_geom_4_fem_int), intent(in) :: sf_grp_int
      type(coefs_4_MHD_AMG), intent(in) ::            ak_AMG
!
      type(DJDS_ordering_table), intent(in) :: fluid_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat_v
!
      integer (kind = kint) :: nd, k2
      integer (kind = kint) :: i, igrp, nsf
!
!
      do k2 = 1, surf%nnod_4_surf
        call reset_sk6_type(n_scalar, mesh%ele, fem_wk)
!
        do i = 1, free_sph_out%ngrp_sf_dat
          igrp = free_sph_out%id_grp_sf_dat(i)
!
          nsf = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
          if (nsf.gt.0) then
            call fem_surf_crank_free_outside(igrp, k2, n_int,           &
     &          mesh%ele%numele, mesh%ele%nnod_4_ele, surf%nnod_4_surf, &
     &          surf%node_on_sf, sf_grp%num_item, sf_grp%num_grp_smp,   &
     &          sf_grp%istack_grp_smp, sf_grp%item_sf_grp,              &
     &          jac_sf_grp%ntot_int, jac_sf_grp%an_sf,                  &
     &          jac_sf_grp%xj_sf, sf_grp_int%xe_sf,                     &
     &          ak_AMG%ak_d_velo, fem_wk%sk6)
!
          do nd = 1, 3
            call cal_surf_matrix_vect_type(k2, nd,                      &
     &          free_sph_out%ngrp_sf_dat, free_sph_out%id_grp_sf_dat,   &
     &          mesh, surf, sf_grp, fluid_tbl, fem_wk, mat_v)
          end do
!
         end if
        end do
       end do
!
      end subroutine aiccg_bc_type_free_sph_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_surf_matrix_vect_type(k2, nd, ngrp_sf, id_grp,     &
     &          mesh, surf, sf_grp, djds_tbl, fem_wk, mat_v)
!
      use set_idx_4_mat_type
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: ngrp_sf
      integer(kind = kint), intent(in) :: id_grp(ngrp_sf)
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(work_finite_element_mat), intent(in) ::  fem_wk
!
      type(DJDS_MATRIX), intent(inout) :: mat_v
!
      integer (kind = kint) :: iele, inum, k1
      integer (kind = kint) :: i, igrp, ist, ied
      integer (kind = kint) :: mat_num, isf
      integer (kind = kint) :: kk1, kk2, nod1, nod2
!
!
      do i = 1, ngrp_sf
        igrp = id_grp(i)
        ist = sf_grp%istack_grp(igrp-1)+1
        ied = sf_grp%istack_grp(igrp)
        do inum = ist, ied
          iele = sf_grp%item_sf_grp(1,inum)
          isf =  sf_grp%item_sf_grp(2,inum)
          kk2 =  surf%node_on_sf(k2,isf)
!
          do k1 = 1, surf%nnod_4_surf
            kk1 = surf%node_on_sf(k1,isf)
!
            nod1 = mesh%ele%ie(iele,kk1)
            nod2 = mesh%ele%ie(iele,kk2)
!
!   ------ coefficients for Crank-Nicolson Scheme
!
            call set_DJDS_off_diag_type                                 &
     &         (mesh%node%numnod, mesh%node%internal_node,              &
     &          djds_tbl, nod1, nod2, mat_num)
!
            mat_v%aiccg(mat_num*9+nd*4-12)                              &
     &        = mat_v%aiccg(mat_num*9+nd*4-12) + fem_wk%sk6(iele,1,kk1)
!
          end do
!
        end do
      end do
!
      end subroutine cal_surf_matrix_vect_type
!
!-----------------------------------------------------------------------
!
      end module aiccg_bc_free_sph_type
