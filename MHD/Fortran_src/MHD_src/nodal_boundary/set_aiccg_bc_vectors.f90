!set_aiccg_bc_vectors.f90
!      module set_aiccg_bc_vectors
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on Nov. 2003
!        modified by H. Matsui on Oct. 2005
!        modified by H. Matsui on Feb. 2009
!
!!      subroutine set_aiccg_bc_phys(num_int,                           &
!!     &          evo_T, evo_C, ele, surf, sf_grp, fl_prop, cd_prop,    &
!!     &          jac_sf_grp, rhs_tbl, MG_mat_fl_q, node_bcs, surf_bcs, &
!!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,     &
!!     &          surf_wk, fem_wk, mat_velo, mat_magne,                 &
!!     &          mat_temp, mat_light, mat_press, mat_magp)
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_boundarty_conditions), intent(in) ::   node_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(jacobians_2d), intent(in) ::          jac_sf_grp
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
      module set_aiccg_bc_vectors
!
      use m_precision
!
      use t_time_stepping_parameter
      use t_physical_property
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_nodal_bc_data
      use t_finite_element_mat
      use t_int_surface_data
      use t_table_FEM_const
      use t_solver_djds
      use t_bc_data_MHD
      use t_MHD_boundary_data
!
      implicit none
!
      private :: set_aiccg_bc_velo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_phys(num_int,                             &
     &          evo_T, evo_C, ele, surf, sf_grp, fl_prop, cd_prop,      &
     &          jac_sf_grp, rhs_tbl, MG_mat_fl_q, node_bcs, surf_bcs,   &
     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,       &
     &          ak_d_velo, surf_wk, fem_wk, mat_velo, mat_magne,        &
     &          mat_temp, mat_light, mat_press, mat_magp)
!
      use set_aiccg_bc_fixed
!
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_boundarty_conditions), intent(in) ::   node_bcs
      type(jacobians_2d), intent(in) ::         jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_fl_q
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(work_surface_element_mat), intent(in) :: surf_wk
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!
      integer(kind = kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
!   set boundary conditions for matrix
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod(num_t_linear, ele,                 &
     &      node_bcs%Vnod_bcs%nod_bc_p, djds_tbl_fl_l, mat_press)
!
        if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
          call set_aiccg_bc_velo(num_int, ele, surf, sf_grp, fl_prop,   &
     &      node_bcs%Vnod_bcs%nod_bc_v, node_bcs%Vnod_bcs%nod_bc_rot,   &
     &      surf_bcs%Vsf_bcs%free_sph_in,                               &
     &      surf_bcs%Vsf_bcs%free_sph_out,                              &
     &      jac_sf_grp, rhs_tbl, MG_mat_fl_q, djds_tbl_fl, ak_d_velo,   &
     &      surf_wk, fem_wk, mat_velo)
        end if
      end if
!
      if (evo_T%iflag_scheme .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod(ele%nnod_4_ele,                    &
     &      ele, node_bcs%Tnod_bcs%nod_bc_s,  djds_tbl_fl, mat_temp)
      end if
!
      if (evo_C%iflag_scheme .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod(ele%nnod_4_ele,                    &
     &      ele, node_bcs%Cnod_bcs%nod_bc_s,                            &
     &      djds_tbl_fl, mat_light)
      end if
!
      if     (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution            &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod(num_t_linear, ele,                 &
     &      node_bcs%Bnod_bcs%nod_bc_f, djds_tbl_l, mat_magp)
!
        if (cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson) then
          call set_aiccg_bc_vector_nod                                  &
     &       (ele, node_bcs%Bnod_bcs%nod_bc_b, djds_tbl, mat_magne)
        end if
!
        if (cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
          call set_aiccg_bc_vector_nod                                  &
     &       (ele, node_bcs%Bnod_bcs%nod_bc_a, djds_tbl, mat_magne)
        end if
      end if
!
!
      end subroutine set_aiccg_bc_phys
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo                                      &
     &         (num_int, ele, surf, sf_grp, fl_prop,                    &
     &          nod_bc_v, nod_bc_rot, free_in_sf, free_out_sf,          &
     &          jac_sf_grp, rhs_tbl, MG_mat_fl_q, DJDS_tbl, ak_d_velo,  &
     &          surf_wk, fem_wk, Vmat_DJDS)
!
      use set_aiccg_free_sph
      use set_aiccg_bc_fixed
!
      type(fluid_property), intent(in) :: fl_prop
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_v
      type(scaler_rotaion_nod_bc_type), intent(in) :: nod_bc_rot
      type(scaler_surf_bc_data_type), intent(in) :: free_in_sf
      type(scaler_surf_bc_data_type), intent(in) :: free_out_sf
!
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_fl_q
      type(DJDS_ordering_table), intent(in) :: DJDS_tbl
      type(work_surface_element_mat), intent(in) :: surf_wk
!
      integer(kind = kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: Vmat_DJDS
!
!
!      matrix setting for free slip on sphere
      call set_aiccg_bc_free_sph_in(ele, surf, sf_grp,                  &
     &    free_in_sf, jac_sf_grp, rhs_tbl, MG_mat_fl_q, surf_wk,        &
     &    fl_prop%coef_imp, num_int, ak_d_velo, fem_wk, Vmat_DJDS)
      call set_aiccg_bc_free_sph_out(ele, surf, sf_grp,                 &
     &    free_out_sf, jac_sf_grp, rhs_tbl, MG_mat_fl_q, surf_wk,       &
     &    fl_prop%coef_imp, num_int, ak_d_velo, fem_wk, Vmat_DJDS)
!
!      matrix setting for fixed boundaries
      call set_aiccg_bc_vector_nod(ele, nod_bc_v, DJDS_tbl, Vmat_DJDS)
!
!        write(*,*) '  velo_bc_rotation'
      if ( nod_bc_rot%num_idx_ibc .ne. 0 ) then
        call set_aiccg_bc_velo_rot                                      &
     &     (ele, nod_bc_rot, DJDS_tbl, Vmat_DJDS)
      end if
!
      end subroutine set_aiccg_bc_velo
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_vectors
