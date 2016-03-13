!
!      module set_aiccg_bc_phys_type
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        Merged by Kemorin on June. 2005
!        Merged by Kemorin on Oct. 2005
!
!!      subroutine s_set_aiccg_bc_phys_type(ele, surf, sf_grp,          &
!!     &          jac_sf_grp, rhs_tbl, mat_tbl_fl, nodal_bc, surface_bc,&
!!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,     &
!!     &          fem_wk, mat_velo, mat_magne, mat_temp, mat_d_scalar,  &
!!     &          mat_press, mat_magp)
!!        type(mesh_data), intent(in) ::      femmesh
!!        type(surface_data), intent(in) ::   surf_mesh
!!        type(nodal_boundarty_conditions), intent(in) ::   nodal_bc
!!        type(surface_boundarty_conditions), intent(in) :: surface_bc
!!        type(jacobians_2d), intent(in) ::          jac_sf_grp
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
      module set_aiccg_bc_phys_type
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_constants
      use t_mesh_data
      use t_surface_group_geometry
      use t_nodal_bc_data
      use t_surface_bc_data
      use t_coefs_element_4_MHD
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_solver_djds
      use t_bc_data_MHD
      use t_MHD_boundary_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_aiccg_bc_phys_type(ele, surf, sf_grp,            &
     &          jac_sf_grp, rhs_tbl, mat_tbl_fl, nodal_bc, surface_bc,  &
     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,       &
     &          fem_wk, mat_velo, mat_magne, mat_temp, mat_d_scalar,    &
     &          mat_press, mat_magp)
!
      use set_aiccg_bc_fixed
      use set_aiccg_bc_vectors
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) ::   surf
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_boundarty_conditions), intent(in) ::   nodal_bc
      type(jacobians_2d), intent(in) ::         jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl_fl
      type(surface_boundarty_conditions), intent(in) :: surface_bc
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
!   set boundary conditions for matrix
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod(num_t_linear, ele,                 &
     &      nodal_bc%Vnod_bcs%nod_bc_p, djds_tbl_fl_l, mat_press)
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call set_aiccg_bc_velo(intg_point_t_evo, ele, surf, sf_grp,     &
     &      nodal_bc%Vnod_bcs%nod_bc_v, nodal_bc%Vnod_bcs%nod_bc_rot,   &
     &      surface_bc%Vsf_bcs%free_sph_in,                             &
     &      surface_bc%Vsf_bcs%free_sph_out,                            &
     &      jac_sf_grp, rhs_tbl, mat_tbl_fl, djds_tbl_fl,               &
     &      fem_wk, mat_velo)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod(ele%nnod_4_ele,                    &
     &      ele, nodal_bc%Tnod_bcs%nod_bc_s,  djds_tbl_fl, mat_temp)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod(ele%nnod_4_ele,                    &
     &      ele, nodal_bc%Cnod_bcs%nod_bc_s,                            &
     &      djds_tbl_fl, mat_d_scalar)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &     .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod(num_t_linear, ele,                 &
     &      nodal_bc%Bnod_bcs%nod_bc_f, djds_tbl_l, mat_magp)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        call set_aiccg_bc_vector_nod                                    &
     &     (ele, nodal_bc%Bnod_bcs%nod_bc_b, djds_tbl, mat_magne)
      end if
!
      if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        call set_aiccg_bc_vector_nod                                    &
     &     (ele, nodal_bc%Bnod_bcs%nod_bc_a, djds_tbl, mat_magne)
      end if
!
!
      end subroutine s_set_aiccg_bc_phys_type
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_phys_type
