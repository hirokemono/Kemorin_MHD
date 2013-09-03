!
!      module set_aiccg_bc_phys_type
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        Merged by Kemorin on June. 2005
!        Merged by Kemorin on Oct. 2005
!
!      subroutine s_set_aiccg_bc_phys_type(femmesh, surf_mesh,          &
!     &          nodal_bc, surface_bc, jac_sf_grp, ak_AMG,              &
!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,      &
!     &          fem_wk, mat_velo, mat_magne, mat_temp, mat_d_scalar,   &
!     &          mat_press, mat_magp)
!        type(mesh_data), intent(in) ::      femmesh
!        type(surface_data), intent(in) ::   surf_mesh
!        type(nodal_boundarty_conditions), intent(in) ::   nodal_bc
!        type(surface_boundarty_conditions), intent(in) :: surface_bc
!        type(jacobians_surf_grp), intent(in) ::          jac_sf_grp
!        type(coefs_4_MHD_AMG), intent(in) ::          ak_AMG
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
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
      use t_finite_element_mat
      use t_solver_djds
      use set_aiccg_bc_node_type
!
      implicit none
!
      private :: set_aiccg_bc_velo_type
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_aiccg_bc_phys_type(femmesh, surf_mesh,           &
     &          nodal_bc, surface_bc, jac_sf_grp, ak_AMG,               &
     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,       &
     &          fem_wk, mat_velo, mat_magne, mat_temp, mat_d_scalar,    &
     &          mat_press, mat_magp)
!
      type(mesh_data), intent(in) ::      femmesh
      type(surface_data), intent(in) ::   surf_mesh
      type(nodal_boundarty_conditions), intent(in) ::   nodal_bc
      type(surface_boundarty_conditions), intent(in) :: surface_bc
      type(jacobians_surf_grp), intent(in) ::          jac_sf_grp
      type(coefs_4_MHD_AMG), intent(in) ::      ak_AMG
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
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
        call set_aiccg_bc_scalar_nod_type(num_t_linear, femmesh%mesh,   &
     &      nodal_bc%press, djds_tbl_fl_l, mat_press)
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call set_aiccg_bc_velo_type(femmesh%mesh, surf_mesh,            &
     &      femmesh%group, nodal_bc%velocity, nodal_bc%rotation,        &
     &      surface_bc%velo, jac_sf_grp, ak_AMG, djds_tbl_fl,           &
     &      fem_wk, mat_velo)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod_type(femmesh%mesh%ele%nnod_4_ele,  &
     &      femmesh%mesh, nodal_bc%temp,  djds_tbl_fl, mat_temp)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod_type(femmesh%mesh%ele%nnod_4_ele,  &
     &      femmesh%mesh, nodal_bc%composition,                         &
     &      djds_tbl_fl, mat_d_scalar)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &     .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod_type(num_t_linear, femmesh%mesh,   &
     &      nodal_bc%magne_p, djds_tbl_l, mat_magp)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
       call set_aiccg_bc_vector_nod_type(femmesh%mesh, nodal_bc%magne,  &
     &     djds_tbl, mat_magne)
      end if
!
      if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
       call set_aiccg_bc_vector_nod_type(femmesh%mesh,                  &
     &     nodal_bc%vector_p, djds_tbl, mat_magne)
      end if
!
!
      end subroutine s_set_aiccg_bc_phys_type
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo_type(mesh, surf_mesh, group,         &
     &          velo_bc, rot_bc, torque_bc, jac_sf_grp,                 &
     &           ak_AMG, djds_tbl_fl, fem_wk, mat_v)
!
      use aiccg_bc_free_sph_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) ::  surf_mesh
      type(mesh_groups), intent(in) ::   group
      type(vect_fixed_nod_bc_type), intent(in) :: velo_bc
      type(scaler_rotaion_nod_bc_type), intent(in) :: rot_bc
      type(velocity_surf_bc_type), intent(in) ::     torque_bc
      type(jacobians_surf_grp), intent(in) ::       jac_sf_grp
      type(coefs_4_MHD_AMG), intent(in) ::          ak_AMG
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl_fl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat_v
!
!
      call s_aiccg_bc_free_sph_type(mesh, surf_mesh, group, torque_bc,  &
     &    jac_sf_grp, ak_AMG, djds_tbl_fl, fem_wk, mat_v)
!
!      matrix setting for fixed boundaries
!
       call set_aiccg_bc_vector_nod_type(mesh, velo_bc,                 &
     &    djds_tbl_fl, mat_v)
!
!
!        write(*,*) '  velo_bc_rotation'
      if ( rot_bc%num_idx_ibc .ne. 0 ) then
        call set_aiccg_bc_vector_rot_type(mesh, rot_bc,                 &
     &     djds_tbl_fl, mat_v)
      end if
!
      end subroutine set_aiccg_bc_velo_type
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_phys_type
