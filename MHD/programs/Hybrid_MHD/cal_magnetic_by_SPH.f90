!
      use t_phys_address
!
      type(mesh_data) :: mesh_fem
      type(surface_geometry) :: surf_mesh_fem
      type(edge_geometry) ::    edge_mesh_fem
      type(phys_data) ::        fem_fld
!
      type(mesh_data) :: mesh_sph
      type(surface_geometry) :: surf_mesh_sph
      type(edge_geometry) ::    edge_mesh_sph
      type(phys_data) :: sph_fld
!
      type(interpolate_table), intent(in) :: itp_FEM_2_SPH
      type(interpolate_table), intent(in) :: itp_SPH_2_FEM
!
!    addresses of nodal field on spherical grid
      type(phys_address), save :: iphys_sph
!
!    addresses of fields for backward transform
      type(phys_address), save :: b_trns
!
!    addresses of forces for forward transform
      type(phys_address), save :: f_trns
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine induction_SPH_initialize
!
      use interpolate_by_type
!
      iphys_sph%i_magne =      1
      iphys_sph%i_current =    4
      iphys_sph%i_b_diffuse =  7
      iphys_sph%i_vp_induct = 10
      iphys_sph%i_induction = 13
      sph_fld%num_phys =   5
      sph_fld%ntot_phys = 15
!
      f_trns%i_vp_induct =  1
      nvect_xyz_2_rj = f_trns%i_vp_induct + 2
!
      b_trns%i_magne =      1
      b_trns%i_current =    4
      b_trns%i_b_diffuse =  7
      b_trns%i_induction = 10
      nvect_rj_2_xyz =  4
!
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        iphys_sph%i_SGS_vp_induct = 16
        iphys_sph%i_SGS_induction = 19
        sph_fld%num_phys =   7
        sph_fld%ntot_phys = 21
!
        f_trns%i_SGS_vp_induct = 4
        nvect_xyz_2_rj = f_trns%i_SGS_vp_induct + 2
!
        b_trns%i_SGS_induction = 13
        nvect_rj_2_xyz = 5
      end if
      ncomp_xyz_2_rj = nvect_xyz_2_rj
      ncomp_rj_2_xyz = 3*nvect_rj_2_xyz
!
      call s_const_linear_mesh_type(mesh_fem, surf_mesh_fem,            &
     &    edge_mesh_fem, fem_fld)
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_mesh'
      call sel_read_mesh(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'set_mesh_data_types'
      call set_mesh_data_types(mesh_sph)
      call s_const_mesh_types_info(mesh_sph,                            &
     &    surf_mesh_sph, edge_mesh_sph)
!
      call alloc_phys_name_type(sph_fld)
      call alloc_phys_data_type(mesh_sph%node%numnod, sph_fld)
!
!     ---------------------
!
      ifmt_itp_table_file = ifile_type
      table_file_header = FEM_2_SPH_tbl_head
      call sel_read_interpolate_table(my_rank, ierr)
      call copy_interpolate_types_from_IO(my_rank, itp_FEM_2_SPH)
!
      table_file_header = SPH_2_FEM_tbl_head
      call sel_read_interpolate_table(my_rank, ierr)
      call copy_interpolate_types_from_IO(my_rank, itp_SPH_2_FEM)
!
      call init_interpolate_mat_type(mesh_fem%ele, itp_FEM_2_SPH)
      call init_interpolate_mat_type(mesh_sph%ele, itp_SPH_2_FEM)
!
      call initialize_sph_trans
      call init_pole_transform
!
      end subroutine induction_SPH_initialize
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_wSGS_SPH
!
!
      call cal_vecp_induction
      call cal_sgs_uxb_2_monitor
!
      call interpolate_vector_type                                      &
     &   (iphys%i_vp_induct,  iphys_sph%i_vp_induct,                    &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
      call interpolate_vector_type                                      &
     &   (iphys%i_SGS_vp_induct, iphys_sph%i_SGS_vp_induct,             &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
!
!
      call copy_xyz_vec_t_to_sph_trans(nvect_xyz_2_rj,                  &
     &    f_trns%i_vp_induct, iphys_sph%i_vp_induct,                    &
     &    mesh_sph%node, sph_fld)
      call copy_xyz_vec_t_to_sph_trans(nvect_xyz_2_rj,                  &
     &    f_trns%i_SGS_vp_induct, iphys_sph%i_SGS_vp_induct,            &
     &    mesh_sph%node, sph_fld)
!
!
      call sph_forward_transforms(ncomp_xyz_2_rj, nvect_xyz_2_rj,       &
     &    izero, izero)
!
!
      call copy_vec_spec_from_trans                                     &
     &   (ipol%i_vp_induct, f_trns%i_vp_induct)
      call copy_vec_spec_from_trans                                     &
     &   (ipol%i_SGS_vp_induct, f_trns%i_SGS_vp_induct)
!
!
      call const_sph_rotation_uxb(ipol%i_vp_induct, ipol%i_induction)
      call const_sph_rotation_uxb(ipol%i_SGS_vp_induct,                 &
     &    ipol%i_SGS_induction)
!*
      end subroutine nonlinear_incuction_wSGS_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_SPH
!
      call cal_vecp_induction
!
      call interpolate_vector_type                                      &
     &   (iphys%i_vp_induct, iphys_sph%i_vp_induct,                     &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
!
      call copy_xyz_vec_t_to_sph_trans(nvect_xyz_2_rj,                  &
     &    f_trns%i_vp_induct, iphys_sph%i_vp_induct,                    &
     &    mesh_sph%node, sph_fld)
!
!
      call sph_forward_transforms(ncomp_xyz_2_rj, nvect_xyz_2_rj,       &
     &    izero, izero)
!
!
      call copy_vec_spec_from_trans                                     &
     &   (ipol%i_vp_induct, f_trns%i_vp_induct)
!
      call const_sph_rotation_uxb(ipol%i_vp_induct, ipol%i_induction)
!
      end subroutine nonlinear_incuction_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine cal_magneitc_field_by_SPH
!
!
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call cal_diff_induction_MHD_adams
      else
        call cal_diff_induction_wSGS_adams
      end if
!
      call cal_sol_magne_sph_crank
      call update_after_magne_sph
!
!
      call copy_vec_spec_to_trans(ncomp_rj_2_xyz,                       &
     &    ipol%i_magne, b_trns%i_magne)
      call copy_vec_spec_to_trans(ncomp_rj_2_xyz,                       &
     &    ipol%i_current, b_trns%i_current)
      call copy_vec_spec_to_trans(ncomp_rj_2_xyz,                       &
     &    ipol%i_b_diffuse, b_trns%i_b_diffuse)
      call copy_vec_spec_to_trans(ncomp_rj_2_xyz,                       &
     &    ipol%i_induction, b_trns%i_induction)
      if (iflag_SGS_induction .ne. id_SGS_none) then
        call copy_vec_spec_to_trans(ncomp_rj_2_xyz,                     &
     &      ipol%i_SGS_induction, b_trns%i_SGS_induction)
      end if
!
!
      call sph_backward_transforms(ncomp_rj_2_xyz, nvect_rj_2_xyz,      &
     &    izero, izero)
      call pole_backward_transforms(ncomp_rj_2_xyz, nvect_rj_2_xyz,     &
     &    izero, izero)
!
!
      call copy_xyz_vec_t_from_trans_wpole(nvect_rj_2_xyz,              &
     &    b_trns%i_magne, iphys_sph%i_magne,                            &
     &    mesh_sph%node, sph_fld)
      call copy_xyz_vec_t_from_trans_wpole(nvect_rj_2_xyz,              &
     &    b_trns%i_current, iphys_sph%i_current,                        &
     &    mesh_sph%node, sph_fld)
      call copy_xyz_vec_t_from_trans_wpole(nvect_rj_2_xyz,              &
     &    b_trns%i_b_diffuse, iphys_sph%i_b_diffuse,                    &
     &    mesh_sph%node, sph_fld)
      call copy_xyz_vec_t_from_trans_wpole(nvect_rj_2_xyz,              &
     &    b_trns%i_induction, iphys_sph%i_induction,                    &
     &    mesh_sph%node, sph_fld)
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call copy_xyz_vec_t_from_trans_wpole(nvect_rj_2_xyz,            &
     &    b_trns%i_SGS_induction, iphys_sph%i_SGS_induction,            &
     &    mesh_sph%node, sph_fld)
      end if
!
!
      call interpolate_vector_type                                      &
     &   (iphys_sph%i_magne, iphys%i_magne,                             &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld)
      call interpolate_vector_type                                      &
     &   (iphys_sph%i_current, iphys%i_current,                         &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld)
      call interpolate_vector_type                                      &
     &   (iphys_sph%i_b_diffuse, iphys%i_b_diffuse,                     &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld)
      call interpolate_vector_type                                      &
     &   (iphys_sph%i_induction, iphys%i_induction,                     &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld)
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call interpolate_vector_type                                    &
     &   (iphys_sph%i_SGS_induction, iphys%i_SGS_induction,             &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld)
      end if
!
      end subroutine cal_magneitc_field_by_SPH

