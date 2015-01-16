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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine induction_SPH_initialize
!
      use interpolate_by_type
      use m_addresses_trans_hbd_MHD
!
      iphys_sph%i_magne =      1
      iphys_sph%i_current =    4
      iphys_sph%i_b_diffuse =  7
      iphys_sph%i_vp_induct = 10
      iphys_sph%i_induction = 13
      sph_fld%num_phys =   5
      sph_fld%ntot_phys = 15
!
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        iphys_sph%i_SGS_vp_induct = 16
        iphys_sph%i_SGS_induction = 19
        sph_fld%num_phys =   7
        sph_fld%ntot_phys = 21
      end if
!
      call init_pole_transform
!
      call set_addresses_trans_hbd_MHD
      call allocate_hbd_trans_rtp
      call check_add_trans_hbd_MHD
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
      if(id_legendre_transfer.eq.iflag_leg_undefined)                   &
     &            id_legendre_transfer = iflag_leg_orginal_loop
      call initialize_sph_trans
!
      end subroutine induction_SPH_initialize
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_wSGS_SPH
!
      use m_solver_SR
      use spherical_SRs_N
      use copy_nodal_type_4_sph_trans
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
      call check_calypso_rtp_2_rtm_buf_N(ncomp_xyz_2_rj)
      call check_calypso_rlm_2_rj_buf_N(ncomp_xyz_2_rj)
!
      call copy_xyz_vec_t_to_sph_trans(nvector_xyz_2_rj,                &
     &    iphys_sph%i_vp_induct, mesh_sph%node, sph_fld,                &
     &    frc_hbd_rtp(1,f_hbd_trns%i_vp_induct))
      call copy_xyz_vec_t_to_sph_trans(nvector_xyz_2_rj,                &
     &    iphys_sph%i_SGS_vp_induct, mesh_sph%node, sph_fld,            &
     &    frc_hbd_rtp(1,f_hbd_trns%i_SGS_vp_induct))
!
      call sph_forward_transforms(ncomp_xyz_2_rj, nvector_xyz_2_rj,     &
     &    izero, frc_hbd_rtp(1,1), n_WS, n_WR, WS(1), WR(1))
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &   ipol%i_vp_induct, f_hbd_trns%i_vp_induct, n_WR, WR(1))
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &   ipol%i_SGS_vp_induct, f_hbd_trns%i_SGS_vp_induct, n_WR, WR(1))
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
      use m_solver_SR
      use spherical_SRs_N
      use copy_nodal_type_4_sph_trans
!
!
      call cal_vecp_induction
!
      call interpolate_vector_type                                      &
     &   (iphys%i_vp_induct, iphys_sph%i_vp_induct,                     &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
!
      call copy_xyz_vec_t_to_sph_trans(nvector_xyz_2_rj,                &
     &    iphys_sph%i_vp_induct, mesh_sph%node, sph_fld,                &
     &    frc_hbd_rtp(1,f_hbd_trns%i_vp_induct))
!
      call check_calypso_rtp_2_rtm_buf_N(ncomp_xyz_2_rj)
      call check_calypso_rlm_2_rj_buf_N(ncomp_xyz_2_rj)
!
      call sph_forward_transforms(ncomp_xyz_2_rj, nvector_xyz_2_rj,     &
     &    izero, izero, frc_hbd_rtp(1,1), n_WS, n_WR, WS(1), WR(1))
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &   (ipol%i_vp_induct, f_hbd_trns%i_vp_induct, n_WR, WR)
!
      call const_sph_rotation_uxb(ipol%i_vp_induct, ipol%i_induction)
!
      end subroutine nonlinear_incuction_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine cal_magneitc_field_by_SPH
!
      use m_solver_SR
      use spherical_SRs_N
      use copy_spectr_4_sph_trans
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
      call check_calypso_rj_2_rlm_buf_N(ncomp_rj_2_xyz)
      call check_calypso_rtm_2_rtp_buf_N(ncomp_rj_2_xyz)
!
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_magne, b_hbd_trns%i_magne, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_current, b_hbd_trns%i_current, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_b_diffuse, b_hbd_trns%i_b_diffuse, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_induction, b_hbd_trns%i_induction, n_WS, WS)
      if (iflag_SGS_induction .ne. id_SGS_none) then
        call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                  &
     &      ipol%i_SGS_induction, b_hbd_trns%i_SGS_induction, n_WS, WS)
      end if
!
      call sph_backward_transforms(ncomp_rj_2_xyz, nvector_rj_2_xyz,    &
     &    n_WS, n_WR, WS, WR, fld_hbd_rtp, flc_hbd_pole, fld_hbd_pole)
!
!
      call copy_xyz_vec_t_from_trans_wpole(nvector_rj_2_xyz,            &
     &    b_hbd_trns%i_magne, fld_hbd_rtp(1,1), fld_hbd_pole,           &
     &    iphys_sph%i_magne, mesh_sph%node, sph_fld)
      call copy_xyz_vec_t_from_trans_wpole(nvector_rj_2_xyz,            &
     &    b_hbd_trns%i_current, fld_hbd_rtp(1,1), fld_hbd_pole,         &
     &    iphys_sph%i_current, mesh_sph%node, sph_fld)
      call copy_xyz_vec_t_from_trans_wpole(nvector_rj_2_xyz,            &
     &    b_hbd_trns%i_b_diffuse, fld_hbd_rtp(1,1), fld_hbd_pole,       &
     &     iphys_sph%i_b_diffuse, mesh_sph%node, sph_fld)
      call copy_xyz_vec_t_from_trans_wpole(nvector_rj_2_xyz,            &
     &    b_hbd_trns%i_induction, fld_hbd_rtp(1,1), fld_hbd_pole,       &
     &    iphys_sph%i_induction, mesh_sph%node, sph_fld)
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call copy_xyz_vec_t_from_trans_wpole(nvector_rj_2_xyz,          &
     &    b_hbd_trns%i_SGS_induction, fld_hbd_rtp(1,1), fld_hbd_pole,   &
     &    iphys_sph%i_SGS_induction, mesh_sph%node, sph_fld)
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

