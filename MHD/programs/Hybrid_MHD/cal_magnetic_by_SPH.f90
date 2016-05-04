!
      use t_phys_address
      use t_phys_data
!
      type(mesh_data) :: mesh_fem
      type(phys_data) ::        fem_fld
!
      type(mesh_data) :: mesh_sph
      type(element_geometry) :: ele_mesh_sph
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
      use const_element_comm_tables
      use load_mesh_data
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
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank, mesh_sph%mesh, mesh_sph%group,           &
     &    ele_mesh_sph%surf%nnod_4_surf, ele_mesh_sph%edge%nnod_4_edge)
      call const_mesh_infos                                             &
     &   (my_rank, mesh_sph%mesh, mesh_sph%group, ele_mesh_sph)
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
!     ---------------------
!
      call const_element_comm_tbls(mesh_sph, ele_mesh_sph)
!
      end subroutine induction_SPH_initialize
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_wSGS_SPH(conduct, rj_fld)
!
      use m_solver_SR
!
      use t_geometry_data_MHD
!
      use spherical_SRs_N
      use copy_1st_nodal_4_sph_trans
!
      type(field_geometry_data), intent(in) :: conduct
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_vecp_induction
      call cal_sgs_uxb_2_monitor
     &   (icomp_sgs%i_induction, iphys_elediff%i_velo,                  &
     &    mesh1%nod_comm, mesh1%node, mesh1%ele, conduct, iphys,        &
     &    iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen,          &
     &    filtering1, wk_filter1, mhd_fem1_wk, fem1_wk,                 &
     &    f1_l, f1_nl, nod_fld1)
!
      call interpolate_vector_type                                      &
     &   (iphys%i_vp_induct,  iphys_sph%i_vp_induct,                    &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
      call interpolate_vector_type                                      &
     &   (iphys%i_SGS_vp_induct, iphys_sph%i_SGS_vp_induct,             &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
!
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_xyz_2_rj, comm_rtp1, comm_rtm1)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_xyz_2_rj, comm_rlm1, comm_rj1)
!
      call copy_nod_vec_to_sph_trans(node1, sph_fld,                    &
     &    iphys_sph%i_vp_induct, frc_hbd_rtp(1,f_hbd_trns%i_vp_induct))
      call copy_nod_vec_to_sph_trans(node1, sph_fld,                    &
     &    iphys_sph%i_SGS_vp_induct,                                    &
     &    frc_hbd_rtp(1,f_hbd_trns%i_SGS_vp_induct))
!
      call sph_forward_transforms(ncomp_xyz_2_rj, nvector_xyz_2_rj,     &
     &    izero, frc_hbd_rtp(1,1), n_WS, n_WR, WS(1), WR(1))
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &    ipol%i_vp_induct, f_hbd_trns%i_vp_induct,                     &
     &    n_WR, WR(1), rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &    ipol%i_SGS_vp_induct, f_hbd_trns%i_SGS_vp_induct,             &
     &    n_WR, WR(1), rj_fld)
!
!
      call const_sph_rotation_uxb                                       &
     &   (ipol%i_vp_induct, ipol%i_induction, rj_fld)
      call const_sph_rotation_uxb(ipol%i_SGS_vp_induct,                 &
     &    ipol%i_SGS_induction, rj_fld)
!*
      end subroutine nonlinear_incuction_wSGS_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_SPH(rj_fld)
!
      use m_solver_SR
      use spherical_SRs_N
      use copy_1st_nodal_4_sph_trans
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_vecp_induction
!
      call interpolate_vector_type                                      &
     &   (iphys%i_vp_induct, iphys_sph%i_vp_induct,                     &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
!
      call copy_nod_vec_to_sph_trans(node1, sph_fld,                    &
     &    iphys_sph%i_vp_induct, frc_hbd_rtp(1,f_hbd_trns%i_vp_induct))
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_xyz_2_rj, comm_rtp1, comm_rtm1)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_xyz_2_rj, comm_rlm1, comm_rj1)
!
      call sph_forward_transforms(ncomp_xyz_2_rj, nvector_xyz_2_rj,     &
     &    izero, izero, frc_hbd_rtp(1,1), n_WS, n_WR, WS(1), WR(1))
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &   (ipol%i_vp_induct, f_hbd_trns%i_vp_induct, n_WR, WR, rj_fld)
!
      call const_sph_rotation_uxb                                       &
     &   (ipol%i_vp_induct, ipol%i_induction, rj_fld)
!
      end subroutine nonlinear_incuction_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine cal_magneitc_field_by_SPH(rj_fld)
!
      use m_solver_SR
      use spherical_SRs_N
      use copy_spectr_4_sph_trans
      use copy_1st_nodal_4_sph_trans
!
      type(phys_data), intent(in) :: rj_fld
!
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call cal_diff_induction_MHD_adams(ntot_phys_rj, d_rj)
      else
        call cal_diff_induction_wSGS_adams(ntot_phys_rj, d_rj)
      end if
!
      call cal_sol_magne_sph_crank(sph_rj1, rj_fld)
      call update_after_magne_sph
!
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_rj_2_xyz, comm_rj1, comm_rlm1)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_rj_2_xyz, comm_rtm1, comm_rtp1)
!
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_magne, b_hbd_trns%i_magne, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_current, b_hbd_trns%i_current, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_b_diffuse, b_hbd_trns%i_b_diffuse, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_induction, b_hbd_trns%i_induction, rj_fld, n_WS, WS)
      if (iflag_SGS_induction .ne. id_SGS_none) then
        call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                  &
     &      ipol%i_SGS_induction, b_hbd_trns%i_SGS_induction, rj_fld,   &
     &      n_WS, WS)
      end if
!
      call sph_backward_transforms(ncomp_rj_2_xyz, nvector_rj_2_xyz,    &
     &    n_WS, n_WR, WS, WR, fld_hbd_rtp, flc_hbd_pole, fld_hbd_pole)
!
!
      call copy_nod_vec_from_trans_wpole(nvector_rj_2_xyz,              &
     &    b_hbd_trns%i_magne, fld_hbd_rtp(1,1), fld_hbd_pole,           &
     &    iphys_sph%i_magne, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole(nvector_rj_2_xyz,              &
     &    b_hbd_trns%i_current, fld_hbd_rtp(1,1), fld_hbd_pole,         &
     &    iphys_sph%i_current, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole(nvector_rj_2_xyz,              &
     &    b_hbd_trns%i_b_diffuse, fld_hbd_rtp(1,1), fld_hbd_pole,       &
     &     iphys_sph%i_b_diffuse, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole(nvector_rj_2_xyz,              &
     &    b_hbd_trns%i_induction, fld_hbd_rtp(1,1), fld_hbd_pole,       &
     &    iphys_sph%i_induction, mesh_sph%node, sph_fld)
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call copy_nod_vec_from_trans_wpole(nvector_rj_2_xyz,            &
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

