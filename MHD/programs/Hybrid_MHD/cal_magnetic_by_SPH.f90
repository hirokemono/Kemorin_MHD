!!
!!      subroutine induction_SPH_initialize(SGS_param,                  &
!!     &         ipol, idpdr, itor, comms_sph, sph, trans_p, rj_fld)
!!      subroutine nonlinear_incuction_wSGS_SPH(FEM_prm, SGS_param,     &
!!     &          sph, comms_sph, trans_p, conduct, ipol, rj_fld)
!!      subroutine cal_magneitc_field_by_SPH(SGS_param, cd_prop,        &
!!     &          sph, comms_sph, trans_p, ipol, itor, rj_fld)
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_file_IO_parameter
      use t_sph_transforms
      use t_jacobians
!
      use calypso_mpi
!
      type(field_IO_params), save ::  mesh_file_H
      type(mesh_data), save :: mesh_fem
      type(phys_data), save :: fem_fld
!
      type(mesh_data), save :: mesh_sph
      type(element_geometry), save :: ele_mesh_sph
      type(phys_data), save :: sph_fld
!
      type(interpolate_table), intent(in) :: itp_FEM_2_SPH
      type(interpolate_table), intent(in) :: itp_SPH_2_FEM
!
!    addresses of nodal field on spherical grid
      type(phys_address), save :: iphys_sph
!
!>      Work structures for various spherical harmonics trasform
      type(spherical_trns_works), save :: WK1_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine induction_SPH_initialize(SGS_param,                    &
     &         ipol, idpdr, itor, comms_sph, sph, trans_p, rj_fld)
!
      use t_work_4_sph_trans
      use m_addresses_trans_hbd_MHD
      use interpolate_by_module
      use const_element_comm_tables
      use mpi_load_mesh_data
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_grids), intent(inout) :: sph
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(phys_data), intent(inout) :: rj_fld
!
!
      iphys_sph%i_magne =      1
      iphys_sph%i_current =    4
      iphys_sph%i_b_diffuse =  7
      iphys_sph%i_vp_induct = 10
      iphys_sph%i_induction = 13
      sph_fld%num_phys =   5
      sph_fld%ntot_phys = 15
!
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        iphys_sph%i_SGS_vp_induct = 16
        iphys_sph%i_SGS_induction = 19
        sph_fld%num_phys =   7
        sph_fld%ntot_phys = 21
      end if
!
      call init_pole_transform(sph%sph_rtp)
!
      call set_addresses_trans_hbd_MHD(evo_magne, SGS_param)
      call allocate_hbd_trans_rtp(sph%sph_rtp)
      call check_add_trans_hbd_MHD(ipol, idpdr, itor)
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh                                               &
     &   (mesh_file_H, nprocs, mesh_sph%mesh, mesh_sph%group,           &
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
      call load_interpolate_table(my_rank, itp_FEM_2_SPH)
!
      table_file_header = SPH_2_FEM_tbl_head
      call load_interpolate_table(my_rank, itp_SPH_2_FEM)
!
!
      call init_interpolate_mat_type(mesh_fem%ele,                      &
     &    itp_FEM_2_SPH%tbl_org, itp_FEM_2_SPH%mat)
      call init_interpolate_mat_type(mesh_sph%ele,                      &
     &    itp_SPH_2_FEM%tbl_org, itp_FEM_2_SPH%mat)
!
      nscalar_sph_trans = 0
      nvector_sph_trans = max(nvector_rj_2_xyz, nvector_xyz_2_rj)
      call initialize_sph_trans                                         &
     &   (ncomp_sph_trans, nscalar_sph_trans, nvector_sph_trans,        &
     &    sph, comms_sph, trans_p, WK1_sph)
!
!     ---------------------
!
      call const_element_comm_tbls(mesh_sph, ele_mesh_sph)
!
      end subroutine induction_SPH_initialize
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_wSGS_SPH(FEM_prm, SGS_param,       &
     &          sph, comms_sph, trans_p, conduct, jacobians,            &
     &          ipol, rj_fld)
!
      use m_solver_SR
      use m_schmidt_poly_on_rtm
!
      use t_geometry_data_MHD
!
      use spherical_SRs_N
      use copy_nodal_fld_4_sph_trans
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(jacobians_type), intent(in) :: jacobians
!
      type(field_geometry_data), intent(in) :: conduct
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sgs_uxb_2_monitor
     &   (icomp_sgs%i_induction, iphys_elediff%i_velo, dt,              &
     &    FEM_prm, SGS_param, mesh1%nod_comm, mesh1%node, mesh1%ele,    &
     &    conduct, cd_prop1, iphys, iphys_ele, fld_ele1,                &
     &    jacobians%jac_3d, fem_int1%rhs_tbl, FEM1_elen, filtering1,            &
     &    wk_filter1, mhd_fem1_wk, rhs_mat1%fem_wk, rhs_mat1%f_l, rhs_mat1%f_nl, nod_fld1)
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
     &   (ncomp_xyz_2_rj, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_xyz_2_rj, comms_sph%comm_rlm, comms_sph%comm_rj)
!
      call copy_nod_vec_to_sph_trans(node1, sph_fld,                    &
     &    iphys_sph%i_vp_induct, frc_hbd_rtp(1,f_hbd_trns%i_vp_induct))
      call copy_nod_vec_to_sph_trans(node1, sph_fld,                    &
     &    iphys_sph%i_SGS_vp_induct,                                    &
     &    frc_hbd_rtp(1,f_hbd_trns%i_SGS_vp_induct))
!
      call sph_forward_transforms                                       &
     &   (ncomp_xyz_2_rj, nvector_xyz_2_rj, izero,                      &
     &    sph, comms_sph, trans_p, frc_hbd_rtp(1,1),                    &
     &    n_WS, n_WR, WS(1), WR(1), WK1_sph)
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &    ipol%i_vp_induct, f_hbd_trns%i_vp_induct,                     &
     &    comms_sph%comm_rj, n_WR, WR(1), rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &    ipol%i_SGS_vp_induct, f_hbd_trns%i_SGS_vp_induct,             &
     &    comms_sph%comm_rj, n_WR, WR(1), rj_fld)
!
!
      call const_sph_rotation_uxb(sph%sph_rj, r_2nd, sph_bc_B,          &
     &    g_sph_rj, ipol%i_vp_induct, ipol%i_induction, rj_fld)
      call const_sph_rotation_uxb(sph%sph_rj, r_2nd, sph_bc_B,          &
     &    g_sph_rj, ipol%i_SGS_vp_induct, ipol%i_SGS_induction, rj_fld)
!*
      end subroutine nonlinear_incuction_wSGS_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_SPH                                &
     &         (sph, comms_sph, trans_p, ipol, rj_fld)
!
      use m_spheric_parameter
      use m_solver_SR
      use spherical_SRs_N
      use copy_nodal_fld_4_sph_trans
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_vecp_induction
     &   (dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,           &
     &    nod_bcs%Bnod_bcs, iphys, iphys_ele, ele_fld, jac_3d,          &
     &    rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      call interpolate_vector_type                                      &
     &   (iphys%i_vp_induct, iphys_sph%i_vp_induct,                     &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld)
!
      call copy_nod_vec_to_sph_trans(node1, sph_fld,                    &
     &    iphys_sph%i_vp_induct, frc_hbd_rtp(1,f_hbd_trns%i_vp_induct))
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_xyz_2_rj, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_xyz_2_rj, comms_sph%comm_rlm, comms_sph%comm_rj)
!
      call sph_forward_transforms                                       &
     &   (ncomp_xyz_2_rj, nvector_xyz_2_rj, izero,                      &
     &    sph, comms_sph, trans_p, frc_hbd_rtp(1,1),                    &
     &    n_WS, n_WR, WS(1), WR(1), WK1_sph)
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &   (ipol%i_vp_induct, f_hbd_trns%i_vp_induct,                     &
     &    comms_sph%comm_rj, n_WR, WR, rj_fld)
!
      call const_sph_rotation_uxb(sph%sph_rj, r_2nd, sph_bc_B,          &
     &    g_sph_rj, ipol%i_vp_induct, ipol%i_induction, rj_fld)
!
      end subroutine nonlinear_incuction_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine cal_magneitc_field_by_SPH(SGS_param, cd_prop,          &
     &          sph, comms_sph, trans_p, ipol, itor, rj_fld)
!
      use m_solver_SR
      use m_schmidt_poly_on_rtm
      use t_physical_property
      use spherical_SRs_N
      use copy_spectr_4_sph_trans
      use copy_nodal_fld_4_sph_trans
!
      type(conductive_property), intent(in) :: cd_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol, itor
!
      type(phys_data), intent(in) :: rj_fld
!
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call cal_diff_induction_MHD_adams(dt, cd_prop%coef_exp,         &
     &      ipol, itor, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_diff_induction_wSGS_adams(dt, cd_prop%coef_exp,        &
     &      ipol, itor, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      call cal_sol_magne_sph_crank                                      &
     &   (sph%sph_rj, band_bp_evo, band_bt_evo, g_sph_rj, rj_fld)
      call update_after_magne_sph                                       &
     &   (sph%sph_rj, r_2nd, cd_prop1, trans_p%leg, ipol, itor, rj_fld)
!
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_rj_2_xyz, comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_rj_2_xyz, comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_magne, b_hbd_trns%i_magne, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_current, b_hbd_trns%i_current, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_b_diffuse, b_hbd_trns%i_b_diffuse, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_induction, b_hbd_trns%i_induction, rj_fld, n_WS, WS)
      if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                  &
     &      ipol%i_SGS_induction, b_hbd_trns%i_SGS_induction, rj_fld,   &
     &      n_WS, WS)
      end if
!
      call sph_b_trans_w_poles                                          &
     &   (ncomp_rj_2_xyz, nvector_rj_2_xyz, izero,                      &
     &    sph, comms_sph, trans_p, n_WS, n_WR, WS, WR,                  &
     &    fld_hbd_rtp, flc_hbd_pole, fld_hbd_pole, WK1_sph)
!
!
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%i_magne, fld_hbd_rtp(1,1), fld_hbd_pole,           &
     &    iphys_sph%i_magne, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%i_current, fld_hbd_rtp(1,1), fld_hbd_pole,         &
     &    iphys_sph%i_current, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%i_b_diffuse, fld_hbd_rtp(1,1), fld_hbd_pole,       &
     &    iphys_sph%i_b_diffuse, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%i_induction, fld_hbd_rtp(1,1), fld_hbd_pole,       &
     &    iphys_sph%i_induction, mesh_sph%node, sph_fld)
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call copy_nod_vec_from_trans_wpole                              &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
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
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call interpolate_vector_type                                    &
     &   (iphys_sph%i_SGS_induction, iphys%i_SGS_induction,             &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld)
      end if
!
      end subroutine cal_magneitc_field_by_SPH

