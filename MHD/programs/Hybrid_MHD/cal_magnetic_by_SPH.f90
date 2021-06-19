!!
!!      subroutine induction_SPH_initialize(SGS_param,                  &
!!     &          ipol, comms_sph, sph, trans_p, rj_fld,                &
!!     &          SR_sig, SR_r, SR_i, SR_il)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(sph_grids), intent(inout) :: sph
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine nonlinear_incuction_wSGS_SPH(FEM_prm, SGS_par,       &
!!     &          mesh, sph, comms_sph, trans_p, conduct, MHD_prop,     &
!!     &          fem_int, Csims_FEM_MHD, iphys, iphys_LES,             &
!!     &          ipol, rj_fld, SR_sig, SR_r)
!!      subroutine nonlinear_incuction_SPH                              &
!!     &         (sph, comms_sph, trans_p, iphys_ele_base, ele_fld,     &
!!     &          ipol, rj_fld, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!!      subroutine cal_magneitc_field_by_SPH                            &
!!     &         (SGS_param, cd_prop, ph, comms_sph, trans_p,           &
!!     &          ipol, rj_fld, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_file_IO_parameter
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_jacobians
      use t_vector_for_solver
      use t_solver_SR
!
      use calypso_mpi
!
      type(field_IO_params), save ::  mesh_file_H
      type(mesh_data), save :: mesh_fem
      type(phys_data), save :: fem_fld
!
      type(mesh_data), save :: mesh_sph
      type(phys_data), save :: sph_fld
!
      type(interpolate_table), intent(in) :: itp_FEM_2_SPH
      type(interpolate_table), intent(in) :: itp_SPH_2_FEM
!
!    addresses of nodal field on spherical grid
      type(phys_address), save :: iphys_sph
!
!>      Work structures for various Legendre trasform
      type(legendre_trns_works), save :: WK_leg_HBD
!>      Structure for work area of FFTs
      type(work_for_FFTs), save :: WK_FFTs_HBD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine induction_SPH_initialize(SGS_param,                    &
     &          ipol, comms_sph, sph, trans_p, rj_fld,                  &
     &          SR_sig, SR_r, SR_i, SR_il)
!
      use t_work_4_sph_trans
      use m_addresses_trans_hbd_MHD
      use interpolate_by_module
      use parallel_FEM_mesh_init
      use mpi_load_mesh_data
      use nod_phys_send_recv
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(phys_address), intent(in) :: ipol
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_grids), intent(inout) :: sph
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      type(field_IO_params) ::  itp_file_IO
!
!
      iphys_sph%i_magne =      1
      iphys_sph%base%i_current =    4
      iphys_sph%diffusion%i_b_diffuse =  7
      iphys_sph%forces%i_vp_induct = 10
      iphys_sph%forces%i_induction = 13
      sph_fld%num_phys =   5
      sph_fld%ntot_phys = 15
!
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        iphys_sph_LES%SGS_term%i_SGS_vp_induct = 16
        iphys_sph_LES%SGS_term%i_SGS_induction = 19
        sph_fld%num_phys =   7
        sph_fld%ntot_phys = 21
      end if
!
      call set_addresses_trans_hbd_MHD(evo_magne, SGS_param)
      call allocate_hbd_trans_rtp(sph%sph_rtp)
      call check_add_trans_hbd_MHD(ipol)
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_file_H, nprocs, mesh_sph)
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(mesh_sph%mesh, vect1,                &
     &                             SR_sig, SR_r, SR_i, SR_il)
      call FEM_mesh_initialization                                      &
     &   (mesh_sph%mesh, mesh_sph%group, SR_sig, SR_i)
!
      call alloc_phys_name(sph_fld)
      call alloc_phys_data(mesh_sph%node%numnod, sph_fld)
!
!     ---------------------
!
      itp_file_IO%iflag_format = ifile_type
      itp_file_IO%file_prefix = FEM_2_SPH_tbl_head
      call load_interpolate_table(my_rank, IO_itp_org, itp_FEM_2_SPH)
!
      itp_file_IO%file_prefix = SPH_2_FEM_tbl_head
      call load_interpolate_table(my_rank, itp_file_IO, itp_SPH_2_FEM)
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
     &    sph, comms_sph, trans_p, WK_leg_HBD, WK_FFTs_HBD,             &
     &    SR_sig, SR_r)
!
!     ---------------------
!
      end subroutine induction_SPH_initialize
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_wSGS_SPH(FEM_prm, SGS_par,         &
     &          mesh, sph, comms_sph, trans_p, conduct, MHD_prop,       &
     &          fem_int, Csims_FEM_MHD, iphys, iphys_LES,               &
     &          ipol, rj_fld, SR_sig, SR_r)
!
      use m_schmidt_poly_on_rtm
!
      use t_geometry_data_MHD
      use t_FEM_SGS_model_coefs
!
      use spherical_transforms
      use spherical_SRs_N
      use copy_nodal_fld_4_sph_trans
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: iphys, ipol
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(finite_element_integration), intent(in) :: fem_int
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
!
      type(field_geometry_data), intent(in) :: conduct
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_sgs_uxb_2_monitor                                        &
     &  (MHD_step1%time_d%dt, FEM_prm, SGS_par%model_p,                 &
     &   SGS_par%filter_p, mesh%nod_comm, mesh%node, mesh%ele, conduct, &
     &   MHD_prop%cd_prop, iphys, iphys_LES,                            &
     &   SGS_MHD_wk1%iphys_ele_base, SGS_MHD_wk1%ele_fld,               &
     &   fem_int%jcs, fem_int%rhs_tbl, FEM1_elen, filtering1,           &
     &   Csims_FEM_MHD%icomp_sgs_term, Csims_FEM_MHD%iphys_elediff_vec, &
     &   Csims_FEM_MHD%sgs_coefs, mhd1_fem_wk%mlump_cd,                 &
     &   SGS_MHD_wk1%FEM_SGS_wk%wk_filter, mhd_fem1_wk,                 &
     &   rhs_mat1%fem_wk, rhs_mat1%f_l, rhs_mat1%f_nl,                  &
     &   FEM_MHD%field, vect1, SR_sig, SR_r)
!
      call interpolate_vector_type                                      &
     &   (iphys%forces%i_vp_induct,  iphys_sph%i_vp_induct,             &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld, vect1,   &
     &    SR_sig, SR_r)
      call interpolate_vector_type(iphys_LES%SGS_term%i_SGS_vp_induct,  &
     &    iphys_sph_LES%SGS_term%i_SGS_vp_induct,                       &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld, vect1,   &
     &    SR_sig, SR_r)
!
!
      call check_calypso_sph_comm_buf_N(ncomp_xyz_2_rj,                 &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm, SR_sig, SR_r)
      call check_calypso_sph_comm_buf_N(ncomp_xyz_2_rj,                 &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, SR_sig, SR_r)
!
      call copy_nod_vec_to_sph_trans                                    &
     &   (node1, sph_fld, iphys_sph%forces%i_vp_induct,                 &
     &    frc_hbd_rtp(1,f_hbd_trns%forces%i_vp_induct))
      call copy_nod_vec_to_sph_trans(node1, sph_fld,                    &
     &    iphys_sph_LES%SGS_term%i_SGS_vp_induct,                       &
     &    frc_hbd_rtp(1,f_hbd_trns_LES%SGS_term%i_SGS_vp_induct))
!
      call sph_forward_transforms                                       &
     &   (ncomp_xyz_2_rj, nvector_xyz_2_rj, izero,                      &
     &    sph, comms_sph, trans_p, frc_hbd_rtp(1,1),                    &
     &    WK_leg_HBD, WK_FFTs_HBD, SR_sig, SR_r)
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &    ipol%forces%i_vp_induct, f_hbd_trns%forces%i_vp_induct,       &
     &    comms_sph%comm_rj, SR_r%n_WR, SR_r%WR(1), rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &    ipol_LES%SGS_term%i_SGS_vp_induct,                            &
     &    f_hbd_trns_LES%SGS_term%i_SGS_vp_induct,                      &
     &    comms_sph%comm_rj, SR_r%n_WR, SR_r%WR(1), rj_fld)
!
!
      call const_sph_rotation_uxb(sph%sph_rj, SPH_WK%r_2nd, sph_bc_B,   &
     &    g_sph_rj, ipol%forces%i_vp_induct, ipol%forces%i_induction,   &
     &    rj_fld)
      call const_sph_rotation_uxb(sph%sph_rj, SPH_WK%r_2nd, sph_bc_B,   &
     &    g_sph_rj, ipol_LES%SGS_term%i_SGS_vp_induct,                  &
     &    ipol_LES%SGS_term%i_SGS_induction, rj_fld)
!*
      end subroutine nonlinear_incuction_wSGS_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_incuction_SPH                                &
     &         (sph, comms_sph, trans_p, iphys_ele_base, ele_fld,       &
     &          ipol, rj_fld, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!
      use spherical_transforms
      use spherical_SRs_N
      use copy_nodal_fld_4_sph_trans
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
!
      type(phys_data), intent(inout) :: rj_fld
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_vecp_induction
     &   (dt, FEM_prm, nod_comm, node, ele, conduct, cd_prop,           &
     &    nod_bcs%Bnod_bcs, iphys, iphys_ele_base, ele_fld, fem_int,    &
     &    mhd_fem_wk%mlump_cd, mhd_fem_wk, rhs_mat, nod_fld,            &
     &    vect1, SR_sig, SR_r)
!
      call interpolate_vector_type                                      &
     &   (iphys%forces%i_vp_induct, iphys_sph%forces%i_vp_induct,       &
     &    itp_FEM_2_SPH, mesh_fem, mesh_sph, fem_fld, sph_fld,          &
     &    v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!
      call copy_nod_vec_to_sph_trans                                    &
     &   (node1, sph_fld, iphys_sph%forces%i_vp_induct,                 &
     &    frc_hbd_rtp(1,f_hbd_trns%forces%i_vp_induct))
!
      call check_calypso_sph_comm_buf_N(ncomp_xyz_2_rj,                 &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm, SR_sig, SR_r)
      call check_calypso_sph_comm_buf_N(ncomp_xyz_2_rj,                 &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, SR_sig, SR_r)
!
      call sph_forward_transforms                                       &
     &   (ncomp_xyz_2_rj, nvector_xyz_2_rj, izero,                      &
     &    sph, comms_sph, trans_p, frc_hbd_rtp(1,1),                    &
     &    WK_leg_HBD, WK_FFTs_HBD, SR_sig, SR_r)
!
      call sel_sph_rj_vector_from_recv(ncomp_xyz_2_rj,                  &
     &   (ipol%forces%i_vp_induct, f_hbd_trns%i_vp_induct,              &
     &    comms_sph%comm_rj, SR_r%n_WR, SR_r%WR, rj_fld)
!
      call const_sph_rotation_uxb(sph%sph_rj, SPH_WK%r_2nd, sph_bc_B,   &
     &    g_sph_rj, ipol%forces%i_vp_induct, ipol%forces%i_induction,   &
     &    rj_fld)
!
      end subroutine nonlinear_incuction_SPH
!
!*   ------------------------------------------------------------------
!
      subroutine cal_magneitc_field_by_SPH                              &
     &         (SGS_param, cd_prop, ph, comms_sph, trans_p,             &
     &          ipol, rj_fld, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!
      use m_schmidt_poly_on_rtm
      use t_physical_property
      use spherical_transforms
      use spherical_SRs_N
      use copy_spectr_4_sph_trans
      use copy_nodal_fld_4_sph_trans
!
      type(conductive_property), intent(in) :: cd_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call cal_diff_induction_MHD_adams                               &
     &     (ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion, dt,  &
     &      cd_prop%coef_exp, rj_fld%n_point, rj_fld%ntot_phys,         &
     &      rj_fld%d_fld)
      else
        call cal_diff_induction_wSGS_adams                              &
     &     (ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,      &
     &      ipol_LES%SGS_term, dt, cd_prop%coef_exp,                    &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      call cal_sol_magne_sph_crank(sph%sph_rj, sph_bc_B, bcs_B,         &
     &    band_bp_evo, band_bt_evo, g_sph_rj, ipol, rj_fld)
      call update_after_magne_sph                                       &
     &   (sph%sph_rj, SPH_WK%r_2nd, MHD_prop%cd_prop, sph_bc_B,         &
     &    trans_p%leg, ipol, rj_fld)
!
!
      call check_calypso_sph_comm_buf_N(ncomp_rj_2_xyz,                 &
     &    comms_sph%comm_rj, comms_sph%comm_rlm, SR_sig, SR_r)
      call check_calypso_sph_comm_buf_N(ncomp_rj_2_xyz,                 &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp, SR_sig, SR_r)
!
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%i_magne, b_hbd_trns%i_magne, rj_fld,                     &
     &    SR_r%n_WS, SR_r%WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%base%i_current, b_hbd_trns%base%i_current,               &
     &    rj_fld, SR_r%n_WS, SR_r%WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%diffusion%i_b_diffuse, b_hbd_trns%diffusion%i_b_diffuse, &
     &    rj_fld, SR_r%n_WS, SR_r%WS)
      call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                    &
     &    ipol%forces%i_induction, b_hbd_trns%forces%i_induction,       &
     &    rj_fld, SR_r%n_WS, SR_r%WS)
      if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call sel_sph_rj_vector_to_send(ncomp_rj_2_xyz,                  &
     &      ipol_LES%SGS_term%i_SGS_induction,                          &
     &      b_hbd_trns%i_SGS_vp_induct,                                 &
     &      rj_fld, SR_r%n_WS, SR_r%WS)
      end if
!
      call sph_b_trans_w_poles                                          &
     &   (ncomp_rj_2_xyz, nvector_rj_2_xyz, izero, sph, comms_sph,      &
     &    trans_p, fld_hbd_rtp, flc_hbd_pole, fld_hbd_pole,             &
     &    WK_leg_HBD, WK_FFTs_HBD), SR_sig, SR_r)
!
!
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%i_magne, fld_hbd_rtp(1,1), fld_hbd_pole,           &
     &    iphys_sph%i_magne, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%base%i_current, fld_hbd_rtp(1,1), fld_hbd_pole,    &
     &    iphys_sph%base%i_current, mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%diffusion%i_b_diffuse, fld_hbd_rtp(1,1),           &
     &    fld_hbd_pole, iphys_sph%diffusion%i_b_diffuse,                &
     &    mesh_sph%node, sph_fld)
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns%forces%i_induction, fld_hbd_rtp(1,1),              &
     &    fld_hbd_pole, iphys_sph%forces%i_induction,                   &
     &    mesh_sph%node, sph_fld)
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call copy_nod_vec_from_trans_wpole                              &
     &   (sph%sph_rtp, sph%sph_params%m_folding, nvector_rj_2_xyz,      &
     &    b_hbd_trns_LES%SGS_term%i_SGS_induction,                      &
     &    fld_hbd_rtp(1,1), fld_hbd_pole,                               &
     &    iphys_sph_LES%SGS_term%i_SGS_induction, mesh_sph%node,        &
     &    sph_fld)
      end if
!
!
      call interpolate_vector_type                                      &
     &   (iphys_sph%i_magne, iphys%i_magne,                             &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld,          &
     &    v_1st_sol, v_2nd_sol, SR_sig, SR_r)
      call interpolate_vector_type                                      &
     &   (iphys_sph%base%i_current, iphys%i_current,                    &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld,          &
     &    v_1st_sol, v_2nd_sol, SR_sig, SR_r)
      call interpolate_vector_type                                      &
     &   (iphys_sph%diffusion%i_b_diffuse, iphys%diffusion%i_b_diffuse, &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld,          &
     &    v_1st_sol, v_2nd_sol, SR_sig, SR_r)
      call interpolate_vector_type                                      &
     &   (iphys_sph%forces%i_induction, iphys%forces%i_induction,       &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld,          &
     &    v_1st_sol, v_2nd_sol, SR_sig, SR_r)
      if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
        call interpolate_vector_type                                    &
     &   (iphys_sph_LES%SGS_term%i_SGS_induction,                       &
     &    iphys_LES%SGS_term%i_SGS_induction,                           &
     &    itp_SPH_2_FEM, mesh_sph, mesh_fem, sph_fld, fem_fld,          &
     &    v_1st_sol, v_2nd_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_magneitc_field_by_SPH
!
!*   ------------------------------------------------------------------
!

