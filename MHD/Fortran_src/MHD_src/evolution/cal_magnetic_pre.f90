!
!      module cal_magnetic_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!      subroutine cal_magnetic_field_pre
!
      module cal_magnetic_pre
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
      use m_t_int_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_field_pre
!
      use m_group_data
      use m_bc_data_magne
      use m_SGS_address
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use int_vol_diffusion_ele
      use int_vol_magne_pre
      use int_surf_magne_pre
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
!      use check_surface_groups
!
!
!      call check_surface_param_smp('cal_magnetic_field_pre start',     &
!     &    my_rank, sf_grp1, sf_grp_nod1)
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call cal_sgs_magne_induction(icomp_sgs_uxb, ie_dvx, ie_dbx,     &
     &     nod_comm, node1, ele1, conduct1, iphys, iphys_ele, fld_ele1, &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk,        &
     &     f1_l, nod_fld1)
      end if
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
! lead diffusion term
!
      if (coef_magne.gt.zero .and. coef_exp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(conduct1%istack_ele_fld_smp,    &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      iak_diff_b, coef_exp_b, ak_d_magne, iphys%i_magne,          &
     &      fem1_wk, f1_l)
      end if
!
! lead induction terms
!
      if (iflag_debug .eq. 0 ) write(*,*) 'coefs_4_time_evolution'
      if (iflag_mag_supg .gt. id_turn_OFF) then
       call int_vol_magne_pre_ele_upm                                   &
     &    (node1, ele1, conduct1, iphys, nod_fld1,                      &
     &     fld_ele1%ntot_phys, fld_ele1%d_fld, iphys_ele, iak_diff_uxb, &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      else
       call int_vol_magne_pre_ele                                       &
     &    (node1, ele1, conduct1, iphys, nod_fld1,                      &
     &     fld_ele1%ntot_phys, fld_ele1%d_fld, iphys_ele, iak_diff_uxb, &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      end if
!
!
      call int_surf_magne_pre_ele                                       &
     &   (iak_diff_uxb, node1, ele1, surf1, sf_grp1, iphys, nod_fld1,   &
     &    jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l, f1_nl)
!
      if (iflag_t_evo_4_magne .eq. id_explicit_euler) then
        call cal_magne_pre_euler
      else if (iflag_t_evo_4_magne .eq. id_explicit_adams2) then
        call cal_magne_pre_adams
      else if (iflag_t_evo_4_magne .eq. id_Crank_nicolson) then
        call cal_magne_pre_lumped_crank(iak_diff_b)
      else if (iflag_t_evo_4_magne .eq. id_Crank_nicolson_cmass) then 
        call cal_magne_pre_consist_crank(iak_diff_b)
      end if
!
      call set_boundary_vect(nod_bc1_b, iphys%i_magne, nod_fld1)
!
      call vector_send_recv(iphys%i_magne, node1, nod_comm, nod_fld1)
!
      end subroutine cal_magnetic_field_pre
!
! ----------------------------------------------------------------------
!
      end module cal_magnetic_pre
