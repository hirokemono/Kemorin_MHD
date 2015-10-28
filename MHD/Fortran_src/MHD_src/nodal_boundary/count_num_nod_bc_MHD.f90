!
!      module count_num_nod_bc_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!        modified by Kemorin on Oct. 2009
!
!     subroutine count_num_bc_nod(nod_grp)
!
      module count_num_nod_bc_MHD
!
      use m_precision
!
      use m_constants
      use m_boundary_condition_IDs
      use t_group_data
      use count_num_nodal_fields
!
      implicit none
!
      private :: count_num_bc_velo, count_num_bc_press
      private :: count_num_bc_temp, count_num_bc_composit
      private :: count_num_bc_vecp, count_num_bc_magne
      private :: count_num_bc_magp, count_num_bc_current
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_nod(nod_grp)
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_temp(nod_grp)
      call count_num_bc_velo(nod_grp)
      call count_num_bc_magne(nod_grp)
      call count_num_bc_vecp(nod_grp)
      call count_num_bc_current(nod_grp)
      call count_num_bc_magp(nod_grp)
      call count_num_bc_composit(nod_grp)
      call count_num_bc_press(nod_grp)
!
      end subroutine count_num_bc_nod
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_velo(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_velo
      use m_bc_data_rotate
      use m_bc_data_vfree
      use m_bc_data_vr0
      use m_bc_data_vsp
      use m_bc_temp_sgs
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    velo_nod%num_bc, velo_nod%bc_name, velo_nod%ibc_type,         &
     &    nod_bc1_v%num_bc_nod, iflag_bc_fixed)
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    velo_nod%num_bc, velo_nod%bc_name, velo_nod%ibc_type,         &
     &    sgs_bc1_v%num_bc_nod, iflag_bc_sgs)
!
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    velo_nod%num_bc, velo_nod%bc_name, velo_nod%ibc_type,         &
     &    num_bc_v10_nod, iflag_bc_rot_x)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    velo_nod%num_bc, velo_nod%bc_name, velo_nod%ibc_type,         &
     &    num_bc_fr_nod, iflag_free_sph)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    velo_nod%num_bc, velo_nod%bc_name, velo_nod%ibc_type,         &
     &    num_bc_vr0_nod, iflag_no_vr)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    velo_nod%num_bc, velo_nod%bc_name, velo_nod%ibc_type,         &
     &    num_bc_vsp_nod, iflag_bc_special)
!
!
      call cal_max_int_4_vector                                         &
     &   (nod_bc1_v%nmax_bc, nod_bc1_v%num_bc_nod)
      call cal_max_int_4_vector                                         &
     &   (sgs_bc1_v%nmax_bc, sgs_bc1_v%num_bc_nod)
!
      end subroutine count_num_bc_velo
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_press(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_press
      use m_bc_press_sgs
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    press_nod%num_bc, press_nod%bc_name, press_nod%ibc_type,      &
     &    num_bc_p_nod, iflag_bc_fix_s)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    press_nod%num_bc, press_nod%bc_name, press_nod%ibc_type,      &
     &    num_bc_ps_nod, iflag_bc_sgs_s)
!
      end subroutine count_num_bc_press
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_temp(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_ene
      use m_bc_temp_sgs
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    temp_nod%num_bc, temp_nod%bc_name, temp_nod%ibc_type,         &
     &    num_bc_e_nod, iflag_bc_fix_s)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    temp_nod%num_bc, temp_nod%bc_name, temp_nod%ibc_type,         &
     &    num_bc_t_sgs_nod, iflag_bc_sgs_s)
!
      end subroutine count_num_bc_temp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_vecp(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_vect_p
      use m_bc_vecp_sgs
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    a_potential_nod%num_bc, a_potential_nod%bc_name,              &
     &    a_potential_nod%ibc_type, num_bc_vp_nod, iflag_bc_fixed)
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    a_potential_nod%num_bc, a_potential_nod%bc_name,              &
     &    a_potential_nod%ibc_type, num_bc_a_sgs_nod, iflag_bc_sgs)
!
!
      call cal_max_int_4_vector(nmax_bc_vp_nod, num_bc_vp_nod)
      call cal_max_int_4_vector(nmax_bc_a_sgs_nod, num_bc_a_sgs_nod)
!
      end subroutine count_num_bc_vecp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magne(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
      use m_bc_magne_sgs
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    magne_nod%num_bc, magne_nod%bc_name, magne_nod%ibc_type,      &
     &     nod_bc1_b%num_bc_nod, iflag_bc_fixed)
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    magne_nod%num_bc, magne_nod%bc_name, magne_nod%ibc_type,      &
     &    num_bc_b_sgs_nod, iflag_bc_sgs)
!
      call add_num_bc_magne                                             &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    magne_nod%num_bc, magne_nod%bc_name, magne_nod%ibc_type,      &
     &    nod_bc1_b%num_bc_nod)
!
!
      call cal_max_int_4_vector  &
     &   (nod_bc1_b%nmax_bc, nod_bc1_b%num_bc_nod)
      call cal_max_int_4_vector(nmax_bc_b_sgs_nod, num_bc_b_sgs_nod)
!
      end subroutine count_num_bc_magne
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magp(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne_p
      use m_bc_mag_p_sgs
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    e_potential_nod%num_bc, e_potential_nod%bc_name,              &
     &    e_potential_nod%ibc_type, num_bc_mag_p_nod, iflag_bc_fix_s)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    e_potential_nod%num_bc, e_potential_nod%bc_name,              &
     &    e_potential_nod%ibc_type, num_bc_mp_sgs_nod, iflag_bc_sgs_s)
!
      call add_num_bc_mag_p                                             &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    e_potential_nod%num_bc, e_potential_nod%bc_name,              &
     &    e_potential_nod%ibc_type, num_bc_mag_p_nod)
!
      end subroutine count_num_bc_magp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_current(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_current
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    current_nod%num_bc, current_nod%bc_name,                      &
     &    current_nod%ibc_type, num_bc_j_nod, izero)
!
      call cal_max_int_4_vector(nmax_bc_j_nod, num_bc_j_nod)
!
      end subroutine count_num_bc_current
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_composit(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_composition
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    light_nod%num_bc, light_nod%bc_name, light_nod%ibc_type,      &
     &    num_bc_composition_nod, iflag_bc_fix_s)
!
      end subroutine count_num_bc_composit
!
!  ---------------------------------------------------------------------
!
      end module count_num_nod_bc_MHD
