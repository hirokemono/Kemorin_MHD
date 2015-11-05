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
      use m_bc_data_list
      use t_group_data
      use count_num_nodal_fields
!
      implicit none
!
      private :: count_num_bc_velo, count_num_bc_press
      private :: count_num_bc_temp, count_num_bc_composit
      private :: count_num_bc_magp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_nod(nod_grp)
!
      use m_bc_data_magne
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_temp(nod_grp)
      call count_num_bc_velo(nod_grp)
!
      call count_num_bc_magne(iflag_bc_fixed, nod_grp,                  &
     &    magne_nod, nod_bc1_b)
      call count_num_bc_vect(iflag_bc_sgs, nod_grp,                     &
     &    magne_nod, sgs_bc1_b)
!
      call count_num_bc_vect(iflag_bc_fixed, nod_grp,                   &
     &    a_potential_nod, nod_bc1_a)
      call count_num_bc_vect(iflag_bc_sgs, nod_grp,                     &
     &    a_potential_nod, sgs_bc1_a)
!
      call count_num_bc_vect (iflag_bc_fixed, nod_grp,                  &
     &    current_nod, nod_bc1_j)
!
      call count_num_bc_magp(nod_grp)
      call count_num_bc_composit(nod_grp)
      call count_num_bc_press(nod_grp)
!
      end subroutine count_num_bc_nod
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_vect                                      &
     &         (iflag_bc, nod_grp, bc_list, nod_bc_vect)
!
      use t_nodal_bc_data
!
      integer(kind = kint), intent(in) :: iflag_bc
!
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_vect
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    bc_list%num_bc, bc_list%bc_name, bc_list%ibc_type,            &
     &    nod_bc_vect%num_bc_nod, iflag_bc)
      call cal_max_int_4_vector                                         &
     &   (nod_bc_vect%nmax_bc, nod_bc_vect%num_bc_nod)
!
      end subroutine count_num_bc_vect
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_velo(nod_grp)
!
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
      call count_num_bc_vect                                           &
     &   (iflag_bc_fixed, nod_grp, velo_nod, nod_bc1_v)
      call count_num_bc_vect                                           &
     &   (iflag_bc_sgs, nod_grp, velo_nod, sgs_bc1_v)
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
!
      end subroutine count_num_bc_velo
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_press(nod_grp)
!
      use m_bc_data_velo
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    press_nod%num_bc, press_nod%bc_name, press_nod%ibc_type,      &
     &    nod_bc1_p%num_bc_nod, iflag_bc_fix_s)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    press_nod%num_bc, press_nod%bc_name, press_nod%ibc_type,      &
     &    sgs_bc1_p%num_bc_nod, iflag_bc_sgs_s)
!
      end subroutine count_num_bc_press
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_temp(nod_grp)
!
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
      subroutine count_num_bc_magne                                     &
     &         (iflag_bc,  nod_grp,  bc_list,  nod_bc_vect)
!
      use t_nodal_bc_data
!
      integer(kind = kint), intent(in) :: iflag_bc
!
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_vect
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    bc_list%num_bc, bc_list%bc_name, bc_list%ibc_type,            &
     &     nod_bc_vect%num_bc_nod, iflag_bc)
!
      call add_num_bc_magne                                             &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    bc_list%num_bc, bc_list%bc_name, bc_list%ibc_type,            &
     &    nod_bc_vect%num_bc_nod)
!
!
      call cal_max_int_4_vector                                        &
     &   (nod_bc_vect%nmax_bc, nod_bc_vect%num_bc_nod)
!
      end subroutine count_num_bc_magne
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magp(nod_grp)
!
      use m_bc_data_magne
!
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    e_potential_nod%num_bc, e_potential_nod%bc_name,              &
     &    e_potential_nod%ibc_type, nod_bc1_f%num_bc_nod,               &
     &    iflag_bc_fix_s)
      call count_num_bc_scalar                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    e_potential_nod%num_bc, e_potential_nod%bc_name,              &
     &    e_potential_nod%ibc_type, sgs_bc1_f%num_bc_nod,               &
     &    iflag_bc_sgs_s)
!
      call add_num_bc_mag_p                                             &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    e_potential_nod%num_bc, e_potential_nod%bc_name,              &
     &    e_potential_nod%ibc_type, nod_bc1_f%num_bc_nod)
!
      end subroutine count_num_bc_magp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_composit(nod_grp)
!
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
