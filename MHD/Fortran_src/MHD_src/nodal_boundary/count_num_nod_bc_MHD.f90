!
!      module count_num_nod_bc_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!        modified by Kemorin on Oct. 2009
!
!     subroutine count_num_bc_nod
!
      module count_num_nod_bc_MHD
!
      use m_precision
!
      use m_constants
      use m_node_group
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
      subroutine count_num_bc_nod
!
      call count_num_bc_temp
      call count_num_bc_velo
      call count_num_bc_magne
      call count_num_bc_vecp
      call count_num_bc_current
      call count_num_bc_magp
      call count_num_bc_composit
      call count_num_bc_press
!
      end subroutine count_num_bc_nod
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_velo
!
      use m_bc_data_list
      use m_bc_data_velo
      use m_bc_velo_sgs
      use m_bc_data_rotate
      use m_bc_data_vfree
      use m_bc_data_vr0
      use m_bc_data_vsp
      use m_bc_temp_sgs
!
!
      call count_num_bc_vector(num_bc, bc_istack, bc_name,              &
     &    num_bc_v, bc_v_name, ibc_v_type, num_bc_v_nod,                &
     &    iflag_bc_fixed)
!
      call count_num_bc_vector(num_bc, bc_istack, bc_name,              &
     &    num_bc_v, bc_v_name, ibc_v_type, num_bc_v_sgs_nod,            &
     &    iflag_bc_sgs)
!
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_v, bc_v_name, ibc_v_type, num_bc_v10_nod,              &
     &    iflag_bc_rot_x)
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_v, bc_v_name, ibc_v_type, num_bc_fr_nod,               &
     &    iflag_free_sph)
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_v, bc_v_name, ibc_v_type, num_bc_vr0_nod,              &
     &    iflag_no_vr)
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_v, bc_v_name, ibc_v_type, num_bc_vsp_nod,              &
     &    iflag_bc_special)
!
!
      call cal_max_int_4_vector(nmax_bc_v_nod, num_bc_v_nod)
      call cal_max_int_4_vector(nmax_bc_v_sgs_nod, num_bc_v_sgs_nod)
!
      end subroutine count_num_bc_velo
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_press
!
      use m_bc_data_list
      use m_bc_data_press
      use m_bc_press_sgs
!
!
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_p, bc_p_name, ibc_p_type, num_bc_p_nod,                &
     &    iflag_bc_fix_s)
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_p, bc_p_name, ibc_p_type, num_bc_ps_nod,               &
     &    iflag_bc_sgs_s)
!
      end subroutine count_num_bc_press
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_temp
!
      use m_bc_data_list
      use m_bc_data_ene
      use m_bc_temp_sgs
!
!
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_e, bc_e_name, ibc_e_type, num_bc_e_nod,                &
     &    iflag_bc_fix_s)
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_e, bc_e_name, ibc_e_type, num_bc_t_sgs_nod,            &
     &    iflag_bc_sgs_s)
!
      end subroutine count_num_bc_temp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_vecp
!
      use m_bc_data_list
      use m_bc_data_vect_p
      use m_bc_vecp_sgs
!
!
      call count_num_bc_vector(num_bc, bc_istack, bc_name,              &
     &    num_bc_vp, bc_vp_name, ibc_vp_type, num_bc_vp_nod,            &
     &    iflag_bc_fixed)
!
      call count_num_bc_vector(num_bc, bc_istack, bc_name,              &
     &    num_bc_vp, bc_vp_name, ibc_vp_type, num_bc_a_sgs_nod,         &
     &    iflag_bc_sgs)
!
!
      call cal_max_int_4_vector(nmax_bc_vp_nod, num_bc_vp_nod)
      call cal_max_int_4_vector(nmax_bc_a_sgs_nod, num_bc_a_sgs_nod)
!
      end subroutine count_num_bc_vecp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magne
!
      use m_bc_data_list
      use m_bc_data_magne
      use m_bc_magne_sgs
!
!
      call count_num_bc_vector(num_bc, bc_istack, bc_name,              &
     &    num_bc_b, bc_b_name, ibc_b_type, num_bc_b_nod,                &
     &    iflag_bc_fixed)
!
      call count_num_bc_vector(num_bc, bc_istack, bc_name,              &
     &    num_bc_b, bc_b_name, ibc_b_type, num_bc_b_sgs_nod,            &
     &    iflag_bc_sgs)
!
      call add_num_bc_magne(num_bc, bc_istack, bc_name,                 &
     &    num_bc_b, bc_b_name, ibc_b_type, num_bc_b_nod)
!
!
      call cal_max_int_4_vector(nmax_bc_b_nod, num_bc_b_nod)
      call cal_max_int_4_vector(nmax_bc_b_sgs_nod, num_bc_b_sgs_nod)
!
      end subroutine count_num_bc_magne
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magp
!
      use m_bc_data_list
      use m_bc_data_magne_p
      use m_bc_mag_p_sgs
!
!
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_mag_p, bc_mag_p_name, ibc_mag_p_type,                  &
     &    num_bc_mag_p_nod, iflag_bc_fix_s)
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_mag_p, bc_mag_p_name, ibc_mag_p_type,                  &
     &    num_bc_mp_sgs_nod, iflag_bc_sgs_s)
!
      call add_num_bc_mag_p(num_bc, bc_istack, bc_name,                 &
     &    num_bc_mag_p, bc_mag_p_name,                                  &
     &    ibc_mag_p_type, num_bc_mag_p_nod)
!
      end subroutine count_num_bc_magp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_current
!
      use m_bc_data_list
      use m_bc_data_current
!
!
      call count_num_bc_vector(num_bc, bc_istack, bc_name,              &
     &    num_bc_j, bc_j_name, ibc_j_type, num_bc_j_nod, izero)
!
      call cal_max_int_4_vector(nmax_bc_j_nod, num_bc_j_nod)
!
      end subroutine count_num_bc_current
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_composit
!
      use m_bc_data_list
      use m_bc_data_composition
!
!
      call count_num_bc_scalar(num_bc, bc_istack, bc_name,              &
     &    num_bc_composit, bc_composit_name, ibc_composit_type,         &
     &    num_bc_composition_nod, iflag_bc_fix_s)
!
      end subroutine count_num_bc_composit
!
!  ---------------------------------------------------------------------
!
      end module count_num_nod_bc_MHD
