!
!      module cal_sol_vector_co_crank
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_co_crank(inter_smp_stack)
!      subroutine cal_sol_vect_p_co_crank(inter_smp_stack)
!      subroutine cal_sol_magne_co_crank(inter_smp_stack)
!
!      subroutine cal_sol_magne_insulator(inter_smp_stack)
!      subroutine cal_vector_co_crank_consist                           &
!     &        (node, inter_smp_stack, coef_field)
!        type(node_data), intent(in) :: node
!
      module cal_sol_vector_co_crank
!
      use m_precision
      use m_machine_parameter
!
      use cal_sol_vector_correct
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co(inter_smp_stack)
!
      use m_phys_constants
      use m_node_phys_data
      use m_int_vol_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
!
      call cal_sol_velocity_co(nod_fld1%n_point, inter_smp_stack,       &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, nod_fld1%ntot_phys,         &
     &    iphys%i_velo, iphys%i_p_phi, nod_fld1%d_fld)
!
      end subroutine cal_sol_velo_co
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co(inter_smp_stack)
!
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vector_co                                            &
     &   (nod_fld1%n_point, inter_smp_stack, m1_lump%ml, f1_l%ff,       &
     &    nod_fld1%ntot_phys, iphys%i_vecp, nod_fld1%d_fld)
!
      end subroutine cal_sol_vect_p_co
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co(inter_smp_stack)
!
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vector_co                                            &
     &   (nod_fld1%n_point, inter_smp_stack, m1_lump%ml, f1_l%ff,       &
     &    nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld)
!
      end subroutine cal_sol_magne_co
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co_crank(inter_smp_stack)
!
      use m_node_phys_data
      use m_int_vol_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
!
      call cal_sol_velo_co_crank_lump                                   &
     &   (nod_fld1%n_point, inter_smp_stack,                            &
     &    mhd_fem1_wk%mlump_fl%ml_o, nod_fld1%ntot_phys,                &
     &    iphys%i_velo, nod_fld1%d_fld, f1_nl%ff, f1_l%ff)
!
      end subroutine cal_sol_velo_co_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co_crank(inter_smp_stack)
!
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank                                        &
     &   (nod_fld1%n_point, inter_smp_stack, m1_lump%ml_o,              &
     &    nod_fld1%ntot_phys, iphys%i_vecp, nod_fld1%d_fld,             &
     &    f1_nl%ff, f1_l%ff)
!
      end subroutine cal_sol_vect_p_co_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co_crank(inter_smp_stack)
!
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank                                        &
     &   (nod_fld1%n_point, inter_smp_stack, m1_lump%ml_o,              &
     &    nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld,            &
     &    f1_nl%ff, f1_l%ff)
!
      end subroutine cal_sol_magne_co_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_insulator
!
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_finite_element_matrix
!
!
      call cal_sol_magne_insulate                                       &
     &   (nod_fld1%n_point, insulate1%istack_inter_fld_smp,             &
     &    insulate1%numnod_fld, insulate1%inod_fld, f1_l%ff,            &
     &    nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld)
!
      end subroutine cal_sol_magne_insulator
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vector_co_crank_consist                            &
     &        (node, inter_smp_stack, coef_field)
!
      use t_geometry_data
      use m_t_int_parameter
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) :: node
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef_field
!
!
      call cal_sol_vect_co_crank_consist                                &
     &   (node%numnod, inter_smp_stack, f1_nl%ff, f1_l%ff)
!
      if (coef_field .eq. 0.0d0) return
      call cal_ff_smp_2_ff(node, rhs_tbl1, n_vector,                    &
     &    mhd_fem1_wk%ff_m_smp, f1_l%ff)
!
      end subroutine cal_vector_co_crank_consist
!
! -----------------------------------------------------------------------!
      end module cal_sol_vector_co_crank
