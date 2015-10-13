!
!      module cal_sol_vector_co_crank
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_co_crank(inter_smp_stack)
!      subroutine cal_sol_vect_p_co_crank(inter_smp_stack)
!      subroutine cal_sol_magne_co_crank(inter_smp_stack)
!      subroutine cal_sol_velo_co_crank_consist(inter_smp_stack)
!      subroutine cal_sol_vect_p_co_crank_consist(inter_smp_stack)
!      subroutine cal_sol_magne_co_crank_consist(inter_smp_stack)
!
!      subroutine cal_sol_magne_insulator(inter_smp_stack)
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
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_int_vol_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
!
      call cal_sol_velocity_co(node1%numnod, inter_smp_stack,           &
     &    mhd_fem1_wk%ml_fl, ff, nod_fld1%ntot_phys,                    &
     &    iphys%i_velo, iphys%i_p_phi, nod_fld1%d_fld)
!
      end subroutine cal_sol_velo_co
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vector_co(node1%numnod, inter_smp_stack, ml, ff,     &
     &    nod_fld1%ntot_phys, iphys%i_vecp, nod_fld1%d_fld)
!
      end subroutine cal_sol_vect_p_co
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vector_co(node1%numnod, inter_smp_stack, ml, ff,     &
     &    nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld)
!
      end subroutine cal_sol_magne_co
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co_crank(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_int_vol_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
!
      call cal_sol_velo_co_crank_lump(node1%numnod, inter_smp_stack,    &
     &    mhd_fem1_wk%ml_o_fl, nod_fld1%ntot_phys,                      &
     &    iphys%i_velo, nod_fld1%d_fld, ff_nl, ff)
!
      end subroutine cal_sol_velo_co_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co_crank(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank(node1%numnod, inter_smp_stack, ml_o,   &
     &    nod_fld1%ntot_phys, iphys%i_vecp, nod_fld1%d_fld, ff_nl, ff)
!
      end subroutine cal_sol_vect_p_co_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co_crank(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank(node1%numnod, inter_smp_stack, ml_o,   &
     &    nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld, ff_nl, ff)
!
      end subroutine cal_sol_magne_co_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co_crank_consist(inter_smp_stack)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_vector_co_crank_consist(inter_smp_stack, coef_velo)
!
      end subroutine cal_sol_velo_co_crank_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co_crank_consist(inter_smp_stack)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_vector_co_crank_consist(inter_smp_stack, coef_magne)
!
      end subroutine cal_sol_vect_p_co_crank_consist
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co_crank_consist(inter_smp_stack)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_vector_co_crank_consist(inter_smp_stack, coef_magne)
!
      end subroutine cal_sol_magne_co_crank_consist
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_insulator
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_finite_element_matrix
!
!
      call cal_sol_magne_insulate(node1%numnod, inter_ins_smp_stack,    &
     &    numnod_insulate, inod_insulate, ff, nod_fld1%ntot_phys,       &
     &    iphys%i_magne, nod_fld1%d_fld)
!
      end subroutine cal_sol_magne_insulator
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vector_co_crank_consist                            &
     &        (inter_smp_stack, coef_field)
!
      use m_t_int_parameter
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_ff_smp_to_ffs
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef_field
!
!
      call cal_sol_vect_co_crank_consist                                &
     &   (node1%numnod, inter_smp_stack, ff_nl, ff)
!
      if (coef_field .eq. 0.0d0) return
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    mhd_fem1_wk%ff_m_smp, ff)
!
      end subroutine cal_vector_co_crank_consist
!
! -----------------------------------------------------------------------!
      end module cal_sol_vector_co_crank
