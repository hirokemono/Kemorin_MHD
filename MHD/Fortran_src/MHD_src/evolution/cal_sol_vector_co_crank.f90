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
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
!
      call cal_sol_velocity_co(node1%numnod, inter_smp_stack,           &
     &    nod_fld1%ntot_phys, iphys%i_velo, iphys%i_p_phi, d_nod)
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
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vector_co(node1%numnod, inter_smp_stack,             &
     &    nod_fld1%ntot_phys, iphys%i_vecp, d_nod)
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
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vector_co(node1%numnod, inter_smp_stack,             &
     &    nod_fld1%ntot_phys, iphys%i_magne, d_nod)
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
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
!
      call cal_sol_velo_co_crank_lump(node1%numnod, inter_smp_stack,    &
     &    nod_fld1%ntot_phys, iphys%i_velo, d_nod)
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
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank(node1%numnod, inter_smp_stack,         &
     &    nod_fld1%ntot_phys, iphys%i_vecp, d_nod)
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
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank(node1%numnod, inter_smp_stack,         &
     &    nod_fld1%ntot_phys, iphys%i_magne, d_nod)
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
      call cal_sol_vect_co_crank_consist(inter_smp_stack, coef_velo)
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
      call cal_sol_vect_co_crank_consist(inter_smp_stack, coef_magne)
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
      call cal_sol_vect_co_crank_consist(inter_smp_stack, coef_magne)
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
!
!
      call cal_sol_magne_insulate(node1%numnod, inter_ins_smp_stack,    &
     &    numnod_insulate, inod_insulate, nod_fld1%ntot_phys,           &
     &    iphys%i_magne, d_nod)
!
      end subroutine cal_sol_magne_insulator
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_co_crank
