!
!     module cal_rotation
!
!     Written by H. Matsui
!
!!      subroutine choose_cal_rotation(iflag_4_supg,                    &
!!     &          iele_fsmp_stack, m_lump, node, ele, i_vector, i_rot)
!!      subroutine choose_int_vol_rotations(iflag_4_supg,               &
!!     &          iele_fsmp_stack, node, ele, i_vector)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!
      module cal_rotation
!
      use m_precision
!
      use t_geometry_data
!
      use m_phys_constants
      use m_nod_comm_table
      use m_node_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine choose_cal_rotation(iflag_4_supg,                      &
     &          iele_fsmp_stack, m_lump, node, ele, i_vector, i_rot)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer(kind = kint), intent(in) :: iflag_4_supg, i_vector, i_rot
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      call choose_int_vol_rotations(iflag_4_supg, iele_fsmp_stack,      &
     &    node, ele, i_vector)
!
      call cal_ff_smp_2_vector(node, rhs_tbl1, f1_nl%ff_smp,            &
     &    m_lump%ml, nod_fld1%ntot_phys, i_rot, nod_fld1%d_fld)
!
! ----------   communications
      call vector_send_recv(i_rot, node, nod_comm, nod_fld1)
!
      end subroutine choose_cal_rotation
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_rotations(iflag_4_supg,                 &
     &          iele_fsmp_stack, node, ele, i_vector)
!
      use m_control_parameter
      use m_element_phys_data
!
      use int_vol_vect_differences
      use int_vol_vect_diff_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: iflag_4_supg, i_vector
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      call reset_ff_smp(node%max_nod_smp, f1_nl)
!
       if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_rotation_upw                                       &
     &     (node, ele, jac1_3d_q, rhs_tbl1, nod_fld1,                   &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,      &
     &      fem1_wk, f1_nl)
       else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_rotation_upw                                       &
     &     (node, ele, jac1_3d_q, rhs_tbl1, nod_fld1,                   &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      fem1_wk, f1_nl)
       else
        call int_vol_rotation                                           &
     &     (node, ele, jac1_3d_q, rhs_tbl1, nod_fld1,                   &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      fem1_wk, f1_nl)
       end if
!
      end subroutine choose_int_vol_rotations
!
!-----------------------------------------------------------------------
!
      end module cal_rotation
