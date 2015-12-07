!
!     module cal_rotation
!
!     Written by H. Matsui
!
!      subroutine cal_rotation_whole(iflag_4_supg, i_res, i_vector)
!      subroutine cal_rotation_in_fluid(iflag_4_supg, i_res, i_vector)
!      subroutine cal_rotation_in_conduct(iflag_4_supg,                 &
!     &          i_res, i_vector)
!
!      private :: choose_int_vol_rotations
!
      module cal_rotation
!
      use m_precision
!
      use m_phys_constants
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_jacobians
      use m_sorted_node
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
      subroutine cal_rotation_whole(iflag_4_supg, i_res, i_vector)
!
      use m_sorted_node
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call choose_int_vol_rotations(iflag_4_supg,                       &
     &     ele1%istack_ele_smp, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, m1_lump%ml, nod_fld1%ntot_phys,                     &
     &    i_res, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_res, node1, nod_comm, nod_fld1)
!
      end subroutine cal_rotation_whole
!
!-----------------------------------------------------------------------
!
      subroutine cal_rotation_in_fluid(iflag_4_supg, i_res, i_vector)
!
      use m_geometry_data_MHD
      use m_sorted_node
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call choose_int_vol_rotations(iflag_4_supg,                       &
     &    iele_fl_smp_stack, i_vector)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_fl%ml, nod_fld1%ntot_phys,        &
     &    i_res, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_res, node1, nod_comm, nod_fld1)
!
      end subroutine cal_rotation_in_fluid
!
!-----------------------------------------------------------------------
!------- subroutine cal_rotation_in_conduct ---------------------
!
      subroutine cal_rotation_in_conduct(iflag_4_supg, i_res, i_vector)
!
      use m_geometry_data_MHD
      use m_sorted_node
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
!
!
       call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
       call choose_int_vol_rotations(iflag_4_supg,                      &
     &     iele_cd_smp_stack, i_vector)
!
       call cal_ff_smp_2_vector(node1, rhs_tbl1,                        &
     &     f1_nl%ff_smp, mhd_fem1_wk%mlump_cd%ml, nod_fld1%ntot_phys,   &
     &     i_res, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_res, node1, nod_comm, nod_fld1)
!
      end subroutine cal_rotation_in_conduct
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_rotations(iflag_4_supg,                 &
     &          iele_fsmp_stack, i_vector)
!
      use m_control_parameter
      use m_element_phys_data
!
      use int_vol_vect_differences
      use int_vol_vect_diff_upw
!
       integer(kind = kint), intent(in) :: iflag_4_supg, i_vector
       integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
       if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_rotation_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,      &
     &      fem1_wk, f1_nl)
       else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_rotation_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      fem1_wk, f1_nl)
       else
        call int_vol_rotation                                           &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      fem1_wk, f1_nl)
       end if
!
      end subroutine choose_int_vol_rotations
!
!-----------------------------------------------------------------------
!
      end module cal_rotation
