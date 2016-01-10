!
!     module cal_rotation
!
!     Written by H. Matsui
!
!!      subroutine choose_cal_rotation                                  &
!!     &        (iflag_4_supg, i_vector, i_rot, iele_fsmp_stack, m_lump,&
!!     &         nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,       &
!!     &         rhs_tbl, fem_wk, f_nl, nod_fld)
!!      subroutine choose_int_vol_rotations                             &
!!     &         (iflag_4_supg, iele_fsmp_stack, i_vector,              &
!!     &          node, ele, nod_fld, iphys_ele, ele_fld, jac_3d,       &
!!     &          rhs_tbl, fem_wk, f_nl)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) ::    ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_rotation
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine choose_cal_rotation                                    &
     &        (iflag_4_supg, i_vector, i_rot, iele_fsmp_stack, m_lump,  &
     &         nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,         &
     &         rhs_tbl, fem_wk, f_nl, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) ::    ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer(kind = kint), intent(in) :: iflag_4_supg, i_vector, i_rot
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call choose_int_vol_rotations(iflag_4_supg, iele_fsmp_stack,      &
     &    i_vector, node, ele, nod_fld, iphys_ele, ele_fld,            &
     &    jac_3d, rhs_tbl, fem_wk, f_nl)
!
      call cal_ff_smp_2_vector(node, rhs_tbl, f_nl%ff_smp,              &
     &    m_lump%ml, nod_fld%ntot_phys, i_rot, nod_fld%d_fld)
!
! ----------   communications
      call vector_send_recv(i_rot, node, nod_comm, nod_fld)
!
      end subroutine choose_cal_rotation
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_rotations                               &
     &         (iflag_4_supg, iele_fsmp_stack, i_vector,                &
     &          node, ele, nod_fld, iphys_ele, ele_fld, jac_3d,         &
     &          rhs_tbl, fem_wk, f_nl)
!
      use int_vol_vect_differences
      use int_vol_vect_diff_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) ::    ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iflag_4_supg, i_vector
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      call reset_ff_smp(node%max_nod_smp, f_nl)
!
       if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_rotation_upw(node, ele, jac_3d, rhs_tbl, nod_fld,  &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      fem_wk, f_nl)
       else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_rotation_upw(node, ele, jac_3d, rhs_tbl, nod_fld,  &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      fem_wk, f_nl)
       else
        call int_vol_rotation(node, ele, jac_3d, rhs_tbl, nod_fld,      &
     &      iele_fsmp_stack, intg_point_t_evo, i_vector,                &
     &      fem_wk, f_nl)
       end if
!
      end subroutine choose_int_vol_rotations
!
!-----------------------------------------------------------------------
!
      end module cal_rotation
