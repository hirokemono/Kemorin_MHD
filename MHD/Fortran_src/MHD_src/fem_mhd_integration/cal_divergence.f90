!
!     module cal_divergence
!
!     Written by H. Matsui
!
!!      subroutine choose_cal_divergence(iflag_4_supg, num_int, dt,     &
!!     &          i_vector, i_div, iele_fsmp_stack, m_lump,             &
!!     &          nod_comm, node, ele, iphys_ele, ele_fld,              &
!!     &          g_FEM, jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_divergence
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_geometry_data
      use t_phys_data
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
      private :: choose_int_vol_divs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine choose_cal_divergence(iflag_4_supg, num_int, dt,       &
     &          i_vector, i_div, iele_fsmp_stack, m_lump,               &
     &          nod_comm, node, ele, iphys_ele, ele_fld,                &
     &          g_FEM, jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iflag_4_supg, num_int
      integer(kind = kint), intent(in) :: i_vector, i_div
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
       call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
       call choose_int_vol_divs                                         &
     &    (iflag_4_supg, num_int, dt, iele_fsmp_stack, i_vector,        &
     &     node, ele, nod_fld, iphys_ele, ele_fld,                      &
     &     g_FEM, jac_3d, rhs_tbl, fem_wk, f_nl)
!
       call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
       call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,           &
     &     f_nl%ff, m_lump%ml, nod_fld%ntot_phys,                       &
     &     i_div, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_div, nod_comm, nod_fld)
!
      end subroutine choose_cal_divergence
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_divs                                    &
     &         (iflag_4_supg, num_int, dt, iele_fsmp_stack, i_vector,   &
     &          node, ele, nod_fld, iphys_ele, ele_fld,                 &
     &          g_FEM, jac_3d, rhs_tbl, fem_wk, f_nl)
!
      use int_vol_vect_differences
      use int_vol_vect_diff_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: i_vector
      integer(kind = kint), intent(in) :: iflag_4_supg, num_int
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_divergence_upw                                     &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      iele_fsmp_stack, num_int, dt, i_vector,                     &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      fem_wk, f_nl)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_divergence_upw                                     &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      iele_fsmp_stack, num_int, dt, i_vector,                     &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      fem_wk, f_nl)
      else
        call int_vol_divergence                                         &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      iele_fsmp_stack, num_int, i_vector, fem_wk, f_nl)
      end if
!
      end subroutine choose_int_vol_divs
!
!-----------------------------------------------------------------------
!
      end module cal_divergence
