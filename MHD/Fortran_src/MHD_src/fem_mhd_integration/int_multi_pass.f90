!
!      module int_multi_pass
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!!      subroutine int_multi_pass_vector(iele_fsmp_stack, m_lump,       &
!!     &          nod_comm, node, ele, jac_3d, rhs_tbl, ff_m_smp,       &
!!     &          fem_wk, f_nl)
!!      subroutine int_multi_pass_scalar(iele_fsmp_stack, m_lump,       &
!!     &          nod_comm, node, ele, jac_3d, rhs_tbl, ff_m_smp,       &
!!     &          fem_wk, f_nl)
!!
!!      subroutine int_multi_pass_vector_upw                            &
!!     &          (iele_fsmp_stack, m_lump, iphys_upw,                  &
!!     &           nod_comm, node, ele, fld_ele, jac_3d, rhs_tbl,       &
!!     &           ff_m_smp, fem_wk, f_nl)
!!      subroutine int_multi_pass_scalar_upw                            &
!!     &         (iele_fsmp_stack, m_lump, iphys_upw,                   &
!!     &          nod_comm, node, ele, fld_ele, jac_3d, rhs_tbl,        &
!!     &          ff_m_smp, fem_wk, f_nl)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(phys_data), intent(in) :: fld_ele
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_multi_pass
!
      use m_precision
      use m_constants
!
      use m_control_parameter
      use m_phys_constants
!
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_jacobians
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
      subroutine int_multi_pass_vector(iele_fsmp_stack, m_lump,         &
     &          nod_comm, node, ele, jac_3d, rhs_tbl, ff_m_smp,         &
     &          fem_wk, f_nl)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node%numnod, f_nl)
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call nod_vector_send_recv(node, nod_comm, f_nl%ff)
!
        call int_vol_multi_pass_vector(iele_fsmp_stack,                 &
     &      node, ele, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_vector
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar(iele_fsmp_stack, m_lump,         &
     &          nod_comm, node, ele, jac_3d, rhs_tbl, ff_m_smp,         &
     &          fem_wk, f_nl)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node%numnod, f_nl)
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call scalar_fld_send_recv                                       &
     &     (node, nod_comm, n_vector, ione, f_nl%ff)
!
        call int_vol_multi_pass_scalar(iele_fsmp_stack,                 &
     &      node, ele, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_scalar
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_upw                              &
     &          (iele_fsmp_stack, m_lump, iphys_upw,                    &
     &           nod_comm, node, ele, fld_ele, jac_3d, rhs_tbl,         &
     &           ff_m_smp, fem_wk, f_nl)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(phys_data), intent(in) :: fld_ele
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: iphys_upw
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node%numnod, f_nl)
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_vector(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call nod_vector_send_recv(node, nod_comm, f_nl%ff)
!
        call int_vol_multi_pass_vector_upw(iele_fsmp_stack,             &
     &      node, ele, jac_3d, rhs_tbl,                                 &
     &      fld_ele%ntot_phys, iphys_upw, fld_ele%d_fld,                &
     &      ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_vector_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_upw                              &
     &         (iele_fsmp_stack, m_lump, iphys_upw,                     &
     &          nod_comm, node, ele, fld_ele, jac_3d, rhs_tbl,          &
     &          ff_m_smp, fem_wk, f_nl)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(phys_data), intent(in) :: fld_ele
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: iphys_upw
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(node%numnod, f_nl)
!
      do imulti = 2, num_multi_pass
        call cal_ff_smp_2_scalar(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call scalar_fld_send_recv                                       &
     &     (node, nod_comm, n_vector, ione, f_nl%ff)
!
        call int_vol_multi_pass_scalar_upw(iele_fsmp_stack,             &
     &      node, ele, jac_3d, rhs_tbl,                                 &
     &      fld_ele%ntot_phys, iphys_upw, fld_ele%d_fld,                &
     &      ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_scalar_upw
!
!-----------------------------------------------------------------------
!
      end module int_multi_pass
