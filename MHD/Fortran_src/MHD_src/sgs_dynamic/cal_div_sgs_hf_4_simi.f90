!
!     module cal_div_sgs_hf_4_simi
!
!     Written by H. Matsui
!
!!      subroutine cal_div_sgs_hf_simi(i_sgs, i_flux, i_vect, i_scalar, &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                  &
!!     &          f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_div_sgs_hf_4_simi
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_constants
!
      use t_geometry_data_MHD
      use t_comm_table
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_hf_simi(i_sgs, i_flux, i_vect, i_scalar,   &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                    &
     &          f_l, f_nl, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_div_sgs_hf_simi
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
      integer(kind = kint), intent(in) :: i_sgs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
       call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
        if ( iflag_temp_supg .gt. id_turn_OFF) then
          call int_div_sgs_hf_simi_upw(i_flux, i_vect, i_scalar,        &
     &        node, ele, fluid, nod_fld, jac_3d, rhs_tbl,               &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        fem_wk, f_nl)
        else
          call int_div_sgs_hf_simi_pg(i_flux, i_vect, i_scalar,         &
     &        node, ele, fluid, nod_fld, jac_3d, rhs_tbl, fem_wk, f_nl)
        end if
!
       call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
       call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,           &
     &     f_nl%ff, mhd_fem_wk%mlump_fl%ml, nod_fld%ntot_phys,          &
     &     i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call scalar_send_recv(i_sgs, node, nod_comm, nod_fld)
!
      end subroutine cal_div_sgs_hf_simi
!
!-----------------------------------------------------------------------
!
       end module cal_div_sgs_hf_4_simi
