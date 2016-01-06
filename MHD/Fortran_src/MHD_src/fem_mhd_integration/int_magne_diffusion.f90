!
!      module int_magne_diffusion
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine s_int_magne_diffusion
!!     &         (nod_comm, node, ele, iphys, jac_3d, rhs_tbl,          &
!!     &          mhd_fem_wk, fem_wk, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!
      module int_magne_diffusion
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_magne_diffusion                                  &
     &         (nod_comm, node, ele, iphys, jac_3d, rhs_tbl,            &
     &          mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      use m_control_parameter
!
      use int_vol_vect_differences
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!  ---------  set number of integral points
!
      call reset_ff_smp(node%max_nod_smp, f_nl)
!
      call int_vol_rotation(node, ele, jac_3d, rhs_tbl, nod_fld, &
     &    ele%istack_ele_smp, intg_point_poisson, iphys%i_vp_diffuse,  &
     &    fem_wk, f_nl)
!
!      call cal_multi_pass_4_vector_ff(ele%istack_ele_smp, m1_lump,    &
!     &    nod_comm, node, ele, jac_3d, rhs_tbl,                  &
!     &    mhd_fem_wk%ff_m_smp, fem_wk, f1_l, f_nl)
!      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,         &
!     &   ff, mhd_fem_wk%mlump_cd%ml, nod_fld%ntot_phys,              &
!     &   iphys%i_magne, nod_fld%d_fld)
       call cal_ff_smp_2_vector(node, rhs_tbl,                        &
     &     f_nl%ff_smp, mhd_fem_wk%mlump_cd%ml,                       &
     &     nod_fld%ntot_phys, iphys%i_b_diffuse, nod_fld%d_fld) 
!
       call vector_send_recv                                            &
     &    (iphys%i_b_diffuse, node, nod_comm, nod_fld)
!
      end subroutine s_int_magne_diffusion
!
! ----------------------------------------------------------------------
!
      end module int_magne_diffusion
