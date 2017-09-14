!
!     module cal_diff_elesize_on_ele
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Mar., 2008
!
!
!!      subroutine cal_filter_moms_ele_by_nod                           &
!!     &         (node, ele, g_FEM, jac_3d, mom_nod, mom_ele)
!!
!!      subroutine cal_diffs_delta_on_element                           &
!!     &         (node, ele, g_FEM, jac_3d, FEM_elen)
!!      subroutine cal_1st_diffs_filter_ele                             &
!!     &         (node, ele, g_FEM, jac_3d, mom_nod, mom_ele)
!!      subroutine cal_2nd_diffs_delta_on_element                       &
!!     &         (node, ele, g_FEM, jac_3d, FEM_elen)
!!      subroutine cal_2nd_diffs_filter_ele                             &
!!     &         (node, ele, g_FEM, jac_3d, mom_nod, mom_ele)
!!        type(node_data),    intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!
      module cal_diff_elesize_on_ele
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
!
      implicit none
!
      private :: take_1st_diffs_ele, take_2nd_diffs_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_filter_moms_ele_by_nod                             &
     &         (node, ele, g_FEM, jac_3d, mom_nod, mom_ele)
!
      use m_ctl_params_4_gen_filter
      use t_filter_moments
      use cal_fields_on_element
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(nod_mom_diffs_type), intent(in) :: mom_nod
!
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_x, mom_ele%moms%f_x)
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_y, mom_ele%moms%f_y)
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_z, mom_ele%moms%f_z)
!
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_x2, mom_ele%moms%f_x2)
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_y2, mom_ele%moms%f_y2)
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_z2, mom_ele%moms%f_z2)
!
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_xy, mom_ele%moms%f_xy)
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_yz, mom_ele%moms%f_yz)
      call scalar_on_element                                            &
     &   (node, ele, g_FEM, jac_3d, ele%istack_ele_smp, num_int_points, &
     &    mom_nod%moms%f_zx, mom_ele%moms%f_zx)
!
      end subroutine cal_filter_moms_ele_by_nod
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_diffs_delta_on_element                             &
     &         (node, ele, g_FEM, jac_3d, FEM_elen)
!
      use t_filter_elength
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
!
!
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%moms%f_x2, FEM_elen%elen_ele%diff%df_x2)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%moms%f_y2, FEM_elen%elen_ele%diff%df_y2)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%moms%f_z2, FEM_elen%elen_ele%diff%df_z2)
!
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%moms%f_xy, FEM_elen%elen_ele%diff%df_xy)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%moms%f_yz, FEM_elen%elen_ele%diff%df_yz)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%moms%f_zx, FEM_elen%elen_ele%diff%df_zx)
!
      end subroutine cal_diffs_delta_on_element
!
!-----------------------------------------------------------------------
!
      subroutine cal_1st_diffs_filter_ele                               &
     &         (node, ele, g_FEM, jac_3d, mom_nod, mom_ele)
!
      use t_filter_moments
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(nod_mom_diffs_type), intent(in) :: mom_nod
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_x,  mom_ele%diff%df_x)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_y,  mom_ele%diff%df_y)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_z,  mom_ele%diff%df_z)
!
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_x2, mom_ele%diff%df_x2)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_y2, mom_ele%diff%df_y2)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_z2, mom_ele%diff%df_z2)
!
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_xy, mom_ele%diff%df_xy)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_yz, mom_ele%diff%df_yz)
      call take_1st_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%moms%f_zx, mom_ele%diff%df_zx)
!
      end subroutine cal_1st_diffs_filter_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_2nd_diffs_delta_on_element                         &
     &         (node, ele, g_FEM, jac_3d, FEM_elen)
!
      use t_filter_elength
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
!
!
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%diff%df_x2, FEM_elen%elen_ele%diff2%df_x2)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%diff%df_y2, FEM_elen%elen_ele%diff2%df_y2)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%diff%df_z2, FEM_elen%elen_ele%diff2%df_z2)
!
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%diff%df_xy, FEM_elen%elen_ele%diff2%df_xy)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%diff%df_yz, FEM_elen%elen_ele%diff2%df_yz)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    FEM_elen%elen_nod%diff%df_zx, FEM_elen%elen_ele%diff2%df_zx)
!
      end subroutine cal_2nd_diffs_delta_on_element
!
!-----------------------------------------------------------------------
!
      subroutine cal_2nd_diffs_filter_ele                               &
     &         (node, ele, g_FEM, jac_3d, mom_nod, mom_ele)
!
      use t_filter_moments
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(nod_mom_diffs_type), intent(in) :: mom_nod
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_x,  mom_ele%diff2%df_x)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_y,  mom_ele%diff2%df_y)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_z,  mom_ele%diff2%df_z)
!
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_x2, mom_ele%diff2%df_x2)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_y2, mom_ele%diff2%df_y2)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_z2, mom_ele%diff2%df_z2)
!
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_xy, mom_ele%diff2%df_xy)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_yz, mom_ele%diff2%df_yz)
      call take_2nd_diffs_ele(node, ele, g_FEM, jac_3d,                 &
     &    mom_nod%diff%df_zx, mom_ele%diff2%df_zx)
!
      end subroutine cal_2nd_diffs_filter_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine take_1st_diffs_ele                                     &
     &         (node, ele, g_FEM, jac_3d, org_nod_field, diff_field)
!
      use m_ctl_params_4_gen_filter
      use cal_differences_on_ele
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      real(kind = kreal), intent(in) :: org_nod_field(node%numnod)
      real(kind = kreal), intent(inout) :: diff_field(ele%numele,3)
      integer(kind=kint) :: nd
!
      do nd = 1, n_vector
        call difference_on_element                                      &
     &     (node, ele, g_FEM, jac_3d, ele%istack_ele_smp,               &
     &      num_int_points, nd, org_nod_field(1), diff_field(1,nd))
      end do
!
      end subroutine take_1st_diffs_ele
!
!-----------------------------------------------------------------------
!
      subroutine take_2nd_diffs_ele                                     &
     &         (node, ele, g_FEM, jac_3d, org_nod_field, diff_field)
!
      use m_ctl_params_4_gen_filter
      use cal_differences_on_ele
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      real(kind = kreal), intent(in) :: org_nod_field(node%numnod,3)
      real(kind = kreal), intent(inout) :: diff_field(ele%numele,3)
      integer(kind=kint) :: nd
!
      do nd = 1, n_vector
        call difference_on_element                                      &
     &     (node, ele, g_FEM, jac_3d, ele%istack_ele_smp,               &
     &      num_int_points, nd, org_nod_field(1,nd), diff_field(1,nd))
      end do
!
      end subroutine take_2nd_diffs_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_diff_elesize_on_ele
