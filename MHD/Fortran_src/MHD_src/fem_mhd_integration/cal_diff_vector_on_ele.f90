!
!     module cal_diff_vector_on_ele
!
!     Written by H.Matsui
!
!!      subroutine sel_int_diff_vector_on_ele                           &
!!     &         (num_int, iele_fsmp_stack, i_fld, i_diff, node, ele,   &
!!     &          nod_fld, jacs, mhd_fem_wk)
!!      subroutine sel_int_diff_scalar_on_ele                           &
!!     &         (num_int, iele_fsmp_stack, i_fld, i_diff, node, ele,   &
!!     &          nod_fld, jacs, mhd_fem_wk)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module cal_diff_vector_on_ele
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_phys_data
      use t_jacobians
      use t_MHD_finite_element_mat
!
      use int_differenciate_on_ele
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_int_diff_vector_on_ele                             &
     &         (num_int, iele_fsmp_stack, i_fld, i_diff, node, ele,     &
     &          nod_fld, jacs, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_fld, i_diff
      integer (kind=kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_diff_vector_on_ele                                     &
     &     (node, ele, jacs%g_FEM, jacs%jac_3d,                         &
     &      iele_fsmp_stack, num_int,                                   &
     &      nod_fld%ntot_phys, i_fld, nod_fld%d_fld,                    &
     &      mhd_fem_wk%n_dvx, i_diff, mhd_fem_wk%dvx)
      else
        call int_diff_vector_on_ele                                     &
     &     (node, ele, jacs%g_FEM, jacs%jac_3d_l,                       &
     &      iele_fsmp_stack, num_int,                                   &
     &      nod_fld%ntot_phys, i_fld, nod_fld%d_fld,                    &
     &      mhd_fem_wk%n_dvx, i_diff, mhd_fem_wk%dvx)
      end if
!
      end subroutine sel_int_diff_vector_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine sel_int_diff_scalar_on_ele                             &
     &         (num_int, iele_fsmp_stack, i_fld, i_diff, node, ele,     &
     &          nod_fld, jacs, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_fld, i_diff
      integer (kind=kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_diff_scalar_on_ele                                     &
     &     (node, ele, jacs%g_FEM, jacs%jac_3d,                         &
     &      iele_fsmp_stack, num_int,                                   &
     &      nod_fld%ntot_phys, i_fld, nod_fld%d_fld,                    &
     &      mhd_fem_wk%n_dvx, i_diff, mhd_fem_wk%dvx)
      else
        call int_diff_scalar_on_ele                                     &
     &     (node, ele, jacs%g_FEM, jacs%jac_3d_l,                       &
     &      iele_fsmp_stack, num_int,                                   &
     &      nod_fld%ntot_phys, i_fld, nod_fld%d_fld,                    &
     &      mhd_fem_wk%n_dvx, i_diff, mhd_fem_wk%dvx)
      end if
!
      end subroutine sel_int_diff_scalar_on_ele
!
! -----------------------------------------------------------------------
!
      end module cal_diff_vector_on_ele
