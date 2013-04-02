!fem_skv_mass_mat_type.f90
!      module fem_skv_mass_mat_type
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on March, 2006
!     Modified by H. Matsui on March, 2009
!
!      subroutine fem_skv_mass_matrix_type(iele_fsmp_stack,             &
!     &          num_int, k2, ele, jac_3d, fem_wk)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer (kind=kint), intent(in) :: num_int, k2
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
!      subroutine fem_skv_mass_matrix_diag_type(iele_fsmp_stack,        &
!     &          num_int, ele, jac_3d, fem_wk)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
!      subroutine fem_skv_mass_mat_diag_HRZ_type(iele_fsmp_stack,       &
!     &          num_int, ele, jac_3d, fem_wk)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer (kind=kint), intent(in) :: num_int
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
!      subroutine sum_skv_diagonal_4_HRZ_type(iele_fsmp_stack, ele,     &
!     &          fem_wk, ele_diag)
!        type(element_data), intent(in) :: ele
!        type(work_finite_element_mat), intent(in) :: fem_wk
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(lumped_mass_mat_node), intent(inout) :: ele_diag
!
!      subroutine vol_average_skv_HRZ_type(iele_fsmp_stack, ele,        &
!     &          fem_wk, ele_diag)
!        type(element_data), intent(in) :: ele
!        type(lumped_mass_mat_node), intent(in) :: ele_diag
!        integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
!      subroutine fem_grp_skv_mass_matrix_t(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, num_int, k2, ele, jac_3d, fem_wk)
!      subroutine fem_grp_skv_mass_matrix_diag_t(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, num_int, ele, jac_3d, fem_wk)
!      subroutine fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,      &
!     &          nele_grp, iele_grp, num_int, ele, jac_3d, fem_wk)
!
!      subroutine grp_volume_average_skv_HRZ_t(iele_fsmp_stack,         &
!     &          nele_grp, iele_grp, ele, fem_wk, ele_diag)
!
      module fem_skv_mass_mat_type
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use m_machine_parameter
      use m_fem_gauss_int_coefs
!
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix_type(iele_fsmp_stack,              &
     &          num_int, k2, ele, jac_3d, fem_wk)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: num_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_mass_matrix                                          &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,          &
     &     iele_fsmp_stack, jac_3d%ntot_int, num_int,                   &
     &     jac_3d%xjac, jac_3d%an, jac_3d%an, k2, fem_wk%sk6)
!
      end  subroutine fem_skv_mass_matrix_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_matrix_diag_type(iele_fsmp_stack,         &
     &          num_int, ele, jac_3d, fem_wk)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_mass_matrix_diag(ele%numele, ele%nnod_4_ele,         &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, num_int,            &
     &    jac_3d%xjac, jac_3d%an, fem_wk%sk6)
!
      end  subroutine fem_skv_mass_matrix_diag_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_mass_mat_diag_HRZ_type(iele_fsmp_stack,        &
     &          num_int, ele, jac_3d, fem_wk)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_mass_mat_diag_HRZ(ele%numele, ele%nnod_4_ele,        &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, num_int,            &
     &    jac_3d%an, jac_3d%xjac, fem_wk%sk6)
!
      end  subroutine fem_skv_mass_mat_diag_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine sum_skv_diagonal_4_HRZ_type(iele_fsmp_stack, ele,      &
     &          fem_wk, ele_diag)
!
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(work_finite_element_mat), intent(in) :: fem_wk
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(lumped_mass_mat_node), intent(inout) :: ele_diag
!
!
      call sum_skv_diagonal_4_HRZ(ele%numele, ele%nnod_4_ele, np_smp,   &
     &    iele_fsmp_stack, fem_wk%sk6, ele_diag%ml_o, ele_diag%ml)
!
      end subroutine sum_skv_diagonal_4_HRZ_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine vol_average_skv_HRZ_type(iele_fsmp_stack, ele,         &
     &          fem_wk, ele_diag)
!
      use m_geometry_data
      use fem_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(lumped_mass_mat_node), intent(in) :: ele_diag
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call volume_average_skv_HRZ(ele%numele, ele%nnod_4_ele, np_smp,   &
     &    iele_fsmp_stack, ele%volume_ele, fem_wk%sk6, ele_diag%ml)
!
      end subroutine vol_average_skv_HRZ_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_matrix_t(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, num_int, k2, ele, jac_3d, fem_wk)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_grp_skv_mass_matrix(ele%numele, ele%nnod_4_ele,          &
     &    ele%nnod_4_ele, np_smp, iele_fsmp_stack, nele_grp, iele_grp,  &
     &    jac_3d%ntot_int, num_int, jac_3d%xjac, jac_3d%an, jac_3d%an,  &
     &    k2, fem_wk%sk6)
!
      end  subroutine fem_grp_skv_mass_matrix_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_matrix_diag_t(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, num_int, ele, jac_3d, fem_wk)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_grp_skv_mass_matrix_diag(ele%numele, ele%nnod_4_ele,     &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    jac_3d%ntot_int, num_int, jac_3d%xjac, jac_3d%an, fem_wk%sk6)
!
      end  subroutine fem_grp_skv_mass_matrix_diag_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_grp_skv_mass_mat_diag_HRZ_t(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, num_int, ele, jac_3d, fem_wk)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: num_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_grp_skv_mass_mat_diag_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    jac_3d%ntot_int, num_int, jac_3d%xjac, jac_3d%an, fem_wk%sk6)
!
      end  subroutine fem_grp_skv_mass_mat_diag_HRZ_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine grp_volume_average_skv_HRZ_t(iele_fsmp_stack,          &
     &          nele_grp, iele_grp, ele, fem_wk, ele_diag)
!
      use fem_grp_skv_mass_mat
!
      type(element_data), intent(in) :: ele
      type(lumped_mass_mat_node), intent(in) :: ele_diag
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call grp_volume_average_skv_HRZ(ele%numele, ele%nnod_4_ele,       &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, ele%volume_ele,  &
     &    fem_wk%sk6, ele_diag%ml)
!
      end subroutine grp_volume_average_skv_HRZ_t
!
!-----------------------------------------------------------------------
!
      end module  fem_skv_mass_mat_type
