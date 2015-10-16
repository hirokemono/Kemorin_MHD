!int_vol_mass_matrix.f90
!      module int_vol_mass_matrix
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!      subroutine int_lumped_mass_matrix(num_int)
!
!      subroutine int_lump_mass_matrix_linear(num_int)
!      subroutine int_lump_mass_matrix_quad(num_int)
!
!      subroutine int_consist_mass_matrix(iele_fsmp_stack, num_int,     &
!     &          nmat_size, aiccg)
!
!      subroutine int_mass_matrix(iele_fsmp_stack, num_int)
!      subroutine int_mass_matrix_diag(iele_fsmp_stack, num_int)
!      subroutine int_mass_matrix_HRZ_full(iele_fsmp_stack, num_int)
!      subroutine int_mass_matrix_HRZ(iele_fsmp_stack, num_int)
!
      module int_vol_mass_matrix
!
      use m_precision
!
      use m_geometry_data
      use m_jacobians
      use m_phys_constants
      use m_sorted_node
      use m_finite_element_matrix
!
      use cal_ff_smp_to_ffs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_lumped_mass_matrix(num_int)
!
      use m_geometry_constants
!
      integer (kind=kint), intent(in) :: num_int
!
!
      if     (ele1%nnod_4_ele.eq.num_t_quad                             &
     &   .or. ele1%nnod_4_ele.eq.num_t_lag) then
        call int_lump_mass_matrix_quad(num_int)
      else
        call int_lump_mass_matrix_linear(num_int)
      end if
!
      end subroutine int_lumped_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_lump_mass_matrix_linear(num_int)
!
      integer (kind=kint), intent(in) :: num_int
!
!
      call int_mass_matrix_diag(ele1%istack_ele_smp, num_int)
      call cal_ff_smp_2_ml                                              &
     &   (node1, rhs_tbl1, f1_l%ff_smp, m1_lump%ml, m1_lump%ml_o)
!
!      call check_mass_martix(my_rank, node1%numnod, m1_lump)
!
      end subroutine int_lump_mass_matrix_linear
!
!-----------------------------------------------------------------------
!
      subroutine int_lump_mass_matrix_quad(num_int)
!
      integer (kind=kint), intent(in) :: num_int
!
!
      call int_mass_matrix_HRZ_full(ele1%istack_ele_smp, num_int)
      call cal_ff_smp_2_ml                                              &
     &   (node1, rhs_tbl1, f1_l%ff_smp, m1_lump%ml, m1_lump%ml_o)
!
!      call check_mass_martix(my_rank, node1%numnod, m1_lump)
!
      end subroutine int_lump_mass_matrix_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_consist_mass_matrix(iele_fsmp_stack, num_int,      &
     &          nmat_size, aiccg)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      integer (kind = kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
        call fem_skv_mass_matrix_type(iele_fsmp_stack, num_int, k2,     &
     &      ele1, jac1_3d_q, fem1_wk%sk6)
        call add_skv1_to_crs_matrix11(ele1, rhs_tbl1, mat_tbl_q1,       &
     &      k2, fem1_wk%sk6, nmat_size, aiccg)
      end do
!
      end subroutine int_consist_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix(iele_fsmp_stack, num_int)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint) :: k2
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smp(node1%max_nod_smp, f1_l)
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call fem_skv_mass_matrix_type(iele_fsmp_stack, num_int, k2,     &
     &      ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_diag(iele_fsmp_stack, num_int)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      call reset_ff_smp(node1%max_nod_smp, f1_l)
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_skv_mass_matrix_diag_type                                &
     &   (iele_fsmp_stack, num_int, ele1, jac1_3d_q, fem1_wk%sk6)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_mass_matrix_diag
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_HRZ_full(iele_fsmp_stack, num_int)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      call reset_ff_smp(node1%max_nod_smp, f1_l)
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_skv_mass_mat_diag_HRZ_type                               &
     &   (iele_fsmp_stack, num_int, ele1, jac1_3d_q, fem1_wk%sk6)
      call sum_skv_diagonal_4_HRZ_type(iele_fsmp_stack, ele1,           &
     &    fem1_wk%sk6, fem1_wk%me_diag)
!
      call vol_average_skv_HRZ_type                                     &
     &   (iele_fsmp_stack, ele1, fem1_wk%sk6, fem1_wk%me_diag)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_mass_matrix_HRZ_full
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_HRZ(iele_fsmp_stack, num_int)
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      call reset_ff_smp(node1%max_nod_smp, f1_l)
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_skv_mass_mat_diag_HRZ_type                               &
     &   (iele_fsmp_stack, num_int, ele1, jac1_3d_q, fem1_wk%sk6)
      call vol_average_skv_HRZ_type                                     &
     &   (iele_fsmp_stack, ele1, fem1_wk%sk6, fem1_wk%me_diag)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_l%ff_smp)
!
      end subroutine int_mass_matrix_HRZ
!
!-----------------------------------------------------------------------
!
      end module int_vol_mass_matrix
