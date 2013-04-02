!int_vol_inertia_1st.f90
!     module int_vol_inertia_1st
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_scalar_inertia_1st(iele_fsmp_stack,           &
!     &          n_int, i_scalar, vxe, coef)
!      subroutine int_vol_vector_inertia_1st(iele_fsmp_stack,           &
!     &          n_int, i_vector, vxe, coef)
!
!      subroutine int_vol_rot_inertia_1st(iele_fsmp_stack,              &
!     &          n_int, i_vector, wxe, coef)
!
!      subroutine int_vol_scalar_inertia_upw_1st(iele_fsmp_stack,       &
!     &          n_int, i_scalar, vxe, vxe_up, coef)
!      subroutine int_vol_vector_inertia_upw_1st(iele_fsmp_stack,       &
!     &          n_int, i_vector, vxe, vxe_up, coef)
!
!      subroutine int_vol_rot_inertia_upw_1st(iele_fsmp_stack,          &
!     &          n_int, i_vector, wxe, vxe_up, coef)
!
      module int_vol_inertia_1st
!
      use m_precision
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia_1st(iele_fsmp_stack,            &
     &          n_int, i_scalar, vxe, coef)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_cst_to_ele_1st
      use fem_skv_nonlinear_1st
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(numele,3)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the physical values
      do k2 = 1, nnod_4_ele
        call scalar_cst_phys_2_each_ele(k2, i_scalar, coef, phi_e)
        call fem_skv_scalar_inertia_1st(iele_fsmp_stack, n_int, k2,     &
     &      phi_e, vxe, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_scalar_inertia_1st
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia_1st(iele_fsmp_stack,            &
     &          n_int, i_vector, vxe, coef)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_cst_to_ele_1st
      use fem_skv_nonlinear_1st
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(numele,3)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the physical values
      do k2 = 1, nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, i_vector, coef, velo_1)
        call fem_skv_vector_inertia_1st(iele_fsmp_stack, n_int, k2,     &
     &      velo_1, vxe, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_vector_inertia_1st
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia_1st(iele_fsmp_stack,               &
     &          n_int, i_vector, wxe, coef)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_cst_to_ele_1st
      use fem_skv_nonlinear_1st
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: wxe(numele,3)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the physical values
      do k2 = 1, nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, i_vector, coef, velo_1)
        call fem_skv_rot_inertia_1st(iele_fsmp_stack, n_int, k2,        &
     &      velo_1, wxe, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_rot_inertia_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia_upw_1st(iele_fsmp_stack,        &
     &          n_int, i_scalar, vxe, vxe_up, coef)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_cst_to_ele_1st
      use fem_skv_nonlinear_upw_1st
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: vxe_up(numele,3)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the physical values
      do k2 = 1, nnod_4_ele
        call scalar_cst_phys_2_each_ele(k2, i_scalar, coef, phi_e)
        call fem_skv_scalar_inertia_upw_1st(iele_fsmp_stack, n_int, k2, &
     &      phi_e, vxe, vxe_up, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_scalar_inertia_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia_upw_1st(iele_fsmp_stack,        &
     &          n_int, i_vector, vxe, vxe_up, coef)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_cst_to_ele_1st
      use fem_skv_nonlinear_upw_1st
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: vxe_up(numele,3)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the physical values
      do k2 = 1, nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, i_vector, coef, velo_1)
        call fem_skv_vector_inertia_upw_1st(iele_fsmp_stack, n_int, k2, &
     &      velo_1, vxe, vxe_up, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_vector_inertia_upw_1st
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia_upw_1st(iele_fsmp_stack,           &
     &          n_int, i_vector, wxe, vxe_up, coef)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_cst_to_ele_1st
      use fem_skv_nonlinear_upw_1st
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: wxe(numele,3)
      real(kind=kreal), intent(in) :: vxe_up(numele,3)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the physical values
      do k2 = 1, nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, i_vector, coef, velo_1)
        call fem_skv_rot_inertia_upw_1st(iele_fsmp_stack, n_int, k2,    &
     &      velo_1, wxe, vxe_up, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_rot_inertia_upw_1st
!
! ----------------------------------------------------------------------
!
      end module int_vol_inertia_1st
