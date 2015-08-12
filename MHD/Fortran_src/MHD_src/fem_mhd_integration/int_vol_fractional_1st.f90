!
!     module int_vol_fractional_1st
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_div_vect_linear_1st(iele_fsmp_stack,          &
!     &          n_int, i_vector)
!      subroutine int_vol_solenoidal_co_1st(iele_fsmp_stack,            &
!     &          n_int, i_scalar)
!
!      subroutine int_vol_scalar_diffuse_1st(iele_fsmp_stack,           &
!     &          n_int, coef_crank, ak_d, i_scalar)
!      subroutine int_vol_vector_diffuse_1st(iele_fsmp_stack,           &
!     &          n_int, coef_crank, ak_d, i_vector)
!
      module int_vol_fractional_1st
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_vect_linear_1st(iele_fsmp_stack,           &
     &          n_int, i_vector)
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_vector_diff_1st
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the physical values
      do k2=1, nnod_4_ele
        call vector_phys_2_each_element(k2, i_vector, velo_1)
        call fem_skv_div_to_linear(iele_fsmp_stack, n_int, k2,          &
     &     velo_1, sk6)
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_vol_div_vect_linear_1st
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_solenoidal_co_1st(iele_fsmp_stack,             &
     &          n_int, i_scalar)
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_vector_diff_1st
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the physical values
      do k2=1, num_t_linear
        call scalar_phys_2_each_element(k2, i_scalar, phi_e)
        call fem_skv_linear_gradient(iele_fsmp_stack, n_int, k2,        &
     &      phi_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_solenoidal_co_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_scalar_diffuse_1st(iele_fsmp_stack,            &
     &          n_int, coef_crank, ak_d, i_scalar)
!
      use m_geometry_data
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_diffusion_1st
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_scalar
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the physical values
      do k2 = 1, nnod_4_ele
        call scalar_phys_2_each_element(k2, i_scalar, phi_e)
        call fem_skv_scalar_diffuse_1st(iele_fsmp_stack, n_int, k2,     &
     &      ak_d, phi_e, sk6)
      end do
!
      call add1_skv_coef_to_ff_v_smp_1st(coef_crank, ff_smp, sk6)
!
      end subroutine int_vol_scalar_diffuse_1st
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_diffuse_1st(iele_fsmp_stack,            &
     &          n_int, coef_crank, ak_d, i_vector)
!
      use m_geometry_data
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_skv_to_ff_smp_1st
      use nodal_fld_2_each_ele_1st
      use fem_skv_diffusion_1st
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_vector
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele1%numele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the physical values
       do k2=1, nnod_4_ele
        call vector_phys_2_each_element(k2, i_vector, velo_1)
        call fem_skv_vector_diffuse_1st(iele_fsmp_stack, n_int, k2,     &
     &      ak_d, velo_1, sk6)
      end do
!
      call add3_skv_coef_to_ff_v_smp_1st(coef_crank, ff_smp, sk6)
!
      end subroutine int_vol_vector_diffuse_1st
!
!  ---------------------------------------------------------------------
!
      end module int_vol_fractional_1st
