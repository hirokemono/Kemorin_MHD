!cal_radial_jacobians.f90
!     module cal_radial_jacobians
!
!     Written by H. Matsui on Sep. 2010
!
!
!      subroutine s_cal_radial_jacobians(nri, radius)
!
!      subroutine cal_linear_radiaul_jacobian(nri, radius)
!      subroutine cal_quad_radiaul_jacobian(nri, radius)
!
      module cal_radial_jacobians
!
      use m_precision
!
      use m_constants
      use m_radial_fem_jacobian
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      use cal_shape_function_1d
      use cal_jacobian_1d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_radial_jacobians(nri, radius)
!
      use m_gauss_int_parameters
      use set_integration_indices
      use set_gauss_int_parameters
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius(nri)
!
!
      call set_num_radial_element(nri)
!
      call allocate_integrate_parameters
      call allocate_gauss_coef_4_fem
!
      call init_gauss_int_parameters
      call set_integrate_indices_1d
      call set_gauss_coefs_4_1d
!
      ntot_int_r = maxtot_int_1d
      call cal_linear_radiaul_jacobian(nri, radius)
!
      if(nedge_r3 .gt. 0) then
        call cal_quad_radiaul_jacobian(nri, radius)
      end if
!
      call deallocate_gen_position_to_4
      call deallocate_shape_functions
!
      end subroutine s_cal_radial_jacobians
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_linear_radiaul_jacobian(nri, radius)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius(nri)
!
      integer(kind = kint) :: i0, ii, ix
!
!
      call alloc_linear_radial_jac
      call set_radial_linear_fem_connect
      call s_cal_shape_function_1d_linear(ntot_int_r, an_r2,            &
       &    dnxi_ed1, xi1)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
          call cal_x_jacobian_1d_2(nri, nedge_r2, num_quad_edge, ie_r2, &
    &         radius, rjac2(1,ix), arjac2(1,ix), reg2(1,ix),            &
    &         dnxi_ed1(1,ix))
!
        end do
      end do
!
      end subroutine cal_linear_radiaul_jacobian
!
!-----------------------------------------------------------------------
!
      subroutine cal_quad_radiaul_jacobian(nri, radius)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius(nri)
!
      integer(kind = kint) :: i0, ii, ix
!
!
      call alloc_quad_radial_jac
      call set_radial_quad_fem_connect
      call s_cal_shape_function_1d_quad(ntot_int_r, an_r3,              &
       &    dnxi_ed20, xi1)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
          call cal_x_jacobian_1d_3(nri, nedge_r3, num_quad_edge, ie_r3, &
    &         radius, rjac3(1,ix), arjac3(1,ix), reg3(1,ix),            &
    &         dnxi_ed20(1,ix))
!
        end do
      end do
!
      end subroutine cal_quad_radiaul_jacobian
!
!-----------------------------------------------------------------------
!
      end module cal_radial_jacobians
