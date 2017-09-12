!>@file   t_radial_fem_jacobian.f90
!!@brief  module t_radial_fem_jacobian
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  Jacobians for radial FEM 
!!
!!@verbatim
!!      subroutine cal_radial_jacobians(nri, radius, jacs_r)
!!      subroutine dealloc_radial_jacobians(jacs_r)
!!@verbatim
!
      module t_radial_fem_jacobian
!
      use m_precision
      use m_constants
!
      use m_fem_gauss_int_coefs
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use cal_shape_function_1d
      use cal_jacobian_1d
!
      implicit none
!
      type radial_fem_jacobian
        integer(kind = kint) :: ntot_int
        integer(kind = kint) :: nedge_r
        integer(kind = kint), allocatable :: ie_r(:,:)
        real(kind = kreal), allocatable :: an_r(:,:)
        real(kind = kreal), allocatable :: reg(:,:)
        real(kind = kreal), allocatable :: rjac(:,:)
        real(kind = kreal), allocatable :: arjac(:,:)
      end type radial_fem_jacobian
!
      type radial_fem_jacobians
        type(radial_fem_jacobian) :: j_lin
        type(radial_fem_jacobian) :: j_quad
      end type radial_fem_jacobians
!
      private :: alloc_radial_jac, dealloc_radial_jac
      private :: set_num_radial_element
      private :: cal_linear_radiaul_jacobian, cal_quad_radiaul_jacobian
      private :: set_radial_linear_fem_connect
      private :: set_radial_quad_fem_connect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_radial_jacobians(nri, radius, jacs_r)
!
      use m_gauss_int_parameters
      use set_integration_indices
      use set_gauss_int_parameters
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius(nri)
!
      type(radial_fem_jacobians), intent(inout) :: jacs_r
!
      type(FEM_gauss_int_coefs) :: g_FEM_r
      type(edge_shape_function) :: spf_1d_r
!
!
      call set_num_radial_element(nri, jacs_r%j_lin, jacs_r%j_quad)
!
!
      if( mod(nri,itwo) .eq. ione) then
        call set_max_integration_points(ithree, g_FEM_r)
      else
        call set_max_integration_points(itwo, g_FEM_r)
      end if
!
      call set_num_of_int_points(g_FEM_r)
      call alloc_gauss_coef_4_fem(g_FEM_r)
!
      call init_gauss_int_parameters
      call alloc_1d_gauss_point_id                                      &
     &   (maxtot_int_1d, max_int_point, spf_1d_r)
      call set_integrate_indices_1d                                     &
     &   (maxtot_int_1d, max_int_point, spf_1d_r%l_int)
      call set_gauss_coefs_4_1d                                         &
     &   (g_FEM_r%max_int_point, g_FEM_r%maxtot_int_1d,                 &
     &    g_FEM_r%int_start1, g_FEM_r%owe, spf_1d_r%xi)
!
      call copy_fem_gauss_int_coefs(g_FEM_r)
!
      call alloc_edge_shape_func(num_linear_edge, g_FEM_r, spf_1d_r)
      call cal_linear_radiaul_jacobian                                  &
     &   (nri, maxtot_int_1d, radius, spf_1d_r, jacs_r%j_lin)
      call dealloc_edge_shape_func(spf_1d_r)
!
      call alloc_edge_shape_func(num_quad_edge, g_FEM_r, spf_1d_r)
      call cal_quad_radiaul_jacobian                                    &
     &   (nri, maxtot_int_1d, radius, spf_1d_r, jacs_r%j_quad)
      call dealloc_edge_shape_func(spf_1d_r)
!
      call dealloc_1d_gauss_point_id(spf_1d_r)
!
      end subroutine cal_radial_jacobians
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_radial_jacobians(jacs_r)
!
      type(radial_fem_jacobians), intent(inout) :: jacs_r
!
      call dealloc_radial_jac(jacs_r%j_quad)
      call dealloc_radial_jac(jacs_r%j_lin)
!
      end subroutine dealloc_radial_jacobians
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_radial_jac(nnod_4_edge, ntot_int, j_radius)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: nnod_4_edge, ntot_int
      type(radial_fem_jacobian), intent(inout) :: j_radius
!
!
      j_radius%ntot_int = ntot_int
!
      allocate(j_radius%ie_r(j_radius%nedge_r, num_quad_edge))
      allocate(j_radius%an_r(nnod_4_edge,j_radius%ntot_int))
!
      allocate(j_radius%rjac(j_radius%nedge_r,j_radius%ntot_int))
!
      allocate(j_radius%rjac(j_radius%nedge_r,j_radius%ntot_int))
      allocate(j_radius%arjac(j_radius%nedge_r,j_radius%ntot_int)) 
!
      j_radius%an_r = zero
      if(j_radius%nedge_r .le. 0) return
!
      j_radius%rjac = zero
      j_radius%rjac = zero
      j_radius%arjac = zero
!
      end subroutine alloc_radial_jac
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_radial_jac(j_radius)
!
      type(radial_fem_jacobian), intent(inout) :: j_radius
!
      deallocate(j_radius%ie_r, j_radius%an_r, j_radius%reg)
      deallocate(j_radius%rjac, j_radius%arjac)
!
      end subroutine dealloc_radial_jac
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_num_radial_element(nri, j_lin, j_quad)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: nri
      type(radial_fem_jacobian), intent(inout) :: j_lin, j_quad
!
      if( mod(nri,itwo) .eq. ione) then
        j_quad%nedge_r = (nri - 1) / 2
      else
        j_quad%nedge_r = 0
      end if
      j_lin%nedge_r = nri - 1
!
      end subroutine set_num_radial_element
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_linear_radiaul_jacobian                            &
     &         (nri, ntot_int, radius, spf_1d_8, j_lin)
!
      integer(kind = kint), intent(in) :: nri, ntot_int
      real(kind = kreal), intent(in) :: radius(nri)
!
      type(edge_shape_function), intent(inout) :: spf_1d_8
      type(radial_fem_jacobian), intent(inout) :: j_lin
!
      integer(kind = kint) :: i0, ii, ix
!
!
      call alloc_radial_jac(num_linear_edge, ntot_int, j_lin)
      call set_radial_linear_fem_connect(j_lin)
      call s_cal_shape_function_1d_linear(j_lin%ntot_int, j_lin%an_r,   &
     &    spf_1d_8%dnxi_ed, spf_1d_8%xi)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
          call cal_x_jacobian_1d_2                                      &
    &        (nri, j_lin%nedge_r, num_quad_edge, j_lin%ie_r, radius,    &
    &         j_lin%rjac(1,ix), j_lin%arjac(1,ix), j_lin%reg(1,ix),     &
    &         spf_1d_8%dnxi_ed(1,ix))
!
        end do
      end do
!
      end subroutine cal_linear_radiaul_jacobian
!
!-----------------------------------------------------------------------
!
      subroutine cal_quad_radiaul_jacobian                              &
     &         (nri, ntot_int, radius, spf_1d_20, j_quad)
!
      integer(kind = kint), intent(in) :: nri, ntot_int
      real(kind = kreal), intent(in) :: radius(nri)
!
      type(edge_shape_function), intent(inout) :: spf_1d_20
      type(radial_fem_jacobian), intent(inout) :: j_quad
!
      integer(kind = kint) :: i0, ii, ix
!
!
      if(j_quad%nedge_r .le. 0) return
!
      call alloc_radial_jac(num_quad_edge, ntot_int, j_quad)
      call set_radial_quad_fem_connect(j_quad)
      call s_cal_shape_function_1d_quad(j_quad%ntot_int, j_quad%an_r,   &
     &    spf_1d_20%dnxi_ed, spf_1d_20%xi)
!
!   jacobian for quadrature elaments
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
          call cal_x_jacobian_1d_3                                      &
    &        (nri, j_quad%nedge_r, num_quad_edge, j_quad%ie_r, radius,  &
    &         j_quad%rjac(1,ix), j_quad%arjac(1,ix), j_quad%reg(1,ix),  &
    &         spf_1d_20%dnxi_ed(1,ix))
!
        end do
      end do
!
      end subroutine cal_quad_radiaul_jacobian
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_radial_linear_fem_connect(j_lin)
!
      type(radial_fem_jacobian), intent(inout) :: j_lin
!
      integer(kind = kint) :: iedge
!
!
      do iedge = 1, j_lin%nedge_r
        j_lin%ie_r(iedge,1) = iedge
        j_lin%ie_r(iedge,2) = iedge + 1
      end do
!
      end subroutine set_radial_linear_fem_connect
!
!-----------------------------------------------------------------------
!
      subroutine set_radial_quad_fem_connect(j_quad)
!
      type(radial_fem_jacobian), intent(inout) :: j_quad
!
      integer(kind = kint) :: iedge
!
!
      do iedge = 1, j_quad%nedge_r
        j_quad%ie_r(iedge,1) = 2*iedge - 1
        j_quad%ie_r(iedge,2) = 2*iedge + 1
        j_quad%ie_r(iedge,3) = 2*iedge
      end do
!
      end subroutine set_radial_quad_fem_connect
!
!-----------------------------------------------------------------------
!
      end module t_radial_fem_jacobian
