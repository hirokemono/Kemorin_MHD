!>@file  t_jacobian_radius.f90
!!       module t_jacobian_radius
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief  Structure of radial Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine init_radial_shape_functios                           &
!!     &         (numedge, nnod_4_edge, maxint_r, jac_r)
!!        integer(kind = kint), intent(in) :: numedge, nnod_4_edge
!!        integer(kind = kint), intent(in) :: maxint_r
!!        type(radial_jacobian), intent(inout) :: jac_r
!!
!!      subroutine dealloc_radial_jac_type(jac_r)
!!
!!      subroutine copy_radial_jacobians                                &
!!     &         (numedge, nnod_4_edge, jac_org, jac_new)
!!        type(radial_jacobian), intent(in) :: jac_org
!!        type(radial_jacobian), intent(inout) :: jac_new
!!@endverbatim
!
      module t_jacobian_radius
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>     Stracture of Jacobians for edge
      type radial_jacobian
!>     Number of Gauss points
        integer(kind = kint) :: max_int_point_r
!>     Number of Gauss points
        integer(kind = kint) :: ntot_int_point_r
!
!>    Start address for gauss integrations
        integer(kind=kint), allocatable :: int_start_r(:)
!
!>    Gauss integration point
        real(kind = kreal), allocatable :: xi_r(:)
!>    Coefficients for Gauss integration
        real(kind = kreal), allocatable :: owe_r(:)
!
!>    Shape function
        real (kind=kreal), allocatable :: an_r(:,:)
!>    Difference of shape function  dn / dxi
        real (kind=kreal), allocatable :: dnxi_r(:,:)
! 
!>    Difference of radius  dr / dxi
        real (kind=kreal), allocatable :: drdxi(:,:)
!>    Difference of radius  dxi / dr
        real (kind=kreal), allocatable :: dxidr(:,:)
!
!>    Difference of radius at integration point
        real (kind=kreal), allocatable :: r_edge(:,:)
!>    Difference of 1/r at integration point
        real (kind=kreal), allocatable :: ar_edge(:,:)
      end type radial_jacobian
!
      private :: alloc_int_start_r, alloc_radial_jac_type
      private :: set_start_addres_1d_FEM_int
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_radial_shape_functios                             &
     &         (numedge, nnod_4_edge, maxint_r, jac_r)
!
      use m_geometry_constants
      use set_gauss_int_parameters
      use cal_shape_function_1d
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: maxint_r
!
      type(radial_jacobian), intent(inout) :: jac_r
!
!
      call alloc_int_start_r(maxint_r, jac_r)
      call set_start_addres_1d_FEM_int(jac_r%max_int_point_r,           &
     &    jac_r%ntot_int_point_r, jac_r%int_start_r)
!
      call alloc_radial_jac_type(numedge, nnod_4_edge, jac_r)
!
      call set_gauss_coefs_4_1d                                         &
     &   (jac_r%max_int_point_r, jac_r%ntot_int_point_r,                &
     &    jac_r%int_start_r, jac_r%xi_r, jac_r%owe_r)
!
      if(nnod_4_edge .eq. num_quad_edge) then
        call s_cal_shape_function_1d_quad(jac_r%ntot_int_point_r,       &
     &      jac_r%an_r, jac_r%dnxi_r, jac_r%xi_r)
      else
        call s_cal_shape_function_1d_linear(jac_r%ntot_int_point_r,     &
     &      jac_r%an_r, jac_r%dnxi_r, jac_r%xi_r)
      end if
!
      end subroutine init_radial_shape_functios
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_radial_jac_type(jac_r)
!
      type(radial_jacobian), intent(inout) :: jac_r
!
!
      deallocate(jac_r%int_start_r, jac_r%owe_r, jac_r%xi_r)
      deallocate(jac_r%an_r,   jac_r%dnxi_r)
      deallocate(jac_r%drdxi, jac_r%dxidr)
!
      deallocate(jac_r%r_edge, jac_r%ar_edge)
!
      end subroutine dealloc_radial_jac_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_int_start_r(maxint_r, jac_r)
!
      integer(kind = kint), intent(in) :: maxint_r
!
      type(radial_jacobian), intent(inout) :: jac_r
!
!
      jac_r%max_int_point_r = maxint_r
      allocate(jac_r%int_start_r(jac_r%max_int_point_r))
      if(jac_r%max_int_point_r .gt. 0) jac_r%int_start_r = 0
!
      end subroutine alloc_int_start_r
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_radial_jac_type(numedge, nnod_4_edge, jac_r)
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
!
      type(radial_jacobian), intent(inout) :: jac_r
!
!
      allocate(jac_r%owe_r(jac_r%ntot_int_point_r))
      allocate(jac_r%xi_r(jac_r%ntot_int_point_r))
!
      allocate(jac_r%an_r(nnod_4_edge,jac_r%ntot_int_point_r))
      allocate(jac_r%dnxi_r(nnod_4_edge,jac_r%ntot_int_point_r))
!
      allocate(jac_r%drdxi(numedge,jac_r%ntot_int_point_r))
      allocate(jac_r%dxidr(numedge,jac_r%ntot_int_point_r))
!
      allocate(jac_r%r_edge(numedge,jac_r%ntot_int_point_r))
      allocate(jac_r%ar_edge(numedge,jac_r%ntot_int_point_r))
!
      if(jac_r%ntot_int_point_r .gt. 0) return
      jac_r%owe_r =  0.0d0
      jac_r%xi_r =   0.0d0
!
      jac_r%an_r =   0.0d0
      jac_r%dnxi_r = 0.0d0
!
      if (numedge .gt. 0) then
        jac_r%drdxi = 0.0d0
        jac_r%dxidr = 0.0d0
!
        jac_r%r_edge =  0.0d0
        jac_r%ar_edge = 0.0d0
      end if
!
      end subroutine alloc_radial_jac_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_radial_jacobians                                  &
     &         (numedge, nnod_4_edge, jac_org, jac_new)
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
!
      type(radial_jacobian), intent(in) :: jac_org
      type(radial_jacobian), intent(inout) :: jac_new
!
!
      call alloc_int_start_r(jac_org%max_int_point_r, jac_new)
      jac_new%int_start_r(1:jac_new%max_int_point_r)                    &
     &      = jac_org%int_start_r(1:jac_new%max_int_point_r)
      jac_new%ntot_int_point_r = jac_org%ntot_int_point_r
!
      call alloc_radial_jac_type                                        &
     &   (numedge, nnod_4_edge, jac_new)
!
      jac_new%owe_r(1:jac_new%ntot_int_point_r)                         &
     &      = jac_org%owe_r(1:jac_new%ntot_int_point_r)
      jac_new%xi_r(1:jac_new%ntot_int_point_r)                          &
     &      = jac_org%xi_r(1:jac_new%ntot_int_point_r)
!
      jac_new%an_r(1:nnod_4_edge,1:jac_new%ntot_int_point_r)            &
     &      = jac_org%an_r(1:nnod_4_edge,1:jac_new%ntot_int_point_r)
      jac_new%dnxi_r(1:nnod_4_edge,1:jac_new%ntot_int_point_r)          &
     &      = jac_org%dnxi_r(1:nnod_4_edge,1:jac_new%ntot_int_point_r)
!
!$omp parallel workshare
      jac_new%drdxi(1:numedge,1:jac_new%ntot_int_point_r)               &
     &      = jac_org%drdxi(1:numedge,1:jac_new%ntot_int_point_r)
      jac_new%dxidr(1:numedge,1:jac_new%ntot_int_point_r)               &
     &      = jac_org%dxidr(1:numedge,1:jac_new%ntot_int_point_r)
!
      jac_new%r_edge(1:numedge,1:jac_new%ntot_int_point_r)              &
     &      = jac_org%r_edge(1:numedge,1:jac_new%ntot_int_point_r)
      jac_new%ar_edge(1:numedge,1:jac_new%ntot_int_point_r)             &
     &      = jac_org%ar_edge(1:numedge,1:jac_new%ntot_int_point_r)
!$omp end parallel workshare
!
       end subroutine copy_radial_jacobians
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_start_addres_1d_FEM_int                            &
     &         (max_int_point, ntot_int_point, int_start1)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(inout) :: ntot_int_point
      integer(kind = kint), intent(inout) :: int_start1(max_int_point)
!
      integer(kind = kint) :: i
!
!
      int_start1(1) = 0
      do i = 1, max_int_point - 1
        int_start1(i+1) = int_start1(i) + i
      end do
      ntot_int_point = int_start1(max_int_point) + max_int_point
!
      end subroutine set_start_addres_1d_FEM_int
!
!-----------------------------------------------------------------------
!
      end module t_jacobian_radius
