!>@file  const_radial_jacobian.f90
!!       module const_radial_jacobian
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2018
!
!> @brief  Construct jacobians for radial FEM
!!
!!@verbatim
!!      subroutine init_jacobian_4_radial_FEM(sph_rj, jac_r)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(radial_jacobian), intent(inout) :: jac_r
!!@endverbatim
!
      module const_radial_jacobian
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      implicit  none
!
      integer(kind = kint), parameter :: max_int_point_r = itwo
      integer(kind = kint), parameter                                   &
     &                     :: nnod_radial_edge = num_linear_edge
!
      private :: radius_at_lin_FEM_int_points
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_jacobian_4_radial_FEM(sph_rj, jac_r)
!
      use t_spheric_rj_data
      use t_jacobian_radius
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(radial_jacobian), intent(inout) :: jac_r
!
!
      call init_radial_shape_functios                                   &
     &   (sph_rj%nidx_rj(1), nnod_radial_edge, max_int_point_r, jac_r)
!
      call radius_at_lin_FEM_int_points(sph_rj%nidx_rj(1),              &
     &    sph_rj%istack_rj_kr_smp, sph_rj%radius_1d_rj_r,               &
     &    jac_r%ntot_int_point_r, jac_r%an_r, jac_r%dnxi_r,             &
     &    jac_r%r_edge, jac_r%ar_edge, jac_r%drdxi, jac_r%dxidr)
!
      end subroutine init_jacobian_4_radial_FEM
!
!-----------------------------------------------------------------------
!
      subroutine radius_at_lin_FEM_int_points                           &
     &         (nri, istack_rj_kr_smp, radius,                          &
     &          ntot_int, an_r, dnxi_r, r_edge, ar_edge, drdxi, dxidr)
!
      integer(kind = kint), intent(in) :: nri
      integer(kind = kint), intent(in) :: istack_rj_kr_smp(0:np_smp)
      real(kind = kreal), intent(in) :: radius(nri)
!
      integer(kind = kint), intent(in) :: ntot_int
      real(kind = kreal), intent(in) :: an_r(num_linear_edge,ntot_int)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_r(num_linear_edge,ntot_int)
!
      real(kind = kreal), intent(inout) :: r_edge(nri,ntot_int)
      real(kind = kreal), intent(inout) :: ar_edge(nri,ntot_int)
      real(kind = kreal), intent(inout) :: drdxi(nri,ntot_int)
      real(kind = kreal), intent(inout) :: dxidr(nri,ntot_int)
!
      integer(kind = kint) :: ix, kr, ist, ied, ip
      real(kind = kreal) :: r1, r2
!
!
!$omp parallel do private(ip,ist,ied,ix,kr,r1,r2)
      do ip = 1, np_smp
        ist = istack_rj_kr_smp(ip-1)
        ied = istack_rj_kr_smp(ip)
        do ix = 1, ntot_int
          if(ist .eq. 0) then
            r1 = zero
          else
            r1 = radius(ist)
          end if
!
          do kr = ist+1, ied
            r1 = radius(kr-1)
            r2 = radius(kr  )
!
            r_edge(kr,ix) = r1 * an_r(1,ix) + r2 * an_r(2,ix)
            drdxi(kr,ix) =  r1 * dnxi_r(1,ix) + r2 * dnxi_r(2,ix)
!
            ar_edge(kr,ix) = one / r_edge(kr,ix)
            dxidr(kr,ix) =   one / drdxi(kr,ix)
!
            r1 = r2
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine radius_at_lin_FEM_int_points
!
!-----------------------------------------------------------------------
!
      end module const_radial_jacobian

