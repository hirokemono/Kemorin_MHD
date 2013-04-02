!m_radial_fem_jacobian.f90
!     module m_radial_fem_jacobian
!
!     Written by H. Matsui on Sep. 2010
!
!      subroutine alloc_linear_radial_jac
!      subroutine alloc_quad_radial_jac
!      subroutine dealloc_linear_radial_jac
!      subroutine dealloc_quad_radial_jac
!
!      subroutine set_num_radial_element(nri)
!      subroutine set_radial_linear_fem_connect
!      subroutine set_radial_quad_fem_connect
!
      module m_radial_fem_jacobian
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      integer(kind = kint) :: ntot_int_r
      integer(kind = kint) :: nedge_r2, nedge_r3
!
      integer(kind = kint), allocatable :: ie_r2(:,:)
      real(kind = kreal), allocatable :: an_r2(:,:)
      real(kind = kreal), allocatable :: reg2(:,:)
      real(kind = kreal), allocatable :: rjac2(:,:)
      real(kind = kreal), allocatable :: arjac2(:,:)
!
      integer(kind = kint), allocatable :: ie_r3(:,:)
      real(kind = kreal), allocatable :: an_r3(:,:)
      real(kind = kreal), allocatable :: reg3(:,:)
      real(kind = kreal), allocatable :: rjac3(:,:)
      real(kind = kreal), allocatable :: arjac3(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_linear_radial_jac
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
!
      allocate(ie_r2(nedge_r2, num_linear_edge))
      allocate(an_r2(num_linear_edge,ntot_int_r))
!
!
      allocate(reg2(nedge_r2,ntot_int_r))
!
      allocate(rjac2(nedge_r2,ntot_int_r))
      allocate(arjac2(nedge_r2,ntot_int_r))
!
      ie_r2 = izero
      an_r2 = zero
      reg2 =  zero
!
      rjac2 =  zero
      arjac2 = zero
!
      end subroutine alloc_linear_radial_jac
!
!-----------------------------------------------------------------------
!
      subroutine alloc_quad_radial_jac
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
!
      allocate(ie_r3(nedge_r3, num_quad_edge))
      allocate(an_r3(num_quad_edge,ntot_int_r))
!
      allocate(reg3(nedge_r3,ntot_int_r))
!
      allocate(rjac3(nedge_r3,ntot_int_r))
      allocate(arjac3(nedge_r3,ntot_int_r)) 
!
       an_r3 = zero
       reg3 = zero
       rjac3 = zero
       arjac3 = zero 
!
      end subroutine alloc_quad_radial_jac
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_linear_radial_jac
!
!
      deallocate(ie_r2, an_r2, reg2)
      deallocate(rjac2, arjac2)
!
      end subroutine dealloc_linear_radial_jac
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_quad_radial_jac
!
!
      deallocate(ie_r3, an_r3, reg3)
      deallocate(rjac3, arjac3)
!
      end subroutine dealloc_quad_radial_jac
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_num_radial_element(nri)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: nri
!
      if( mod(nri,2) .eq. 1) then
        max_int_point = 3
        nedge_r3 = (nri - 1) / 2
      else
        max_int_point = 2
        nedge_r3 = 0
      end if
      nedge_r2 = nri - 1
!
      end subroutine set_num_radial_element
!
!-----------------------------------------------------------------------
!
      subroutine set_radial_linear_fem_connect
!
      integer(kind = kint) :: iedge
!
      do iedge = 1, nedge_r2
        ie_r2(iedge,1) = iedge
        ie_r2(iedge,2) = iedge + 1
      end do
!
      end subroutine set_radial_linear_fem_connect
!
!-----------------------------------------------------------------------
!
      subroutine set_radial_quad_fem_connect
!
      integer(kind = kint) :: iedge
!
      do iedge = 1, nedge_r3
        ie_r3(iedge,1) = 2*iedge - 1
        ie_r3(iedge,2) = 2*iedge + 1
        ie_r3(iedge,3) = 2*iedge
      end do
!
      end subroutine set_radial_quad_fem_connect
!
!-----------------------------------------------------------------------
!
      end module m_radial_fem_jacobian
