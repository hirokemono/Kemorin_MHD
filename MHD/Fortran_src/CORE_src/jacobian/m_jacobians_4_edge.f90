!
!   module   m_jacobians_4_edge
!.......................................................................
!
!       Written by H. Matsui
!
!      subroutine allocate_jacobians_edge_linear
!      subroutine allocate_jacobians_edge_quad
!      subroutine allocate_jacobians_edge_l_quad
!      subroutine copy_jacobians_edge_quad
!
!      subroutine deallocate_jac_edge_linear
!      subroutine deallocate_jac_edge_quad
!      subroutine deallocate_jac_edge_l_quad
!
      module   m_jacobians_4_edge
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: ntot_int_1d
      real (kind=kreal), allocatable :: an_edge(:,:)
! 
      real (kind=kreal), allocatable :: xeg_edge(:,:,:)
      real (kind=kreal), allocatable :: xj_edge(:,:)
      real (kind=kreal), allocatable :: axj_edge(:,:)
!
!
!
      real (kind=kreal), allocatable :: aw_edge(:,:)
! 
      real (kind=kreal), allocatable :: xeq_edge(:,:,:)
      real (kind=kreal), allocatable :: xjq_edge(:,:)
      real (kind=kreal), allocatable :: axjq_edge(:,:)
!
!
!
      real (kind=kreal), allocatable :: am_edge(:,:)
! 
      real (kind=kreal), allocatable :: xelq_edge(:,:,:)
      real (kind=kreal), allocatable :: xjlq_edge(:,:)
      real (kind=kreal), allocatable :: axjlq_edge(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_jacobians_edge_linear
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
!
!
      allocate(an_edge(num_linear_edge,ntot_int_1d))
!
      allocate(xeg_edge(numedge,ntot_int_1d,3))
!
      allocate(xj_edge(numedge,ntot_int_1d))
      allocate(axj_edge(numedge,ntot_int_1d))
!
       an_edge = 0.0d0
!
       xj_edge = 0.0d0
       axj_edge = 0.0d0
!
       end subroutine allocate_jacobians_edge_linear
!
!  ------------------------------------------------------------------
!
      subroutine allocate_jacobians_edge_quad
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
!
!
      allocate(aw_edge(nnod_4_edge,ntot_int_1d))
!
      allocate(xeq_edge(numedge,ntot_int_1d,3))
!
      allocate(xjq_edge(numedge,ntot_int_1d))
      allocate(axjq_edge(numedge,ntot_int_1d)) 
!
      aw_edge = 0.0d0
      xeq_edge = 0.0d0
      xjq_edge = 0.0d0
      axjq_edge = 0.0d0 
!
      end subroutine allocate_jacobians_edge_quad
!
!  ------------------------------------------------------------------
!
      subroutine allocate_jacobians_edge_l_quad
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
!
      allocate(am_edge(num_quad_edge,ntot_int_1d))
!
      allocate(xelq_edge(numedge,ntot_int_1d,3))
!
      allocate(xjlq_edge(numedge,ntot_int_1d))
      allocate(axjlq_edge(numedge,ntot_int_1d)) 
!
      am_edge = 0.0d0
      xelq_edge = 0.0d0
      xjlq_edge = 0.0d0
      axjlq_edge = 0.0d0 
!
      end subroutine allocate_jacobians_edge_l_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine copy_jacobians_edge_quad
!
       aw_edge   = an_edge
       xeq_edge  = xeg_edge
       xjq_edge  = xj_edge
       axjq_edge = axj_edge
!
       end subroutine copy_jacobians_edge_quad
!
!  ------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_jac_edge_linear
!
      deallocate(an_edge)
      deallocate(xeg_edge)
!
      deallocate(xj_edge)
      deallocate(axj_edge)
!
      end subroutine deallocate_jac_edge_linear
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_edge_quad
!
      deallocate(aw_edge)
!
      deallocate(xeq_edge)
!
      deallocate(xjq_edge)
      deallocate(axjq_edge)
!
      end subroutine deallocate_jac_edge_quad
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_edge_l_quad
!
      deallocate(am_edge)
!
      deallocate(xelq_edge)
!
      deallocate(xjlq_edge)
      deallocate(axjlq_edge)
!
      end subroutine deallocate_jac_edge_l_quad
!
!  ------------------------------------------------------------------
!
      end module   m_jacobians_4_edge
