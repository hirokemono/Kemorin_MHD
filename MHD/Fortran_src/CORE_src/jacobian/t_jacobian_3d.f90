!>@file  t_jacobian_3d.f90
!!       module t_jacobian_3d
!!
!!@author H. Matsui
!!@date   Programmed on Dec., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief  Structure of 3D Jacobian and difference of shape functions
!!
!!@verbatim
!!      subroutine alloc_jacobians_type(numele, nnod_4_ele, jac_3d)
!!      subroutine alloc_dxi_dx_type(numele, jac_3d)
!!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!!        type(jacobians_3d), intent(inout) :: jac_3d
!!
!!      subroutine dealloc_jacobians_type(jac_3d)
!!      subroutine dealloc_inv_jac_type(jac_3d)
!!      subroutine dealloc_dxi_dx_type(jac_3d)
!!
!!  definision of matrix
!!         dxidx_3d(iele,ix,1,1) :: dxi / dx
!!         dxidx_3d(iele,ix,2,1) :: dei / dx
!!         dxidx_3d(iele,ix,3,1) :: dzi / dx
!!
!!         dxidx_3d(iele,ix,1,2) :: dxi / dy
!!         dxidx_3d(iele,ix,2,2) :: dei / dy
!!         dxidx_3d(iele,ix,3,2) :: dzi / dy
!!
!!         dxidx_3d(iele,ix,1,3) :: dxi / dz
!!         dxidx_3d(iele,ix,2,3) :: dei / dz
!!         dxidx_3d(iele,ix,3,3) :: dzi / dz
!!
!!         iele: element ID
!!         ix:   integration point ID
!!@endverbatim
!
      module t_jacobian_3d
!
      use m_precision
!
      implicit  none
!
!>     Stracture for Jacobians for element
      type jacobians_3d
!>   Total number of integration points
        integer(kind = kint) :: ntot_int
!>   Shape function at integration points
        real (kind=kreal), pointer :: an(:,:)
!>   Spatial differnce of Shape function  at integration points
        real (kind=kreal), pointer :: dnx(:,:,:,:)
!
!>   Shape function for infinite element at integration points
        real (kind=kreal), pointer :: an_infty(:,:,:)
!
!>   Jacobian at integration points
        real (kind=kreal), pointer :: xjac(:,:)
!>   1 / Jacbian
        real (kind=kreal), pointer :: axjac(:,:)
!
!>   dxi / dx
        real(kind=kreal),   pointer :: dxidx_3d(:,:,:,:)
      end type jacobians_3d
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_jacobians_type(numele, nnod_4_ele, jac_3d)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      allocate(jac_3d%an(nnod_4_ele,jac_3d%ntot_int))
      allocate(jac_3d%an_infty(nnod_4_ele,nsurf_4_ele,jac_3d%ntot_int))
!
      allocate(jac_3d%dnx(numele,nnod_4_ele,jac_3d%ntot_int,3))
!
      allocate(jac_3d%xjac(numele,jac_3d%ntot_int))
      allocate(jac_3d%axjac(numele,jac_3d%ntot_int))
!
      jac_3d%an = 0.0d0
      jac_3d%an_infty = 0.0d0
!
      if (numele .gt. 0) then
        jac_3d%dnx = 0.0d0
!
        jac_3d%xjac = 0.0d0
        jac_3d%axjac = 0.0d0
      end if
!
      end subroutine alloc_jacobians_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dxi_dx_type(numele, jac_3d)
!
      integer(kind = kint), intent(in) :: numele
      type(jacobians_3d), intent(inout) :: jac_3d
!
!
      allocate( jac_3d%dxidx_3d(numele,jac_3d%ntot_int,3,3) )
      if (numele .gt. 0) jac_3d%dxidx_3d = 0.0d0
!
      end subroutine alloc_dxi_dx_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_type(jac_3d)
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
      deallocate(jac_3d%an, jac_3d%an_infty)
      deallocate(jac_3d%dnx)
!
      deallocate(jac_3d%xjac)
      deallocate(jac_3d%axjac)
!
      end subroutine dealloc_jacobians_type
!
!  ------------------------------------------------------------------
!
      subroutine dealloc_inv_jac_type(jac_3d)
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
      deallocate(jac_3d%axjac)
!
      end subroutine dealloc_inv_jac_type
!
!  ------------------------------------------------------------------
!
      subroutine dealloc_dxi_dx_type(jac_3d)
!
      type(jacobians_3d), intent(inout) :: jac_3d
!
      deallocate( jac_3d%dxidx_3d )
!
      end subroutine dealloc_dxi_dx_type
!
!-----------------------------------------------------------------------
!
      end module t_jacobian_3d
