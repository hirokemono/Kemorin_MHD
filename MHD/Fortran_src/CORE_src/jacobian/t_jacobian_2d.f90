!t_jacobian_2d.f90
!     module t_jacobian_2d
!
!      Written by H. Matsui on Dec., 2008
!
!>   Structure of 2D Jacobian and difference of shape functions
!
!
!      subroutine alloc_2d_jac_type(numsurf, nnod_4_surf, jac_2d)
!        integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
!        type(jacobians_2d), intent(inout) :: jac_2d
!       subroutine alloc_2d_jac_sf_grp_type(nnod_4_surf,                &
!     &           num_surf_bc, jac_sf_grp)
!        integer(kind = kint), intent(in) :: nnod_4_surf, num_surf_bc
!        type(jacobians_surf_grp), intent(inout) :: jac_sf_grp
!
!      subroutine dealloc_2d_jac_type(jac_2d)
!      subroutine dealloc_2d_jac_sf_grp_type(jac_sf_grp)
!
      module t_jacobian_2d
!
      use m_precision
!
      implicit  none
!
!>     Stracture for Jacobians for surface
      type jacobians_2d
        integer(kind=kint) :: ntot_int
        real (kind=kreal), pointer :: an_surf(:,:)
! 
        real (kind=kreal), pointer :: xsf_surf(:,:,:)
!
        real (kind=kreal), pointer :: xj_surf(:,:)
        real (kind=kreal), pointer :: axj_surf(:,:)
      end type jacobians_2d
!
!
!>     Stracture for Jacobians for surafce group
      type jacobians_surf_grp
        integer(kind=kint) :: ntot_int
        real (kind=kreal), pointer :: an_sf(:,:)
! 
        real (kind=kreal), pointer :: xsf_sf(:,:,:)
!
        real (kind=kreal), pointer :: xj_sf(:,:)
        real (kind=kreal), pointer :: axj_sf(:,:)
!
        real (kind=kreal), pointer :: sgs_sf(:,:)
      end type jacobians_surf_grp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_2d_jac_type(numsurf, nnod_4_surf, jac_2d)
!
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
!
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      allocate(jac_2d%an_surf(nnod_4_surf,jac_2d%ntot_int))
!
      allocate(jac_2d%xsf_surf(numsurf,jac_2d%ntot_int,3))
!
      allocate(jac_2d%xj_surf(numsurf,jac_2d%ntot_int))
      allocate(jac_2d%axj_surf(numsurf,jac_2d%ntot_int))
!
      jac_2d%an_surf = 0.0d0
!
      if (numsurf .gt. 0) then
        jac_2d%xsf_surf = 0.0d0
!
        jac_2d%xj_surf = 0.0d0
        jac_2d%axj_surf = 0.0d0
      end if
!
      end subroutine alloc_2d_jac_type
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_2d_jac_sf_grp_type(nnod_4_surf,                 &
     &           num_surf_bc, jac_sf_grp)
!
      integer(kind = kint), intent(in) :: nnod_4_surf, num_surf_bc
!
      type(jacobians_surf_grp), intent(inout) :: jac_sf_grp
!
!
      allocate(jac_sf_grp%an_sf(nnod_4_surf,jac_sf_grp%ntot_int))
!
      allocate(jac_sf_grp%xsf_sf(num_surf_bc,jac_sf_grp%ntot_int,3))
!
      allocate(jac_sf_grp%xj_sf(num_surf_bc,jac_sf_grp%ntot_int))
      allocate(jac_sf_grp%axj_sf(num_surf_bc,jac_sf_grp%ntot_int))
      allocate(jac_sf_grp%sgs_sf(num_surf_bc,14))
!
      if (num_surf_bc .gt. 0) then
        jac_sf_grp%an_sf = 0.0d0
!
        jac_sf_grp%xj_sf = 0.0d0
        jac_sf_grp%axj_sf = 0.0d0
        jac_sf_grp%sgs_sf = 0.0d0
      end if
!
      end subroutine alloc_2d_jac_sf_grp_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_2d_jac_type(jac_2d)
!
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      deallocate(jac_2d%an_surf)
      deallocate(jac_2d%xsf_surf)
!
      deallocate(jac_2d%xj_surf, jac_2d%axj_surf)
!
      end subroutine dealloc_2d_jac_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_2d_jac_sf_grp_type(jac_sf_grp)
!
      type(jacobians_surf_grp), intent(inout) :: jac_sf_grp
!
!
      deallocate(jac_sf_grp%an_sf)
      deallocate(jac_sf_grp%xsf_sf)
!
      deallocate(jac_sf_grp%xj_sf, jac_sf_grp%axj_sf)
      deallocate(jac_sf_grp%sgs_sf)
!
      end subroutine dealloc_2d_jac_sf_grp_type
!
!  ------------------------------------------------------------------
!
      end module t_jacobian_2d
