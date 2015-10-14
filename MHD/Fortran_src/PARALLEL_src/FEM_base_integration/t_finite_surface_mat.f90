!
!      module t_finite_surface_mat
!
!      Writtn  by H. Matsui on Sep., 2005
!
!      subroutine alloc_work_fem_surf_mat_t(surf, sf_grp, fem_sf_wk)
!      subroutine dealloc_work_fem_surf_mat_t(fem_sf_wk)
!      subroutine check_vect_sf_t(my_rank, txt, sf_grp, fem_sf_wk)
!
      module t_finite_surface_mat
!
      use m_precision
!
      implicit none
!
!
      type work_finite_surface_mat
        real (kind=kreal), pointer :: scalar_sf(:)
        real (kind=kreal), pointer :: vector_sf(:,:)
!
        real (kind=kreal), pointer :: xe_sf(:,:,:)
        real (kind=kreal), pointer :: dxe_sf(:,:,:)
      end type work_finite_surface_mat
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_work_fem_surf_mat_t(surf, sf_grp, fem_sf_wk)
!
      use t_surface_data
      use t_group_data
!
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(work_finite_surface_mat), intent(inout) :: fem_sf_wk
!
      allocate( fem_sf_wk%scalar_sf(sf_grp%num_item)   )
      allocate( fem_sf_wk%vector_sf(sf_grp%num_item,3) )
!
      allocate( fem_sf_wk%xe_sf(sf_grp%num_item,4,surf%nnod_4_surf) )
      allocate( fem_sf_wk%dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf) )
!
      if(sf_grp%num_item .gt. 0) then
        fem_sf_wk%scalar_sf = 0.0d0
        fem_sf_wk%vector_sf = 0.0d0
        fem_sf_wk%xe_sf =   0.0d0
        fem_sf_wk%dxe_sf = 0.0d0
      end if
!
      end subroutine alloc_work_fem_surf_mat_t
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_work_fem_surf_mat_t(fem_sf_wk)
!
      type(work_finite_surface_mat), intent(inout) :: fem_sf_wk
!
!
      deallocate( fem_sf_wk%scalar_sf, fem_sf_wk%vector_sf )
      deallocate( fem_sf_wk%xe_sf , fem_sf_wk%dxe_sf )
!
      end subroutine dealloc_work_fem_surf_mat_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vect_sf_t(my_rank, txt, sf_grp, fem_sf_wk)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: sf_grp
      type(work_finite_surface_mat), intent(inout) :: fem_sf_wk
      integer(kind = kint), intent(in) :: my_rank
      character(*), intent(in) :: txt
      integer(kind = kint) :: isurf
!
!
      write(50+my_rank,*) txt
      do isurf = 1, sf_grp%num_item
        write(50+my_rank,*) isurf, fem_sf_wk%vector_sf(isurf,1:3)
      end do
!
       end subroutine check_vect_sf_t
!
!-----------------------------------------------------------------------
!
      end module t_finite_surface_mat
