!
!      module t_int_surface_data
!
!      Writtn  by H. Matsui on Sep., 2005
!
!!      subroutine alloc_int_surf_data                                  &
!!     &         (num_surf_bc, nnod_4_surf, surf_wk)
!!      subroutine dealloc_int_surf_data(surf_wk)
!!      subroutine check_vect_sf(num_surf_bc, my_rank, txt, surf_wk)
!!      type(work_surface_element_mat), intent(inout) :: surf_wk
!
      module t_int_surface_data
!
      use m_precision
!
      implicit none
!
!
      type work_surface_element_mat
        integer(kind = kint) :: ntot_item
        real(kind=kreal), pointer :: scalar_sf(:)
        real(kind=kreal), pointer :: vect_sf(:,:)
!
        real(kind=kreal), pointer :: xe_sf(:,:,:)
        real(kind=kreal), pointer :: dxe_sf(:,:,:)
      end type work_surface_element_mat
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_int_surf_data                                    &
     &         (num_surf_bc, nnod_4_surf, surf_wk)
!
      integer(kind = kint), intent(in) :: num_surf_bc, nnod_4_surf
      type(work_surface_element_mat), intent(inout) :: surf_wk
!
!
      surf_wk%ntot_item = num_surf_bc
      allocate( surf_wk%scalar_sf(surf_wk%ntot_item) )
      allocate( surf_wk%vect_sf(surf_wk%ntot_item,3) )
      allocate( surf_wk%xe_sf(surf_wk%ntot_item,4,nnod_4_surf) )
      allocate( surf_wk%dxe_sf(surf_wk%ntot_item,4,nnod_4_surf) )
!
      if(surf_wk%ntot_item .gt. 0) then
        surf_wk%scalar_sf = 0.0d0
        surf_wk%vect_sf = 0.0d0
        surf_wk%xe_sf =   0.0d0
        surf_wk%dxe_sf = 0.0d0
      end if
!
      end subroutine alloc_int_surf_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_int_surf_data(surf_wk)
!
      type(work_surface_element_mat), intent(inout) :: surf_wk
!
!
      deallocate( surf_wk%scalar_sf, surf_wk%vect_sf )
      deallocate( surf_wk%xe_sf, surf_wk%dxe_sf )
!
      end subroutine dealloc_int_surf_data
!
! -----------------------------------------------------------------------
!
      subroutine check_vect_sf(my_rank, txt, surf_wk)
!
      integer(kind = kint), intent(in) :: my_rank
      character(*), intent(in) :: txt
      type(work_surface_element_mat), intent(in) :: surf_wk
!
      integer(kind = kint) :: isurf
!
      write(50+my_rank,*) txt
      do isurf = 1, surf_wk%ntot_item
        write(50+my_rank,*) isurf, surf_wk%vect_sf(isurf,1:3)
      end do
!
      end subroutine check_vect_sf
!
!-----------------------------------------------------------------------
!
      end module t_int_surface_data
