!
!      module m_int_surface_data
!
!      Writtn  by H. Matsui on Sep., 2005
!
!      subroutine allocate_int_surf_data
!      subroutine deallocate_int_surf_data
!      subroutine check_vect_sf(my_rank, txt)
!
      module m_int_surface_data
!
      use m_precision
!
      implicit none
!
!
      real (kind=kreal), allocatable :: scalar_sf(:)
      real (kind=kreal), allocatable :: vect_sf(:,:)
!
      real (kind=kreal), allocatable :: xe_sf(:,:,:)
      real (kind=kreal), allocatable :: dxe_sf(:,:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_int_surf_data
!
      use m_geometry_parameter
      use m_surface_group
!
      allocate( scalar_sf(num_surf_bc) )
      allocate( vect_sf(num_surf_bc,3) )
      allocate( xe_sf(num_surf_bc,4,nnod_4_surf) )
      allocate( dxe_sf(num_surf_bc,4,nnod_4_surf) )
!
      if(num_surf_bc .gt. 0) then
        scalar_sf = 0.0d0
        vect_sf = 0.0d0
        xe_sf =   0.0d0
        dxe_sf = 0.0d0
      end if
!
      end subroutine allocate_int_surf_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_int_surf_data
!
      deallocate( scalar_sf, vect_sf )
      deallocate( xe_sf, dxe_sf )
!
      end subroutine deallocate_int_surf_data
!
! -----------------------------------------------------------------------
!
       subroutine check_vect_sf(my_rank, txt)
!
       use m_surface_group
!
       integer(kind = kint), intent(in) :: my_rank
       character(*), intent(in) :: txt
       integer(kind = kint) :: i, isurf
!
       write(50+my_rank,*) txt
       do isurf = 1, num_surf_bc
         write(50+my_rank,*) isurf, (vect_sf(isurf,i),i=1, 3)
       end do
!
       end subroutine check_vect_sf
!
!-----------------------------------------------------------------------
!
      end module m_int_surface_data
