!write_surf_grp_cube.f90
!     module write_surf_grp_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!       subroutine write_surface_group(ndz, nx, ny, nz, kpe)
!
      module write_surf_grp_cube
!
      use m_precision
      use m_constants
!
      use m_grp_data_cub_kemo
      use m_cube_files_data
!
      implicit none
!
      character(len=kchara), parameter :: zmin_head = 'zmin_'
      character(len=kchara), parameter :: zmax_head = 'zmax_'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_surface_group(c_size, c_each, kpe)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      integer(kind = kint), intent(in) :: kpe
!
!
      call count_surface_group                                          &
     &   (c_size, c_each%nx, c_each%ny, kpe)
!
       cube_surf_grp%num_item                                           &
     &      = cube_surf_grp%istack_grp(cube_surf_grp%num_grp)
       call alloc_sf_group_item(cube_surf_grp)
!
       call set_surface_group(c_size%ndepth, c_size%ndz,                &
     &     c_each%nx, c_each%ny, c_each%nz, kpe)
       call write_surface_group
!
      end subroutine const_surface_group
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_surface_group(c_size, nx, ny, kpe)
!
      use set_parallel_file_name
!
      type(size_of_cube), intent(in) :: c_size
      integer (kind=kint), intent(in) :: nx, ny
      integer(kind = kint), intent(in) :: kpe
!
      integer(kind = kint) :: ibd
      integer(kind = kint) :: item_tot
      integer(kind = kint) :: item_pos
!
      item_tot = 0
      item_pos = 0
      cube_surf_grp%istack_grp = 0
!                                                 .. zmin
      do ibd = 1, c_size%ndepth
        item_pos = 2*(ibd-1)+1
        if (kpe == 1) then 
          item_tot = item_tot +  (nx-1)*(ny-1)
          cube_surf_grp%istack_grp(item_pos) = item_tot
        else
          item_tot = item_tot
          cube_surf_grp%istack_grp(item_pos) = item_tot
        endif
!
        if (ibd .eq. 1) then
          cube_surf_grp%grp_name(item_pos) = 'zmin_surf'
        else
          call add_index_after_name                                     &
      &      (ibd, zmin_head, cube_surf_grp%grp_name(item_pos))
        end if
!
!                                                 .. zmax
        item_pos = 2*ibd
        if (kpe == c_size%ndz) then 
          item_tot = item_tot +  (nx-1)*(ny-1)
          cube_surf_grp%istack_grp(item_pos) = item_tot
        else
          item_tot = item_tot
          cube_surf_grp%istack_grp(item_pos) = item_tot
        endif
!
        if (ibd .eq. 1) then
          cube_surf_grp%grp_name(item_pos) = 'zmax_surf'
        else
          call add_index_after_name                                     &
      &      (ibd, zmax_head, cube_surf_grp%grp_name(item_pos))
        end if
      end do
!
      end subroutine count_surface_group
!
!-----------------------------------------------------------------------
!
      subroutine set_surface_group(ndepth, ndz, nx, ny, nz, kpe)
!
      integer(kind = kint), intent(in) :: ndepth, ndz
      integer(kind = kint), intent(in) :: nx, ny, nz
      integer(kind = kint), intent(in) :: kpe
!
      integer(kind = kint) :: ibd, item_pos, icou
      integer(kind = kint) :: i, j, istart
!
!
       do ibd = 1, ndepth
!                                                 .. zmin_surf
         item_pos = 2*(ibd-1)+1
         icou = cube_surf_grp%istack_grp(item_pos-1)
!
         if (kpe == 1) then 
           istart = (nx-1)*(ny-1)*(ibd-1)
           do j = 1, ny-1
             do i = 1, nx-1
               icou = icou + 1
               cube_surf_grp%item_sf_grp(1,icou)                        &
     &              = i+(j-1)*(nx-1) + istart
               cube_surf_grp%item_sf_grp(2,icou) = ifive
             end do
           end do
         endif
!                                                 .. zmax_surf
         item_pos = 2*ibd
         icou = cube_surf_grp%istack_grp(item_pos-1)
!
         if (kpe == ndz) then 
           istart = (nx-1)*(ny-1)*(nz-ibd-1)
           do j = 1, ny-1
             do i = 1, nx-1
               icou = icou + 1
               cube_surf_grp%item_sf_grp(1,icou)                        &
     &              = i+(j-1)*(nx-1) + istart
               cube_surf_grp%item_sf_grp(2,icou) = isix
             end do
           end do
         end if
       end do
!
      end subroutine set_surface_group
!
! ----------------------------------------------------------------------
!
      subroutine write_surface_group
!
      use m_fem_mesh_labels
      use groups_IO
!
!
       write(l_out,'(a)', advance='NO') hd_fem_sfgrp()
       call write_surf_grp_data(l_out, cube_surf_grp)
!
      end subroutine write_surface_group
!
! ----------------------------------------------------------------------
!
      end module write_surf_grp_cube
