!
!      module m_grp_data_cub_kemo
!
!      written by Kemorin
!
!!      subroutine count_node_group                                     &
!!     &         (c_size, elm_type, nx, ny, ipe, jpe, kpe)
!!      subroutine count_surface_group(c_size, nx, ny, kpe)
!!        type(size_of_cube), intent(in) :: c_size
!
      module m_grp_data_cub_kemo
!
      use m_precision
      use t_group_data
      use t_size_of_cube
!
      implicit none
!
      integer(kind=kint )  :: neib
!
      integer(kind=kint ), parameter            ::  nindex = 10000
      integer(kind=kint ), dimension(0:nindex)  ::   index
!
!>     Structure for node group
        type (group_data), save ::         cube_nod_grp
!>     Structure for element group
        type (group_data), save ::         cube_ele_grp
!>     Structure for surface group
        type (surface_group_data), save :: cube_surf_grp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine write_labels_4_group(c_size)
!
       use t_size_of_cube
       use m_fem_mesh_labels
       use m_cube_files_data
!
      type(size_of_cube), intent(in) :: c_size
!
!
! ..... write 4.group information
!
       write(l_out,'(a)', advance='NO') hd_fem_nodgrp()
! 
! ***** write boundary condition (x,y,z=0 plane sym., x-force)
!
       cube_nod_grp%num_grp = 3
       cube_ele_grp%num_grp = 3 + (c_size%nz_all-1)
       cube_surf_grp%num_grp = 2*neib
!
       call alloc_group_num(cube_nod_grp)
       call alloc_group_num(cube_ele_grp)
       call alloc_sf_group_num(cube_surf_grp)
!
       end subroutine write_labels_4_group
!
! ---------------------------------------------------------------------
!
      subroutine count_node_group                                       &
     &         (c_size, elm_type, nx, ny, ipe, jpe, kpe)
!
      type(size_of_cube), intent(in) :: c_size
      integer(kind = kint), intent(in)  :: elm_type
      integer (kind=kint), intent(in) :: nx, ny
      integer(kind = kint), intent(in)  :: ipe, jpe, kpe
!
      integer(kind = kint) :: item_tot
      integer(kind = kint) :: item_pos
!
      item_tot = 0
      item_pos = 0
      index = 0
!                                                 .. zmin
      item_pos = 1
      if (kpe == 1) then 
        item_tot = item_tot +  nx*ny
        if ( elm_type.eq.332) then
         item_tot = item_tot +  (nx-1)*ny + nx*(ny-1)
        end if
        index(item_pos) = item_tot
      else
        item_tot = item_tot
        index(item_pos) = item_tot
      endif
!                                                 .. zmax
      item_pos = 2
      if (kpe .eq. c_size%ndz) then 
        item_tot = item_tot +  nx*ny
        if ( elm_type.eq.332) then
         item_tot = item_tot +  (nx-1)*ny + nx*(ny-1)
        end if
        index(item_pos) = item_tot
      else
        item_tot = item_tot
        index(item_pos) = item_tot
      endif
!
      item_pos = 3
      if(kpe .eq. c_size%ndz .and. ipe .eq. 1                           &
     &                       .and. jpe .eq. 1) then
        item_tot = item_tot+1
      endif
      if(kpe .eq. c_size%ndz .and. ipe .eq. c_size%ndx                  &
     &                       .and. jpe .eq. 1) then
        item_tot = item_tot+1
      endif
      if(kpe .eq. c_size%ndz .and. ipe .eq. c_size%ndx                  &
     &                       .and. jpe .eq. c_size%ndy) then
        item_tot = item_tot+1
      endif
      if(kpe .eq. c_size%ndz .and. ipe .eq. 1                           &
     &                       .and. jpe .eq. c_size%ndy) then
        item_tot = item_tot+1
      endif
      index(item_pos) = item_tot
!
      end subroutine count_node_group
!
! ---------------------------------------------------------------------
!
      subroutine count_surface_group(c_size, nx, ny, kpe)
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
      index = 0
!                                                 .. zmin
      do ibd = 1, neib
        item_pos = 2*(ibd-1)+1
        if (kpe == 1) then 
          item_tot = item_tot +  (nx-1)*(ny-1)
          index(item_pos) = item_tot
        else
          item_tot = item_tot
         index(item_pos) = item_tot
        endif
!                                                 .. zmax
        item_pos = 2*ibd
        if (kpe == c_size%ndz) then 
          item_tot = item_tot +  (nx-1)*(ny-1)
          index(item_pos) = item_tot
        else
          item_tot = item_tot
          index(item_pos) = item_tot
        endif
!
      end do
!
       end subroutine count_surface_group
!
!-----------------------------------------------------------------------
!
      end module m_grp_data_cub_kemo
