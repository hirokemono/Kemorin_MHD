!write_nod_grp_cube.f90
!     module write_nod_grp_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine write_node_group                                     &
!!     &         (c_size, loc_id, nx, ny, nz, ipe, jpe, kpe)
!!      subroutine write_node_group_quad                                &
!!     &         (c_size, loc_id, nx, ny, nz, ipe, jpe, kpe)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(local_node_id_cube), intent(in) :: loc_id
!
      module write_nod_grp_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_local_node_id_cube
      use m_grp_data_cub_kemo
      use m_cube_files_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_node_group                                       &
     &         (c_size, loc_id, nx, ny, nz, ipe, jpe, kpe)
!
      type(size_of_cube), intent(in) :: c_size
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind=kint), intent(in) :: nx, ny, nz
      integer (kind=kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind=kint) :: i, j, i1, j1
      integer (kind=kint) :: num0
!
!
       write(l_out,'(10i16)')   nodgrptot
       write(l_out,'(10i16)')  (index(i),i=1,nodgrptot)
!                                                 ..zmin
       write(l_out,'(a)'  )  'zmin'
       if (kpe == 1) then 
         write(l_out,'(6i16)') (loc_id%node_id_lc(1:nx,j,1),j=1,ny)
       else
         write(l_out,'(6i16)')
       endif
!                                                 .. zmax
      write(l_out,'(a)'  )  'zmax'
      if(kpe == c_size%ndz) then 
        write(l_out,'(6i16)') (loc_id%node_id_lc(1:nx,j,nz),j=1,ny)
      else
        write(l_out,'(6i16)')
      endif
!                                                           . mid.
      write(l_out,'(a)'  )  'Press'
      num0 = 0
      if(kpe == c_size%ndz .and. ipe==1 .and. jpe==1 ) then
        write(l_out,'(i16)') loc_id%node_id_lc(1,1,nz)
        num0 = 1
      endif
      if(kpe == c_size%ndz .and. ipe==c_size%ndx .and. jpe==1) then
        i1 = c_size%nxi + c_size%ndepth
        write(l_out,'(i16)') loc_id%node_id_lc(i1,1,nz)
        num0 = 1
      endif
      if (kpe == c_size%ndz .and. ipe == c_size%ndx                     &
    &                       .and. jpe == c_size%ndy) then
        i1 = c_size%nxi + c_size%ndepth
        j1 = c_size%nyi + c_size%ndepth
        write(l_out,'(i16)') loc_id%node_id_lc(i1,j1,nz)
        num0 = 1
      endif
      if(kpe == c_size%ndz .and. ipe==1 .and. jpe == c_size%ndy) then
        j1 = c_size%nyi + c_size%ndepth
        write(l_out,'(i16)') loc_id%node_id_lc(1,j1,nz)
        num0 = 1
      endif
      if ( num0 .eq. 0 ) then
        write(l_out,'(i16)')
      endif
!
       end subroutine write_node_group
!
! ----------------------------------------------------------------------
!
      subroutine write_node_group_quad                                  &
     &         (c_size, loc_id, nx, ny, nz, ipe, jpe, kpe)
!
      type(size_of_cube), intent(in) :: c_size
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind=kint), intent(in) :: nx, ny, nz
      integer (kind=kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind=kint) :: ia, ja, ib, jb
      integer (kind=kint) :: i, j, i1, j1
      integer (kind=kint) :: num0
!
!
      write(l_out,'(10i16)')   nodgrptot
      write(l_out,'(10i16)')  (index(i),i=1,nodgrptot)
!                                                 ..zmin
      write(l_out,'(a)'  )  'zmin'
!
      if (kpe == 1) then 
        write(l_out,'(i16)') (loc_id%node_id_lc(1:nx,j,1),j=1,ny),      &
     &                     (loc_id%edge_id_lc(1:nx-1,ja,1,1),ja=1,ny),  &
     &                     (loc_id%edge_id_lc(1:nx,jb,1,2),jb=1,ny-1)
      else
        write(l_out,'(6i16)')
      endif
!                                                 .. zmax
      write(l_out,'(a)'  )  'zmax'
      if(kpe == c_size%ndz) then 
        write(l_out,'(i16)') (loc_id%node_id_lc(1:nx,j,nz),j=1,ny),     &
     &                     (loc_id%edge_id_lc(1:nx-1,ja,nz,1),ja=1,ny), &
     &                     (loc_id%edge_id_lc(1:nx,jb,nz,2),jb=1,ny-1)

      else
        write(l_out,'(i16)')
      endif
!                                                           . mid.
      write(l_out,'(a)'  )  'Press'
      num0 = 0
      if (kpe == c_size%ndz .and. ipe==1 .and. jpe==1 ) then
        write(l_out,'(i16)') loc_id%node_id_lc(1,1,nz)
        num0 = 1
      endif
      if(kpe == c_size%ndz .and. ipe == c_size%ndx .and. jpe==1) then
        i1 = c_size%nxi + c_size%ndepth
        write(l_out,'(i16)') loc_id%node_id_lc(i1,1,nz)
        num0 = 1
      endif
      if(kpe == c_size%ndz .and. ipe == c_size%ndx                      &
     &                     .and. jpe == c_size%ndy) then
        i1 = c_size%nxi + c_size%ndepth
        j1 = c_size%nyi + c_size%ndepth
        write(l_out,'(i16)') loc_id%node_id_lc(i1,j1,nz)
        num0 = 1
      endif
      if(kpe == c_size%ndz .and. ipe==1 .and. jpe == c_size%ndy) then
        j1 = c_size%nyi + c_size%ndepth
        write(l_out,'(i16)') loc_id%node_id_lc(1,j1,nz)
        num0 = 1
      endif
      if ( num0 .eq. 0 ) then
        write(l_out,'(i16)')
      endif
!
       end subroutine write_node_group_quad
!
! ----------------------------------------------------------------------
!
      end module write_nod_grp_cube
