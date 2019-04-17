!write_nod_grp_cube.f90
!     module write_nod_grp_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine const_node_group(c_size, c_each, loc_id,             &
!!     &          ipe, jpe, kpe, nod_grp)
!!      subroutine const_node_group_quad(c_size, c_each, loc_id,        &
!!     &          ipe, jpe, kpe, nod_grp)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(local_node_id_cube), intent(in) :: loc_id
!!        type (group_data), intent(inout) :: nod_grp
!
      module write_nod_grp_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_local_node_id_cube
      use t_group_data
      use m_cube_files_data
!
      implicit none
!
      private :: count_node_group, set_node_group, set_node_group_quad
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_node_group(c_size, c_each, loc_id,               &
     &          ipe, jpe, kpe, nod_grp)
!
      use m_fem_mesh_labels
      use groups_IO
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(local_node_id_cube), intent(in) :: loc_id
      integer(kind = kint), intent(in) :: ipe, jpe, kpe
!
      type (group_data), intent(inout) :: nod_grp
!
!
      nod_grp%num_grp = 3
      call alloc_group_num(nod_grp)
!
      call count_node_group(c_size, 331, c_each%nx, c_each%ny,          &
     &    ipe, jpe, kpe, nod_grp)
!
      nod_grp%num_item = nod_grp%istack_grp(nod_grp%num_grp)
      call alloc_group_item(nod_grp)
!
      call set_node_group(c_size, loc_id,                               &
     &    c_each%nx, c_each%ny, c_each%nz, ipe, jpe, kpe, nod_grp)
!
      write(l_out,'(a)', advance='NO') hd_fem_nodgrp()
      call write_grp_data(l_out, nod_grp)
!
      end subroutine const_node_group
!
! ----------------------------------------------------------------------
!
      subroutine const_node_group_quad(c_size, c_each, loc_id,          &
     &          ipe, jpe, kpe, nod_grp)
!
      use m_fem_mesh_labels
      use groups_IO
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(local_node_id_cube), intent(in) :: loc_id
      integer(kind = kint), intent(in) :: ipe, jpe, kpe
!
      type (group_data), intent(inout) :: nod_grp
!
!
      nod_grp%num_grp = 3
      call alloc_group_num(nod_grp)
! 
      call count_node_group(c_size, 332, c_each%nx, c_each%ny,          &
     &    ipe, jpe, kpe, nod_grp)
!
      nod_grp%num_item = nod_grp%istack_grp(nod_grp%num_grp)
      call alloc_group_item(nod_grp)
!
      call set_node_group_quad(c_size, loc_id,                          &
     &    c_each%nx, c_each%ny, c_each%nz, ipe, jpe, kpe, nod_grp)
      call write_grp_data(l_out, nod_grp)
!
      end subroutine const_node_group_quad
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_node_group                                       &
     &         (c_size, elm_type, nx, ny, ipe, jpe, kpe, nod_grp)
!
      type(size_of_cube), intent(in) :: c_size
      integer(kind = kint), intent(in)  :: elm_type
      integer (kind=kint), intent(in) :: nx, ny
      integer(kind = kint), intent(in)  :: ipe, jpe, kpe
!
      type (group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: item_tot
      integer(kind = kint) :: item_pos
!
      item_tot = 0
      item_pos = 0
      nod_grp%istack_grp = 0
!                                                 .. zmin
      item_pos = 1
      nod_grp%grp_name(item_pos) = 'zmin'
      if (kpe == 1) then 
        item_tot = item_tot +  nx*ny
        if(elm_type .eq. 332) then
         item_tot = item_tot +  (nx-1)*ny + nx*(ny-1)
        end if
        nod_grp%istack_grp(item_pos) = item_tot
      else
        item_tot = item_tot
        nod_grp%istack_grp(item_pos) = item_tot
      end if
!                                                 .. zmax
      item_pos = 2
      nod_grp%grp_name(item_pos) = 'zmax'
      if (kpe .eq. c_size%ndz) then 
        item_tot = item_tot +  nx*ny
        if(elm_type.eq. 332) then
          item_tot = item_tot +  (nx-1)*ny + nx*(ny-1)
        end if
        nod_grp%istack_grp(item_pos) = item_tot
      else
        item_tot = item_tot
        nod_grp%istack_grp(item_pos) = item_tot
      endif
!
      item_pos = 3
      nod_grp%grp_name(item_pos) = 'Press'
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
      nod_grp%istack_grp(item_pos) = item_tot
!
      end subroutine count_node_group
!
! ---------------------------------------------------------------------
!
      subroutine set_node_group                                         &
     &         (c_size, loc_id, nx, ny, nz, ipe, jpe, kpe, nod_grp)
!
      type(size_of_cube), intent(in) :: c_size
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind=kint), intent(in) :: nx, ny, nz
      integer (kind=kint), intent(in) :: ipe, jpe, kpe
!
      type (group_data), intent(inout) :: nod_grp
!
      integer (kind=kint) :: i, j, i1, j1
      integer (kind=kint) :: icou
!
!
      icou = nod_grp%istack_grp(0)
      if (kpe == 1) then 
        do j = 1, ny
          do i = 1, nx
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%node_id_lc(i,j,1)
          end do
        end do
      endif
!                                                 .. zmax
      icou = nod_grp%istack_grp(1)
      if(kpe == c_size%ndz) then 
        do j = 1, ny
          do i = 1, nx
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%node_id_lc(i,j,nz)
          end do
        end do
      endif
!                                                           . mid.
      icou = nod_grp%istack_grp(2)
      if(kpe == c_size%ndz .and. ipe==1 .and. jpe==1 ) then
        icou = icou + 1
        nod_grp%item_grp(icou) = loc_id%node_id_lc(1,1,nz)
      endif
      if(kpe == c_size%ndz .and. ipe==c_size%ndx .and. jpe==1) then
        icou = icou + 1
        i1 = c_size%nxi + c_size%ndepth
        nod_grp%item_grp(icou) =  loc_id%node_id_lc(i1,1,nz)
      endif
      if (kpe == c_size%ndz .and. ipe == c_size%ndx                     &
    &                       .and. jpe == c_size%ndy) then
        icou = icou + 1
        i1 = c_size%nxi + c_size%ndepth
        j1 = c_size%nyi + c_size%ndepth
        nod_grp%item_grp(icou) =  loc_id%node_id_lc(i1,j1,nz)
      endif
      if(kpe == c_size%ndz .and. ipe==1 .and. jpe == c_size%ndy) then
        icou = icou + 1
        j1 = c_size%nyi + c_size%ndepth
        nod_grp%item_grp(icou) =  loc_id%node_id_lc(1,j1,nz)
      endif
!
       end subroutine set_node_group
!
! ----------------------------------------------------------------------
!
      subroutine set_node_group_quad                                    &
     &         (c_size, loc_id, nx, ny, nz, ipe, jpe, kpe, nod_grp)
!
      type(size_of_cube), intent(in) :: c_size
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind=kint), intent(in) :: nx, ny, nz
      integer (kind=kint), intent(in) :: ipe, jpe, kpe
!
      type (group_data), intent(inout) :: nod_grp
!
      integer (kind=kint) :: ja, jb
      integer (kind=kint) :: i, j, i1, j1, icou
!
!
      icou = nod_grp%istack_grp(0)
      if (kpe == 1) then 
        do j = 1, ny
          do i = 1, nx
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%node_id_lc(i,j,1)
          end do
        end do
        do ja = 1, ny
          do i = 1, nx-1
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%edge_id_lc(i,ja,1,1)
          end do
        end do
        do jb = 1, ny-1
          do i = 1, nx
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%edge_id_lc(i,jb,1,2)
          end do
        end do
      endif
!                                                 .. zmax
      icou = nod_grp%istack_grp(1)
      if(kpe == c_size%ndz) then 
        do j = 1, ny
          do i = 1, nx
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%node_id_lc(i,j,nz)
          end do
        end do
        do ja = 1, ny
          do i = 1, nx-1
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%edge_id_lc(i,ja,nz,1)
          end do
        end do
        do jb = 1, ny-1
          do i = 1, nx
            icou = icou + 1
            nod_grp%item_grp(icou) = loc_id%edge_id_lc(i,jb,nz,2)
          end do
        end do
      end if
!                                                           . mid.
      icou = nod_grp%istack_grp(2)
      if (kpe == c_size%ndz .and. ipe==1 .and. jpe==1 ) then
        icou = icou + 1
        nod_grp%item_grp(icou) = loc_id%node_id_lc(1,1,nz)
      endif
      if(kpe == c_size%ndz .and. ipe == c_size%ndx .and. jpe==1) then
        icou = icou + 1
        i1 = c_size%nxi + c_size%ndepth
        nod_grp%item_grp(icou) = loc_id%node_id_lc(i1,1,nz)
      endif
      if(kpe == c_size%ndz .and. ipe == c_size%ndx                      &
     &                     .and. jpe == c_size%ndy) then
        icou = icou + 1
        i1 = c_size%nxi + c_size%ndepth
        j1 = c_size%nyi + c_size%ndepth
        nod_grp%item_grp(icou) = loc_id%node_id_lc(i1,j1,nz)
      endif
      if(kpe == c_size%ndz .and. ipe==1 .and. jpe == c_size%ndy) then
        icou = icou + 1
        j1 = c_size%nyi + c_size%ndepth
        nod_grp%item_grp(icou) = loc_id%node_id_lc(1,j1,nz)
      endif
!
       end subroutine set_node_group_quad
!
! ----------------------------------------------------------------------
!
      end module write_nod_grp_cube
