!
!      module m_grp_data_cub_kemo
!
      module m_grp_data_cub_kemo
!
!      written by Kemorin
!
      use m_precision
!
      implicit none
!
      integer(kind=kint )  ::  nodgrptot
      integer(kind=kint )  ::  elmgrptot
      integer(kind=kint )  ::  sufgrptot
!
      integer(kind=kint )  :: neib
!
      integer(kind=kint ), dimension(:), allocatable  :: iele_group_id
!
      integer(kind=kint ), parameter            ::  nindex = 10000
      integer(kind=kint ), dimension(0:nindex)  ::   index
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_cube_ele_group_id
!
       use m_size_of_cube
!
       allocate(iele_group_id(2*nnod_cubmesh))
       call reset_cube_ele_group_id
!
       end subroutine allocate_cube_ele_group_id
!
!-----------------------------------------------------------------------
!
       subroutine reset_cube_ele_group_id
!
       iele_group_id = 0
!
       end subroutine reset_cube_ele_group_id
!
! ---------------------------------------------------------------------
!
       subroutine write_labels_4_group
!
       use m_size_4_plane
       use m_size_of_cube
       use m_cube_files_data
!
!
! ..... write 4.group information
!
          if ( iflag_data_f .ne. 1) then
            write(l_out,'( a )') '!'
            write(l_out,'( a )')                                        &
              '! boundary condition (x,y,z sym., x-forc.)'
            write(l_out,'( a )')                                        &
     &        '! 4. group information'
            write(l_out,'( a )')                                        &
     &        '! 4.1 node group'
          end if
! 
! ***** write boundary condition (x,y,z=0 plane sym., x-force)
!
            elmgrptot = 3 + (nz_all-1)
            nodgrptot = 3
            sufgrptot = 2*neib
!
       end subroutine write_labels_4_group
!
! ---------------------------------------------------------------------
!
       subroutine count_node_group(elm_type, ipe, jpe, kpe)
!
       use m_size_4_plane
       use m_size_of_cube
!
       integer(kind = kint) :: elm_type
       integer(kind = kint) :: ipe, jpe, kpe
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
            if (kpe == ndz) then 
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
            if (kpe ==ndz .and. ipe==1 .and. jpe==1 ) then
              item_tot = item_tot+1
            endif
            if (kpe ==ndz .and. ipe==ndx .and. jpe==1 ) then
              item_tot = item_tot+1
            endif
            if (kpe ==ndz .and. ipe==ndx .and. jpe==ndy ) then
              item_tot = item_tot+1
            endif
            if (kpe ==ndz .and. ipe==1 .and. jpe==ndy ) then
              item_tot = item_tot+1
            endif
            index(item_pos) = item_tot


!
       end subroutine count_node_group
!
! ---------------------------------------------------------------------
!
       subroutine count_surface_group(kpe)
!
       use m_size_4_plane
       use m_size_of_cube
!
       implicit none
!
       integer(kind = kint) :: kpe
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
              if (kpe == ndz) then 
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
