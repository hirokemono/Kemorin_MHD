!write_ele_grp_cube.f90
!     module write_ele_grp_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!       subroutine count_element_group
!       subroutine write_cube_ele_group(kpe)
!
      module write_ele_grp_cube
!
      use m_precision
!
      use m_grp_data_cub_kemo
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine count_element_group
!
       use m_size_4_plane
       use m_size_of_cube
!
       integer(kind = kint) :: item_tot
       integer(kind = kint) :: item_pos
       integer(kind = kint) :: k
!
       item_tot = 0
       item_pos = 0
       index = 0
!                                                 .. all
!
       item_pos = 1
       item_tot = item_tot + (nz_all-1)
       index(item_pos) = item_tot
       item_pos = 2
       item_tot = item_tot + (nz_all-1)
       index(item_pos) = item_tot
!
       item_pos = 3
       item_tot = item_tot + elm_fil1_tot
       index(item_pos) = item_tot
!
       end subroutine count_element_group
!
!-----------------------------------------------------------------------
!
       subroutine write_cube_ele_group(kpe)
!
       use m_size_4_plane
       use m_size_of_cube
       use m_offset_size_cube
       use m_cube_files_data
       use m_fem_mesh_labels
!
       integer(kind = kint) :: kpe
!
       integer(kind = kint) :: iele, element_id
       integer(kind = kint) :: i, j, k, item_pos, item_tot
!
       character(len=kchara) :: group_name
!
       iele = 0
!
       element_id = 1
       do k = 1, nz_all-1
        iele = iele + 1
        iele_group_id(iele) = element_id
        if ( k.gt.koff .and. k.le.(koff+nz-1) ) then
         element_id = element_id + (nx-1)*(ny-1)
        end if
       end do
!
       element_id = 0
       do k = 1, nz_all-1
        if ( k.gt.koff .and. k.le.(koff+nz-1) ) then
         element_id = element_id + (nx-1)*(ny-1)
        end if
        iele = iele + 1
        iele_group_id(iele) = element_id
       end do
!
       element_id = 0
       do k=1,nz-1
        do j=1,ny-1
         do i=1,nx-1

           element_id    =  element_id + 1
           if ( i.ge.ndepth .and. i.le.(nx-ndepth) ) then
            if ( j.ge.ndepth .and. j.le.(ny-ndepth) ) then
             if ( kpe.eq.1 .and. k.le.(nz-ndepth) ) then
              iele = iele + 1
              iele_group_id(iele) = element_id
             else if ( kpe.eq.ndz .and. k.ge.ndepth ) then
              iele = iele + 1
              iele_group_id(iele) = element_id
             else if ( k.ge.ndepth .and. k.le.(nz-ndepth) ) then
              iele = iele + 1
              iele_group_id(iele) = element_id
             end if
            end if
           end if
!
           enddo
         enddo
       enddo
!
!
!
!       write(*,*) 'nz_all gc', nx_all, ny_all, nz_all
!       write(*,*) 'index', size(index)
       item_tot = index(3)
       do k = 1, (nz_all-1)
         item_pos = 3 + k
         item_tot = item_tot + 1                                        &
     &                  + iele_group_id(k+index(1)) - iele_group_id(k)
         index(item_pos) = item_tot
       end do
!
       do k = 1, (nz_all-1)
         do i = iele_group_id(k), iele_group_id(k+index(1))
           iele = iele + 1
           iele_group_id(iele) = i
         end do
       end do
!
!
       if ( iflag_data_f .eq. 1) then
!
        write(l_out) elmgrptot
        write(l_out) (index(i),i=1,elmgrptot)
!
        write(l_out) 'layer_start'
        write(l_out) (iele_group_id(i),i=1, index(1))
!
        write(l_out) 'layer_end'
        write(l_out) (iele_group_id(i),i=index(1)+1, index(2))
!
        write(l_out) 'conductive_fluid'
        write(l_out) (iele_group_id(i),i=index(2)+1, index(3))
!
        do k = 1, (nz_all-1)
          if      (k.lt.10) then
            write(group_name,1001) k
          else if (k.lt.100) then
            write(group_name,1002) k
          else if (k.lt.1000) then
            write(group_name,1003) k
          else if (k.lt.10000) then
            write(group_name,1004) k
          else if (k.lt.100000) then
            write(group_name,1005) k
          else if (k.lt.1000000) then
            write(group_name,1006) k
          end if
!
          write(l_out) trim(group_name)
          write(l_out) iele_group_id(index(2+k)+1:index(3+k))
!
        end do
!
       else
!
         write(l_out,'(a)', advance='NO') hd_fem_elegrp()
         write(l_out,'(10i16)') elmgrptot
         write(l_out,'(10i16)') (index(i),i=1,elmgrptot)
!
         write(l_out,'(  a  )') 'layer_start'
         write(l_out,'(6i16)') (iele_group_id(i),i=1, index(1))
!
         write(l_out,'(  a  )') 'layer_end'
         write(l_out,'(6i16)')                                          &
     &            (iele_group_id(i),i=index(1)+1, index(2))
!
         write(l_out,'(  a  )') 'conductive_fluid'
         write(l_out,'(6i16)')                                          &
     &            (iele_group_id(i),i=index(2)+1, index(3))
!
        do k = 1, (nz_all-1)
!          write(*,*) 'layering group', k, nz_all
          if      (k.lt.10) then
            write(group_name,1001) k
          else if (k.lt.100) then
            write(group_name,1002) k
          else if (k.lt.1000) then
            write(group_name,1003) k
          else if (k.lt.10000) then
            write(group_name,1004) k
          else if (k.lt.100000) then
             write(group_name,1005) k
          else if (k.lt.1000000) then
            write(group_name,1006) k
          end if
!
          write(l_out,'(a)') trim(group_name)
          write(l_out,'(6i16)')                                         &
     &            (iele_group_id(i),i=index(2+k)+1, index(3+k))
        end do
!
      end if
!
 1001 format('fluid_layer_',i1)
 1002 format('fluid_layer_',i2)
 1003 format('fluid_layer_',i3)
 1004 format('fluid_layer_',i4)
 1005 format('fluid_layer_',i5)
 1006 format('fluid_layer_',i6)
!
       end subroutine write_cube_ele_group
!
! ----------------------------------------------------------------------
!
      end module write_ele_grp_cube
