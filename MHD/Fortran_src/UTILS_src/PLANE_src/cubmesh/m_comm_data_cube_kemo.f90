!m_comm_data_cube_kemo.f90
!      module m_comm_data_cube_kemo
!
!      Written by Kemorin
!
      module m_comm_data_cube_kemo
!
      use m_precision
!
       implicit none
!
! ......................................................................
!
      integer(kind=kint ), parameter  ::  neibpetot_max = 26
!
      integer(kind=kint )                            ::  neibpetot
      integer(kind=kint )                            ::  neibpetot_new
      integer(kind=kint ), dimension(neibpetot_max)  ::  neibpe
      integer(kind=kint ), dimension(neibpetot_max)  ::  neibpe_new
!
      integer(kind=kint ) :: num_import
      integer(kind=kint ) :: num_export
!
      integer(kind=kint ), dimension(0:neibpetot_max)                   &
     &      :: stack_import = 0
      integer(kind=kint ), dimension(0:neibpetot_max)                   &
     &      :: stack_export = 0
!
      integer(kind=kint ), dimension(0:neibpetot_max)                   &
     &      :: stack_import_new = 0
      integer(kind=kint ), dimension(0:neibpetot_max)                   &
     &      :: stack_export_new = 0
!
      integer(kind=kint ), dimension(:), allocatable :: item_import
      integer(kind=kint ), dimension(:), allocatable :: item_import_new
!
      integer(kind=kint ), dimension(:), allocatable :: item_export
      integer(kind=kint ), dimension(:), allocatable :: item_export_new
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_communication_data(elm_type)
!
       use m_size_of_cube
!
       integer (kind = kint) :: elm_type
       integer (kind = kint) :: inum0
!
!
       if (elm_type.eq.331) then
        inum0 = (nxi+2*ndepth)*(nyi+2*ndepth)*(nzi+2*ndepth)            &
     &         - nxi*nyi*nzi
       else if (elm_type.eq.332) then
        inum0 = (nxi+2*ndepth)*(nyi+2*ndepth)*(nzi+2*ndepth)            &
     &        + (nxi+2*ndepth-1)*(nyi+2*ndepth)*(nzi+2*ndepth)          &
     &        + (nxi+2*ndepth)*(nyi+2*ndepth-1)*(nzi+2*ndepth)          &
     &        + (nxi+2*ndepth)*(nyi+2*ndepth)*(nzi+2*ndepth-1)          &
     &         - nxi*nyi*nzi     - (nxi-1)*nyi*nzi                      &
     &         - nxi*(nyi-1)*nzi - nxi*nyi*(nzi-1)
       else
         inum0 = 0
       end if
!
       allocate ( item_import(inum0) )
       allocate ( item_import_new(inum0) )
!
       allocate ( item_export(inum0) )
       allocate ( item_export_new(inum0) )
!
       item_import = 0
       item_export = 0
       item_import_new = 0
       item_export_new = 0
!
       call reset_communication_data
!
       end subroutine allocate_communication_data
!
! ----------------------------------------------------------------------
!
       subroutine reset_communication_data
!
             item_import = 0
             item_import_new = 0
             item_export = 0
             item_export_new = 0
!
       end subroutine reset_communication_data
!
! ----------------------------------------------------------------------
!
       subroutine sort_neighboring_pes
!
       integer(kind = kint) :: inum0, inum1, iflag
!
!
             neibpetot_new = 0
             do inum0 = 1, neibpetot
              iflag = 0
              do inum1 = 1, inum0-1
               if ( neibpe(inum0) .eq. neibpe(inum1) ) iflag = 1
              end do
              if (iflag .eq. 0 ) then
               neibpetot_new = neibpetot_new+1
               neibpe_new(neibpetot_new) = neibpe(inum0)
              end if
             end do
!
       end subroutine sort_neighboring_pes
!
! ---------------------------------------------------------------------
!
       subroutine sort_communication_table
!
       integer(kind = kint) :: inum0, inod, node_id, n0, n1
!
!
            node_id = 0
            inum0 = 0
            do n0 = 1, neibpetot_new
             inum0 = inum0 + 1
             do n1 = 1, neibpetot
              if ( neibpe_new(n0) .eq. neibpe(n1) ) then
               do inod = stack_import(n1-1)+1, stack_import(n1)
                node_id = node_id + 1
                item_import_new(node_id) = item_import(inod)
               end do
              end if
             end do
             stack_import_new(n0) = node_id
            end do
!
            node_id = 0
            inum0 = 0
            do n0 = 1, neibpetot_new
             inum0 = inum0 + 1
             do n1 = 1, neibpetot
              if ( neibpe_new(n0) .eq. neibpe(n1) ) then
               do inod = stack_export(n1-1)+1, stack_export(n1)
                node_id = node_id + 1
                item_export_new(node_id) = item_export(inod)
              end do
              end if
             end do
             stack_export_new(n0) = node_id
            end do
!
       end subroutine sort_communication_table
!
! ----------------------------------------------------------------------
!
       subroutine write_pe_data(pe_id)
!
       use m_cube_files_data
       use m_fem_mesh_labels
!
       integer(kind = kint) :: pe_id
       integer(kind = kint) :: i
!

       write(pe_id,'(a)', advance='NO') hd_fem_para()

       write(l_out,'(i16)')  pe_id-1
       write(l_out,'(i16)')  neibpetot_new
       write(l_out,'(10i16)') (neibpe_new(i)-1,i=1,neibpetot_new)
!
       end subroutine write_pe_data
!
! ----------------------------------------------------------------------
!
       subroutine write_communication_data
!
       use m_fem_mesh_labels
       use m_cube_files_data
!
       integer(kind = kint) :: inod
!
!
       write(l_out,'(a)', advance='NO') hd_fem_import()
       write(l_out,'(10i16)') stack_import_new(1:neibpetot_new)

       do inod = 1, num_import
         write(l_out,'(10i16)') item_import_new(inod)
       end do
!
       write(l_out,'(a)', advance='NO') hd_fem_export()
       write(l_out,'(10i16)') stack_export_new(1:neibpetot_new)

       do inod = 1, num_export
         write(l_out,'(10i16)') item_export_new(inod)
       end do
!
       end subroutine write_communication_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine write_org_communication_data(pe_id)
!
       use m_fem_mesh_labels
       use m_cube_files_data
!
       integer(kind = kint) :: i, inod, pe_id, ifile
!
       ifile = 29 + pe_id
       write(ifile,'(a)', advance='NO') hd_fem_import()
       write(ifile,'(10i16)') stack_import(1:neibpetot)

      do inod = 1, num_import
        write(ifile,'(10i16)') item_import(inod)
      end do
!
      write(ifile,'(a)', advance='NO') hd_fem_export()
      write(ifile,'(10i16)') stack_export(1:neibpetot)

      do inod = 1, num_export
        write(ifile,'(10i16)') item_export(inod)
      end do
!
       end subroutine write_org_communication_data
!
! ----------------------------------------------------------------------
!
      end module m_comm_data_cube_kemo
