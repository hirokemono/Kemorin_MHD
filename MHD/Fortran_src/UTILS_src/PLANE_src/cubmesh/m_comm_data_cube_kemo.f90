!m_comm_data_cube_kemo.f90
!      module m_comm_data_cube_kemo
!
!      Written by Kemorin
!
!!      subroutine allocate_communication_data(elm_type, c_size)
!!        type(size_of_cube), intent(in) :: c_size
!
      module m_comm_data_cube_kemo
!
      use m_precision
      use t_comm_table
!
       implicit none
!
! ......................................................................
!
      integer(kind=kint ), parameter  ::  neibpetot_max = 26
!
      type(communication_table), save :: comm
      type(communication_table), save :: comm_new
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_communication_data(elm_type, c_size)
!
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      integer (kind = kint) :: elm_type
      integer (kind = kint) :: inum0
!
!
       if (elm_type.eq.331) then
        inum0 = (c_size%nxi + 2*c_size%ndepth)                          &
     &          * (c_size%nyi + 2*c_size%ndepth)                        &
     &          * (c_size%nzi + 2*c_size%ndepth)                        &
     &         - c_size%nxi * c_size%nyi * c_size%nzi
       else if (elm_type.eq.332) then
        inum0 = (c_size%nxi + 2*c_size%ndepth)                          &
     &         * (c_size%nyi + 2*c_size%ndepth)                         &
     &         * (c_size%nzi + 2*c_size%ndepth)                         &
     &        + (c_size%nxi+2*c_size%ndepth - 1)                        &
     &         * (c_size%nyi + 2*c_size%ndepth)                         &
     &         * (c_size%nzi + 2*c_size%ndepth)                         &
     &        + (c_size%nxi + 2*c_size%ndepth)                          &
     &         * (c_size%nyi + 2*c_size%ndepth - 1)                     &
     &         * (c_size%nzi + 2*c_size%ndepth)                         &
     &        + (c_size%nxi + 2*c_size%ndepth)                          &
     &         * (c_size%nyi + 2*c_size%ndepth)                         &
     &         * (c_size%nzi + 2*c_size%ndepth - 1)                     &
     &        - c_size%nxi * c_size%nyi * c_size%nzi                    &
     &        - (c_size%nxi - 1) * c_size%nyi * c_size%nzi              &
     &        - c_size%nxi * (c_size%nyi - 1) * c_size%nzi              &
     &        - c_size%nxi * c_size%nyi * (c_size%nzi - 1)
       else
         inum0 = 0
       end if
!
       allocate ( comm%id_neib(neibpetot_max) )
       allocate ( comm%istack_import(0:neibpetot_max) )
       allocate ( comm%istack_export(0:neibpetot_max) )
!
       allocate ( comm_new%id_neib(neibpetot_max) )
       allocate ( comm_new%istack_import(0:neibpetot_max) )
       allocate ( comm_new%istack_export(0:neibpetot_max) )
!
       allocate ( comm%item_import(inum0) )
       allocate ( comm%item_export(inum0) )
!
       comm%item_import = 0
       comm%item_export = 0
!
       allocate ( comm_new%item_import(inum0) )
       allocate ( comm_new%item_export(inum0) )
!
       comm_new%item_import = 0
       comm_new%item_export = 0
!
       call reset_communication_data
!
       end subroutine allocate_communication_data
!
! ----------------------------------------------------------------------
!
      subroutine reset_communication_data
!
      comm%item_import = 0
      comm_new%item_import = 0
      comm%item_export = 0
      comm_new%item_export = 0
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
      comm_new%num_neib = 0
      do inum0 = 1, comm%num_neib
        iflag = 0
        do inum1 = 1, inum0-1
         if(comm%id_neib(inum0) .eq. comm%id_neib(inum1)) iflag = 1
        end do
        if (iflag .eq. 0 ) then
          comm_new%num_neib = comm_new%num_neib + 1
          comm_new%id_neib(comm_new%num_neib) = comm%id_neib(inum0)
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
      integer(kind = kint) :: ist, ied
!
!
      node_id = 0
      inum0 = 0
      do n0 = 1, comm_new%num_neib
        inum0 = inum0 + 1
        do n1 = 1, comm%num_neib
          if(comm_new%id_neib(n0) .eq. comm%id_neib(n1)) then
            ist = comm%istack_import(n1-1)+1
            ied = comm%istack_import(n1)
            do inod = ist, ied
              node_id = node_id + 1
              comm_new%item_import(node_id) = comm%item_import(inod)
            end do
          end if
        end do
        comm_new%istack_import(n0) = node_id
      end do
      comm_new%ntot_import = comm_new%istack_import(comm_new%num_neib)
!
      node_id = 0
      inum0 = 0
      do n0 = 1, comm_new%num_neib
        inum0 = inum0 + 1
        do n1 = 1, comm%num_neib
          if ( comm_new%id_neib(n0) .eq. comm%id_neib(n1)) then
            ist = comm%istack_export(n1-1)+1
            ied = comm%istack_export(n1)
            do inod = ist, ied
              node_id = node_id + 1
              comm_new%item_export(node_id) = comm%item_export(inod)
           end do
          end if
        end do
        comm_new%istack_export(n0) = node_id
      end do
      comm_new%ntot_export = comm_new%istack_export(comm_new%num_neib)
!
      end subroutine sort_communication_table
!
! ----------------------------------------------------------------------
!
      subroutine write_pe_data(pe_id)
!
      use m_cube_files_data
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer(kind = kint), intent(in) :: pe_id
!
!
      write(l_out,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(l_out, pe_id-1, comm_new)
!
      end subroutine write_pe_data
!
! ----------------------------------------------------------------------
!
      subroutine write_communication_data
!
      use m_fem_mesh_labels
      use m_cube_files_data
      use domain_data_IO
!
!
      write(l_out,'(a)', advance='NO') hd_fem_import()
      call write_import_data(l_out, comm_new)
!
      write(l_out,'(a)', advance='NO') hd_fem_export()
      call write_export_data(l_out, comm_new)
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
      use domain_data_IO
!
      integer(kind = kint) :: inod, pe_id, ifile
!
       ifile = 29 + pe_id
       write(ifile,'(a)', advance='NO') hd_fem_import()
       call write_import_data(ifile, comm)
!
      write(ifile,'(a)', advance='NO') hd_fem_export()
      call write_export_data(ifile, comm)
!
      end subroutine write_org_communication_data
!
! ----------------------------------------------------------------------
!
      end module m_comm_data_cube_kemo
