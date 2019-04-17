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
      type(communication_table), save :: comm
      type(communication_table), save :: comm_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sort_neighboring_pes
!
      integer(kind = kint) :: inum0, inum1, iflag
!
!
      comm_IO%num_neib = 0
      do inum0 = 1, comm%num_neib
        iflag = 0
        do inum1 = 1, inum0-1
         if(comm%id_neib(inum0) .eq. comm%id_neib(inum1)) iflag = 1
        end do
        if (iflag .eq. 0 ) then
          comm_IO%num_neib = comm_IO%num_neib + 1
        end if
      end do
!
      call alloc_comm_table_num(comm_IO)
!
      comm_IO%num_neib = 0
      do inum0 = 1, comm%num_neib
        iflag = 0
        do inum1 = 1, inum0-1
         if(comm%id_neib(inum0) .eq. comm%id_neib(inum1)) iflag = 1
        end do
        if (iflag .eq. 0 ) then
          comm_IO%num_neib = comm_IO%num_neib + 1
          comm_IO%id_neib(comm_IO%num_neib) = comm%id_neib(inum0)
        end if
      end do
!
      end subroutine sort_neighboring_pes
!
! ---------------------------------------------------------------------
!
      subroutine sort_communication_table
!
      integer(kind = kint) :: inod, n0, n1
      integer(kind = kint) :: ist, ied, icou_in, icou_ex
!
!
      icou_in = 0
      icou_ex = 0
      do n0 = 1, comm_IO%num_neib
        do n1 = 1, comm%num_neib
          if(comm_IO%id_neib(n0) .eq. comm%id_neib(n1)) then
            icou_in = icou_in                                           &
     &             + comm%istack_import(n1) - comm%istack_import(n1-1)
            icou_ex = icou_ex                                           &
     &             + comm%istack_export(n1) - comm%istack_export(n1-1)
          end if
        end do
        comm_IO%istack_import(n0) = icou_in
        comm_IO%istack_export(n0) = icou_ex
      end do
      comm_IO%ntot_import = comm_IO%istack_import(comm_IO%num_neib)
      comm_IO%ntot_export = comm_IO%istack_export(comm_IO%num_neib)
!
      call alloc_import_item(comm_IO)
      call alloc_export_item(comm_IO)
!
      icou_in = 0
      icou_ex = 0
      do n0 = 1, comm_IO%num_neib
        do n1 = 1, comm%num_neib
          if(comm_IO%id_neib(n0) .eq. comm%id_neib(n1)) then
            ist = comm%istack_import(n1-1)+1
            ied = comm%istack_import(n1)
            do inod = ist, ied
              icou_in = icou_in + 1
              comm_IO%item_import(icou_in) = comm%item_import(inod)
            end do
!
            ist = comm%istack_export(n1-1)+1
            ied = comm%istack_export(n1)
            do inod = ist, ied
              icou_ex = icou_ex + 1
              comm_IO%item_export(icou_ex) = comm%item_export(inod)
           end do
          end if
        end do
      end do
!
      call dealloc_comm_table(comm)
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
      call write_domain_info(l_out, pe_id-1, comm_IO)
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
      call write_import_data(l_out, comm_IO)
!
      write(l_out,'(a)', advance='NO') hd_fem_export()
      call write_export_data(l_out, comm_IO)
!
      call dealloc_comm_table(comm_IO)
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
