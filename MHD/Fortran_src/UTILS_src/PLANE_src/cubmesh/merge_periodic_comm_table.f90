!merge_periodic_comm_table.f90
!      module merge_periodic_comm_table
!
!      Written by Kemorin
!
!!      subroutine allocate_communication_data(elm_type, c_size)
!!        type(size_of_cube), intent(in) :: c_size
!
      module merge_periodic_comm_table
!
      use m_precision
      use t_comm_table
!
       implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sort_neighboring_pes(comm, comm_IO)
!
      type(communication_table), intent(in) :: comm
      type(communication_table), intent(inout) :: comm_IO
!
!
      call count_sort_neib_ids(comm, comm_IO)
      call alloc_comm_table_num(comm_IO)
      call set_sort_neib_ids(comm, comm_IO)
!
      end subroutine sort_neighboring_pes
!
! ---------------------------------------------------------------------
!
      subroutine sort_communication_table(comm, comm_IO)
!
      type(communication_table), intent(inout) :: comm
      type(communication_table), intent(inout) :: comm_IO
!
!
      call set_sortedd_comm_stack(comm, comm_IO)
!
      call alloc_import_item(comm_IO)
      call alloc_export_item(comm_IO)
!
      call set_sortedd_comm_table(comm, comm_IO)
      call dealloc_comm_table(comm)
!
      end subroutine sort_communication_table
!
! ----------------------------------------------------------------------
!
      subroutine write_communication_data(id_file, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(in) :: comm_IO
!
      write(id_file,'(a)', advance='NO') hd_fem_import()
      call write_import_data(id_file, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_fem_export()
      call write_export_data(id_file, comm_IO)
!
      end subroutine write_communication_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_sort_neib_ids(comm, comm_IO)
!
      type(communication_table), intent(in) :: comm
      type(communication_table), intent(inout) :: comm_IO
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
      end subroutine count_sort_neib_ids
!
! ---------------------------------------------------------------------
!
      subroutine set_sort_neib_ids(comm, comm_IO)
!
      type(communication_table), intent(in) :: comm
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: inum0, inum1, iflag, icou
!
!
      icou = 0
      do inum0 = 1, comm%num_neib
        iflag = 0
        do inum1 = 1, inum0-1
         if(comm%id_neib(inum0) .eq. comm%id_neib(inum1)) iflag = 1
        end do
        if (iflag .eq. 0 ) then
          icou = icou + 1
          comm_IO%id_neib(icou) = comm%id_neib(inum0)
        end if
      end do
!
      end subroutine set_sort_neib_ids
!
! ---------------------------------------------------------------------
!
      subroutine set_sortedd_comm_stack(comm, comm_IO)
!
      type(communication_table), intent(in) :: comm
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: n0, n1, icou_in, icou_ex
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
      end subroutine set_sortedd_comm_stack
!
! ----------------------------------------------------------------------
!
      subroutine set_sortedd_comm_table(comm, comm_IO)
!
      type(communication_table), intent(in) :: comm
      type(communication_table), intent(inout) :: comm_IO
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
      end subroutine set_sortedd_comm_table
!
! ----------------------------------------------------------------------
!
      end module merge_periodic_comm_table
