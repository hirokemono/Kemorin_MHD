!sel_part_nod_comm_input.f90
!     module sel_part_nod_comm_input
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine save_node_import_4_part(ip, new_comm, comm_part)
!!      subroutine save_node_export_4_part(ip, new_comm, comm_part)
!!        type(communication_table), intent(inout) :: new_comm
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!!
!!      subroutine load_node_import_4_part(ip, comm_part, new_comm)
!!      subroutine load_node_comm_tbl_4_part(ip, comm_part, new_comm)
!!        type(partitioner_comm_tables), intent(in) :: comm_part
!!        type(communication_table), intent(inout) :: new_comm
!!
!!      subroutine load_node_import_num_tmp(jp, comm_part)
!!      subroutine load_node_import_item_tmp(jp, comm_part)
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!
      module sel_part_nod_comm_input
!
      use m_precision
      use t_partitioner_comm_table
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine save_node_import_4_part(ip, new_comm, comm_part)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      type(communication_table), intent(inout) :: new_comm
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      integer :: id_rank
      character(len=kchara) :: file_name
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(comm_part%iflag_memory_conserve .eq. 0) then
        call copy_neib_pe_type                                          &
     &     (new_comm, comm_part%nod_comm_tbl_part(ip))
        call copy_import_table_type                                     &
     &     (new_comm, comm_part%nod_comm_tbl_part(ip))
      else
        id_rank = int(ip - 1)
        file_name = add_process_id(id_rank, comm_part%work_f_head)
        open (id_work_file,file=file_name, status='unknown',            &
     &       form='unformatted')
        call write_node_import_to_work(id_work_file, new_comm)
        close(id_work_file)
      end if
!
      end subroutine save_node_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine save_node_export_4_part(ip, new_comm, comm_part)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      type(communication_table), intent(inout) :: new_comm
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      integer :: id_rank
      character(len=kchara) :: file_name
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(comm_part%iflag_memory_conserve .eq. 0) then
        call copy_export_table_type                                     &
     &     (new_comm, comm_part%nod_comm_tbl_part(ip))
      else
        id_rank = int(ip - 1)
        file_name = add_process_id(id_rank, comm_part%work_f_head)
        write(*,*) 'write export table: ', trim(file_name)
        open(id_work_file,file=file_name,status='unknown',              &
     &       form='unformatted')
        call write_node_import_to_work(id_work_file, new_comm)
        call write_node_export_to_work(id_work_file, new_comm)
        close(id_work_file)
      end if
!
      end subroutine save_node_export_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine load_node_import_4_part(ip, comm_part, new_comm)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      type(partitioner_comm_tables), intent(in) :: comm_part
      type(communication_table), intent(inout) :: new_comm
!
      integer :: id_rank
      character(len=kchara) :: file_name
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(comm_part%iflag_memory_conserve .eq. 0) then
        call copy_neib_pe_type                                          &
     &     (comm_part%nod_comm_tbl_part(ip), new_comm)
        call copy_import_table_type                                     &
     &     (comm_part%nod_comm_tbl_part(ip), new_comm)
      else
        id_rank = int(ip - 1)
        file_name = add_process_id(id_rank, comm_part%work_f_head)
        write(*,*) 'read import table: ', trim(file_name)
        open(id_work_file,file=file_name,status='unknown',              &
     &       form='unformatted')
        call read_node_import_from_work(id_work_file, new_comm)
        close(id_work_file)
      end if
!
      end subroutine load_node_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine load_node_comm_tbl_4_part(ip, comm_part, new_comm)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      type(partitioner_comm_tables), intent(in) :: comm_part
!
      type(communication_table), intent(inout) :: new_comm
!
      integer :: id_rank
      character(len=kchara) :: file_name
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(comm_part%iflag_memory_conserve .eq. 0) then
        call copy_comm_tbl_type                                         &
     &     (comm_part%nod_comm_tbl_part(ip), new_comm)
      else
        id_rank = int(ip - 1)
        file_name = add_process_id(id_rank, comm_part%work_f_head)
        open (id_work_file,file=file_name, status='unknown',            &
     &      form='unformatted')
!
        call read_node_import_from_work(id_work_file, new_comm)
        call read_node_export_from_work(id_work_file, new_comm)
        close (id_work_file)
      end if
!
      end subroutine load_node_comm_tbl_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine load_node_import_num_tmp(jp, comm_part)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: jp
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      integer :: id_rank
      character(len=kchara) :: file_name
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(comm_part%iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_import_num_tmp', jp
        call copy_node_import_num_tmp                                   &
     &     (comm_part%nod_comm_tbl_part(jp), comm_part%ipt_tmp)
      else
        id_rank = int(jp - 1)
        file_name = add_process_id(id_rank, comm_part%work_f_head)
        open(id_work_file,file=file_name,status='old',                  &
     &       form='unformatted')
        call read_node_import_num_tmp(id_work_file, comm_part%ipt_tmp)
        close(id_work_file)
      end if
      comm_part%ipt_tmp%ISTACK_NOD_TMP(0) = 0
!
      end subroutine load_node_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine load_node_import_item_tmp(jp, comm_part)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: jp
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      integer :: id_rank
      character(len=kchara) :: file_name
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(comm_part%iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_node_import_num_tmp', jp
        call copy_node_import_num_tmp                                   &
     &     (comm_part%nod_comm_tbl_part(jp), comm_part%ipt_tmp)
        call copy_node_import_item_tmp                                  &
     &     (comm_part%nod_comm_tbl_part(jp), comm_part%ipt_tmp)
      else
        id_rank = int(jp - 1)
        file_name = add_process_id(id_rank, comm_part%work_f_head)
        open(id_work_file,file=file_name,status='old',                  &
     &       form='unformatted')
        call read_node_import_num_tmp(id_work_file, comm_part%ipt_tmp)
        call read_node_import_item_tmp(id_work_file, comm_part%ipt_tmp)
        close(id_work_file)
      end if
      comm_part%ipt_tmp%ISTACK_NOD_TMP(0) = 0
!
      end subroutine load_node_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module sel_part_nod_comm_input
