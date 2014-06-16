!sel_part_nod_comm_input.f90
!     module sel_part_nod_comm_input
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine output_local_mesh(my_rank, femmesh)
!
!      subroutine save_node_import_4_part(ip, work_f_head, new_comm)
!      subroutine save_node_export_4_part(ip, work_f_head, new_comm)
!
!      subroutine load_node_import_4_part(ip, work_f_head, new_comm)
!      subroutine load_node_comm_tbl_4_part(ip, work_f_head, new_comm)
!
!      subroutine load_node_import_num_tmp(jp, work_f_head)
!      subroutine load_node_import_item_tmp(jp, work_f_head)
!
      module sel_part_nod_comm_input
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine output_local_mesh(my_rank, femmesh)
!
      use t_mesh_data
      use m_read_mesh_data
      use m_ctl_param_partitioner
      use load_2nd_mesh_data
!
      integer(kind= kint), intent(in) :: my_rank
      type(mesh_data), intent(inout) :: femmesh
!
      iflag_mesh_file_fmt = iflag_para_mesh_file_fmt
      mesh_file_head = local_file_header
      call output_2nd_mesh(my_rank, femmesh%mesh, femmesh%group)
!
      call deallocate_new_mesh(femmesh%mesh, femmesh%group)
!
      end subroutine output_local_mesh
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine save_node_import_4_part(ip, work_f_head, new_comm)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use copy_part_nod_comm_tbl
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
        call copy_node_import_to_mem(ip, new_comm)
!
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open (id_work_file,file=work_f_name, status='unknown',          &
     &       form='unformatted')
        call write_node_import_to_work(id_work_file, new_comm)
        close(id_work_file)
      end if
!
      call deallocate_type_import(new_comm)
      call deallocate_type_neib_id(new_comm)
!
      end subroutine save_node_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine save_node_export_4_part(ip, work_f_head, new_comm)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use copy_part_nod_comm_tbl
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_node_export_to_mem', ip
        call copy_node_export_to_mem(ip, new_comm)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        write(*,*) 'write export table: ', trim(work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',          &
     &       form='unformatted')
        call write_node_import_to_work(id_work_file, new_comm)
        call write_node_export_to_work(id_work_file, new_comm)
        close(id_work_file)
      end if
!
      call deallocate_type_comm_tbl(new_comm)
!
      end subroutine save_node_export_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine load_node_import_4_part(ip, work_f_head, new_comm)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use copy_part_nod_comm_tbl
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_node_import_from_mem', ip
        call copy_node_import_from_mem(ip, new_comm)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        write(*,*) 'read import table: ', trim(work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',            &
     &       form='unformatted')
        call read_node_import_from_work(id_work_file, new_comm)
        close(id_work_file)
      end if
!
      end subroutine load_node_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine load_node_comm_tbl_4_part(ip, work_f_head, new_comm)
!
      use t_comm_table
      use work_nod_comm_table_IO
      use copy_part_nod_comm_tbl
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!          write(*,*) 'copy_all_comm_table_from_mem', ip
          call copy_node_import_from_mem(ip, new_comm)
          call copy_node_export_from_mem(ip, new_comm)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open (id_work_file,file=work_f_name, status='unknown',          &
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
      subroutine load_node_import_num_tmp(jp, work_f_head)
!
      use work_nod_comm_table_IO
      use copy_part_nod_comm_tbl
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: jp
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_import_num_tmp', jp
        call copy_node_import_num_tmp(jp)
      else
        my_rank = jp - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='old',                &
     &       form='unformatted')
        call read_node_import_num_tmp(id_work_file)
        close(id_work_file)
      end if
!
      end subroutine load_node_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine load_node_import_item_tmp(jp, work_f_head)
!
      use work_nod_comm_table_IO
      use copy_part_nod_comm_tbl
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: jp
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_node_import_num_tmp', jp
        call copy_node_import_num_tmp(jp)
        call copy_node_import_item_tmp(jp)
      else
        my_rank = jp - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='old',                &
     &       form='unformatted')
        call read_node_import_num_tmp(id_work_file)
        call read_node_import_item_tmp(id_work_file)
        close(id_work_file)
      end if
!
      end subroutine load_node_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module sel_part_nod_comm_input
