!sel_part_comm_tbl_input.f90
!     module sel_part_comm_tbl_input
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine output_local_ele_surf_mesh(my_rank)
!
!      subroutine save_all_import_4_part(ip, work_f_head)
!      subroutine save_all_export_4_part(ip, work_f_head)
!
!      subroutine load_all_import_4_part(ip, work_f_head)
!      subroutine load_all_comm_tbl_4_part(ip, work_f_head)
!
!      subroutine load_all_import_num_tmp(jp, work_f_head)
!      subroutine load_all_import_item_tmp(jp, work_f_head)
!
      module sel_part_comm_tbl_input
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
      subroutine output_local_ele_surf_mesh(my_rank)
!
      use m_read_mesh_data
      use m_ctl_param_partitioner
      use load_2nd_ele_surf_edge
!
      integer(kind= kint), intent(in) :: my_rank
!
      mesh_ele_file_head =  local_ele_header
      mesh_surf_file_head = local_surf_header
      mesh_edge_file_head = local_edge_header
!
      iflag_mesh_file_fmt =  iflag_para_mesh_file_fmt
!
      call output_2nd_ele_surf_edge_mesh(my_rank)
      call dealloc_2nd_ele_surf_edge_mesh
!
      end subroutine output_local_ele_surf_mesh
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine save_all_import_4_part(ip, work_f_head)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_edge_comm_table
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_import_to_mem', ip
        call copy_all_import_to_mem(ip)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',            &
     &       form='unformatted')
        call write_all_import_to_work(id_work_file)
        close(id_work_file)
      end if
!
      call deallocate_2nd_edge_import
      call deallocate_2nd_surf_import
      call deallocate_2nd_ele_import
      call deallocate_2nd_nod_import
      call deallocate_2nd_edge_neib_id
      call deallocate_2nd_surf_neib_id
      call deallocate_2nd_ele_neib_id
      call deallocate_2nd_neib_id
!
      end subroutine save_all_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine save_all_export_4_part(ip, work_f_head)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_edge_comm_table
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_export_to_mem', ip
        call copy_all_export_to_mem(ip)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',            &
     &       form='unformatted')
        call write_all_import_to_work(id_work_file)
        call write_all_export_to_work(id_work_file)
        close(id_work_file)
      end if
!
      call deallocate_2nd_edge_import
      call deallocate_2nd_surf_import
      call deallocate_2nd_ele_import
      call deallocate_2nd_nod_import
      call deallocate_2nd_edge_neib_id
      call deallocate_2nd_surf_neib_id
      call deallocate_2nd_ele_neib_id
      call deallocate_2nd_neib_id
!
      call deallocate_2nd_edge_export
      call deallocate_2nd_surf_export
      call deallocate_2nd_ele_export
      call deallocate_2nd_nod_export
!
      end subroutine save_all_export_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine load_all_import_4_part(ip, work_f_head)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_edge_comm_table
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_import_from_mem', ip
        call copy_all_import_from_mem(ip)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',          &
     &       form='unformatted')
        call read_all_import_from_work(id_work_file)
        close(id_work_file)
      end if
!
      end subroutine load_all_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine load_all_comm_tbl_4_part(ip, work_f_head)
!
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_comm_table_from_mem', ip
        call copy_all_import_from_mem(ip)
        call copy_all_export_from_mem(ip)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open (id_work_file,file=work_f_name, status='unknown',          &
     &      form='unformatted')
!
        call read_all_import_from_work(id_work_file)
        call read_all_export_from_work(id_work_file)
        close (id_work_file)
      end if
!
      end subroutine load_all_comm_tbl_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine load_all_import_num_tmp(jp, work_f_head)
!
      use work_comm_table_IO
      use copy_partitioner_comm_table
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
        call copy_all_import_num_tmp(jp)
      else
        my_rank = jp - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='old',                &
     &         form='unformatted')
        call read_all_import_num_tmp(id_work_file)
        close(id_work_file)
      end if
!
      end subroutine load_all_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine load_all_import_item_tmp(jp, work_f_head)
!
      use work_comm_table_IO
      use copy_partitioner_comm_table
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
        call copy_all_import_num_tmp(jp)
        call copy_all_import_item_tmp(jp)
      else
        my_rank = jp - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        write(*,*) 'read temporal file: ', trim(work_f_name)
        open(id_work_file,file=work_f_name,status='old',             &
     &       form='unformatted')
        call read_all_import_num_tmp(id_work_file)
        call read_all_import_item_tmp(id_work_file)
        close(id_work_file)
      end if
!
      end subroutine load_all_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module sel_part_comm_tbl_input
