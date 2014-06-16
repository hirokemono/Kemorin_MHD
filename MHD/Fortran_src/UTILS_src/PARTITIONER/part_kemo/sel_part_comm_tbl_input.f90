!sel_part_comm_tbl_input.f90
!     module sel_part_comm_tbl_input
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine output_local_ele_surf_mesh(my_rank)
!
!      subroutine save_all_import_4_part(ip, work_f_head,               &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine save_all_export_4_part(ip, work_f_head,               &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
!      subroutine load_all_import_4_part(ip, work_f_head,               &
!     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine load_all_comm_tbl_4_part(ip, work_f_head,             &
!     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
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
      subroutine output_local_ele_surf_mesh(my_rank, newmesh,           &
     &          new_ele_mesh, new_surf_mesh, new_edge_mesh)
!
      use t_mesh_data
      use m_read_mesh_data
      use m_ctl_param_partitioner
      use load_2nd_ele_surf_edge
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(element_comms), intent(inout) ::    new_ele_mesh
      type(surface_geometry), intent(inout) :: new_surf_mesh
      type(edge_geometry), intent(inout) ::    new_edge_mesh
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      mesh_ele_file_head =  local_ele_header
      mesh_surf_file_head = local_surf_header
      mesh_edge_file_head = local_edge_header
!
      iflag_mesh_file_fmt =  iflag_para_mesh_file_fmt
!
      call output_ele_surf_edge_type(my_rank, newmesh%ele,              &
     &    new_ele_mesh, new_surf_mesh, new_edge_mesh)
!
      end subroutine output_local_ele_surf_mesh
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine save_all_import_4_part(ip, work_f_head,                &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_import_to_mem', ip
        call copy_all_import_to_mem                                     &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',            &
     &       form='unformatted')
        call write_all_import_to_work(id_work_file,                     &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        close(id_work_file)
      end if
!
      call deallocate_type_import(new_edge_comm)
      call deallocate_type_neib_id(new_edge_comm)
      call deallocate_type_import(new_surf_comm)
      call deallocate_type_neib_id(new_surf_comm)
      call deallocate_type_import(new_ele_comm)
      call deallocate_type_neib_id(new_ele_comm)
      call deallocate_type_import(new_comm)
      call deallocate_type_neib_id(new_comm)
!
      end subroutine save_all_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine save_all_export_4_part(ip, work_f_head,                &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_export_to_mem', ip
        call copy_all_export_to_mem                                     &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',            &
     &       form='unformatted')
        call write_all_import_to_work(id_work_file,                     &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        call write_all_export_to_work(id_work_file,                     &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        close(id_work_file)
      end if
!
      call deallocate_type_comm_tbl(new_edge_comm)
      call deallocate_type_comm_tbl(new_surf_comm)
      call deallocate_type_comm_tbl(new_ele_comm)
      call deallocate_type_comm_tbl(new_comm)
!
      end subroutine save_all_export_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine load_all_import_4_part(ip, work_f_head,                &
     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_import_from_mem', ip
        call copy_all_import_from_mem                                   &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open(id_work_file,file=work_f_name,status='unknown',            &
     &       form='unformatted')
        call read_all_import_from_work(id_work_file,                    &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        close(id_work_file)
      end if
!
      end subroutine load_all_import_4_part
!
!   --------------------------------------------------------------------
!
      subroutine load_all_comm_tbl_4_part(ip, work_f_head,              &
     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use work_comm_table_IO
      use copy_partitioner_comm_table
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: my_rank
      integer(kind = kint), parameter :: id_work_file = 11
!
!
      if(iflag_memory_conserve .eq. 0) then
!        write(*,*) 'copy_all_comm_table_from_mem', ip
        call copy_all_import_from_mem                                   &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        call copy_all_export_from_mem                                   &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
      else
        my_rank = ip - 1
        call add_int_suffix(my_rank, work_f_head, work_f_name)
        open (id_work_file,file=work_f_name, status='unknown',          &
     &      form='unformatted')
!
        call read_all_import_from_work(id_work_file,                    &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        call read_all_export_from_work(id_work_file,                    &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
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
