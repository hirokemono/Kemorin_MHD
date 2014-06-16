!work_comm_table_IO.f90
!     module work_comm_table_IO
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine write_all_import_to_work(id_file,                     &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine write_all_export_to_work(id_file,                     &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
!      subroutine read_all_import_from_work(id_file                     &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine read_all_export_from_work(id_file                     &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
!      subroutine read_all_import_num_tmp(id_file)
!      subroutine read_all_import_item_tmp(id_file)
!
      module work_comm_table_IO
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
      subroutine write_all_import_to_work(id_file,                      &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(in) :: new_comm
      type(communication_table), intent(in) :: new_ele_comm
      type(communication_table), intent(in) :: new_surf_comm
      type(communication_table), intent(in) :: new_edge_comm
!
!
      rewind (id_file)
      write (id_file) new_comm%num_neib
      write (id_file) new_comm%id_neib(1:new_comm%num_neib)
      write (id_file) new_comm%istack_import(1:new_comm%num_neib)
      write (id_file) new_ele_comm%num_import(1:new_ele_comm%num_neib)
      write (id_file)                                                   &
     &     new_surf_comm%istack_import(1:new_surf_comm%num_neib)
      write (id_file)                                                   &
     &     new_edge_comm%istack_import(1:new_edge_comm%num_neib)
      write (id_file) new_comm%item_import(1:new_comm%ntot_import)
      write (id_file)                                                   &
     &     new_ele_comm%item_import(1:new_ele_comm%ntot_import)
      write (id_file)                                                   &
     &     new_surf_comm%item_import(1:new_surf_comm%ntot_import)
      write (id_file)                                                   &
     &     new_edge_comm%item_import(1:new_edge_comm%ntot_import)
!
      end subroutine write_all_import_to_work
!
!   --------------------------------------------------------------------
!
      subroutine write_all_export_to_work(id_file,                      &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(in) :: new_comm
      type(communication_table), intent(in) :: new_ele_comm
      type(communication_table), intent(in) :: new_surf_comm
      type(communication_table), intent(in) :: new_edge_comm
!
!
      write (id_file) new_comm%istack_export(1:new_comm%num_neib)
      write (id_file) new_ele_comm%istack_export(1:new_ele_comm%num_neib)
      write (id_file)                                                   &
     &     new_surf_comm%istack_export(1:new_surf_comm%num_neib)
      write (id_file)                                                   &
     &     new_edge_comm%istack_export(1:new_edge_comm%num_neib)
      write (id_file) new_comm%item_export(1:new_comm%ntot_export)
      write (id_file)                                                   &
     &     new_ele_comm%item_export(1:new_ele_comm%ntot_export)
      write (id_file)                                                   &
     &     new_surf_comm%item_export(1:new_surf_comm%ntot_export)
      write (id_file)                                                   &
     &     new_edge_comm%item_export(1:new_edge_comm%ntot_export)
!
      end subroutine write_all_export_to_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_all_import_from_work(id_file,                     &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: i
!
      rewind (id_file)
      read (id_file) new_comm%num_neib
!
      new_ele_comm%num_neib =  new_comm%num_neib
      new_surf_comm%num_neib = new_comm%num_neib
      new_edge_comm%num_neib = new_comm%num_neib
!
      call allocate_type_neib_id(new_comm)
      call allocate_type_import_num(new_comm)
!
      call allocate_type_neib_id(new_ele_comm)
      call allocate_type_import_num(new_ele_comm)
!
      call allocate_type_neib_id(new_surf_comm)
      call allocate_type_import_num(new_surf_comm)
!
      call allocate_type_neib_id(new_edge_comm)
      call allocate_type_import_num(new_edge_comm)
!
      read (id_file) new_comm%id_neib(1:new_comm%num_neib)
      read (id_file) new_comm%istack_import(1:new_comm%num_neib)
      read (id_file) new_ele_comm%num_import(1:new_ele_comm%num_neib)
      read (id_file)                                                   &
     &      new_surf_comm%istack_import(1:new_surf_comm%num_neib)
      read (id_file)                                                   &
     &      new_edge_comm%istack_import(1:new_edge_comm%num_neib)
!
      new_ele_comm%id_neib(1:new_comm%num_neib)                         &
     &      =  new_comm%id_neib(1:new_comm%num_neib)
      new_surf_comm%id_neib(1:new_comm%num_neib)                        &
     &      = new_comm%id_neib(1:new_comm%num_neib)
      new_edge_comm%id_neib(1:new_comm%num_neib)                        &
     &      = new_comm%id_neib(1:new_comm%num_neib)
!
      new_comm%ntot_import = new_comm%istack_import(new_comm%num_neib)
      new_ele_comm%ntot_import                                          &
     &      =  new_ele_comm%num_import(new_ele_comm%num_neib)
      new_surf_comm%ntot_import                                         &
     &      = new_surf_comm%istack_import(new_surf_comm%num_neib)
      new_edge_comm%ntot_import                                         &
     &      = new_edge_comm%istack_import(new_edge_comm%num_neib)
!
      call allocate_type_import_item(new_comm)
      call allocate_type_import_item(new_ele_comm)
      call allocate_type_import_item(new_surf_comm)
      call allocate_type_import_item(new_edge_comm)
!
      read (id_file) new_comm%item_import(1:new_comm%ntot_import)
      read (id_file)                                                    &
     &     new_ele_comm%item_import(1:new_ele_comm%ntot_import)
      read (id_file)                                                    &
     &     new_surf_comm%item_import(1:new_surf_comm%ntot_import)
      read (id_file)                                                    &
     &     new_edge_comm%item_import(1:new_edge_comm%ntot_import)
!
        do i = 1, new_comm%num_neib
          new_comm%num_import(i) = new_comm%istack_import(i)            &
     &                            - new_comm%istack_import(i-1)
        end do
!
        do i = 1, new_ele_comm%num_neib
          new_ele_comm%num_import(i) = new_ele_comm%istack_import(i)    &
     &                          - new_ele_comm%istack_import(i-1)
        end do
!
        do i = 1, new_surf_comm%num_neib
          new_surf_comm%num_import(i) = new_surf_comm%istack_import(i)  &
     &                          - new_surf_comm%istack_import(i-1)
        end do
!
        do i = 1, new_edge_comm%num_neib
          new_edge_comm%num_import(i) = new_edge_comm%istack_import(i)  &
     &                          - new_edge_comm%istack_import(i-1)
        end do
!
      end subroutine read_all_import_from_work
!
!   --------------------------------------------------------------------
!
      subroutine read_all_export_from_work(id_file,                     &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: i
!
      call allocate_type_export_num(new_comm)
      call allocate_type_export_num(new_ele_comm)
      call allocate_type_export_num(new_surf_comm)
      call allocate_type_export_num(new_edge_comm)
!
      read (id_file) new_comm%istack_export(1:new_comm%num_neib)
      read (id_file)                                                    &
     &      new_ele_comm%istack_export(1:new_ele_comm%num_neib)
      read (id_file)                                                    &
     &      new_surf_comm%istack_export(1:new_surf_comm%num_neib)
      read (id_file)                                                    &
     &      new_edge_comm%istack_export(1:new_edge_comm%num_neib)
!
      new_comm%ntot_export =                                            &
     &           new_comm%istack_export(new_comm%num_neib)
      new_ele_comm%ntot_export                                          &
     &      =  new_ele_comm%istack_export(new_ele_comm%num_neib)
      new_surf_comm%ntot_export                                         &
     &      = new_surf_comm%istack_export(new_surf_comm%num_neib)
      new_edge_comm%ntot_export                                         &
     &      = new_edge_comm%istack_export(new_edge_comm%num_neib)
!
      call allocate_type_export_item(new_comm)
      call allocate_type_export_item(new_ele_comm)
      call allocate_type_export_item(new_surf_comm)
      call allocate_type_export_item(new_edge_comm)
!
      read (id_file) new_comm%item_export(1:new_comm%ntot_export)
      read (id_file)                                                    &
     &     new_ele_comm%item_export(1:new_ele_comm%ntot_export)
      read (id_file)                                                    &
     &     new_surf_comm%item_export(1:new_surf_comm%ntot_export)
      read (id_file)                                                    &
     &     new_edge_comm%item_export(1:new_edge_comm%ntot_export)
!
        do i = 1, new_comm%num_neib
          new_comm%num_export(i) = new_comm%istack_export(i)            &
     &                          - new_comm%istack_export(i-1)
        end do
!
        do i = 1, new_ele_comm%num_neib
          new_ele_comm%num_export(i) = new_ele_comm%istack_export(i)    &
     &                          - new_ele_comm%istack_export(i-1)
        end do
!
        do i = 1, new_surf_comm%num_neib
          new_surf_comm%num_export(i) = new_surf_comm%istack_export(i)  &
     &                          - new_surf_comm%istack_export(i-1)
        end do
!
        do i = 1, new_edge_comm%num_neib
          new_edge_comm%num_export(i) = new_edge_comm%istack_export(i)  &
     &                          - new_edge_comm%istack_export(i-1)
        end do
!
      end subroutine read_all_export_from_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_all_import_num_tmp(id_file)
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      rewind (id_file)
      read (id_file) NP_TMP
!
      call allocate_all_import_num_tmp
!
      read (id_file) NEIB_TMP(1:NP_TMP)
      read (id_file) ISTACK_NOD_TMP(1:NP_TMP)
      read (id_file) ISTACK_ELE_TMP(1:NP_TMP)
      read (id_file) ISTACK_SURF_TMP(1:NP_TMP)
      read (id_file) ISTACK_EDGE_TMP(1:NP_TMP)
!
      end subroutine read_all_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine read_all_import_item_tmp(id_file)
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call allocate_all_import_item_tmp
!
      read (id_file) IMPORT_NOD_TMP(1:NTOT_NOD_TMP)
      read (id_file) IMPORT_ELE_TMP(1:NTOT_ELE_TMP)
      read (id_file) IMPORT_SURF_TMP(1:NTOT_SURF_TMP)
      read (id_file) IMPORT_EDGE_TMP(1:NTOT_EDGE_TMP)
!
      end subroutine read_all_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module work_comm_table_IO
