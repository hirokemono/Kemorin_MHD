!add_export_item_4_part.f90
!     module add_export_item_4_part
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine add_all_export_item_4_part(nprocs, ip, work_f_head,   &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      module add_export_item_4_part
!
      use m_precision
!
      use m_domain_group_4_partition
      use m_internal_4_partitioner
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint) :: nneib2_old
      integer(kind = kint), allocatable :: iflag_neib(:)
      integer(kind = kint), allocatable :: id_neib_copy(:)
      integer(kind = kint), allocatable :: num_import_copy(:)
      integer(kind = kint), allocatable :: istack_import_copy(:)
      integer(kind = kint), allocatable :: num_export_copy(:)
!
      private :: iflag_neib, nneib2_old
      private :: id_neib_copy, num_export_copy
      private :: num_import_copy, istack_import_copy
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine add_all_export_item_4_part(nprocs, ip, work_f_head,    &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
      use sel_part_comm_tbl_input
!
      integer(kind = kint), intent(in) :: nprocs, ip
      character(len=kchara), intent(in) :: work_f_head
!
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: j, jp, jg
!
!
      allocate( iflag_neib(nprocs) )
      iflag_neib(1:nprocs) = 1
!
      do j = 1, new_comm%num_neib
        jp = new_comm%id_neib(j)
        iflag_neib(jp) = 0
      end do
!
      do jp = 1, nprocs
        if ( iflag_neib(jp) .eq. 1) then
          call load_all_import_num_tmp(jp, work_f_head)
!
          ISTACK_NOD_TMP(0) = 0
          do jg = 1, NP_TMP
            if (NEIB_TMP(jg) .eq. ip) then
              allocate ( id_neib_copy(new_comm%num_neib) )
!
              allocate ( num_import_copy(new_comm%num_neib) )
              allocate ( istack_import_copy(0:new_comm%num_neib) )
              allocate ( num_export_copy(new_comm%num_neib) )
!
              nneib2_old = new_comm%num_neib
              new_comm%num_neib = new_comm%num_neib + 1
!
              id_neib_copy(1:nneib2_old)                                &
     &              = new_comm%id_neib(1:nneib2_old)
              num_import_copy(1:nneib2_old)                             &
     &              = new_comm%num_import(1:nneib2_old)
              istack_import_copy(0:nneib2_old)                          &
     &              = new_comm%istack_import(0:nneib2_old)
              num_export_copy(1:nneib2_old)                             &
     &              = new_comm%num_export(1:nneib2_old)
!
              call deallocate_type_comm_tbl(new_comm)
!
              call allocate_type_comm_tbl_num(new_comm)
!
              new_comm%num_import(1:nneib2_old)                         &
     &              = num_import_copy(1:nneib2_old)
              new_comm%istack_import(0:nneib2_old)                      &
     &              = istack_import_copy(0:nneib2_old)
              new_comm%num_export(1:nneib2_old)                         &
     &              = num_export_copy(1:nneib2_old)
!
!
              num_import_copy(1:nneib2_old)                             &
     &              = new_ele_comm%num_import(1:nneib2_old)
              istack_import_copy(0:nneib2_old)                          &
     &              = new_ele_comm%istack_import(0:nneib2_old)
              num_export_copy(1:nneib2_old)                             &
     &              = new_ele_comm%num_export(1:nneib2_old)
!
              call deallocate_type_export_num(new_ele_comm)
              call deallocate_type_import_num(new_ele_comm)
!
              call allocate_type_import_num(new_ele_comm)
              call allocate_type_export_num(new_ele_comm)
!
              new_ele_comm%num_import(1:nneib2_old)                     &
     &              = num_import_copy(1:nneib2_old) 
              new_ele_comm%istack_import(0:nneib2_old)                  &
     &              = istack_import_copy(0:nneib2_old) 
              new_ele_comm%num_export(1:nneib2_old)                     &
     &              = num_export_copy(1:nneib2_old)
!
!
              num_import_copy(1:nneib2_old)                             &
     &              = new_surf_comm%num_import(1:nneib2_old)
              istack_import_copy(0:nneib2_old)                          &
     &              = new_surf_comm%istack_import(0:nneib2_old)
              num_export_copy(1:nneib2_old)                             &
     &              = new_surf_comm%num_export(1:nneib2_old)
!
              call deallocate_type_export_num(new_surf_comm)
              call deallocate_type_import_num(new_surf_comm)
!
              call allocate_type_import_num(new_surf_comm)
              call allocate_type_export_num(new_surf_comm)
!
              new_surf_comm%num_import(1:nneib2_old)                    &
     &              = num_import_copy(1:nneib2_old)
              new_surf_comm%istack_import(0:nneib2_old)                 &
     &              = istack_import_copy(0:nneib2_old)
              new_surf_comm%num_export(1:nneib2_old)                    &
     &              = num_export_copy(1:nneib2_old)
!
!
              num_import_copy(1:nneib2_old)                             &
     &              = new_edge_comm%num_import(1:nneib2_old)
              istack_import_copy(0:nneib2_old)                          &
     &              = new_edge_comm%istack_import(0:nneib2_old)
              num_export_copy(1:nneib2_old)                             &
     &              = new_edge_comm%num_export(1:nneib2_old)
!
              call deallocate_type_export_num(new_edge_comm)
              call deallocate_type_import_num(new_edge_comm)
!
              call allocate_type_import_num(new_edge_comm)
              call allocate_type_export_num(new_edge_comm)
!
              new_edge_comm%num_import(1:nneib2_old)                    &
     &              = num_import_copy(1:nneib2_old)
              new_edge_comm%istack_import(0:nneib2_old)                 &
     &              = istack_import_copy(0:nneib2_old)
              new_edge_comm%num_export(1:nneib2_old)                    &
     &              = num_export_copy(1:nneib2_old)
!
              deallocate ( id_neib_copy )
              deallocate ( num_import_copy )
              deallocate ( istack_import_copy )
              deallocate ( num_export_copy )
!
!
!
              new_comm%id_neib(new_comm%num_neib) =   jp
              new_comm%num_import(new_comm%num_neib) = 0
              new_ele_comm%num_import(new_comm%num_neib) = 0
              new_comm%istack_import(new_comm%num_neib)                 &
     &              = new_comm%istack_import(nneib2_old)
              new_ele_comm%num_import(new_comm%num_neib)                &
     &              = new_ele_comm%num_import(nneib2_old)
              new_comm%num_export(new_comm%num_neib)                    &
     &              = ISTACK_NOD_TMP(jg) - ISTACK_NOD_TMP(jg-1)
              new_ele_comm%num_export(new_comm%num_neib)                &
     &              =  ISTACK_ELE_TMP(jg) - ISTACK_ELE_TMP(jg-1)
              new_surf_comm%num_export(new_comm%num_neib)               &
     &               = ISTACK_SURF_TMP(jg) - ISTACK_SURF_TMP(jg-1)
              new_edge_comm%num_export(new_comm%num_neib)               &
     &               = ISTACK_EDGE_TMP(jg) - ISTACK_EDGE_TMP(jg-1)
              exit
            end if
          end do
!
          call deallocate_all_import_num_tmp
        end if
      end do
!
      deallocate( iflag_neib )
!
      end subroutine add_all_export_item_4_part
!
!   --------------------------------------------------------------------
!
      end module add_export_item_4_part
