!>@file   set_comm_table_4_IO.f90
!!@brief  module set_comm_table_4_IO
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Copy node communication table between IO buffer
!!
!!@verbatim
!!      subroutine copy_comm_tbl_type_from_IO(comm_tbls)
!!      subroutine copy_comm_tbl_type_to_IO(my_rank, comm_tbls)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(communication_table), intent(in) :: comm_tbls
!!@endverbatim
!
      module set_comm_table_4_IO
!
      use m_precision
!
      use t_comm_table
      use m_comm_data_IO
      use copy_communication_table
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_comm_tbl_type_from_IO(comm_tbls)
!
      type(communication_table), intent(inout) :: comm_tbls
!
!
      comm_tbls%num_neib = comm_IO%num_neib
!
      call allocate_type_comm_tbl_num(comm_tbls)
!
      call copy_num_communication                                       &
     &    (comm_tbls%num_neib, comm_tbls%id_neib,                       &
     &    comm_tbls%istack_import, comm_tbls%istack_export,             &
     &    comm_tbls%ntot_import, comm_tbls%ntot_export,                 &
     &    comm_IO%id_neib, comm_IO%istack_import,                       &
     &    comm_IO%istack_export)
      call copy_num_import_export(comm_tbls%num_neib,                   &
     &    comm_tbls%num_import, comm_tbls%num_export,                   &
     &    comm_tbls%istack_import, comm_tbls%istack_export)
!
      call allocate_type_comm_tbl_item(comm_tbls)
!
      call copy_communication_item                                      &
     &    (comm_tbls%ntot_import, comm_tbls%ntot_export,                &
     &    comm_tbls%item_import, comm_tbls%item_export,                 &
     &    comm_IO%item_import, comm_IO%item_export)
!
      call deallocate_type_comm_tbl(comm_IO)
!
      end subroutine copy_comm_tbl_type_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_comm_tbl_type_to_IO(my_rank, comm_tbls)
!
      integer(kind = kint), intent(in) :: my_rank
      type(communication_table), intent(in) :: comm_tbls
!
!
      my_rank_IO = my_rank
      comm_IO%num_neib = comm_tbls%num_neib
!
      call allocate_type_comm_tbl_num(comm_IO)
!
      call copy_num_communication                                       &
     &   (comm_IO%num_neib, comm_IO%id_neib,                            &
     &    comm_IO%istack_import, comm_IO%istack_export,                 &
     &    comm_IO%ntot_import, comm_IO%ntot_export, comm_tbls%id_neib,  &
     &    comm_tbls%istack_import, comm_tbls%istack_export)
!
      call allocate_type_comm_tbl_item(comm_IO)
!
      call copy_communication_item                                      &
     &   (comm_IO%ntot_import, comm_IO%ntot_export,                     &
     &    comm_IO%item_import, comm_IO%item_export,                     &
     &    comm_tbls%item_import, comm_tbls%item_export)
!
      end subroutine copy_comm_tbl_type_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_comm_table_4_IO
