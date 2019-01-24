!t_partitioner_comm_table.f90
!     module t_partitioner_comm_table
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine allocate_nod_import_num_tmp(ipt_tmp)
!!      subroutine allocate_nod_import_item_tmp(ipt_tmp)
!!        type(temporary_import_4_part), intent(inout) :: ipt_tmp
!!
!!      subroutine deallocate_nod_import_num_tmp(ipt_tmp)
!!      subroutine deallocate_nod_import_tmp(ipt_tmp)
!!        type(temporary_import_4_part), intent(inout) :: ipt_tmp
!!
!!      subroutine copy_node_import_num_tmp(ip, ipt_tmp)
!!      subroutine copy_node_import_item_tmp(ip, ipt_tmp)
!!        type(communication_table), intent(in) :: comm_tbl_org
!!        type(temporary_import_4_part), intent(inout) :: ipt_tmp
!!
!!      subroutine write_node_import_num_tmp(id_file, ipt_tmp)
!!      subroutine write_node_import_item_tmp(id_file, ipt_tmp)
!!        type(temporary_import_4_part), intent(in) :: ipt_tmp
!!      subroutine read_node_import_num_tmp(id_file, ipt_tmp)
!!      subroutine read_node_import_item_tmp(id_file, ipt_tmp)
!!        type(temporary_import_4_part), intent(inout) :: ipt_tmp
!!
!!      subroutine alloc_nod_comm_tbl_part(nprocs, comm_part)
!!      subroutine dealloc_nod_comm_tbl_part(comm_part)
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!!
!
      module t_partitioner_comm_table
!
      use m_precision
!
      use t_comm_table
!
      implicit none
!
!
      type temporary_import_4_part
        integer(kind=kint) :: NP_TMP
        integer(kind=kint) :: NTOT_NOD_TMP
        integer(kind=kint), allocatable :: NEIB_TMP(:)
        integer(kind=kint), allocatable :: ISTACK_NOD_TMP(:)
        integer(kind=kint), allocatable :: IMPORT_NOD_TMP(:)
      end type temporary_import_4_part
!
      type partitioner_comm_tables
        integer(kind=kint) :: np_comm
        type(communication_table), allocatable :: nod_comm_tbl_part(:)
!
        integer(kind = kint) :: iflag_memory_conserve = 0
        character(len=kchara) :: work_f_head = 'work'
!
        type(temporary_import_4_part) :: ipt_tmp
      end type partitioner_comm_tables
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_num_tmp(ipt_tmp)
!
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
!
      allocate( ipt_tmp%NEIB_TMP(ipt_tmp%NP_TMP) )
      allocate( ipt_tmp%ISTACK_NOD_TMP(0:ipt_tmp%NP_TMP) )
      if(ipt_tmp%NP_TMP .gt. 0) ipt_tmp%NEIB_TMP = 0
      ipt_tmp%ISTACK_NOD_TMP =  0
!
      end subroutine allocate_nod_import_num_tmp
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_item_tmp(ipt_tmp)
!
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
!
      ipt_tmp%NTOT_NOD_TMP = ipt_tmp%ISTACK_NOD_TMP(ipt_tmp%NP_TMP)
      allocate( ipt_tmp%IMPORT_NOD_TMP(ipt_tmp%NTOT_NOD_TMP) )
      if(ipt_tmp%NTOT_NOD_TMP .gt. 0) ipt_tmp%IMPORT_NOD_TMP = 0
!
      end subroutine allocate_nod_import_item_tmp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_nod_import_tmp(ipt_tmp)
!
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
      call deallocate_nod_import_num_tmp(ipt_tmp)
      deallocate( ipt_tmp%IMPORT_NOD_TMP )
!
      end subroutine deallocate_nod_import_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_import_num_tmp(ipt_tmp)
!
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
!
      deallocate( ipt_tmp%NEIB_TMP )
      deallocate( ipt_tmp%ISTACK_NOD_TMP )
!
      end subroutine deallocate_nod_import_num_tmp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_import_num_tmp(comm_tbl_org, ipt_tmp)
!
      type(communication_table), intent(in) :: comm_tbl_org
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
!
      ipt_tmp%NP_TMP = comm_tbl_org%num_neib
!
      call allocate_nod_import_num_tmp(ipt_tmp)
!
      ipt_tmp%NEIB_TMP(1:ipt_tmp%NP_TMP)                                &
     &        = comm_tbl_org%id_neib(1:ipt_tmp%NP_TMP)
      ipt_tmp%ISTACK_NOD_TMP(0:ipt_tmp%NP_TMP)                          &
     &        =  comm_tbl_org%istack_import(0:ipt_tmp%NP_TMP)
!
      end subroutine copy_node_import_num_tmp
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_item_tmp(comm_tbl_org, ipt_tmp)
!
      type(communication_table), intent(in) :: comm_tbl_org
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
!
      call allocate_nod_import_item_tmp(ipt_tmp)
!
      ipt_tmp%IMPORT_NOD_TMP(1:ipt_tmp%NTOT_NOD_TMP)                    &
     &   =  comm_tbl_org%item_import(1:ipt_tmp%NTOT_NOD_TMP)
!
      end subroutine copy_node_import_item_tmp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_node_import_num_tmp(id_file, ipt_tmp)
!
      integer(kind = kint), intent(in) :: id_file
      type(temporary_import_4_part), intent(in) :: ipt_tmp
!
      rewind (id_file)
      write(id_file) ipt_tmp%NP_TMP
!
      write(id_file) ipt_tmp%NEIB_TMP(1:ipt_tmp%NP_TMP)
      write(id_file) ipt_tmp%ISTACK_NOD_TMP(1:ipt_tmp%NP_TMP)
!
      end subroutine write_node_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine write_node_import_item_tmp(id_file, ipt_tmp)
!
      integer(kind = kint), intent(in) :: id_file
      type(temporary_import_4_part), intent(in) :: ipt_tmp
!
!
      write(id_file) ipt_tmp%IMPORT_NOD_TMP(1:ipt_tmp%NTOT_NOD_TMP)
!
      end subroutine write_node_import_item_tmp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_node_import_num_tmp(id_file, ipt_tmp)
!
      integer(kind = kint), intent(in) :: id_file
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
      rewind (id_file)
      read (id_file) ipt_tmp%NP_TMP
!
      call allocate_nod_import_num_tmp(ipt_tmp)
!
      read (id_file) ipt_tmp%NEIB_TMP(1:ipt_tmp%NP_TMP)
      read (id_file) ipt_tmp%ISTACK_NOD_TMP(1:ipt_tmp%NP_TMP)
!
      end subroutine read_node_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine read_node_import_item_tmp(id_file, ipt_tmp)
!
      integer(kind = kint), intent(in) :: id_file
      type(temporary_import_4_part), intent(inout) :: ipt_tmp
!
!
      call allocate_nod_import_item_tmp(ipt_tmp)
!
      read (id_file) ipt_tmp%IMPORT_NOD_TMP(1:ipt_tmp%NTOT_NOD_TMP)
!
      end subroutine read_node_import_item_tmp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_nod_comm_tbl_part(nprocs, comm_part)
!
      integer(kind = kint),  intent(in) :: nprocs 
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
!
      comm_part%np_comm = nprocs
      allocate(comm_part%nod_comm_tbl_part(comm_part%np_comm))
!
      end subroutine alloc_nod_comm_tbl_part
!
!------------------------------------------------------------------
!
      subroutine dealloc_nod_comm_tbl_part(comm_part)
!
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      deallocate(comm_part%nod_comm_tbl_part )
!
      end subroutine dealloc_nod_comm_tbl_part
!
!------------------------------------------------------------------
!
      end module t_partitioner_comm_table
