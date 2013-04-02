!m_partitioner_comm_table.f90
!     module m_partitioner_comm_table
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine alloc_nod_comm_tbl_part(nprocs)
!      subroutine alloc_all_comm_tbl_part(nprocs)
!      subroutine dealloc_nod_comm_tbl_part
!      subroutine dealloc_all_comm_tbl_part
!
!      subroutine allocate_all_import_num_tmp
!      subroutine allocate_nod_import_num_tmp
!      subroutine allocate_all_import_item_tmp
!      subroutine allocate_nod_import_item_tmp
!
!      subroutine deallocate_all_import_tmp
!      subroutine deallocate_nod_import_tmp
!      subroutine deallocate_all_import_num_tmp
!      subroutine deallocate_nod_import_num_tmp
!
      module m_partitioner_comm_table
!
      use m_precision
!
      use t_comm_table
!
      implicit none
!
!
      integer(kind = kint) :: iflag_memory_conserve = 0
!
      type(communication_table), allocatable :: nod_comm_tbl_part(:)
      type(communication_table), allocatable :: ele_comm_tbl_part(:)
      type(communication_table), allocatable :: surf_comm_tbl_part(:)
      type(communication_table), allocatable :: edge_comm_tbl_part(:)
!
!
      character(len=kchara) :: work_f_name
!
      integer(kind=kint) :: NP_TMP
      integer(kind=kint) :: NTOT_NOD_TMP
      integer(kind=kint) :: NTOT_ELE_TMP
      integer(kind=kint) :: NTOT_SURF_TMP
      integer(kind=kint) :: NTOT_EDGE_TMP
!
      integer(kind=kint), allocatable :: NEIB_TMP(:)
      integer(kind=kint), allocatable :: ISTACK_NOD_TMP(:)
      integer(kind=kint), allocatable :: ISTACK_ELE_TMP(:)
      integer(kind=kint), allocatable :: ISTACK_SURF_TMP(:)
      integer(kind=kint), allocatable :: ISTACK_EDGE_TMP(:)
!
      integer(kind=kint), allocatable :: IMPORT_NOD_TMP(:)
      integer(kind=kint), allocatable :: IMPORT_ELE_TMP(:)
      integer(kind=kint), allocatable :: IMPORT_SURF_TMP(:)
      integer(kind=kint), allocatable :: IMPORT_EDGE_TMP(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_nod_comm_tbl_part(nprocs)
!
      integer(kind = kint),  intent(in) :: nprocs 
!
!
      allocate( nod_comm_tbl_part(nprocs) )
!
      end subroutine alloc_nod_comm_tbl_part
!
!------------------------------------------------------------------
!
      subroutine alloc_all_comm_tbl_part(nprocs)
!
      integer(kind = kint),  intent(in) :: nprocs 
!
!
      call alloc_nod_comm_tbl_part(nprocs)
      allocate( ele_comm_tbl_part(nprocs) )
      allocate( surf_comm_tbl_part(nprocs) )
      allocate( edge_comm_tbl_part(nprocs) )
!
      end subroutine alloc_all_comm_tbl_part
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_nod_comm_tbl_part
!
      deallocate( nod_comm_tbl_part )
!
      end subroutine dealloc_nod_comm_tbl_part
!
!------------------------------------------------------------------
!
      subroutine dealloc_all_comm_tbl_part
!
      call dealloc_nod_comm_tbl_part
      deallocate( ele_comm_tbl_part, surf_comm_tbl_part )
      deallocate( edge_comm_tbl_part )
!
      end subroutine dealloc_all_comm_tbl_part
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_all_import_tmp
!
      call deallocate_all_import_num_tmp
!
      deallocate( IMPORT_NOD_TMP )
      deallocate( IMPORT_ELE_TMP )
      deallocate( IMPORT_SURF_TMP )
      deallocate( IMPORT_EDGE_TMP )
!
      end subroutine deallocate_all_import_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_import_tmp
!
      call deallocate_nod_import_num_tmp
      deallocate( IMPORT_NOD_TMP )
!
      end subroutine deallocate_nod_import_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_all_import_num_tmp
!
!
      call deallocate_nod_import_num_tmp
      deallocate( ISTACK_ELE_TMP )
      deallocate( ISTACK_SURF_TMP )
      deallocate( ISTACK_EDGE_TMP )
!
      end subroutine deallocate_all_import_num_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_import_num_tmp
!
      deallocate( NEIB_TMP )
      deallocate( ISTACK_NOD_TMP )
!
      end subroutine deallocate_nod_import_num_tmp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_all_import_num_tmp
!
!
      call allocate_nod_import_num_tmp
!
      allocate( ISTACK_ELE_TMP(0:NP_TMP) )
      allocate( ISTACK_SURF_TMP(0:NP_TMP) )
      allocate( ISTACK_EDGE_TMP(0:NP_TMP) )
      ISTACK_ELE_TMP =  0
      ISTACK_SURF_TMP = 0
      ISTACK_EDGE_TMP = 0
!
      end subroutine allocate_all_import_num_tmp
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_num_tmp
!
!
      allocate( NEIB_TMP(NP_TMP) )
      allocate( ISTACK_NOD_TMP(0:NP_TMP) )
      if(NP_TMP .gt. 0) NEIB_TMP = 0
      ISTACK_NOD_TMP =  0
!
      end subroutine allocate_nod_import_num_tmp
!
!------------------------------------------------------------------
!
      subroutine allocate_all_import_item_tmp
!
!
      call allocate_nod_import_item_tmp
!
      NTOT_ELE_TMP =  ISTACK_ELE_TMP(NP_TMP)
      NTOT_SURF_TMP = ISTACK_SURF_TMP(NP_TMP)
      NTOT_EDGE_TMP = ISTACK_EDGE_TMP(NP_TMP)
!
      allocate( IMPORT_ELE_TMP(NTOT_ELE_TMP) )
      allocate( IMPORT_SURF_TMP(NTOT_SURF_TMP) )
      allocate( IMPORT_EDGE_TMP(NTOT_EDGE_TMP) )
!
      if(NTOT_ELE_TMP .gt. 0)  IMPORT_ELE_TMP = 0
      if(NTOT_SURF_TMP .gt. 0) IMPORT_SURF_TMP = 0
      if(NTOT_EDGE_TMP .gt. 0) IMPORT_EDGE_TMP = 0
!
      end subroutine allocate_all_import_item_tmp
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_item_tmp
!
!
      NTOT_NOD_TMP =  ISTACK_NOD_TMP(NP_TMP)
      allocate( IMPORT_NOD_TMP(NTOT_NOD_TMP) )
      if(NTOT_NOD_TMP .gt. 0) IMPORT_NOD_TMP = 0
!
      end subroutine allocate_nod_import_item_tmp
!
!------------------------------------------------------------------
!
      end module m_partitioner_comm_table
