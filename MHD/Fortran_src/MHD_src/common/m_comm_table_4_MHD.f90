!
!      module m_comm_table_4_MHD
!
!     Written by H. Matsui on Sep., 2007
!
      module m_comm_table_4_MHD
!
      use m_precision
!
      implicit  none
!
!
! ------communication table for fluid----------------------
!
      integer(kind=kint) :: neigh_pe_num_fl
!     total neighboring pe count
      integer(kind=kint) :: ntot_import_fl
!    number of import data 
      integer(kind=kint) :: ntot_export_fl
!    number of export data 
      integer(kind=kint), allocatable, target :: neigh_pe_data_fl(:)
!     neighboring pe id
      integer(kind=kint), allocatable, target :: num_import_fl(:)
      integer(kind=kint), allocatable, target :: istack_import_fl(:)
!     import data count for each neighbor pe (i-th pe)
      integer(kind=kint), allocatable, target :: item_import_fl(:)
!     id for import data                     (i-th)
      integer(kind=kint), allocatable, target :: num_export_fl(:)
      integer(kind=kint), allocatable, target :: istack_export_fl(:)
!     export data count for each neighbor pe (i-th pe)
      integer(kind=kint), allocatable, target :: item_export_fl(:)
!     id for export data                     (i-th)
!
! ------communication table for conductor------------------
!
      integer(kind=kint) :: neigh_pe_num_cd
!     total neighboring pe count
      integer(kind=kint) :: ntot_import_cd
!    number of import data 
      integer(kind=kint) :: ntot_export_cd
!    number of export data 
      integer(kind=kint), allocatable, target :: neigh_pe_data_cd(:)
!     neighboring pe id
      integer(kind=kint), allocatable, target :: num_import_cd(:)
      integer(kind=kint), allocatable, target :: istack_import_cd(:)
!     import data count for each neighbor pe (i-th pe)
      integer(kind=kint), allocatable, target :: item_import_cd(:)
!     id for import data                     (i-th)
      integer(kind=kint), allocatable, target :: num_export_cd(:)
      integer(kind=kint), allocatable, target :: istack_export_cd(:)
!     export data count for each neighbor pe (i-th pe)
      integer(kind=kint), allocatable, target :: item_export_cd(:)
!     id for export data                     (i-th)
!
! ------communication table for insulator ------------------
!
      integer(kind=kint) :: neigh_pe_num_ins
!     total neighboring pe count
      integer(kind=kint) :: ntot_import_ins
!    number of import data 
      integer(kind=kint) :: ntot_export_ins
!    number of export data 
      integer(kind=kint), allocatable :: neigh_pe_data_ins(:)
!     neighboring pe id
      integer(kind=kint), allocatable, target :: num_import_ins(:)
      integer(kind=kint), allocatable, target :: istack_import_ins(:)
!     import data count for each neighbor pe (i-th pe)
      integer(kind=kint), allocatable, target :: item_import_ins(:)
!     id for import data                     (i-th)
      integer(kind=kint), allocatable, target :: num_export_ins(:)
      integer(kind=kint), allocatable, target :: istack_export_ins(:)
!     export data count for each neighbor pe (i-th pe)
      integer(kind=kint), allocatable, target :: item_export_ins(:)
!     id for export data                     (i-th)
!
!       subroutine allocate_comm_table_fluid
!
!       subroutine allocate_comm_stack_conduct
!       subroutine allocate_comm_table_conduct
!
!       subroutine allocate_comm_stack_insulate
!       subroutine allocate_comm_table_insulate
!
!       subroutine deallocate_comm_table_conduct
!       subroutine deallocate_comm_table_insulate
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_comm_stack_fluid
!
       allocate ( neigh_pe_data_fl(neigh_pe_num_fl)   )
       allocate ( num_import_fl(neigh_pe_num_fl)      )
       allocate ( num_export_fl(neigh_pe_num_fl)      )
       allocate ( istack_import_fl(0:neigh_pe_num_fl) )
       allocate ( istack_export_fl(0:neigh_pe_num_fl) )
       neigh_pe_data_fl  = 0
       num_import_fl = 0
       num_export_fl = 0
       istack_import_fl = 0
       istack_export_fl = 0
!
       end subroutine allocate_comm_stack_fluid
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_comm_table_fluid
!
       allocate ( item_import_fl(ntot_import_fl) )
       allocate ( item_export_fl(ntot_export_fl) )
       item_import_fl = 0
       item_export_fl = 0
!
       end subroutine allocate_comm_table_fluid
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_comm_stack_conduct
!
       allocate ( neigh_pe_data_cd(neigh_pe_num_cd) )
!
       allocate ( num_import_cd(neigh_pe_num_cd) )
       allocate ( istack_import_cd(0:neigh_pe_num_cd) )
!
       allocate ( num_export_cd(neigh_pe_num_cd) )
       allocate ( istack_export_cd(0:neigh_pe_num_cd) )
!
       neigh_pe_data_cd  = 0
       num_import_cd =     0
       num_export_cd =     0
       istack_import_cd =  0
       istack_export_cd =  0
!
       end subroutine allocate_comm_stack_conduct
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_comm_table_conduct
!
       allocate ( item_import_cd(ntot_import_cd) )
       allocate ( item_export_cd(ntot_export_cd) )
       item_import_cd = 0
       item_export_cd = 0
!
       end subroutine allocate_comm_table_conduct
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_comm_stack_insulate
!
       allocate ( neigh_pe_data_ins(neigh_pe_num_ins) )
!
       allocate ( num_import_ins(neigh_pe_num_ins) )
       allocate ( istack_import_ins(0:neigh_pe_num_ins) )
!
       allocate ( num_export_ins(neigh_pe_num_ins) )
       allocate ( istack_export_ins(0:neigh_pe_num_ins) )
       neigh_pe_data_ins  = 0
       num_import_ins =     0
       num_export_ins =     0
       istack_import_ins =  0
       istack_export_ins =  0
!
       end subroutine allocate_comm_stack_insulate
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_comm_table_insulate
!
       allocate ( item_import_ins(ntot_import_ins) )
       allocate ( item_export_ins(ntot_export_ins) )
       item_import_ins = 0
       item_export_ins = 0
!
       end subroutine allocate_comm_table_insulate
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine deallocate_comm_table_fluid
!
       deallocate ( neigh_pe_data_fl )
       deallocate ( num_import_fl    )
       deallocate ( num_export_fl    )
       deallocate ( istack_import_fl )
       deallocate ( istack_export_fl )
       deallocate ( item_import_fl   )
       deallocate ( item_export_fl   )
!
       end subroutine deallocate_comm_table_fluid
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_comm_table_conduct
!
       deallocate ( neigh_pe_data_cd )
       deallocate ( num_import_cd    )
       deallocate ( istack_import_cd )
       deallocate ( num_export_cd    )
       deallocate ( istack_export_cd )
       deallocate ( item_import_cd   )
       deallocate ( item_export_cd   )
!
       end subroutine deallocate_comm_table_conduct
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_comm_table_insulate
!
       deallocate ( neigh_pe_data_ins )
       deallocate ( num_import_ins    )
       deallocate ( istack_import_ins )
       deallocate ( item_import_ins   )
       deallocate ( num_export_ins    )
       deallocate ( istack_export_ins )
       deallocate ( item_export_ins   )
!
       end subroutine deallocate_comm_table_insulate
!
!  ---------------------------------------------------------------------
!
      end module m_comm_table_4_MHD
