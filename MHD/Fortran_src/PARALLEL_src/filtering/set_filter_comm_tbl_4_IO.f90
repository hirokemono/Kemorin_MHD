!
!      module set_filter_comm_tbl_4_IO
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine copy_filter_comm_tbl_from_IO
!      subroutine copy_filter_comm_tbl_to_IO(my_rank)
!
      module set_filter_comm_tbl_4_IO
!
      use m_precision
!
      use m_nod_filter_comm_table
      use m_comm_data_IO
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_filter_comm_tbl_from_IO
!
      use set_comm_table_4_IO
!
      call copy_comm_tbl_type_from_IO(flt_comm)
!
      end subroutine copy_filter_comm_tbl_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_filter_comm_tbl_to_IO(my_rank)
!
      use set_comm_table_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_comm_tbl_type_to_IO(my_rank, flt_comm)
!
      end subroutine copy_filter_comm_tbl_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_filter_comm_tbl_4_IO
