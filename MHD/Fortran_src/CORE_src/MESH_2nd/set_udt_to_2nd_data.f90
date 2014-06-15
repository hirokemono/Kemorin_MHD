!
!      module set_udt_to_2nd_data
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine set_2nd_data_by_udt(my_rank, istep_ucd,           &
!     &          ifile_format, ucd_prefix)
!
      module set_udt_to_2nd_data
!
      use m_precision
      use t_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_2nd_data_by_udt_once(my_rank, istep_ucd,           &
     &          ifile_format, ucd_prefix)
!
      use m_2nd_geometry_data
      use m_2nd_phys_data
      use set_ucd_data_to_type
!
      character(len=kchara), intent(in) :: ucd_prefix
      integer(kind = kint),  intent(in) :: ifile_format
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
      type(ucd_data) :: ucd
!
!
      ucd%ifmt_file = ifile_format
      ucd%file_prefix = ucd_prefix
!
      call set_ucd_data_type_from_IO_once(my_rank, istep_ucd,           &
     &    node_2nd%numnod, ucd, phys_2nd)
      ucd%nnod = node_2nd%numnod
!
      end subroutine set_2nd_data_by_udt_once
!
! -----------------------------------------------------------------------
!
      end module set_udt_to_2nd_data
