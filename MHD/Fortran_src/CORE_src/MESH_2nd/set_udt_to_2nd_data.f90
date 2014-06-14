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
      use ucd_IO_select
      use set_and_cal_udt_data
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
      ucd%nnod = node_2nd%numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, ucd)
      call set_field_by_udt_data(node_2nd%numnod, phys_2nd%num_phys,    &
     &    phys_2nd%ntot_phys, phys_2nd%istack_component,                &
     &    phys_2nd%phys_name, phys_2nd%d_fld, ucd)
      call deallocate_ucd_data(ucd)
!
      end subroutine set_2nd_data_by_udt_once
!
! -----------------------------------------------------------------------
!
      end module set_udt_to_2nd_data
