!
!      module set_merged_udt_2_IO
!
!      Written by H. Matsui on Apr., 2010
!
!      subroutine link_write_merged_grd_2_ucd(ifile_format, ucd_prefix)
!      subroutine link_merged_data_2_udt_IO
!      subroutine link_merged_phys_2_udt_IO
!
      module set_merged_udt_2_IO
!
      use m_precision
!
      use m_geometry_data_4_merge
      use m_ucd_data
      use set_ucd_data_to_type
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine link_write_merged_grd_2_ucd(ifile_format, ucd_prefix)
!
      use set_and_cal_udt_data
      use set_ucd_data
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: ifile_format
      character(len = kchara), intent(in) :: ucd_prefix
!
!
      call link_node_data_2_output(merge_tbl%nnod_merged,               &
     &    merged%node%inod_global, merged%node%xx, fem_ucd)
!
      call count_udt_elements(merged%node%numnod,                       &
     &    merge_tbl%nele_merged, merged%ele%nnod_4_ele,                 &
     &    merged%ele%ie, fem_ucd)
      call allocate_ucd_ele(fem_ucd)
!
      call set_udt_global_connect(merged%node%numnod,                   &
     &    merge_tbl%nele_merged, merged%ele%nnod_4_ele,                 &
     &    merged%ele%iele_global, merged%ele%ie, fem_ucd)
!
!
!
      fem_ucd%ifmt_file = ifile_format
      fem_ucd%file_prefix = ucd_prefix
      call sel_write_grd_file(izero, fem_ucd)
!
      if(    mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_vtd/10            &
     &  .or. mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_udt/10) then
        call deallocate_ucd_ele(fem_ucd)
      end if
!
      end subroutine link_write_merged_grd_2_ucd
!
! -----------------------------------------------------------------------
!
      subroutine link_write_merged_udt(istep, ifile_format, ucd_prefix)
!
      use set_ucd_data_to_type
      use set_ucd_data
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: ifile_format, istep
      character(len = kchara), intent(in) :: ucd_prefix
!
!
      call link_field_data_type_2_output(merge_tbl%nnod_merged,         &
     &    merged_fld, fem_ucd)
!
      fem_ucd%ifmt_file = ifile_format
      fem_ucd%file_prefix = ucd_prefix
      call sel_write_ucd_file(-1, istep, fem_ucd)
      call disconnect_ucd_data(fem_ucd)
!
      end subroutine link_write_merged_udt
!
! -----------------------------------------------------------------------
!
      subroutine link_write_merged_ucd(ifile_format, istep)
!
      use set_ucd_data_to_type
      use set_ucd_data
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: ifile_format, istep
!
!
      call link_node_data_2_output(merge_tbl%nnod_merged,               &
     &    merged%node%inod_global, merged%node%xx, fem_ucd)
      call link_field_data_type_2_output(merge_tbl%nnod_merged,         &
     &    merged_fld, fem_ucd)
!
      fem_ucd%ifmt_file = ifile_format
      call sel_write_ucd_file(izero, istep, fem_ucd)
      call disconnect_ucd_data(fem_ucd)
!
      end subroutine link_write_merged_ucd
!
! -----------------------------------------------------------------------
!
      end module set_merged_udt_2_IO
