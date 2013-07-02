!parallel_udt_IO_select.F90
!      module parallel_udt_IO_select
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!!      subroutine set_control_parallel_field_def
!!
!!      subroutine sel_write_parallel_ucd_file(istep_udt)
!!      subroutine sel_write_parallel_ucd_mesh
!!
!
      module parallel_udt_IO_select
!
      use m_precision
      use m_parallel_var_dof
      use m_file_format_switch
      use m_field_file_format
!
      implicit none
!
      private :: choose_para_fld_file_format
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_control_parallel_field_def
!
      use m_ctl_data_4_platforms
      use m_ucd_data
!
!
      itype_ucd_data_file = i_udt_header
      if (i_udt_header .gt. 0) then
        ucd_header_name = udt_file_head_ctl
      end if
!
      call choose_para_fld_file_format(udt_file_fmt_ctl,                &
     &    i_udt_files_fmt, itype_ucd_data_file)
!
      end subroutine set_control_parallel_field_def
!
! -----------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_file(istep_udt)
!
      use m_ucd_data
!
      use udt_file_IO
      use ucd_field_file_IO
      use ucd_field_file_IO_b
      use write_ucd_to_vtk_file
      use merged_udt_vtk_file_IO
!
      use gz_udt_file_IO
      use gz_merged_udt_vtk_file_IO
      use gz_ucd_field_file_IO
      use gz_write_ucd_to_vtk_file
      use hdf5_file_IO
!
      integer(kind=kint), intent(in) :: istep_udt
!
!
      if (itype_ucd_data_file .eq. iflag_bin) then
        call write_ucd_2_fld_file_b(my_rank, istep_udt)
!
#ifdef ZLIB_IO
      else if (itype_ucd_data_file .eq. iflag_sgl_vtk_gz) then
        call write_gz_merged_vtk_file(istep_udt)
      else if (itype_ucd_data_file .eq. iflag_sgl_vtd_gz) then
        call write_gz_merged_vtk_phys(istep_udt)
      else if (itype_ucd_data_file .eq. iflag_sgl_ucd_gz) then
        call write_gz_merged_ucd_file(istep_udt)
      else if (itype_ucd_data_file .eq. iflag_sgl_udt_gz) then
        call write_gz_merged_udt_file(istep_udt)
!
      else if(itype_ucd_data_file .eq. iflag_vtk_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_udt)
        call write_ucd_data_2_gz_vtk(my_rank, istep_udt)
      else if (itype_ucd_data_file .eq. iflag_vtd_gz) then
        call write_gz_parallel_vtk_file(my_rank, nprocs, istep_udt)
        call write_ucd_data_2_gz_vtk_phys(my_rank, istep_udt)
      else if(itype_ucd_data_file .eq. iflag_ucd_gz) then
        call write_gz_ucd_file(my_rank, istep_udt)
      else if(itype_ucd_data_file .eq. iflag_udt_gz) then
        call write_gz_udt_file(my_rank, istep_udt)
      else if(itype_ucd_data_file .eq. iflag_fld_gz) then
        call write_ucd_2_gz_fld_file(my_rank, istep_udt)
#endif
!
#ifdef HDF5_IO
      else if(itype_ucd_data_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_field_file(istep_udt)
        call parallel_write_xdmf_snap_file(istep_udt)
        call parallel_write_xdmf_evo_file(istep_udt)
#endif
!
      else if (itype_ucd_data_file .eq. iflag_sgl_vtk) then
        call write_merged_vtk_file(istep_udt)
      else if (itype_ucd_data_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_phys(istep_udt)
      else if (itype_ucd_data_file .eq. iflag_sgl_ucd) then
        call write_merged_ucd_file(istep_udt)
      else if (itype_ucd_data_file .eq. iflag_sgl_udt) then
        call write_merged_udt_file(istep_udt)
!
      else if(itype_ucd_data_file .eq. iflag_vtk) then
        call write_parallel_vtk_file(my_rank, nprocs, istep_udt)
        call write_udt_data_2_vtk_file(my_rank, istep_udt)
      else if (itype_ucd_data_file .eq. iflag_vtd) then
        call write_parallel_vtk_file(my_rank, nprocs, istep_udt)
        call write_udt_data_2_vtk_phys(my_rank, istep_udt)
      else if(itype_ucd_data_file .eq. iflag_ucd) then
        call write_ucd_file(my_rank, istep_udt)
      else if(itype_ucd_data_file .eq. iflag_udt) then
        call write_udt_file(my_rank, istep_udt)
      else
        call write_ucd_2_fld_file(my_rank, istep_udt)
      end if
!
      end subroutine sel_write_parallel_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_parallel_ucd_mesh
!
      use m_ucd_data
!
      use udt_file_IO
      use write_ucd_to_vtk_file
      use merged_udt_vtk_file_IO
!
      use gz_udt_file_IO
      use gz_merged_udt_vtk_file_IO
      use gz_write_ucd_to_vtk_file
      use hdf5_file_IO
!
!
      if (itype_ucd_data_file .eq. iflag_bin) then
        return
!
#ifdef ZLIB_IO
      else if (itype_ucd_data_file .eq. iflag_sgl_vtd_gz) then
        call write_gz_merged_vtk_grid
      else if(itype_ucd_data_file .eq. iflag_sgl_udt_gz) then
        call write_gz_merged_grd_file
!
      else if(itype_ucd_data_file .eq. iflag_vtd_gz) then
        call write_ucd_data_2_gz_vtk_grid(my_rank)
      else if(itype_ucd_data_file .eq. iflag_udt_gz) then
        call write_gz_grd_file(my_rank)
#endif
!
#ifdef HDF5_IO
      else if(itype_ucd_data_file .eq. iflag_sgl_hdf5) then
        call parallel_write_hdf5_mesh_file
#endif
!
      else if(itype_ucd_data_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_grid
      else if(itype_ucd_data_file .eq. iflag_sgl_udt) then
        call write_merged_grd_file
!
      else if(itype_ucd_data_file .eq. iflag_vtd) then
        call write_udt_data_2_vtk_grid(my_rank)
      else if(itype_ucd_data_file .eq. iflag_udt) then
        call write_grd_file(my_rank)
      end if
!
      end subroutine sel_write_parallel_ucd_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine choose_para_fld_file_format(file_fmt_ctl, i_file_fmt,  &
     &          id_field_file_format)
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(inout) :: id_field_file_format
!
!
      if (i_file_fmt .eq. 0) then
        id_field_file_format = iflag_udt
        return
      end if
!
      if(file_fmt_ctl.eq.'single'                                       &
     &   .or. file_fmt_ctl.eq.'SINGLE'                                  &
     &   .or. file_fmt_ctl.eq.'merged'                                  &
     &   .or. file_fmt_ctl.eq.'MERGED'                                  &
     &   .or. file_fmt_ctl.eq.'single_ascii'                            &
     &   .or. file_fmt_ctl.eq.'SINGLE_ASCII'                            &
     &   .or. file_fmt_ctl.eq.'single_udt'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT'                              &
     &   .or. file_fmt_ctl.eq.'single_udt_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_ascii'                            &
     &   .or. file_fmt_ctl.eq.'MERGED_ASCII'                            &
     &   .or. file_fmt_ctl.eq.'merged_udt'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT'                              &
     &   .or. file_fmt_ctl.eq.'merged_udt_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT_ASCII') then
           id_field_file_format = iflag_sgl_udt
      else if(file_fmt_ctl.eq.'single_gzip'                             &
     &   .or. file_fmt_ctl.eq.'SINGLE_GZIP'                             &
     &   .or. file_fmt_ctl.eq.'single_udt_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_gzip'                             &
     &   .or. file_fmt_ctl.eq.'MERGED_GZIP'                             &
     &   .or. file_fmt_ctl.eq.'merged_udt_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'single_gz'                               &
     &   .or. file_fmt_ctl.eq.'SINGLE_GZ'                               &
     &   .or. file_fmt_ctl.eq.'single_udt_gz'                           &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT_GZ'                           &
     &   .or. file_fmt_ctl.eq.'merged_gz'                               &
     &   .or. file_fmt_ctl.eq.'MERGED_GZ'                               &
     &   .or. file_fmt_ctl.eq.'merged_udt_gz'                           &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT_GZ') then
           id_field_file_format = iflag_sgl_udt + iflag_gzip
!
      else if(file_fmt_ctl.eq.'single_ucd'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_UCD'                              &
     &   .or. file_fmt_ctl.eq.'single_ucd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_UCD_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_ucd'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_UCD'                              &
     &   .or. file_fmt_ctl.eq.'merged_ucd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_UCD_ASCII') then
           id_field_file_format = iflag_sgl_ucd
      else if(file_fmt_ctl.eq.'single_ucd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_UCD_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_ucd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_UCD_GZIP') then
           id_field_file_format = iflag_sgl_ucd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'single_vtd'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD'                              &
     &   .or. file_fmt_ctl.eq.'single_vtd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_vtd'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD'                              &
     &   .or. file_fmt_ctl.eq.'merged_vtd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD_ASCII') then
           id_field_file_format = iflag_sgl_vtd
      else if(file_fmt_ctl.eq.'single_vtd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_vtd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'single_vtd_gz'                           &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD_GZ'                           &
     &   .or. file_fmt_ctl.eq.'merged_vtd_gz'                           &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD_GZ') then
           id_field_file_format = iflag_sgl_vtd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'single_vtk'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK'                              &
     &   .or. file_fmt_ctl.eq.'single_vtk_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_vtk'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK'                              &
     &   .or. file_fmt_ctl.eq.'merged_vtk_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_ASCII') then
           id_field_file_format = iflag_sgl_vtk
      else if(file_fmt_ctl.eq.'single_vtk_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_vtk_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'single_vtk_gz'                           &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_GZ'                           &
     &   .or. file_fmt_ctl.eq.'merged_vtk_gz'                           &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_GZ') then
           id_field_file_format = iflag_sgl_vtk + iflag_gzip
      else if(file_fmt_ctl.eq.'single_hdf5'                             &
     &   .or. file_fmt_ctl.eq.'single_HDF5'                             &
     &   .or. file_fmt_ctl.eq.'Single_HDF5'                             &
     &   .or. file_fmt_ctl.eq.'SINGLE_HDF5'                             &
     &   .or. file_fmt_ctl.eq.'merged_hdf5'                             &
     &   .or. file_fmt_ctl.eq.'merged_HDF5'                             &
     &   .or. file_fmt_ctl.eq.'Merged_HDF5'                             &
     &   .or. file_fmt_ctl.eq.'MERGED_HDF5') then
           id_field_file_format = iflag_sgl_hdf5
      else
        call choose_ucd_file_format(file_fmt_ctl, i_file_fmt,           &
     &          id_field_file_format)
      end if
!
      end subroutine choose_para_fld_file_format
!
! -----------------------------------------------------------------------
!
      end module parallel_udt_IO_select
