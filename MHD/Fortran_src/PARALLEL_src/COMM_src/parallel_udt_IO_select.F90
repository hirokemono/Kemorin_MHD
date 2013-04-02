!parallel_udt_IO_select.F90
!      module parallel_udt_IO_select
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine sel_write_parallel_ucd_file(istep_udt)
!      subroutine sel_write_parallel_ucd_mesh
!
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
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
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
!
      end module parallel_udt_IO_select
