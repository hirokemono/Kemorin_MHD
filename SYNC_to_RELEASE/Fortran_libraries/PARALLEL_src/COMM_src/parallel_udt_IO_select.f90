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
      use merged_udt_vtk_file_IO
!
      integer(kind=kint), intent(in) :: istep_udt
!
!
      if (itype_ucd_data_file .eq. iflag_sgl_vtk) then
        call write_merged_vtk_file(istep_udt)
      else if (itype_ucd_data_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_phys(istep_udt)
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
      use merged_udt_vtk_file_IO
!
!
      if(itype_ucd_data_file .eq. iflag_sgl_vtd) then
        call write_merged_vtk_grid
      end if
!
      end subroutine sel_write_parallel_ucd_mesh
!
!------------------------------------------------------------------
!
      end module parallel_udt_IO_select
