!output_parallel_ucd_file.f90
!      module output_parallel_ucd_file
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine output_grd_file
!
!      subroutine output_udt_one_snapshot(istep_ucd)
!
      module output_parallel_ucd_file
!
      use m_precision
      use m_parallel_var_dof
      use m_field_file_format
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_grd_file
!
      use m_ucd_data
      use set_ucd_data
      use merged_udt_vtk_file_IO
      use parallel_udt_IO_select
!
!
      call link_num_field_2_output
      call link_local_mesh_4_ucd
      call link_field_data_2_output
!
      if (fem_ucd%ifmt_file/100 .eq. iflag_single/100) then
        call init_merged_ucd(fem_ucd)
      end if
!
      call sel_write_parallel_ucd_mesh
!
      if(   mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_udt/10             &
     & .or. mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_vtd/10) then
        call deallocate_ucd_ele(fem_ucd)
      end if
!
      if(mod(fem_ucd%ifmt_file,100)/10 .eq. iflag_vtd/10) then
        call deallocate_ucd_node(fem_ucd)
      end if
!
!
      end subroutine output_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_udt_one_snapshot(istep_ucd)
!
      use m_ucd_data
      use set_ucd_data
      use merged_udt_vtk_file_IO
      use copy_time_steps_4_restart
      use parallel_udt_IO_select
!
      integer(kind = kint), intent(in) :: istep_ucd
!
!
      call link_num_field_2_output
      call link_local_mesh_4_ucd
      call link_field_data_2_output
!
      if (fem_ucd%ifmt_file/100 .eq. iflag_single/100) then
        call init_merged_ucd(fem_ucd)
      end if
!
      call copy_time_steps_to_restart
      call sel_write_parallel_ucd_file(istep_ucd)
!
      call deallocate_ucd_node(fem_ucd)
!
      call deallocate_ucd_ele(fem_ucd)
      call disconnect_ucd_data(fem_ucd)
!
      if (fem_ucd%ifmt_file/100 .eq. iflag_single/100) then
        call finalize_merged_ucd(fem_ucd)
      end if
!
      end subroutine output_udt_one_snapshot
!
!-----------------------------------------------------------------------
!
      subroutine finalize_ucd_file_output
!
      use m_ucd_data
      use merged_udt_vtk_file_IO
!
!
      call finalize_merged_ucd(fem_ucd)
!
      end subroutine finalize_ucd_file_output
!
!-----------------------------------------------------------------------
!
      end module output_parallel_ucd_file
