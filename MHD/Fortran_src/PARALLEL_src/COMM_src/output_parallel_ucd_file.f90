!output_parallel_ucd_file.f90
!      module output_parallel_ucd_file
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine set_control_parallel_field_def
!
!      subroutine output_grd_file
!      subroutine output_udt_one_snapshot(istep_ucd)
!      subroutine finalize_ucd_file_output
!
      module output_parallel_ucd_file
!
      use m_precision
      use m_parallel_var_dof
      use m_field_file_format
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_control_parallel_field_def
!
      use parallel_udt_IO_select
!
!
      call set_merged_ucd_file_define(fem_ucd)
!
      end subroutine set_control_parallel_field_def
!
! -----------------------------------------------------------------------
!
      subroutine output_grd_file
!
      use merged_udt_vtk_file_IO
      use parallel_udt_IO_select
!
!
      call link_fem_num_field_2_ucd_out
      call link_local_mesh_4_ucd_out
      call link_fem_field_data_2_ucd_out
!
      if (fem_ucd%ifmt_file/100 .eq. iflag_single/100) then
        call init_merged_ucd(fem_ucd, merged_ucd)
      end if
!
      call sel_write_parallel_ucd_mesh(fem_ucd, merged_ucd)
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
      end subroutine output_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_udt_one_snapshot(istep_ucd)
!
      use merged_udt_vtk_file_IO
      use copy_time_steps_4_restart
      use parallel_udt_IO_select
!
      integer(kind = kint), intent(in) :: istep_ucd
!
!
      call link_fem_num_field_2_ucd_out
      call link_local_mesh_4_ucd_out
      call link_fem_field_data_2_ucd_out
!
      if (fem_ucd%ifmt_file/100 .eq. iflag_single/100) then
        call init_merged_ucd(fem_ucd, merged_ucd)
      end if
!
      call copy_time_steps_to_restart
      call sel_write_parallel_ucd_file(istep_ucd, fem_ucd, merged_ucd)
!
      call deallocate_ucd_node(fem_ucd)
!
      call deallocate_ucd_ele(fem_ucd)
      call disconnect_ucd_data(fem_ucd)
!
      if (fem_ucd%ifmt_file/100 .eq. iflag_single/100) then
        call finalize_merged_ucd(fem_ucd, merged_ucd)
      end if
!
      end subroutine output_udt_one_snapshot
!
!-----------------------------------------------------------------------
!
      subroutine finalize_ucd_file_output
!
      use merged_udt_vtk_file_IO
!
!
      call finalize_merged_ucd(fem_ucd, merged_ucd)
!
      end subroutine finalize_ucd_file_output
!
!-----------------------------------------------------------------------
!
      end module output_parallel_ucd_file
