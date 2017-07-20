!update_restart
! ----- program  update_restart
!
!    change restart data for different number of domains
!     By H. Matsui
!
!
      program update_restart
!
      use m_precision
!
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
!
      use t_time_data
      use t_field_data_IO
      use t_ucd_data
!
      use set_merged_geometry
      use set_2nd_geometry_4_serial
      use new_FEM_restart
      use input_old_file_sel_4_zlib
!
      implicit none
!
      type(time_data), save :: merged_time_IO
      type(field_IO), save :: merged_IO
!
      integer (kind = kint) :: istep
!
! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================

      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' original mesh data:  mesh/in.PE#'
      write(*,*) ' new mesh data:  mesh_target/in.PE#'
      write(*,*) ' original restart data'
      write(*,*) '    restart/rst.step#.PE#, restart/adams.step#.PE#'
      write(*,*) ' new restart data'
      write(*,*) '    rst_new/rst.step#.PE#, rst_new/adams.step#.PE#'
!
!
      call read_control_4_merge
!
      call set_control_4_merge
      call set_control_4_newrst
!
!     read outline of mesh
!
      call set_merged_node_and_element(merge_org_mesh_file)
!
      call s_set_2nd_geometry_4_serial(merged_mesh_file)
!
      call deallocate_node_geometry_type(merged%node)
      call deallocate_2nd_merge_table
!
!  allocate restart data
!
      call init_by_old_restart_data                                     &
     &   (org_fst_param, merged_time_IO, merged_IO)
!
!   loop for time integration
!
      do istep = istep_start, istep_end, increment_step
        call update_restart_file(istep, org_fst_param, new_fst_param,   &
     &      merged_time_IO, merged_IO)
        write(*,*) 'step', istep, 'finish '
      end do
      call dealloc_phys_name_IO(merged_IO)
!
!
      if(iflag_delete_org .gt. 0) then
        do istep = istep_start, istep_end, increment_step
          call delete_old_restart(istep, org_fst_param)
        end do
      end if
!
!
      stop ' //// program normally finished //// '
!
      end program update_restart 
