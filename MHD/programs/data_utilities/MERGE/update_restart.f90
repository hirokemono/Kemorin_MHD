!update_restart
! ----- program  update_restart
!
      program    update_restart
!
!    change restart data for different number of domains
!     By H. Matsui
!
!
      use m_precision
!
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use m_read_mesh_data
      use m_field_data_IO
!
      use set_merged_geometry
      use set_2nd_geometry_4_serial
      use gen_new_restart_snap
      use input_old_file_sel_4_zlib
!
      implicit    none
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
      iflag_mesh_file_fmt = iorg_mesh_file_fmt
      call set_merged_node_and_element
!
      mesh_file_head = new_mesh_head
      iflag_mesh_file_fmt = inew_mesh_file_fmt
      call s_set_2nd_geometry_4_serial
!
      call deallocate_geom_ex_glnod
      call deallocate_2nd_merge_table
!
!  allocate restart data
!
      phys_file_head =       org_rst_head
      iflag_field_data_fmt = iorg_rst_file_fmt
      numgrid_phys_IO = subdomain(ione)%node%numnod
      call sel_read_rst_comps(izero, istep_start)
      call count_restart_data_fields
!
!   loop for time integration
!
      do istep = istep_start, istep_end, increment_step
!
        call update_restart_file(istep)
        write(*,*) 'step', istep, 'finish '
      end do
      call deallocate_phys_data_name_IO
!
!
      if(iflag_delete_org .gt. 0) then
        do istep = istep_start, istep_end, increment_step
          call delete_old_restart(istep)
        end do
      end if
!
!
      stop ' //// program normally finished //// '
!
      end program update_restart 
