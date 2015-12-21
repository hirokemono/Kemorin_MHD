!
! ----- program  newrst
!
      program    newrst 
!
!    change restart data for different number of domains
!     By H. Matsui
!
!
      use m_precision
      use calypso_mpi
!
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use m_read_mesh_data
      use m_ucd_data
!
      use set_merged_geometry
      use set_2nd_geometry_4_serial
      use new_FEM_restart
      use field_IO_select
!
      implicit    none
!
      integer (kind = kint) :: istep
!
! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================
!
      call calypso_MPI_init
!
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
      call set_control_4_merge(fem_ucd)
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
      call deallocate_node_geometry_type(merged%node)
      call deallocate_2nd_merge_table
!
!  allocate restart data
!
      call count_restart_data_fields
!
!   loop for time integration
!
      do istep = istep_start, istep_end, increment_step
!
        call generate_new_restart_snap(istep)
        write(*,*) 'step', istep, 'finish '
      end do
      call dealloc_newrst_phys_name_IO
!
!
      if(iflag_delete_org .gt. 0) then
        do istep = istep_start, istep_end, increment_step
          call delete_restart_files(istep)
        end do
      end if
!
      call calypso_MPI_finalize
!
      stop ' //// program normally finished //// '
!
      end program newrst 
