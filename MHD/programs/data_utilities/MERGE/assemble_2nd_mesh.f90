!assemble_2nd_mesh.f90
!     program assemble_2nd_mesh
! * program 'assemble' assemble UCD of parts files to one UCD file
!         programmed by m.iizuka (rist)  on sep. 1998 (ver 1.0)
!         modified  by  H.Matsui (rist)  on sep. 2000 (ver 1.1)
!         modified  by  H.Matsui (U. Chicago)  on Oct., 2003 (ver 1.2)
!         modified  by  H.Matsui (U. Chicago)  on June, 2007 (ver 1.3)
!
      program assemble_2nd_mesh
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_read_mesh_data
      use m_geometry_data_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use m_original_ucd_4_merge
!
      use set_merged_geometry
      use set_2nd_geometry_4_serial
      use set_merged_udt_2_IO
      use output_newdomain_ucd
      use field_IO_select
      use set_ucd_file_names
!
      implicit    none
!
!  ===========
! . for local 
!  ===========
      integer(kind=kint ) :: istep
!
! ==============================================                                                                                                            
! * get number of  nodes,elements for whole PES
! ==============================================
!
      call calypso_MPI_init
!
       write(*,*) ' Dou you prepare folloing data???'
       write(*,*) ' original mesh data:  mesh/in.PE#'
       write(*,*) ' transfered mesh data:  mesh_target/in.PE#'
       write(*,*) ' control data for this routine:  control_merge'
       write(*,*) ' simulation results: field/out.step#.PE#.udt'
       write(*,*) ' transfered results: field_new/out.step#.PE#.udt'
!
!   read control data for merge
!
      call read_control_4_merge
!
      call set_control_4_merge
      call set_control_4_newudt
!
!  read mesh information
!
      iflag_mesh_file_fmt = iorg_mesh_file_fmt
      call set_merged_mesh_and_group
!
      mesh_file_head = new_mesh_head
      iflag_mesh_file_fmt = inew_mesh_file_fmt
      call s_set_2nd_geometry_4_serial
!
!   read field name and number of components
!
      call init_ucd_data_4_merge(istep_start)
!
!    set list array for merged field
!
      call set_field_list_4_merge
      call alloc_phys_data_type(merged%node%numnod, merged_fld)
!
!   Cnostract grid data
!
      call assemble_2nd_udt_nesh
!
!   loop for snap shots
!
!
      do istep = istep_start, istep_end, increment_step
!        write(*,*) 'read_ucd_data_4_merge', istep
        call read_ucd_data_4_merge(istep)
        call assemble_2nd_udt_phys(istep)
        write(*,*) 'step', istep, 'finish '
      end do
!
!
      if(iflag_delete_org .gt. 0) then
        do istep = istep_start, istep_end, increment_step
          call delete_para_ucd_file(udt_original_header,                &
     &        itype_org_ucd_file, num_pe, istep)
        end do
      end if
!
      call calypso_MPI_finalize
!
      stop ' //// program normally finished //// '
!
      end program assemble_2nd_mesh


