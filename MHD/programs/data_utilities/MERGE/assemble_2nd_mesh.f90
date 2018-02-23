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
      use m_geometry_data_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use m_original_ucd_4_merge
      use t_time_data
      use t_ucd_data
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
!>        Instance for FEM field data IO
      type(time_data), save :: fem_time_IO
      type(ucd_data), save :: fem_ucd
      type(ucd_data), save :: second_ucd
!
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
      call set_merged_mesh_and_group(merge_org_mesh_file)
!
      call s_set_2nd_geometry_4_serial(merged_mesh_file)
!
!   read field name and number of components
!
      call init_ucd_data_4_merge                                        &
     &   (istep_start, original_ucd_param, fem_time_IO, fem_ucd)
!
!    set list array for merged field
!
      call set_field_list_4_merge(mgd_mesh1%merged_fld)
      call alloc_phys_data_type                                         &
     &   (mgd_mesh1%merged%node%numnod, mgd_mesh1%merged_fld)
!
!   Cnostract grid data
!
      call assemble_2nd_udt_mesh                                        &
     &   (assemble_ucd_param, mgd_mesh1%merged, second_ucd)
!
!   loop for snap shots
!
!
      do istep = istep_start, istep_end, increment_step
!        write(*,*) 'read_ucd_data_4_merge', istep
        call read_ucd_data_4_merge(istep, mgd_mesh1%num_pe,             &
     &      mgd_mesh1%subdomain, mgd_mesh1%merge_tbl,                   &
     &      original_ucd_param, fem_time_IO, fem_ucd,                   &
     &      mgd_mesh1%merged_fld)
        call assemble_2nd_udt_phys(istep, assemble_ucd_param,           &
     &      mgd_mesh1%merged, mgd_mesh1%merged_fld,                     &
     &      fem_time_IO, second_ucd)
        write(*,*) 'step', istep, 'finish '
      end do
!
!
      if(iflag_delete_org .gt. 0) then
        do istep = istep_start, istep_end, increment_step
          call delete_para_ucd_file(original_ucd_param%file_prefix,     &
     &        original_ucd_param%iflag_format, mgd_mesh1%num_pe, istep)
        end do
      end if
!
      call calypso_MPI_finalize
!
      stop ' //// program normally finished //// '
!
      end program assemble_2nd_mesh


