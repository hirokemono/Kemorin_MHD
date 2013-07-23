! \beginFILE
!     program  assemble_merge

      program    assemble_merge
! \beginSUBROUTINE
! * program 'assemble' assemble UCD of parts files to one UCD file
!    \begin{flushright}
!         programmed by m.iizuka (rist)  on sep. 1998 (ver 1.0)
!         modified  by  H.Matsui (rist)  on sep. 2000 (ver 1.1)
!         modified  by  H.Matsui (U. Chicago)  on Oct., 2003 (ver 1.2)
!         modified  by  H.Matsui (U. Chicago)  on June, 2007 (ver 1.3)
!         modified  by  H.Matsui (UC Berkeley)  on Oct., 2008 (ver 1.4)
!    \end{flushright}
!
      use m_precision
!
      use m_constants
      use m_read_mesh_data
      use m_geometry_parameter
      use m_geometry_data_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use m_ucd_data
      use m_original_ucd_4_merge
!
      use set_merged_geometry
      use set_merged_udt_2_IO
      use ucd_IO_select
      use set_ucd_data_to_type

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
      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' mesh data:  mesh/in.PE#'
      write(*,*) ' control data for this routine:  control_merge'
      write(*,*) ' simulation results: field/out.step#.PE#.udt'
!
!   read control data for merge
!
      call read_control_4_merge
      call set_control_4_merge
!
!  read mesh information
!
      iflag_mesh_file_fmt = iorg_mesh_file_fmt
      call set_merged_mesh_and_group
!
!   read field name and number of components
!
      call init_ucd_data_4_merge(istep_start)
!
!    set list array for merged field
!
      call set_field_list_4_merge
      call allocate_merged_field_data
!
!   output grid data
!
      call link_write_merged_grd_2_ucd(itype_assembled_data, &
     &     merged_data_head)
!
!   loop for snap shots
!
      do istep = istep_start, istep_end, increment_step
        call read_ucd_data_4_merge(istep)
!
        call link_write_merged_udt(istep, itype_assembled_data,         &
     &      merged_data_head)
!
      write(*,*) 'step', istep, 'finish '
      end do
!
      stop ' //// program normally terminated //// '
!
      end program assemble_merge
