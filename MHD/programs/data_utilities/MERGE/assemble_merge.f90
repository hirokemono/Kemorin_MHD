! \beginFILE
!     program  assemble_merge
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
      program assemble_merge
!
      use m_precision
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
      use set_merged_udt_2_IO
      use ucd_IO_select
      use set_ucd_data_to_type

      implicit    none
!
!  ===========
! . for local 
!  ===========
!>        Instance for FEM field data IO
      type(time_data), save :: fem_time_IO
      type(ucd_data), save :: fem_ucd
!
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
      call set_control_4_merge(mgd_mesh1%num_pe)
!
!  read mesh information
!
      call set_merged_mesh_and_group(merge_org_mesh_file, mgd_mesh1)
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
!   output grid data
!
      call link_merged_node_2_ucd_IO                                    &
     &   (mgd_mesh1%merged, mgd_mesh1%merge_tbl, fem_ucd)
      call link_merged_ele_2_ucd_IO                                     &
     &   (mgd_mesh1%merged, mgd_mesh1%merge_tbl, fem_ucd)
!
      call sel_write_grd_file(izero, assemble_ucd_param, fem_ucd)
!
      if(    mod(assemble_ucd_param%iflag_format,100)/10                &
     &                                     .eq. iflag_vtd/10            &
     &  .or. mod(assemble_ucd_param%iflag_format,100)/10                &
     &                                     .eq. iflag_udt/10) then
        call deallocate_ucd_ele(fem_ucd)
      end if
!
!   loop for snap shots
!
      do istep = istep_start, istep_end, increment_step
        call read_ucd_data_4_merge(istep, mgd_mesh1%num_pe,             &
     &      mgd_mesh1%subdomain, mgd_mesh1%merge_tbl,                   &
     &      original_ucd_param, fem_time_IO, fem_ucd,                   &
     &      mgd_mesh1%merged_fld)
        call link_merged_field_2_udt_IO                                 &
     &     (mgd_mesh1%merged_fld, mgd_mesh1%merge_tbl, fem_ucd)
!
        call sel_write_ucd_file                                         &
     &     (iminus, istep, assemble_ucd_param, fem_time_IO, fem_ucd)
        call deallocate_ucd_data(fem_ucd)
      write(*,*) 'step', istep, 'finish '
      end do
!
      stop ' //// program normally finished //// '
!
      end program assemble_merge
