! \beginFILE
!     program  assemble_mesh

      program    assemble_mesh 
!
! * program 'assemble' assemble UCD of parts files to one UCD file
!    \begin{flushright}
!         programmed by m.iizuka (rist)  on sep. 1998 (ver 1.0)
!         modified  by  H.Matsui (rist)  on sep. 2000 (ver 1.1)
!         modified  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.2)
!    \end{flushright}

      use m_precision
!
      use m_read_mesh_data
      use m_geometry_data_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use set_merged_geometry
      use write_merged_mesh
      use t_ucd_data
!
      implicit    none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: fem_ucd
!
! ==============================================                                                                                                            
! * get number of  nodes,elements for whole PES
! ==============================================
!
      call read_control_4_merge
      call set_control_4_merge(fem_ucd)
      call set_control_4_merged_mesh
!
!  read mesh information
!
      iflag_mesh_file_fmt = iorg_mesh_file_fmt
      call set_merged_mesh_and_group
!
!   output grid data
!
      call s_write_merged_mesh(merge_tbl, merged, merged_grp)
      call deallocate_array_4_merge
!
      stop ' //// program normally finished //// '
!
!
      end program assemble_mesh
