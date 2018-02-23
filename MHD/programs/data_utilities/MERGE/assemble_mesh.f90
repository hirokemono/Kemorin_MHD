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
      use t_ucd_data
      use m_geometry_data_4_merge
      use m_control_data_4_merge
      use m_control_param_merge
      use m_default_file_prefix
      use set_merged_geometry
      use set_control_platform_data
      use write_merged_mesh
!
      implicit    none
!
!
! ==============================================                                                                                                            
! * get number of  nodes,elements for whole PES
! ==============================================
!
      call read_control_4_merge
      call set_control_4_merge(mgd_mesh1%num_pe)
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, assemble_plt, merged_mesh_file)
!
!  read mesh information
!
      call set_merged_mesh_and_group(merge_org_mesh_file, mgd_mesh1)
!
!   output grid data
!
      call s_write_merged_mesh                                          &
     &   (mgd_mesh1%merge_tbl, mgd_mesh1%merged, mgd_mesh1%merged_grp)
      call dealloc_array_4_merge(mgd_mesh1)
!
      stop ' //// program normally finished //// '
!
!
      end program assemble_mesh
