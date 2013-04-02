!
!     module input_control
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!
!      subroutine input_control_4_MHD
!
!
      module input_control
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_MHD
!
      use m_iccg_parameter
      use input_MG_data
!
!
      call time_prog_barrier
!
      call input_meshes_4_MHD
!
      if (     ((method_4_solver(1:1).eq.'M')                           &
     &      .or.(method_4_solver(1:1).eq.'m'))                          &
     &   .and. ((method_4_solver(2:2).eq.'G')                           &
     &      .or.(method_4_solver(2:2).eq.'g'))                          &
     &   .and. ((method_4_solver(3:3).eq.'C')                           &
     &      .or.(method_4_solver(3:3).eq.'c'))                          &
     &   .and. ((method_4_solver(4:4).eq.'G')                           &
     &      .or.(method_4_solver(4:4).eq.'g')) ) then
        call input_MG_mesh
        call input_MG_itp_tables
      end if
!
      end subroutine input_control_4_MHD
!
! ----------------------------------------------------------------------
!
      subroutine input_meshes_4_MHD
!
      use m_machine_parameter
      use m_control_parameter
      use m_read_mesh_data
!
      use element_IO_select
      use surface_IO_select
      use edge_IO_select
      use read_boundary_condition_file
      use set_3d_filtering_group_id
      use read_filtering_data
      use set_ele_comm_tbl_4_IO
      use set_surf_comm_tbl_4_IO
      use set_edge_comm_tbl_4_IO
      use set_surface_geometry_4_IO
      use set_edge_geometry_4_IO
      use node_monitor_IO
!
!  --  read geometry
!
      if (iflag_ele_file_name .gt. 0) then
        call sel_input_element_comm_table(my_rank)
        call copy_ele_comm_tbl_from_IO
      end if
!
! ---------------------------------
!
      if (iflag_surf_file_name .gt. 0) then
        call sel_input_surface_connect(my_rank)
        call copy_surf_comm_table_from_IO
        call copy_surf_connect_from_IO
      end if
!
! ---------------------------------
!
      if (iflag_edge_file_name .gt. 0) then
        call sel_input_edge_connect(my_rank)
        call copy_edge_comm_tbl_from_IO
        call copy_edge_connect_from_IO
      end if
!
! ---------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_node_id_4_monitor'
      call set_local_node_id_4_monitor
!
      if (iflag_debug.eq.1) write(*,*) 'read_boundary_files'
      call read_boundary_files
!
! ---------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_read_filtering_data'
      call s_read_filtering_data
!
      if(mod(iflag_SGS_filter,10).eq.1) then
        if(iflag_debug.eq.1) write(*,*) 's_set_3d_filtering_group_id'
        call s_set_3d_filtering_group_id
!
        if (iflag_SGS_model .eq. id_SGS_similarity                      &
     &       .and. iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_ON) then
          if (iflag_debug.eq.1)                                         &
     &         write(*,*) 's_set_w_filtering_group_id'
          call s_set_w_filtering_group_id
        end if
      end if
!
! ---------------------------------
!
      end subroutine input_meshes_4_MHD
!
! ----------------------------------------------------------------------
!
      end module input_control
