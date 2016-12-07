!
      program generate_initial_4_plane
!
!***********************************************************
!
!     program for initial value generation for plane layer
!
!***********************************************************
!
!***********************************************************
!
!     required files
!
!        mesh/in.#:          mesh data
!        mesh/domainsize.dat: size information
!        restart/rst.0.#:    initial data
!        restart/adams.0.#:  initial data for Adams-Bashforth (zero)
!
!***********************************************************
!
      use m_precision
!
      use m_constants
      use m_phys_labels
      use m_size_4_plane
      use m_cube_position
      use m_setting_4_ini
      use m_geometry_data_4_merge
      use m_ctl_data_4_cub_kemo
      use m_time_data_IO
      use m_read_mesh_data
      use set_ctl_data_plane_mesh
      use set_parallel_file_name
      use mesh_IO_select
      use field_IO_select
      use set_field_to_restart
      use copy_mesh_structures
!
      use t_mesh_data
      use t_geometry_data
      use t_field_data_IO
!
      implicit none
!
      integer(kind=kint) :: ip, id_rank, ierr, inod
      integer(kind=kint) :: np, jst, jed
      type(node_data) :: node_plane
      type(field_IO) :: plane_fst_IO
      type(mesh_geometry) :: mesh_IO_p
!
      character(len=kchara), parameter                                  &
     &      :: org_rst_f_header = 'restart/rst'
!
      pi = four*atan(one)
!
!     read outline of mesh
!
      call read_control_data_plane_mesh
      call s_set_ctl_data_plane_mesh
!
      call set_initial_components
!
      i_time_step_IO = izero
      time_IO =    zero
      delta_t_IO = zero
      num_pe = ndx * ndy * ndz
!
      merged%node%numnod = node_plane%numnod
!
      call alloc_merged_field_stack(nprocs, plane_fst_IO)
      plane_fst_IO%istack_numnod_IO(0) = 0
      do ip = 1, num_pe
        plane_fst_IO%istack_numnod_IO(ip)                               &
     &      = plane_fst_IO%istack_numnod_IO(ip-1) + merged%node%numnod
      end do
!
      do ip = 1, num_pe
        id_rank = ip-1
!
!    read mesh file
!
        call copy_mesh_format_and_prefix                                &
     &     (def_mesh_file_head, id_ascii_file_fmt, mesh1_file)
        call sel_read_geometry_size                                     &
     &     (mesh1_file, id_rank, mesh_IO_p, ierr)
        if(ierr .gt. 0) stop 'Mesh is wrong!!'
!
        call copy_node_geometry_types(mesh_IO_p%node, node_plane)
!
        call dealloc_node_geometry_base(mesh_IO_p%node)
        call deallocate_type_neib_id(mesh_IO_p%nod_comm)
!
        call alloc_phys_data_type(merged%node%numnod, merged_fld)
!
!   set up of physical values
!
        do np = 1, merged_fld%num_phys
          jst = merged_fld%istack_component(np-1)
          jed = merged_fld%istack_component(np)
!
          if (merged_fld%phys_name(np) .eq. fhd_temp) then
!
            do inod = 1, merged%node%numnod
              if (node_plane%xx(inod,3).eq.zmin) then
                merged_fld%d_fld(inod,jst+1) = 1.0d0
              else if (node_plane%xx(inod,3).eq.zmax) then
                merged_fld%d_fld(inod,jst+1) = 0.0d0
              else
                merged_fld%d_fld(inod,jst+1)                            &
     &              = -(node_plane%xx(inod,3)-zmax)/(zmax-zmin)         &
     &               + 0.5*(node_plane%xx(inod,3)-zmin)/(zmax-zmin)     &
     &                * sin(2.0d0*pi*node_plane%xx(inod,1)/(xmax-xmin)) &
     &                * sin(2.0d0*pi*node_plane%xx(inod,2)/(xmax-xmin))
              end if
            end do
!
          else if (merged_fld%phys_name(np) .eq. fhd_vecp) then
!
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+1)                              &
     &            = 0.01d0*sin(pi*node_plane%xx(inod,3) / (zmax-zmin))
            end do
!
          else if (merged_fld%phys_name(np) .eq. fhd_magne) then
!
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+2) = (0.01d0*pi/two)            &
     &                * cos( pi*node_plane%xx(inod,3) / (zmax-zmin))
            end do
          end if
        end do
!
!     write data
!
!
        plane_fst_IO%nnod_IO = merged%node%numnod
!
        plane_fst_IO%num_field_IO = merged_fld%num_phys
        plane_fst_IO%ntot_comp_IO = merged_fld%ntot_phys
        call alloc_phys_name_IO(plane_fst_IO)
        call alloc_phys_data_IO(plane_fst_IO)
!
        call simple_copy_fld_name_to_rst(merged_fld, plane_fst_IO)
        call simple_copy_fld_data_to_rst                                &
     &     (merged%node, merged_fld, plane_fst_IO)
!
        call set_field_file_fmt_prefix                                  &
     &     (izero, org_rst_f_header, plane_fst_IO)
        call sel_write_step_FEM_field_file                              &
     &     (num_pe, id_rank, izero, plane_fst_IO)
!
        call dealloc_phys_name_IO(plane_fst_IO)
        call dealloc_phys_data_IO(plane_fst_IO)
!
        call dealloc_phys_data_type(merged_fld)
        call dealloc_node_geometry_base(node_plane)
      end do
!
      stop
      end program generate_initial_4_plane

