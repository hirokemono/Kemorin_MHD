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
      use m_size_of_cube
      use m_cube_position
      use m_setting_4_ini
      use m_ctl_data_4_cub_kemo
      use m_cube_files_data
      use set_ctl_data_plane_mesh
      use set_parallel_file_name
      use mesh_IO_select
      use field_IO_select
      use set_field_to_restart
      use copy_mesh_structures
!
      use t_mesh_data
      use t_geometry_data
      use t_time_data
      use t_field_data_IO
      use t_file_IO_parameter
      use t_mesh_data_4_merge
!
      implicit none
!
      integer :: id_rank
      integer(kind=kint) :: ip, ierr, inod
      integer(kind=kint) :: np, jst, jed
      type(node_data) :: node_plane
      type(time_data), save :: plane_t_IO
      type(field_IO) :: plane_fst_IO
      type(mesh_geometry) :: mesh_IO_p
      type(field_IO_params), save :: cube_mesh_file
      type(field_IO_params), save :: plane_fld_file
      type(merged_mesh), save :: mgd_mesh_pl
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
      call set_initial_components(mgd_mesh_pl%merged_fld)
      call reset_time_data(plane_t_IO)
!
      mgd_mesh_pl%num_pe = ndx * ndy * ndz
!
      mgd_mesh_pl%merged%node%numnod = node_plane%numnod
!
      call alloc_merged_field_stack(nprocs, plane_fst_IO)
      plane_fst_IO%istack_numnod_IO(0) = 0
      do ip = 1, mgd_mesh_pl%num_pe
        plane_fst_IO%istack_numnod_IO(ip)                               &
     &      = plane_fst_IO%istack_numnod_IO(ip-1)                       &
     &       + mgd_mesh_pl%merged%node%numnod
      end do
!
      do ip = 1, mgd_mesh_pl%num_pe
        id_rank = ip-1
!
!    read mesh file
!
        call copy_mesh_format_and_prefix                                &
     &     (mesh_file_header, id_ascii_file_fmt, cube_mesh_file)
        call sel_read_geometry_size                                     &
     &     (cube_mesh_file, id_rank, mesh_IO_p, ierr)
        if(ierr .gt. 0) stop 'Mesh is wrong!!'
!
        call copy_node_geometry_types(mesh_IO_p%node, node_plane)
        call dealloc_node_geometry_IO(mesh_IO_p)
!
        call alloc_phys_data_type                                       &
     &     (mgd_mesh_pl%merged%node%numnod, mgd_mesh_pl%merged_fld)
!
!   set up of physical values
!
        call initial_field_on_plane                                     &
     &     (c_size1, mgd_mesh_pl%merged, mgd_mesh_pl%merged_fld)
!
!     write data
!
!
        plane_fst_IO%nnod_IO = mgd_mesh_pl%merged%node%numnod
!
        call simple_init_fld_name_to_rst                                &
     &     (mgd_mesh_pl%merged%node%numnod, mgd_mesh_pl%merged_fld,     &
     &      plane_fst_IO)
        call simple_copy_fld_data_to_rst                                &
     &     (mgd_mesh_pl%merged%node, mgd_mesh_pl%merged_fld,            &
     &      plane_fst_IO)
!
        call set_file_fmt_prefix                                        &
     &     (izero, org_rst_f_header, plane_fld_file)
        call sel_write_step_FEM_field_file                              &
     &     (mgd_mesh_pl%num_pe, id_rank, izero,                         &
     &      plane_fld_file, plane_t_IO, plane_fst_IO)
!
        call dealloc_phys_name_IO(plane_fst_IO)
        call dealloc_phys_data_IO(plane_fst_IO)
!
        call dealloc_phys_data_type(mgd_mesh_pl%merged_fld)
        call dealloc_node_geometry_base(node_plane)
      end do
!
      stop
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine initial_field_on_plane(c_size, merged, merged_fld)
!
      use t_size_of_cube
      use t_mesh_data
      use t_phys_data
!
      type(size_of_cube), intent(in) :: c_size
      type(mesh_geometry), intent(in) :: merged
      type(phys_data), intent(inout) :: merged_fld
!
      integer(kind = kint) :: ip
!
!
        do ip = 1, merged_fld%num_phys
          jst = merged_fld%istack_component(ip-1)
          jed = merged_fld%istack_component(ip)
!
          if (merged_fld%phys_name(ip) .eq. fhd_temp) then
!
            do inod = 1, merged%node%numnod
              if (node_plane%xx(inod,3) .eq. c_size%zmin) then
                merged_fld%d_fld(inod,jst+1) = 1.0d0
              else if (node_plane%xx(inod,3) .eq. c_size%zmax) then
                merged_fld%d_fld(inod,jst+1) = 0.0d0
              else
                merged_fld%d_fld(inod,jst+1)                            &
     &              = -(node_plane%xx(inod,3) - c_size%zmax)            &
     &                 / (c_size%zmax - c_size%zmin)                    &
     &               + 0.5*(node_plane%xx(inod,3) - c_size%zmin)        &
     &                 / (c_size%zmax - c_size%zmin)                    &
     &                * sin(2.0d0*pi*node_plane%xx(inod,1)              &
     &                 / (c_size%xmax - c_size%xmin))                   &
     &                * sin(2.0d0*pi*node_plane%xx(inod,2               &
     &                ) /(c_size%xmax - c_size%xmin))
              end if
            end do
!
          else if (merged_fld%phys_name(ip) .eq. fhd_vecp) then
!
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+1)                              &
     &            = 0.01d0*sin(pi*node_plane%xx(inod,3)                 &
     &             / (c_size%zmax - c_size%zmin))
            end do
!
          else if (merged_fld%phys_name(ip) .eq. fhd_magne) then
!
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+2) = (0.01d0*pi/two)            &
     &                * cos( pi*node_plane%xx(inod,3)                   &
     &                 / (c_size%zmax - c_size%zmin))
            end do
          end if
        end do
!
      end subroutine initial_field_on_plane
!
!------------------------------------------------------------------
!
      end program generate_initial_4_plane

