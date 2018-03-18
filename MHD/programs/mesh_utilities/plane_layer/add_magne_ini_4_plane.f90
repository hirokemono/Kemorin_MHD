!
      program add_magne_ini_4_plane
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
!        original/rst.#.#:   original restart data
!        original/adams.#.#: original restart data for Adams-Bashforth
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
      use m_ctl_data_4_cub_kemo
      use m_cube_files_data
      use t_field_data_IO
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
      character(len=kchara), parameter                                  &
     &      :: org_rst_f_header = 'restart/rst'
      character(len=kchara), parameter                                  &
     &      :: new_rst_file_header = 'rst_new/rst'
!
      integer(kind=kint) :: istep, ip, id_rank, ierr, np, inod
      integer(kind=kint) :: jst
!
      type(node_data) :: node_plane
      type(time_data), save :: plane_t_IO
      type(field_IO) :: plane_fst_IO
      type(mesh_geometry) :: mesh_IO_p
      type(field_IO_params), save :: cube_mesh_file
      type(field_IO_params), save :: plane_fld_file
      type(merged_mesh), save :: mgd_mesh_pl
!
!
      pi = four*atan(one)
      write(*,*) 'pi', pi
!
!     read outline of mesh
!
      call read_control_data_plane_mesh
      call s_set_ctl_data_plane_mesh
!
       mgd_mesh_pl%num_pe = ndx * ndy * ndz
!
!
!
      write(*,*) 'input step number of original data'
      read(*,*) istep
!
!     open original data
!
      ip = 1
      id_rank = 0
!
      call set_file_fmt_prefix                                          &
     &   (izero, org_rst_f_header, plane_fld_file)
      call sel_read_alloc_FEM_fld_head                                  &
     &   (mgd_mesh_pl%num_pe, izero, istep,                             &
     &    plane_fld_file, plane_t_IO, plane_fst_IO)
!
      num_rst_org = plane_fst_IO%num_field_IO
!
      call add_initial_num_comp_mhd(mgd_mesh_pl%merged_fld)
!
      mgd_mesh_pl%merged_fld%num_phys = num_rst_new
      call alloc_phys_name_type(mgd_mesh_pl%merged_fld)
!
      ntot_rst_org =  plane_fst_IO%ntot_comp_IO
      mgd_mesh_pl%merged_fld%phys_name(1:num_rst_org)                   &
     &             =    plane_fst_IO%fld_name(1:num_rst_org)
      mgd_mesh_pl%merged_fld%num_component(1:num_rst_org)               &
     &             = plane_fst_IO%num_comp_IO(1:num_rst_org)
      mgd_mesh_pl%merged_fld%istack_component(0:num_rst_org)            &
     &             = plane_fst_IO%istack_comp_IO(0:num_rst_org)
!
      call add_initial_comp_mhd(mgd_mesh_pl%merged_fld)
      mgd_mesh_pl%merged_fld%ntot_phys =  ntot_rst_org
!
!    construct new data
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
!
        call dealloc_node_geometry_base(mesh_IO_p%node)
        call deallocate_type_neib_id(mesh_IO_p%nod_comm)
!
!     allocate added restart data
!
        mgd_mesh_pl%merged_fld%num_phys =  num_rst_new
        mgd_mesh_pl%merged_fld%ntot_phys = ntot_rst_new
        call alloc_phys_data_type                                       &
     &     (mgd_mesh_pl%merged%node%numnod, mgd_mesh_pl%merged_fld)
!
!     read original restart data
!
        plane_fst_IO%nnod_IO = mgd_mesh_pl%merged%node%numnod
!
        plane_fst_IO%num_field_IO =  num_rst_org
        plane_fst_IO%ntot_comp_IO = ntot_rst_org
!
        call set_file_fmt_prefix                                        &
     &     (izero, org_rst_f_header, plane_fld_file)
        call sel_read_step_FEM_field_file                               &
     &     (mgd_mesh_pl%num_pe, id_rank, istep,                         &
     &      plane_fld_file, plane_t_IO, plane_fst_IO)
!
        do np = 1, ntot_rst_org
          do inod = 1, mgd_mesh_pl%merged%node%numnod
            mgd_mesh_pl%merged_fld%d_fld(inod,np)                      &
     &           = plane_fst_IO%d_IO(inod,np)
          end do
        end do
!
        call dealloc_phys_name_IO(plane_fst_IO)
        call dealloc_phys_data_IO(plane_fst_IO)
!
!     construct added data
!
        do np = num_rst_org+1, num_rst_new
          jst = mgd_mesh_pl%merged_fld%istack_component(np-1)
          if (mgd_mesh_pl%merged_fld%phys_name(np) .eq. fhd_vecp) then
            do inod = 1, mgd_mesh_pl%merged%node%numnod
              mgd_mesh_pl%merged_fld%d_fld(inod,jst+1)                  &
     &                = 0.01d0*sin( pi*node_plane%xx(inod,3)            &
     &                 / (zmax-zmin))
            end do
          else if(mgd_mesh_pl%merged_fld%phys_name(np) .eq. fhd_magne)  &
     &        then
            do inod = 1, mgd_mesh_pl%merged%node%numnod
              mgd_mesh_pl%merged_fld%d_fld(inod,jst+2)                  &
     &              = (0.01d0*pi/two) * cos( pi*node_plane%xx(inod,3)   &
     &               / (zmax-zmin))
            end do
          end if
        end do
!
!     write data
!
        plane_fst_IO%num_field_IO =  num_rst_new
        plane_fst_IO%ntot_comp_IO = ntot_rst_new
        call alloc_phys_name_IO(plane_fst_IO)
        call alloc_phys_data_IO(plane_fst_IO)
!
        call simple_copy_fld_name_to_rst                                &
     &     (mgd_mesh_pl%merged_fld, plane_fst_IO)
        call simple_copy_fld_data_to_rst                                &
     &     (mgd_mesh_pl%merged%node, mgd_mesh_pl%merged_fld,            &
     &      plane_fst_IO)
!
        plane_fld_file%file_prefix = new_rst_file_header
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
      end program add_magne_ini_4_plane

