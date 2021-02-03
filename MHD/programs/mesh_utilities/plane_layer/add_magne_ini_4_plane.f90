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
      use m_constants
!
      use m_base_field_labels
      use m_field_file_format
!
      use t_size_of_cube
      use t_field_data_IO
      use t_mesh_data
      use t_geometry_data
      use t_time_data
      use t_field_data_IO
      use t_file_IO_parameter
      use t_mesh_data_4_merge
      use t_ctl_data_4_cub_kemo
      use t_ctl_data_plane_fft
      use t_control_param_plane_mesh
      use t_setting_4_ini
!
      use set_parallel_file_name
      use mesh_IO_select
      use set_field_file_names
      use field_file_IO
      use field_IO_select
      use set_field_to_restart
      use copy_mesh_structures
!!
      implicit none
!
      real(kind = kreal) :: pi
      character(len=kchara), parameter                                  &
     &      :: org_rst_f_header = 'restart/rst'
      character(len=kchara), parameter                                  &
     &      :: new_rst_file_header = 'rst_new/rst'
!
      integer :: id_rank
      integer(kind=kint) :: istep, ip, ierr, np, inod
      integer(kind=kint) :: jst
!
      type(ctl_data_4_cub_kemo), save :: cubmesh_c1
      type(ctl_param_plane_mesh), save :: cube_p1
      type(size_of_cube), save :: c_size1
      type(node_data), save :: node_plane
      type(time_data), save :: plane_t_IO
      type(field_IO), save :: plane_fst_IO
      type(mesh_geometry) :: mesh_IO_p
      type(field_IO_params), save :: cube_mesh_file
      type(field_IO_params), save :: plane_fld_file
      type(merged_mesh), save :: mgd_mesh_pl
      type(plane_initial_setting), save :: pini_p1
!
      character(len=kchara) :: file_name
!
!
      pi = four*atan(one)
      write(*,*) 'pi', pi
!
!     read outline of mesh
!
      call read_control_data_plane_mesh(cubmesh_c1)
      call s_set_ctl_data_plane_mesh(cubmesh_c1, cube_p1, c_size1)
!
       mgd_mesh_pl%num_pe                                               &
     &      = int(c_size1%ndx * c_size1%ndy * c_size1%ndz)
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
     &   (mgd_mesh_pl%num_pe, 0, istep,                                 &
     &    plane_fld_file, plane_t_IO, plane_fst_IO)
!
      pini_p1%num_rst_org = plane_fst_IO%num_field_IO
!
      call add_initial_num_comp_mhd(mgd_mesh_pl%merged_fld, pini_p1)
!
      mgd_mesh_pl%merged_fld%num_phys = pini_p1%num_rst_new
      call alloc_phys_name(mgd_mesh_pl%merged_fld)
!
      pini_p1%ntot_rst_org =  plane_fst_IO%ntot_comp_IO
      mgd_mesh_pl%merged_fld%phys_name(1:pini_p1%num_rst_org)           &
     &             =    plane_fst_IO%fld_name(1:pini_p1%num_rst_org)
      mgd_mesh_pl%merged_fld%num_component(1:pini_p1%num_rst_org)       &
     &             = plane_fst_IO%num_comp_IO(1:pini_p1%num_rst_org)
      mgd_mesh_pl%merged_fld%istack_component(0:pini_p1%num_rst_org)    &
     &             = plane_fst_IO%istack_comp_IO(0:pini_p1%num_rst_org)
!
      call add_initial_comp_mhd(mgd_mesh_pl%merged_fld, pini_p1)
      mgd_mesh_pl%merged_fld%ntot_phys =  pini_p1%ntot_rst_org
!
!    construct new data
!
      mgd_mesh_pl%merged%node%numnod = node_plane%numnod
!      
      call alloc_merged_field_stack(mgd_mesh_pl%num_pe, plane_fst_IO)
      plane_fst_IO%istack_numnod_IO(0) = 0
      do ip = 1, mgd_mesh_pl%num_pe
        plane_fst_IO%istack_numnod_IO(ip)                               &
     &      = plane_fst_IO%istack_numnod_IO(ip-1)                       &
     &       + mgd_mesh_pl%merged%node%numnod
      end do
!
      do ip = 1, mgd_mesh_pl%num_pe
        id_rank = int(ip-1)
!
!    read mesh file
!
        call copy_mesh_format_and_prefix(cube_p1%mesh_file%file_prefix, &
     &      id_ascii_file_fmt, cube_mesh_file)
        call sel_read_geometry_size                                     &
     &     (cube_mesh_file, id_rank, mesh_IO_p, ierr)
        if(ierr .gt. 0) stop 'Mesh is wrong!!'
!
        call copy_node_geometry_types(mesh_IO_p%node, node_plane)
!
        call dealloc_node_geometry_IO(mesh_IO_p)
!
!     allocate added restart data
!
        mgd_mesh_pl%merged_fld%num_phys =  pini_p1%num_rst_new
        mgd_mesh_pl%merged_fld%ntot_phys = pini_p1%ntot_rst_new
        call alloc_phys_data(mgd_mesh_pl%merged%node%numnod,            &
     &                       mgd_mesh_pl%merged_fld)
!
!     read original restart data
!
        plane_fst_IO%nnod_IO = mgd_mesh_pl%merged%node%numnod
!
        plane_fst_IO%num_field_IO = pini_p1%num_rst_org
        plane_fst_IO%ntot_comp_IO = pini_p1%ntot_rst_org
!
        call set_file_fmt_prefix                                        &
     &     (izero, org_rst_f_header, plane_fld_file)
        call sel_read_step_FEM_field_file                               &
     &     (mgd_mesh_pl%num_pe, id_rank, istep,                         &
     &      plane_fld_file, plane_t_IO, plane_fst_IO)
!
        do np = 1, pini_p1%ntot_rst_org
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
        do np = pini_p1%num_rst_org+1, pini_p1%num_rst_new
          jst = mgd_mesh_pl%merged_fld%istack_component(np-1)
          if(mgd_mesh_pl%merged_fld%phys_name(np)                       &
     &                                 .eq. vector_potential%name) then
            do inod = 1, mgd_mesh_pl%merged%node%numnod
              mgd_mesh_pl%merged_fld%d_fld(inod,jst+1)                  &
     &                = 0.01d0*sin( pi*node_plane%xx(inod,3)            &
     &                 / (c_size1%zmax - c_size1%zmin))
            end do
          else if(mgd_mesh_pl%merged_fld%phys_name(np)                  &
     &            .eq. magnetic_field%name) then
            do inod = 1, mgd_mesh_pl%merged%node%numnod
              mgd_mesh_pl%merged_fld%d_fld(inod,jst+2)                  &
     &              = (0.01d0*pi/two) * cos( pi*node_plane%xx(inod,3)   &
     &               / (c_size1%zmax - c_size1%zmin))
            end do
          end if
        end do
!
!     write data
        call simple_init_fld_name_to_rst                                &
     &     (mgd_mesh_pl%merged%node%numnod, mgd_mesh_pl%merged_fld,     &
     &      plane_fst_IO)
        call simple_copy_fld_data_to_rst                                &
     &     (mgd_mesh_pl%merged%node, mgd_mesh_pl%merged_fld,            &
     &      plane_fst_IO)
!
        file_name = set_FEM_fld_file_name(new_rst_file_header,          &
     &             iflag_ascii, id_rank, izero)
        call write_step_field_file                                      &
     &     (file_name, id_rank, plane_t_IO, plane_fst_IO)
!
        call dealloc_phys_name_IO(plane_fst_IO)
        call dealloc_phys_data_IO(plane_fst_IO)
!
        call dealloc_phys_data(mgd_mesh_pl%merged_fld)
        call dealloc_node_geometry_base(node_plane)
      end do
!
      stop
!
      end program add_magne_ini_4_plane

