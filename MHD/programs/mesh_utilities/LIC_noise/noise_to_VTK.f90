!
      program noise_to_VTK
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_3d_noise
      use t_control_data_LIC_noise
!
      use t_control_param_plane_mesh
      use t_size_of_cube
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_file_IO_parameter
      use m_array_for_send_recv
      use m_file_format_switch
      use m_spheric_constants
!
      use cubmesh_311
      use load_mesh_data
      use set_mesh_extensions
      use set_ucd_extensions
      use set_ucd_data_to_type
      use vtk_file_IO
      use nod_phys_send_recv
!
      integer(kind = kint), parameter :: id_control = 11
      character(len = kchara) :: ctl_file_name = 'ctl_noise'
      character(len = kchara) :: hd_cube_noise = 'cube_noise_ctl'
!
      type(noise_cube) :: noise_t1
      type(cube_noise_ctl) :: noise_c1
!
      integer(kind=kint), parameter  ::   elm_type = 331
      type(ctl_param_plane_mesh), save :: cube_p1
      type(size_of_cube), save :: c_size1
!
      character(len=kchara) :: vtk_file_name
      type(mesh_geometry) :: mesh
      type(mesh_groups) :: group
      type(phys_data) :: nod_fld
      type(ucd_data) :: ucd
!
      integer(kind = kint) :: inod
!
!
      iflag_debug = 0
!
      call  read_cube_noise_control_file(id_control, ctl_file_name,     &
     &    hd_cube_noise, noise_c1)
      call set_control_3d_cube_noise(noise_c1, noise_t1)
      call sel_const_3d_cube_noise(noise_t1)
!
!
      vtk_file_name = add_vtk_extension(noise_t1%noise_file_name)
      cube_p1%mesh_file%file_prefix = noise_t1%noise_file_name
!      cube_p1%mesh_file%iflag_format = id_ascii_file_fmt
      cube_p1%mesh_file%iflag_format = id_binary_file_fmt
      cube_p1%iflag_ztype = igrid_equidistance
!
      call calypso_MPI_init
      if(nprocs .gt. 1) call calypso_mpi_abort                          &
     &                     (0, 'Run with single process')
!
!
      c_size1%xsize = noise_t1%size_cube(1)
      c_size1%ysize = noise_t1%size_cube(2)
      c_size1%zsize = noise_t1%size_cube(3)
      call set_plane_size(c_size1)
!
      c_size1%nx_all = noise_t1%nidx_xyz(1)
      c_size1%ny_all = noise_t1%nidx_xyz(2)
      c_size1%nz_all = noise_t1%nidx_xyz(3)
!
      c_size1%ndx = 1
      c_size1%ndy = 1
      c_size1%ndz = 1
      c_size1%ndepth = 1
!
      call const_single_cubmesh311                                      &
     &   (elm_type, cube_p1, c_size1, mesh, group)
!
        nod_fld%num_phys =     2
        nod_fld%num_phys_viz = nod_fld%num_phys
        call alloc_phys_name_type(nod_fld)
!
        nod_fld%phys_name(1) = 'noise'
        nod_fld%phys_name(2) = 'grad_noise'
!
        nod_fld%num_component(1) = 1
        nod_fld%num_component(2) = 3
!
        nod_fld%istack_component(0) = 1
        nod_fld%istack_component(1) = nod_fld%num_component(1)
        nod_fld%istack_component(2) = nod_fld%istack_component(1)       &
       &                             + nod_fld%num_component(2)
!
        nod_fld%ntot_phys =      nod_fld%istack_component(2)
        nod_fld%ntot_phys_viz = nod_fld%ntot_phys
        call alloc_phys_data_type(mesh%node%numnod, nod_fld)
!
!$omp parallel do
        do inod = 1, mesh%node%internal_node
          nod_fld%d_fld(inod,1) = noise_t1%rnoise(inod)
          nod_fld%d_fld(inod,2) = noise_t1%rnoise_grad(inod,1)
          nod_fld%d_fld(inod,3) = noise_t1%rnoise_grad(inod,2)
          nod_fld%d_fld(inod,4) = noise_t1%rnoise_grad(inod,3)
        end do
!$omp end parallel do
!
        call dealloc_3d_cube_noise(noise_t1)
        write(*,*) 'copy data end'
!
      call alloc_iccgN_vec_type(isix, mesh%node%numnod, vect1)
      call init_nod_send_recv(mesh)
      call fields_send_recv(mesh%nod_comm, nod_fld, vect1)
!
      call link_local_mesh_2_ucd(mesh%node, mesh%ele, ucd)
      call link_field_data_to_ucd(nod_fld, ucd)
!
      call write_vtk_file(id_rank, vtk_file_name, ucd)
!
      call calypso_mpi_barrier
      call calypso_MPI_finalize
!
      end program noise_to_VTK
