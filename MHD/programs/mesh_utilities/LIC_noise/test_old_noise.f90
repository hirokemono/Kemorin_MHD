!
      program check_noise
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use t_lic_noise_generator
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
      type(lic_old_noise) :: noise_p1
!
      integer(kind=kint), parameter  ::   elm_type = 331
      type(ctl_param_plane_mesh), save :: cube_p1
      type(size_of_cube), save :: c_size1
!
      character(len=kchara) :: file_name
      type(mesh_geometry) :: mesh
      type(mesh_groups) :: group
      type(phys_data) :: nod_fld
      type(ucd_data) :: ucd
!
      integer(kind = kint) :: inod, ierr
!
      iflag_debug = 0
      noise_p1%noise_file_name = 'noise_128_10'
!
      noise_p1%reflection_file_name                                     &
     &       = add_grd_extension(noise_p1%noise_file_name)
      cube_p1%mesh_file%file_prefix = noise_p1%noise_file_name
!      cube_p1%mesh_file%iflag_format = id_ascii_file_fmt
      cube_p1%mesh_file%iflag_format = id_binary_file_fmt
      file_name = add_vtk_extension(noise_p1%noise_file_name)
!
      call calypso_MPI_init
      if(nprocs .gt. 1) call calypso_mpi_abort                          &
     &                     (0, 'Run with single process')
!
      call load_noise_data(noise_p1)
!
      cube_p1%iflag_ztype = igrid_equidistance
!
      c_size1%xsize = one
      c_size1%ysize = one
      c_size1%zsize = one
      call set_plane_size(c_size1)
!
      c_size1%nx_all = noise_p1%noise_dim(1)
      c_size1%ny_all = noise_p1%noise_dim(2)
      c_size1%nz_all = noise_p1%noise_dim(3)
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
        do inod = 1, mesh%node%internal_node
          nod_fld%d_fld(inod,1)                                         &
     &        = dble(ichar(noise_p1%noise_data(inod))) / 255.0
          nod_fld%d_fld(inod,2)                                         &
     &        = dble(ichar(noise_p1%noise_grad_data(3*inod-2))) / 255.0
          nod_fld%d_fld(inod,3)                                         &
     &        = dble(ichar(noise_p1%noise_grad_data(3*inod-1))) / 255.0
          nod_fld%d_fld(inod,4)                                         &
     &        = dble(ichar(noise_p1%noise_grad_data(3*inod  ))) / 255.0
        end do
        write(*,*) 'copy data end'
!
      call allocate_vector_for_solver(isix, mesh%node%numnod)
      call init_nod_send_recv(mesh)
      call fields_send_recv(mesh%nod_comm, nod_fld)
!
      call link_local_mesh_2_ucd(mesh%node, mesh%ele, ucd)
      call link_field_data_to_ucd(nod_fld, ucd)
!
      call write_vtk_file(id_rank, file_name, ucd)
!
      call calypso_mpi_barrier
      call calypso_MPI_finalize
!
      end program check_noise
!

