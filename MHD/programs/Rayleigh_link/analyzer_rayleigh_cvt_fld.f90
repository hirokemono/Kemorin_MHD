!>@file   analyzer_rayleigh_cvt_fld.f90
!!@brief  module analyzer_rayleigh_cvt_fld
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_rayleigh_cvt_fld
!!      subroutine analyze_rayleigh_cvt_fld
!!@endverbatim
!
      module analyzer_rayleigh_cvt_fld
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_comm_table_4_assemble
      use t_rayleigh_field_IO
      use t_vector_for_solver
!
      use field_IO_select
      use assemble_nodal_fields
      use set_control_assemble
!
      implicit none
!
      type(mesh_geometry), allocatable, save :: org_mesh(:)
      type(mesh_data), save :: geofem
      type(mesh_data), save :: rayleigh_fem
      type(phys_data), save :: new_fld
      type(control_data_4_merge), save :: mgd_ctl_u
      type(comm_table_4_assemble), save :: asbl_comm_u
!
      type(rayleigh_field), save :: rayleigh_rtp_A
!>      Structure for communicatiors for solver
      type(vectors_4_solver), save :: v_sol_A
!
      type(time_data), save :: t_IO_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_rayleigh_cvt_fld
!
      use m_error_IDs
!
      use t_ctl_data_const_sph_mesh
      use t_const_spherical_grid
      use t_ctl_params_gen_sph_shell
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use const_element_comm_tables
      use share_field_data
      use load_mesh_data_4_merge
      use bcast_4_assemble_sph_ctl
!
      use const_FEM_mesh_sph_mhd
      use const_fem_nodes_4_rayleigh
      use mpi_load_mesh_data
!
      use nod_and_ele_derived_info
      use copy_mesh_structures
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_G
!
!
      type(field_IO_params) ::  rayleigh_mesh_file
      type(field_IO_params) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      write(*,*) 'Assemble start: PE. ', my_rank
!
      rayleigh_mesh_file%file_prefix = 'Rayleigh_in'
      rayleigh_mesh_file%iflag_format = id_ascii_file_fmt
      file_name = 'Spherical_3D/00007000_grid'
      call read_rayleigh_field_param(file_name, rayleigh_rtp_A)
      call bcast_rayleigh_field_param(rayleigh_rtp_A)
      call set_rayleigh_parallel_param(rayleigh_rtp_A)
!
!
!      call s_const_fem_nodes_4_rayleigh                                &
!     &   (rayleigh_rtp_A, rayleigh_fem%mesh, rayleigh_fem%group)
      call fem_nodes_4_rayleigh_file                                    &
     &   (rayleigh_rtp_A, rayleigh_fem%mesh, rayleigh_fem%group)
      call shell_params_from_rayleigh                                   &
     &   (rayleigh_rtp_A, sph_const, gen_sph_G)
      call mpi_output_mesh(rayleigh_mesh_file,                          &
     &    rayleigh_fem%mesh, rayleigh_fem%group)
!
      allocate( org_mesh(nprocs) )
      call copy_node_geometry                                           &
     &   (rayleigh_fem%mesh%node, org_mesh(my_rank+1)%node)
      call const_global_numnod_list(org_mesh(my_rank+1)%node)
      call dealloc_node_geometry_base(rayleigh_fem%mesh%node)
      call dealloc_groups_data(rayleigh_fem%group)
!
!
      if (iflag_debug.gt.0) write(*,*) 'base_FEM_mesh_sph_mhd'
      call base_FEM_mesh_sph_mhd                                        &
     &   (sph_const%sph_params, sph_const%sph_rtp, sph_const%sph_rj,    &
     &    geofem%mesh, geofem%group, gen_sph_G)
      mesh_file%file_prefix = 'aho/tako'
      mesh_file%iflag_format = id_ascii_file_fmt
      call mpi_output_mesh                                              &
     &   (mesh_file, geofem%mesh, geofem%group)
!
!  Initialize communicator
!
      if (iflag_debug.gt.0 ) write(*,*) 'alloc_iccgN_vec_type'
      call alloc_iccgN_vec_type                                         &
     &   (n_sym_tensor, geofem%mesh%node%numnod, v_sol_A)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(geofem%mesh)
!
!  set new mesh data
!
      call set_nod_and_ele_infos(geofem%mesh%node, geofem%mesh%ele)
      call const_global_numnod_list(geofem%mesh%node)
      call const_global_numele_list(geofem%mesh%ele)
!
!
      call s_search_original_domain_node                                &
     &   (nprocs, geofem%mesh%node, org_mesh, asbl_comm_u)
!
!
!
      if(my_rank .eq. 0) then
        new_fld%num_phys = 1
        call alloc_phys_name(new_fld)
        new_fld%phys_name(1) = 'temperature'
        new_fld%num_component(1) = 1
        new_fld%istack_component(1) = 1
        new_fld%ntot_phys = 1
      end if
!
      call share_phys_field_names(new_fld)
      new_fld%num_phys_viz =  new_fld%num_phys
      new_fld%ntot_phys_viz = new_fld%ntot_phys
!
      call alloc_phys_data(geofem%mesh%node%numnod, new_fld)
!
      call calypso_MPI_barrier
!
      end subroutine init_rayleigh_cvt_fld
!
! ----------------------------------------------------------------------
!
      subroutine analyze_rayleigh_cvt_fld
!
      use t_ucd_data
!
      use set_ucd_data_to_type
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
      use nod_phys_send_recv
      use load_mesh_data_4_merge
      use set_field_file_names
      use const_element_comm_tables
      use share_field_data
!
      integer(kind = kint) :: istep
!
      type(field_IO), allocatable :: org_fIO(:)
!
      type(field_IO_params) :: new_fld_file
      integer(kind = kint) :: istep_start = 1
      integer(kind = kint) :: istep_end = 1
      integer(kind = kint) :: istep_increment = 1
!
      type(ucd_data), save :: ucd_m
!
      character(len=kchara) :: file_name
!
      new_fld_file%iflag_format = iflag_fld
      new_fld_file%file_prefix =  'field_12/tako'
!
      call link_local_mesh_2_ucd                                        &
     &   (geofem%mesh%node, geofem%mesh%ele, ucd_m)
      call link_field_data_to_ucd(new_fld, ucd_m)
!
      if(new_fld_file%iflag_format/icent .eq. iflag_single/icent) then
!        write(*,*) 'init_merged_ucd_element'
        call init_merged_ucd_element(new_fld_file%iflag_format,         &
     &      geofem%mesh%node, geofem%mesh%ele, geofem%mesh%nod_comm,    &
     &      ucd_m)
      end if
!
      call sel_write_parallel_ucd_mesh(new_fld_file, ucd_m)
!
      allocate(org_fIO(nprocs))
!
      do istep = istep_start, istep_end, istep_increment
        file_name = 'Spherical_3D/00007000_0501'
        call alloc_rayleigh_component                                   &
     &     (org_mesh(my_rank+1)%node%numnod,                            &
     &      org_mesh(my_rank+1)%node%istack_numnod(my_rank),            &
     &      rayleigh_rtp_A)
        call load_local_field_from_rayleigh                             &
     &     (rayleigh_rtp_A, t_IO_m, org_fIO(my_rank+1))
!
        call read_each_rayleigh_component(file_name, ione,              &
     &      org_fIO(my_rank+1)%nnod_IO,                                 &
     &      org_fIO(my_rank+1)%ntot_comp_IO, org_fIO(my_rank+1)%d_IO,   &
     &      rayleigh_rtp_A)
        call dealloc_rayleigh_component(rayleigh_rtp_A)
!
        call share_time_step_data(t_IO_m)
        call assemble_field_data                                        &
     &     (nprocs, asbl_comm_u, new_fld, org_fIO)
!
!        write(*,*) 'nod_fields_send_recv'
        call nod_fields_send_recv(geofem%mesh, new_fld, v_sol_A)
        call calypso_MPI_barrier
!
!        write(*,*) 'sel_write_parallel_ucd_file'
        call sel_write_parallel_ucd_file                                &
     &     (istep, new_fld_file, t_IO_m, ucd_m)
      end do
      call dealloc_comm_table_4_assemble(asbl_comm_u)
      call dealloc_numnod_stack(org_mesh(my_rank+1)%node)
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_rayleigh_cvt_fld
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine load_local_field_from_rayleigh                         &
     &         (rayleigh_rtp, t_IO, fld_IO)
!
      use t_time_data
!
      type(rayleigh_field), intent(in) :: rayleigh_rtp
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      t_IO%i_time_step = 7000
      t_IO%time = 0.7
      t_IO%dt = 0.0d0
!
      fld_IO%nnod_IO = int(rayleigh_rtp%nnod_rayleigh_in)
      fld_IO%num_field_IO = 1
      call alloc_phys_name_IO(fld_IO)
!
      fld_IO%fld_name = 'temperature'
      fld_IO%num_comp_IO(1) = 1
      call cal_istack_phys_comp_IO(fld_IO)
!
      call alloc_phys_data_IO(fld_IO)
!
      end subroutine load_local_field_from_rayleigh
!
! -----------------------------------------------------------------------
!
      end module analyzer_rayleigh_cvt_fld
