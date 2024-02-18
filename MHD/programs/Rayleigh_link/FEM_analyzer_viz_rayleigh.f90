!>@file   FEM_analyzer_viz_rayleigh.f90
!!@brief  module FEM_analyzer_viz_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2019
!
!>@brief  Main loop of visualization of Rayleigh data
!!
!!@verbatim
!!      subroutine FEM_initialize_viz_rayleigh(FEM_Rayleigh, m_SR)
!!        type(FEM_mesh_field_rayleigh_viz), intent(inout)              &
!!     &                                      :: FEM_Rayleigh
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_analyze_viz_rayleigh(visval, i_step, time_d,     &
!!     &                                    FEM_Rayleigh, m_SRr)
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_rayleigh_viz), intent(inout)              &
!!     &                                      :: FEM_Rayleigh
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module FEM_analyzer_viz_rayleigh
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_mesh_data
      use t_file_IO_parameter
!
      use t_spheric_parameter
      use t_const_spherical_grid
      use t_comm_table_4_assemble
      use t_mesh_SR
      use t_viz_4_rayleigh
!
      implicit none
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_R
      type(mesh_data), save :: rayleigh_fem
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!
!>       Structure of parallel Rayleigh mesh
      type(mesh_geometry), allocatable, save :: rayleigh_pmesh(:)
!>       Structure of parallel Rayleigh field
      type(field_IO), allocatable, save :: rayleigh_fIO(:)
!>       Structure of data asssemble table
      type(comm_table_4_assemble), save :: asbl_comm_R
!
      private :: gen_sph_R, rayleigh_fem, sph_const
      private :: rayleigh_pmesh, rayleigh_fIO, asbl_comm_R
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_viz_rayleigh(FEM_Rayleigh, m_SR)
!
      use const_fem_nodes_4_rayleigh
      use const_FEM_mesh_sph_mhd
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use copy_mesh_structures
      use mpi_load_mesh_data
      use nod_phys_send_recv
!
      type(FEM_mesh_field_rayleigh_viz), intent(inout)                  &
     &                                      :: FEM_Rayleigh
      type(mesh_SR), intent(inout) :: m_SR
!
      character(len=kchara), parameter :: file_name = 'grid_info'
!
!   --------------------------------
!       setup Rayleigh information
!   --------------------------------
      call read_rayleigh_grid_info                                      &
     &   (file_name, FEM_Rayleigh%rayleigh_rtp)
      call bcast_rayleigh_grid_info(FEM_Rayleigh%rayleigh_rtp)
      call set_rayleigh_parallel_param(FEM_Rayleigh%rayleigh_rtp)
!
      call fem_nodes_4_rayleigh_file(FEM_Rayleigh%rayleigh_rtp,         &
     &    rayleigh_fem%mesh, rayleigh_fem%group)
!
      call shell_params_from_rayleigh                                   &
     &   (FEM_Rayleigh%rayleigh_rtp, sph_const, gen_sph_R)
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'base_FEM_mesh_sph_mhd'
      call base_FEM_mesh_sph_mhd                                        &
     &   (sph_const%sph_params, sph_const%sph_rtp, sph_const%sph_rj,    &
     &    FEM_Rayleigh%geofem%mesh, FEM_Rayleigh%geofem%group,          &
     &    gen_sph_R)
!
      call dealloc_gen_sph_radial_groups(gen_sph_R)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_comm_initialization'
      call FEM_comm_initialization(FEM_Rayleigh%geofem%mesh, m_SR)
      call const_global_numele_list(FEM_Rayleigh%geofem%mesh%ele)
!
!   --------------------------------
!       Construct data shuffle table
!   --------------------------------
!
      allocate(rayleigh_pmesh(nprocs))
      allocate(rayleigh_fIO(nprocs))
!
      call copy_node_geometry                                           &
     &   (rayleigh_fem%mesh%node, rayleigh_pmesh(my_rank+1)%node)
      call const_global_numnod_list(rayleigh_pmesh(my_rank+1)%node)
      call dealloc_node_geometry_base(rayleigh_fem%mesh%node)
      call dealloc_groups_data(rayleigh_fem%group)
!
      call s_search_original_domain_node                                &
     &   (nprocs, FEM_Rayleigh%geofem%mesh%node,                        &
     &    rayleigh_pmesh, asbl_comm_R)
!
!   --------------------------------
!       setup field information
!   --------------------------------
!
      call init_fields_by_rayleigh(FEM_Rayleigh%iphys_ftb,              &
     &    FEM_Rayleigh%geofem%mesh, FEM_Rayleigh%field)
!
      end subroutine FEM_initialize_viz_rayleigh
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_viz_rayleigh(visval, i_step, time_d,       &
     &                                    FEM_Rayleigh, m_SR)
!
      use t_ucd_data
      use assemble_nodal_fields
      use nod_phys_send_recv
!
      use coordinate_convert_4_sph
      use share_field_data
!
      logical, intent(in) :: visval
      integer(kind = kint), intent(in) :: i_step
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_rayleigh_viz), intent(inout)                  &
     &                                      :: FEM_Rayleigh
      type(mesh_SR), intent(inout) :: m_SR
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: nd
!
      time_d%i_time_step = i_step
      time_d%time = 0.0d0
      time_d%dt = 0.0d0
      if(visval .eqv. .FALSE.) return

!
      call init_fields_IO_by_rayleigh(FEM_Rayleigh%iphys_ftb,           &
     &    rayleigh_pmesh(my_rank+1), rayleigh_fIO(my_rank+1))
!
      call alloc_rayleigh_component                                     &
     &   (rayleigh_pmesh(my_rank+1)%node%numnod,                        &
     &    rayleigh_pmesh(my_rank+1)%node%istack_numnod(my_rank),        &
     &    FEM_Rayleigh%rayleigh_rtp)
      do nd = 1, FEM_Rayleigh%iphys_ftb%ntot_comp
        write(file_name,'(a,a1,i8.8,a1,i4.4)')                          &
     &             trim(FEM_Rayleigh%iphys_ftb%field_dir), '/',         &
     &             i_step, '_', FEM_Rayleigh%iphys_ftb%id_Rayleigh(nd)
        call read_each_rayleigh_component(file_name, nd,                &
     &      rayleigh_pmesh(my_rank+1)%node%numnod,                      &
     &      rayleigh_fIO(my_rank+1)%ntot_comp_IO,                       &
     &      rayleigh_fIO(my_rank+1)%d_IO, FEM_Rayleigh%rayleigh_rtp)
      end do
      call dealloc_rayleigh_component(FEM_Rayleigh%rayleigh_rtp)
!
      call assemble_field_data                                          &
     &   (nprocs, asbl_comm_R, FEM_Rayleigh%field, rayleigh_fIO)
!
      call overwrite_nodal_sph_2_xyz(FEM_Rayleigh%geofem%mesh%node,     &
     &                               FEM_Rayleigh%field)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(FEM_Rayleigh%geofem%mesh,               &
     &    FEM_Rayleigh%field, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      end subroutine FEM_analyze_viz_rayleigh
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_rayleigh
