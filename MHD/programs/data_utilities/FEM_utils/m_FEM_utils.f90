!>@file   m_FEM_utils.f90
!!@brief  module m_FEM_utils
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for FEM utilities
!!
!!@verbatim
!!      subroutine mesh_setup_4_FEM_UTIL(mesh_file)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!@endverbatim
!
      module m_FEM_utils
!
      use m_precision
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_ucd_data
      use t_file_IO_parameter
      use t_shape_functions
      use t_jacobians
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use calypso_mpi
!
      implicit none
!
!       Structure for time stepping parameters
      type(time_step_param), save :: time_U
!
!>      Structure for mesh file IO paramters
      type(field_IO_params), save :: mesh_file_FUTIL
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type(mesh_data), save :: femmesh_FUTIL
!
!>     Structure for element, surface, and edge mesh
!!        (position, connectivity, and communication)
      type(element_geometry), save :: elemesh_FUTIL
!
!
!>       Structure for nodal field data
      type(phys_data), save :: field_FUTIL
!>       address for nodal fields
      type(phys_address), save :: iphys_FUTIL
!
!
!>      Structure for field data IO paramters
      type(field_IO_params), save :: udt_param_FUTIL
      type(time_data), save :: time_IO_FUTIL
!>        Instance for FEM field data IO
      type(ucd_data), save :: ucd_FUTIL
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: m_ucd_FUTIL
!
      type(shape_finctions_at_points), save :: spfs_FUTIL
!>      Stracture for Jacobians
      type(jacobians_type), save :: jacobians_FUTIL
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_FEM_UTIL(mesh_file)
!
      use m_array_for_send_recv
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use const_mesh_information
!
      type(field_IO_params), intent(in) ::  mesh_file
!
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh                                               &
     &   (mesh_file, nprocs, femmesh_FUTIL%mesh, femmesh_FUTIL%group,   &
     &    elemesh_FUTIL%surf%nnod_4_surf,                               &
     &    elemesh_FUTIL%edge%nnod_4_edge)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (isix, femmesh_FUTIL%mesh%node%numnod)
      call init_nod_send_recv(femmesh_FUTIL%mesh)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos                                        &
     &   (femmesh_FUTIL%mesh%node, femmesh_FUTIL%mesh%ele)
!
      end subroutine mesh_setup_4_FEM_UTIL
!
!   ---------------------------------------------------------------------
!
      end module m_FEM_utils
