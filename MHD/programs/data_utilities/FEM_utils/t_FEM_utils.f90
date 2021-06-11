!>@file   t_FEM_utils.f90
!!@brief  module t_FEM_utils
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
      module t_FEM_utils
!
      use m_precision
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_ucd_data
      use t_file_IO_parameter
      use t_shape_functions
      use t_jacobians
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use t_vector_for_solver
      use m_solver_SR
      use calypso_mpi
!
      implicit none
!
!
!       Structure for time stepping parameters
      type FEM_utils
!>        Structure for mesh file IO paramters
        type(field_IO_params) :: mesh_file
!>        Structure for field data IO paramters
        type(field_IO_params) :: udt_file
!
!>        Structure for mesh data
!>        (position, connectivity, group, and communication)
        type(mesh_data) :: geofem
!>        Structure for nodal field data
        type(phys_data) :: nod_fld
!>        address of nodal fields
        type(phys_address) :: iphys
!>       address of nodal fields for SGS model
        type(SGS_model_addresses) :: iphys_LES
!
!>       Work area for solver communication
        type(vectors_4_solver) :: v_sol
!
        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type) :: jacobians
      end type FEM_utils
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_FEM_UTIL(mesh_file, geofem, v_sol)
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use nod_and_ele_derived_info
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_data), intent(inout) :: geofem
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_file, nprocs, geofem)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_iccgN_vector'
      call alloc_iccgN_vector                                           &
     &   (isix, geofem%mesh%node%numnod, v_sol)
      call init_nod_send_recv(geofem%mesh,                              &
     &                        SR_sig1, SR_r1, SR_i1, SR_il1)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos                                        &
     &   (geofem%mesh%node, geofem%mesh%ele)
!
      end subroutine mesh_setup_4_FEM_UTIL
!
!   ---------------------------------------------------------------------
!
      end module t_FEM_utils
