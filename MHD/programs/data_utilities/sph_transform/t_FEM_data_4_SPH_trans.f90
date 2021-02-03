!>@file   t_FEM_data_4_SPH_trans.f90
!!@brief  module t_FEM_data_4_SPH_trans
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for spherical transform utilities
!!
!!@verbatim
!!      subroutine mesh_setup_4_SPH_TRANS
!!@endverbatim
!
      module t_FEM_data_4_SPH_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_SGS_model_addresses
      use t_next_node_ele_4_node
      use t_jacobians
      use t_phys_name_4_sph_trans
      use t_control_param_vol_grping
      use t_vector_for_solver
      use t_visualizer
      use t_field_data_IO
      use t_file_IO_parameter
!
      implicit none
!
!>      Structure of FEM mesh and field structures
      type FEM_for_SPH_transforms
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure for field data IO paramters
        type(field_IO_params) :: ucd_file_IO
!>        Structure for original field file  paramters
        type(field_IO_params) :: org_ucd_file_IO
!>        Structure for zonal mean field file  paramters
        type(field_IO_params) :: zonal_ucd_param
!>        Structure for input UCD for zonal mean
        type(field_IO_params) :: zm_ucd_input_file
!
!>        Structure for mesh data
!>        (position, connectivity, group, and communication)
        type(mesh_data) :: geofem
!>        Structure for nodal field data
        type(phys_data) :: field
!
!>        Instance for FEM field data IO
        type(time_data) :: time_IO
!>        Instance for numbers of FEM field for merged IO
        type(ucd_data) :: ucd
!
!>        Structure for vectors for solver
        type(vectors_4_solver) :: v_sol
!
!>        Structure of included element list for each node
        type(element_around_node) :: ele_4_nod
!>        Stracture for Jacobians
        type(jacobians_type) :: jacobians
!
!>        Increment for visualizations
        type(VIZ_step_params) :: viz_step
!>        Structure for visualization
        type(visualize_modules) :: vizs
!
!>        Structure for repartitioning parameters
        type(volume_partioning_param) :: repart
      end type FEM_for_SPH_transforms
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine mesh_setup_4_SPH_TRANS(FEM_STR)
!
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_ucd_data_to_type
!
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization                                      &
     &   (FEM_STR%geofem%mesh, FEM_STR%v_sol)
      call FEM_mesh_initialization                                      &
     &   (FEM_STR%geofem%mesh, FEM_STR%geofem%group)
!
      if (iflag_debug.gt.0) write(*,*) 'alloc_phys_data'
      call alloc_phys_data(FEM_STR%geofem%mesh%node%numnod,             &
     &                     FEM_STR%field)
!
      end subroutine mesh_setup_4_SPH_TRANS
!
! ----------------------------------------------------------------------
!
      end module t_FEM_data_4_SPH_trans
