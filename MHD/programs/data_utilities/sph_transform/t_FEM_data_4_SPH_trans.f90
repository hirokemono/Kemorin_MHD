!>@file   t_FEM_data_4_SPH_trans.f90
!!@brief  module t_FEM_data_4_SPH_trans
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for spherical transform utilities
!!
!!@verbatim
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
      use t_four_visualizers
      use t_field_data_IO
      use t_file_IO_parameter
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
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
!>        Structure of included element list for each node
        type(element_around_node) :: ele_4_nod
!>        Stracture for Jacobians
        type(jacobians_type) :: jacobians
!
!>        Increment for visualizations
        type(VIZ_step_params) :: viz_step
!>        Structure for visualization
        type(four_visualize_modules) :: four_vizs
      end type FEM_for_SPH_transforms
!
      end module t_FEM_data_4_SPH_trans
