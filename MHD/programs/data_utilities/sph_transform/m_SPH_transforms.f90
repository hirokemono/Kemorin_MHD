!>@file   m_SPH_transforms.f90
!!@brief  module m_SPH_transforms
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
      module m_SPH_transforms
!
      use m_precision
      use m_machine_parameter
!
      use t_ctl_params_sph_trans
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_jacobians
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_global_gauss_coefs
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_phys_name_4_sph_trans
      use t_check_and_make_SPH_mesh
      use t_control_param_vol_grping
      use t_visualizer
!
      implicit none
!
!       Structure for time stepping parameters
      type(time_step_param), save :: t_STR
!
!>      Structure for field data IO paramters
      type(SPH_TRNS_file_IO_params), save ::  files_STR
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type(mesh_data), save :: femmesh_STR
!>      Structure to check and construct spherical shell mesh
      type(sph_grid_maker_in_sim), save :: sph_maker_TRNS
!>        Structure for repartitioning parameters
      type(volume_partioning_param), save :: repart_TRNS
!
!>       Structure for nodal field data
      type(phys_data), save :: field_STR
!
!
!>        Instance for FEM field data IO
      type(time_data), save :: time_IO_TRNS
!>      Increment for visualizations
      type(VIZ_step_params), save :: viz_step_STR
!
!>        Instance for numbers of FEM field for merged IO
      type(ucd_data), save :: ucd_SPH_TRNS
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod_SPH_TRANS
!
!>      Stracture for Jacobians
      type(jacobians_type), save :: jacobians_STR
!
!>        Structures of parameters for spherical transform
      type(parameters_4_sph_trans), save :: trns_param
!
!>        Structures of Gauss points
      type(global_gauss_points), save :: d_gauss_trans
!
!>      Work structures for various Legendre trasform
      type(legendre_trns_works), save :: WK_leg_TRNS
!>      Structure for work area of FFTs
      type(work_for_FFTs), save :: WK_FFTs_TRNS
!
      type(field_name_4_sph_trans), save :: fld_rtp_TRNS
!
!>      Structure for visualization
      type(visualize_modules), save :: vizs_TRNS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine mesh_setup_4_SPH_TRANS
!
      use calypso_mpi
      use m_array_for_send_recv
      use load_mesh_data
      use nod_phys_send_recv
      use set_parallel_file_name
      use parallel_FEM_mesh_init
      use set_ucd_data_to_type
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(femmesh_STR%mesh, vect1)
      call FEM_mesh_initialization                                      &
     &   (femmesh_STR%mesh, femmesh_STR%group)
!
      if (iflag_debug.gt.0) write(*,*) 'alloc_phys_data_type'
      call alloc_phys_data_type                                         &
     &   (femmesh_STR%mesh%node%numnod, field_STR)
!
      end subroutine mesh_setup_4_SPH_TRANS
!
! ----------------------------------------------------------------------
!
      end module m_SPH_transforms
