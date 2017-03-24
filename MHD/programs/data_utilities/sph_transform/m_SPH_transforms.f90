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
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_global_gauss_coefs
      use t_file_IO_parameter
      use t_sph_transforms
      use t_phys_name_4_sph_trans
!
      implicit none
!
!       Structure for time stepping parameters
      type(time_step_param), save :: t_STR
!
!>      Structure for field data IO paramters
      type(field_IO_params), save ::  mesh_file_STR
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type(mesh_data), save :: femmesh_STR
!
!>     Structure for element, surface, and edge mesh
!!        (position, connectivity, and communication)
      type(element_geometry), save :: elemesh_STR
!
!
!>       Structure for nodal field data
      type(phys_data), save :: field_STR
!
!
!>        Instance for FEM field data IO
      type(time_data), save :: time_IO_TRNS
      type(ucd_data), save :: ucd_SPH_TRNS
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: m_ucd_SPH_TRNS
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod_SPH_TRANS
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac_STR_l
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac_STR_q
!
!
!>        Structures of parameters for spherical transform
      type(parameters_4_sph_trans), save :: trns_param
!
!>        Structures of Gauss points
      type(global_gauss_points), save :: d_gauss_trans
!
!>      Work structures for various spherical harmonics trasform
      type(spherical_trns_works), save :: WK_sph_TRNS
      type(field_name_4_sph_trans), save :: fld_rtp_TRNS
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
      use const_mesh_information
      use set_parallel_file_name
      use const_element_comm_tables
      use set_ucd_data_to_type
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (isix, femmesh_STR%mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(femmesh_STR%mesh%nod_comm)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos                                             &
     &   (my_rank, femmesh_STR%mesh, femmesh_STR%group, elemesh_STR)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(femmesh_STR%mesh, elemesh_STR)
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
