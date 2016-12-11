!>@file   m_visualization.f90
!!@brief  module m_visualization
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine mesh_setup_4_VIZ
!!      subroutine element_normals_4_VIZ
!!@endverbatim
!
      module m_visualization
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
      use t_file_IO_parameter
      use m_time_data_IO
!
      implicit none
!
!>      Structure for mesh file IO paramters
      type(field_IO_params), save :: mesh_file_VIZ
!>      Structure for field file IO paramters
      type(field_IO_params), save :: udt_org_param
!>      Structure for original restart file  paramters
      type(field_IO_params), save :: rst_org_param
!
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type(mesh_data), save :: femmesh_VIZ
!
!>     Structure for element, surface, and edge mesh
!!        (position, connectivity, and communication)
      type(element_geometry), save :: elemesh_VIZ
!
!
!>       Structure for nodal field data
      type(phys_data), save :: field_VIZ
!
!
!>        Instance for FEM field data IO
      type(time_params_IO), save :: VIZ_time_IO
      type(ucd_data), save :: ucd_VIZ
!>        Instance for numbers of FEM mesh for merged IO
!      type(merged_ucd_data), save :: m_ucd_SPH_TRNS
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod_VIZ
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac_VIZ_l
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac_VIZ_q
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine mesh_setup_4_VIZ
!
      use calypso_mpi
      use m_t_step_parameter
      use m_array_for_send_recv
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use const_mesh_information
      use set_parallel_file_name
      use const_element_comm_tables
      use set_ucd_data_to_type
      use ucd_IO_select
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       load mesh informations
      call mpi_input_mesh(mesh_file_VIZ,                                &
     &    femmesh_VIZ%mesh, femmesh_VIZ%group,                          &
     &    elemesh_VIZ%surf%nnod_4_surf, elemesh_VIZ%edge%nnod_4_edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos                                             &
     &   (my_rank, femmesh_VIZ%mesh, femmesh_VIZ%group, elemesh_VIZ)
!
      call allocate_vector_for_solver                                   &
     &   (isix, femmesh_VIZ%mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(femmesh_VIZ%mesh%nod_comm)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(femmesh_VIZ%mesh, elemesh_VIZ)
!
!     ---------------------
!
      ucd_VIZ%nnod =      femmesh_VIZ%mesh%node%numnod
      call sel_read_udt_param(my_rank, i_step_init, t1_IO, ucd_VIZ)
      call alloc_phys_data_type_by_output                               &
     &   (ucd_VIZ, femmesh_VIZ%mesh%node, field_VIZ)
!
      end subroutine mesh_setup_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine element_normals_4_VIZ
!
      use m_fem_gauss_int_coefs
!
      use int_volume_of_domain
      use set_ele_id_4_node_type
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
      call set_ele_id_4_node                                            &
     &   (femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele, ele_4_nod_VIZ)
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_volume'
      call max_int_point_by_etype(femmesh_VIZ%mesh%ele%nnod_4_ele)
      call const_jacobian_volume_normals(femmesh_VIZ%mesh,              &
     &    elemesh_VIZ%surf, femmesh_VIZ%group, jac_VIZ_l, jac_VIZ_q)
!
      end subroutine element_normals_4_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_field_data_4_VIZ(iflag, istep_ucd)
!
      use set_ucd_data_to_type
      use copy_time_steps_4_restart
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: iflag, istep_ucd
!
!
      if(iflag .ne. 0) return
      call set_data_by_read_ucd                                         &
     &   (my_rank, istep_ucd, VIZ_time_IO, ucd_VIZ, field_VIZ)
      call copy_time_steps_from_field(VIZ_time_IO)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv                                         &
     &   (femmesh_VIZ%mesh%nod_comm, field_VIZ)
!
      end subroutine set_field_data_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module m_visualization
