!>@file   m_FEM_utils.f90
!!@brief  module m_FEM_utils
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for FEM utilities
!!
!!@verbatim
!!@endverbatim
!
      module m_FEM_utils
!
      use m_precision
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_ucd_data
      use t_jacobian_3d
!
      implicit none
!
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
!>        Instance for FEM field data IO
      type(ucd_data), save :: ucd_FUTIL
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: m_ucd_FUTIL
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac_FUTIL_l
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac_FUTIL_q
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_FEM_UTIL
!
      use m_read_mesh_data
      use m_array_for_send_recv
      use load_mesh_data
      use nod_phys_send_recv
      use const_mesh_information
!
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank, femmesh_FUTIL%mesh, femmesh_FUTIL%group,  &
     &   elemesh_FUTIL%surf%nnod_4_surf, elemesh_FUTIL%edge%nnod_4_edge)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (isix, femmesh_FUTIL%mesh%node%numnod)
      call init_send_recv(femmesh_FUTIL%mesh%nod_comm)
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
