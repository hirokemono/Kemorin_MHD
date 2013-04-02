!nodal_fld_2_each_ele_type.f90
!      module nodal_fld_2_each_ele_type
!
!      Written by H. Matsui on July, 2005
!
!      subroutine position_2_each_ele_type(mesh, k2, xe, radius_e)
!
!      subroutine scalar_2_each_ele_type(mesh, k2, scalar, scalar_e)
!      subroutine vector_2_each_ele_type(mesh, k2, vector, vect_e)
!
!      subroutine scalar_phys_2_each_ele_type(mesh, nod_fld,            &
!     &          k2, i_fld, scalar_e)
!      subroutine vector_phys_2_each_ele_type(mesh, nod_fld,            &
!     &          k2, i_fld, vect_e)
!      subroutine tensor_phys_2_each_ele_type(mesh, nod_fld,            &
!     &          k2, i_fld, tensor_e)
!
!      subroutine tensor_2_vec_each_ele_type(mesh, nod_fld,             &
!     &          k2, i_fld, nd, vect_e)
!      subroutine as_tensor_2_vev_each_ele_type(mesh, nod_fld,          &
!     &          k2, i_fld, nd, vect_e)
!
      module nodal_fld_2_each_ele_type
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_mesh_data
!
       implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine position_2_each_ele_type(mesh, k2, xe, radius_e)
!
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(inout) :: xe(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: radius_e(mesh%ele%numele)
!
!
      call position_to_local_ele                                        &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,             &
     &    mesh%node%xx, mesh%node%rr, xe, radius_e)
!
      end subroutine position_2_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_2_each_ele_type(mesh, k2, scalar, scalar_e)
!
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: scalar(mesh%node%numnod)
      real (kind=kreal), intent(inout) :: scalar_e(mesh%ele%numele)
!
!
      call scalar_to_local_ele                                          &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, ione,       &
     &    ione, scalar(1), scalar_e)
!
      end subroutine scalar_2_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine vector_2_each_ele_type(mesh, k2, vector, vect_e)
!
      use t_phys_data
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: vector(mesh%node%numnod,3)
      real (kind=kreal), intent(inout) :: vect_e(mesh%ele%numele,3)
!
!
      call vector_to_local_ele                                          &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, ione,       &
     &    ithree, vector, vect_e)
!
      end subroutine vector_2_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_phys_2_each_ele_type(mesh, nod_fld,             &
     &          k2, i_fld, scalar_e)
!
      use t_phys_data
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(inout) :: scalar_e(mesh%ele%numele)
!
!
      call scalar_to_local_ele                                          &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld,scalar_e)
!
      end subroutine scalar_phys_2_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine vector_phys_2_each_ele_type(mesh, nod_fld,             &
     &          k2, i_fld, vect_e)
!
      use t_phys_data
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(inout) :: vect_e(mesh%ele%numele,3)
!
!
      call vector_to_local_ele                                          &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, vect_e)
!
      end subroutine vector_phys_2_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_phys_2_each_ele_type(mesh, nod_fld,             &
     &          k2, i_fld, tensor_e)
!
      use t_phys_data
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(inout) :: tensor_e(mesh%ele%numele,6)
!
!
      call tensor_to_local_ele                                          &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, tensor_e)
!
      end subroutine tensor_phys_2_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine tensor_2_vec_each_ele_type(mesh, nod_fld,              &
     &          k2, i_fld, nd, vect_e)
!
      use m_phys_constants
      use t_phys_data
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd, i_fld
      real(kind=kreal), intent(inout) :: vect_e(mesh%ele%numele,3)
!
      call tensor_to_local_ele_v                                        &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld, nd,  &
     &    l_sim_t, nod_fld%ntot_phys, nod_fld%d_fld, vect_e)
!
      end subroutine tensor_2_vec_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine as_tensor_2_vev_each_ele_type(mesh, nod_fld,           &
     &          k2, i_fld, nd, vect_e)
!
      use m_phys_constants
      use t_phys_data
      use set_nodal_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, nd, i_fld
      real(kind=kreal), intent(inout) :: vect_e(mesh%ele%numele,3)
!
!
      call as_tensor_to_local_ele_v                                     &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld, nd,  &
     &    l_asim_t, nod_fld%ntot_phys, nod_fld%d_fld, vect_e)
!
      end subroutine as_tensor_2_vev_each_ele_type
!
!  ---------------------------------------------------------------------
!
      end module nodal_fld_2_each_ele_type
