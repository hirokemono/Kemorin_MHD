!nodal_fld_cst_to_ele_type.f90
!      module nodal_fld_cst_to_ele_type
!
!      Written by H. Matsui on July, 2005
!
!      subroutine scalar_const_each_ele_type(mesh, k2, scalar, coef,    &
!     &          scalar_e)
!      subroutine vector_const_each_ele_type(mesh, k2, vector, coef,    &
!     &          vector_e)
!      subroutine tensor_const_each_ele_type(mesh, k2, tensor, coef,    &
!     &          tensor_e)
!
!      subroutine scalar_cst_phys_each_ele_type(mesh, nod_fld,          &
!     &          k2, i_fld, coef, scalar_e)
!      subroutine vector_cst_phys_each_ele_type(mesh, nod_fld,          &
!     &          k2, i_fld, coef, vector_e)
!      subroutine tensor_cst_phys_each_ele_type(mesh, nod_fld,          &
!     &          k2, i_fld, coef, tensor_e)
!
!      subroutine tensor_cst_vec_each_ele_type(mesh, nod_fld,           &
!     &          k2, i_fld, nd, coef, vector_e)
!      subroutine as_tsr_cst_vec_each_ele_type(mesh, nod_fld,           &
!     &          k2, i_fld, nd, coef, vector_e)
!
      module nodal_fld_cst_to_ele_type
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_mesh_data
!
      use nodal_cst_fld_each_element
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine scalar_const_each_ele_type(mesh, k2, scalar, coef,     &
     &          scalar_e)
!
      type(mesh_geometry), intent(in) :: mesh
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: scalar(mesh%node%numnod)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: scalar_e(mesh%ele%numele)
!
!
      call const_scalar_2_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, ione,       &
     &    ione, scalar(1), coef, scalar_e)
!
      end subroutine scalar_const_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine vector_const_each_ele_type(mesh, k2, vector, coef,     &
     &          vector_e)
!
      type(mesh_geometry), intent(in) :: mesh
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: vector(mesh%node%numnod,3)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(mesh%ele%numele,3)
!
!
      call const_vector_2_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, ione,       &
     &    ithree, vector, coef, vector_e)
!
      end subroutine vector_const_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_const_each_ele_type(mesh, k2, tensor, coef,     &
     &          tensor_e)
!
      type(mesh_geometry), intent(in) :: mesh
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: tensor(mesh%node%numnod,6)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: tensor_e(mesh%ele%numele,6)
!
!
      call const_tensor_2_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, ione,       &
     &    isix, tensor, coef, tensor_e)
!
      end subroutine tensor_const_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_cst_phys_each_ele_type(mesh, nod_fld,           &
     &          k2, i_fld, coef, scalar_e)
!
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: scalar_e(mesh%ele%numele)
!
!
      call const_scalar_2_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, scalar_e)
!
      end subroutine scalar_cst_phys_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine vector_cst_phys_each_ele_type(mesh, nod_fld,           &
     &          k2, i_fld, coef, vector_e)
!
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(mesh%ele%numele,3)
!
!
      call const_vector_2_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, vector_e)
!
      end subroutine vector_cst_phys_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_cst_phys_each_ele_type(mesh, nod_fld,           &
     &          k2, i_fld, coef, tensor_e)
!
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: tensor_e(mesh%ele%numele,6)
!
!
      call const_tensor_2_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, tensor_e)
!
      end subroutine tensor_cst_phys_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine tensor_cst_vec_each_ele_type(mesh, nod_fld,            &
     &          k2, i_fld, nd, coef, vector_e)
!
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd, i_fld
      real (kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(inout) :: vector_e(mesh%ele%numele,3)
!
      call const_tensor_2_vec_each_ele                                  &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld, nd,  &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, vector_e)
!
      end subroutine tensor_cst_vec_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine as_tsr_cst_vec_each_ele_type(mesh, nod_fld,            &
     &          k2, i_fld, nd, coef, vector_e)
!
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, nd, i_fld
      real (kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(inout) :: vector_e(mesh%ele%numele,3)
!
!
      call const_as_tsr_2_vec_each_ele                                  &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, i_fld, nd,  &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, vector_e)
!
      end subroutine as_tsr_cst_vec_each_ele_type
!
!  ---------------------------------------------------------------------
!
      end module nodal_fld_cst_to_ele_type
