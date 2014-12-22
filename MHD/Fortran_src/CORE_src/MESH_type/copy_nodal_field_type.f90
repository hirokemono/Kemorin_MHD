!copy_nodal_field_type.f90
!     module copy_nodal_field_type
!
!      Written by H. Matsui
!
!      subroutine copy_scalar_type(i_target, i_org, node, fld_nod)
!      subroutine copy_vector_type(i_target, i_org, node, fld_nod)
!      subroutine copy_tensor_type(i_target, i_org, node, fld_nod)
!        integer (kind = kint), intent(in) :: i_target, i_org
!        type(node_data), intent(in) :: node
!        type(phys_data), intent(inout) :: fld_nod
!
      module copy_nodal_field_type
!
      use m_precision
!
      implicit none
!
      private :: copy_scalar_fld, copy_vector_fld, copy_tensor_fld
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_scalar_type(i_target, i_org, node, fld_nod)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld_nod
!
!
      call copy_scalar_fld(np_smp, node%numnod, node%istack_nod_smp,    &
     &    fld_nod%ntot_phys, i_target, i_org, fld_nod%d_fld )
!
      end subroutine copy_scalar_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_vector_type(i_target, i_org, node, fld_nod)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld_nod
!
!
      call copy_vector_fld(np_smp, node%numnod, node%istack_nod_smp,    &
     &    fld_nod%ntot_phys, i_target, i_org, fld_nod%d_fld )
!
      end subroutine copy_vector_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_tensor_type(i_target, i_org, node, fld_nod)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld_nod
!
!
      call copy_tensor_fld(np_smp, node%numnod, node%istack_nod_smp,    &
     &    fld_nod%ntot_phys, i_target, i_org, fld_nod%d_fld )
!
      end subroutine copy_tensor_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_scalar_fld(np_smp, numnod, inod_smp_stack,        &
     &          n_comp, i_target, i_org, d_nod)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      integer (kind = kint) :: np_smp, numnod, n_comp
      integer (kind = kint) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(inout)    :: d_nod(numnod,n_comp)
!
!
!$omp parallel
      call copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,          &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!$omp end parallel
!
      end subroutine copy_scalar_fld
!
! ----------------------------------------------------------------------
!
      subroutine copy_vector_fld(np_smp, numnod, inod_smp_stack,        &
     &          n_comp, i_target, i_org, d_nod)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      integer (kind = kint) :: np_smp, numnod, n_comp
      integer (kind = kint) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(inout)    :: d_nod(numnod,n_comp)
!
!
!$omp parallel
      call copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,          &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!$omp end parallel
!
      end subroutine copy_vector_fld
!
! ----------------------------------------------------------------------
!
      subroutine copy_tensor_fld(np_smp, numnod, inod_smp_stack,        &
     &          n_comp, i_target, i_org, d_nod)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      integer (kind = kint) :: np_smp, numnod, n_comp
      integer (kind = kint) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(inout)    :: d_nod(numnod,n_comp)
!
!
!$omp parallel
      call copy_nod_sym_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!$omp end parallel
!
      end subroutine copy_tensor_fld
!
! ----------------------------------------------------------------------
!
      end module copy_nodal_field_type
