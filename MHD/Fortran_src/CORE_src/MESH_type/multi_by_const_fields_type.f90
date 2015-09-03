!multi_by_const_fields_type.f90
!     module multi_by_const_fields_type
!
!      Written by H. Matsui
!
!      subroutine multi_by_const_nod_scalar_type(node, fld,             &
!     &          const, i_v1, i_r)
!      subroutine multi_by_const_nod_vector_type(node, fld,             &
!     &          const, i_v1, i_r)
!      subroutine multi_by_const_nod_tensor_type(node, fld,             &
!     &          const, i_v1, i_r)
!        integer(kind = kint), intent(in) :: i_r, i_v1
!        real(kind = kreal), intent(in) :: const
!        type(node_data), intent(in) :: node
!        type(phys_data), intent(inout) :: fld
!
!         d_nod(inod,i_r) =  const * d_nod(inod,i_v1)
!        i_r: result field ID
!        i_v1: source field IDs
!
      module multi_by_const_fields_type
!
      use m_precision
!
      implicit none
!
!      private :: multi_by_const_nod_phys1, multi_by_const_nod_phys3
!      private :: multi_by_const_nod_phys6
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_scalar_type(node, fld,              &
     &          const, i_v1, i_r)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
      call multi_by_const_nod_phys1(np_smp,                             &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys,              &
     &    fld%d_fld, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_scalar_type
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_vector_type(node, fld,              &
     &          const, i_v1, i_r)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
      call multi_by_const_nod_phys3(np_smp,                             &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys,              &
     &    fld%d_fld, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_vector_type
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_tensor_type(node, fld,              &
     &          const, i_v1, i_r)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      use cal_products_w_const_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
      call multi_by_const_nod_phys6(np_smp,                             &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys,              &
     &    fld%d_fld, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_tensor_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_phys1(np_smp, nnod,                 &
     &          inod_smp_stack, ntot_comp, d_phys, const, i_v1, i_r)
!
      use cal_products_w_const_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
!$omp parallel
      call cal_coef_prod_scalar_smp(np_smp, nnod, inod_smp_stack,       &
     &    const, d_phys(1,i_v1), d_phys(1,i_r) )
!$omp end parallel
!
      end subroutine multi_by_const_nod_phys1
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_phys3(np_smp, nnod,                 &
     &          inod_smp_stack, ntot_comp, d_phys, const, i_v1, i_r)
!
      use cal_products_w_const_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
!$omp parallel
      call cal_coef_prod_vect_smp(np_smp, nnod, inod_smp_stack,         &
     &    const, d_phys(1,i_v1), d_phys(1,i_r) )
!$omp end parallel
!
      end subroutine multi_by_const_nod_phys3
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_phys6(np_smp, nnod,                 &
     &          inod_smp_stack, ntot_comp, d_phys, const, i_v1, i_r)
!
      use cal_products_w_const_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
!$omp parallel
      call cal_coef_prod_tensor_smp(np_smp, nnod, inod_smp_stack,       &
     &    const, d_phys(1,i_v1), d_phys(1,i_r) )
!$omp end parallel
!
      end subroutine multi_by_const_nod_phys6
!
!-----------------------------------------------------------------------
!
      end module multi_by_const_fields_type
