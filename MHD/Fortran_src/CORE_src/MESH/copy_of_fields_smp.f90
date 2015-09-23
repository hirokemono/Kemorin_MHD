!>@file   copy_of_fields_smp.f90
!!@brief  module copy_of_fields_smp
!!
!!@author H. Matsui
!!@date Programmed in ??
!
!>@brief Copy, add, and subtract of fields
!!
!!@verbatim
!!      subroutine copy_scalar_fld(np_smp, numnod, inod_smp_stack,      &
!!     &          n_comp, i_org, i_target, d_nod)
!!      subroutine copy_vector_fld(np_smp, numnod, inod_smp_stack,      &
!!     &          n_comp, i_org, i_target, d_nod)
!!      subroutine copy_tensor_fld(np_smp, numnod, inod_smp_stack,      &
!!     &          n_comp, i_org, i_target, d_nod)
!!        integer (kind = kint), intent(in) :: i_target, i_org
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: fld_nod
!!
!!      subroutine add_scalar_array_smp(np_smp, nnod, inod_smp_stack,   &
!!     &          ntot_comp, d_phys, i_v1, i_v2, i_r)
!!      subroutine add_vector_array_smp(np_smp, nnod, inod_smp_stack,   &
!!     &          ntot_comp, d_phys, i_v1, i_v2, i_r)
!!      subroutine add_tensor_array_smp(np_smp, nnod, inod_smp_stack,   &
!!     &          ntot_comp, d_phys, i_v1, i_v2, i_r)
!!
!!      subroutine subtract_scalar_array_smp(np_smp, nnod,              &
!!     &          inod_smp_stack, ntot_comp, d_phys, i_v1, i_v2, i_r)
!!      subroutine subtract_vector_array_smp(np_smp, nnod,              &
!!     &          inod_smp_stack, ntot_comp, d_phys, i_v1, i_v2, i_r)
!!      subroutine subtract_tensor_array_smp(np_smp, nnod,              &
!!     &          inod_smp_stack, ntot_comp, d_phys, i_v1, i_v2, i_r)
!!@endverbatim
!
      module copy_of_fields_smp
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_scalar_fld(np_smp, numnod, inod_smp_stack,        &
     &          n_comp, i_org, i_target, d_nod)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      integer (kind = kint) :: np_smp, numnod, n_comp
      integer (kind = kint) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(inout)    :: d_nod(numnod,n_comp)
!
!
      call copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,          &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!
      end subroutine copy_scalar_fld
!
! ----------------------------------------------------------------------
!
      subroutine copy_vector_fld(np_smp, numnod, inod_smp_stack,        &
     &          n_comp, i_org, i_target, d_nod)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      integer (kind = kint) :: np_smp, numnod, n_comp
      integer (kind = kint) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(inout)    :: d_nod(numnod,n_comp)
!
!
      call copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,          &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!
      end subroutine copy_vector_fld
!
! ----------------------------------------------------------------------
!
      subroutine copy_tensor_fld(np_smp, numnod, inod_smp_stack,        &
     &          n_comp, i_org, i_target, d_nod)
!
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      integer (kind = kint) :: np_smp, numnod, n_comp
      integer (kind = kint) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(inout)    :: d_nod(numnod,n_comp)
!
!
      call copy_nod_sym_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!
      end subroutine copy_tensor_fld
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scalar_array_smp(np_smp, nnod, inod_smp_stack,     &
     &          ntot_comp, d_phys, i_v1, i_v2, i_r)
!
      use cal_add_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
      call add_scalars_smp(np_smp, nnod, inod_smp_stack,               &
     &    d_phys(1,i_v1), d_phys(1,i_v2), d_phys(1,i_r))
!
       end subroutine add_scalar_array_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_array_smp(np_smp, nnod, inod_smp_stack,     &
     &          ntot_comp, d_phys, i_v1, i_v2, i_r)
!
      use cal_add_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
      call add_vectors_smp(np_smp, nnod, inod_smp_stack,                &
     &    d_phys(1,i_v1), d_phys(1,i_v2), d_phys(1,i_r))
!
       end subroutine add_vector_array_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_tensor_array_smp(np_smp, nnod, inod_smp_stack,     &
     &          ntot_comp, d_phys, i_v1, i_v2, i_r)
!
      use cal_add_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
      call add_tensors_smp(np_smp, nnod, inod_smp_stack,                &
     &    d_phys(1,i_v1), d_phys(1,i_v2), d_phys(1,i_r))
!
      end subroutine add_tensor_array_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine subtract_scalar_array_smp(np_smp, nnod,                &
     &          inod_smp_stack, ntot_comp, d_phys, i_v1, i_v2, i_r)
!
      use cal_subtract_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
      call subtract_scalars_smp(np_smp, nnod, inod_smp_stack,           &
     &    d_phys(1,i_v1), d_phys(1,i_v2), d_phys(1,i_r))
!
       end subroutine subtract_scalar_array_smp
!
!-----------------------------------------------------------------------
!
      subroutine subtract_vector_array_smp(np_smp, nnod,                &
     &          inod_smp_stack, ntot_comp, d_phys, i_v1, i_v2, i_r)
!
      use cal_subtract_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
      call subtract_vectors_smp(np_smp, nnod, inod_smp_stack,           &
     &    d_phys(1,i_v1), d_phys(1,i_v2), d_phys(1,i_r))
!
       end subroutine subtract_vector_array_smp
!
!-----------------------------------------------------------------------
!
      subroutine subtract_tensor_array_smp(np_smp, nnod,                &
     &          inod_smp_stack, ntot_comp, d_phys, i_v1, i_v2, i_r)
!
      use cal_subtract_smp
!
      integer (kind=kint), intent(in) :: np_smp, nnod, ntot_comp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
      real (kind=kreal), intent(inout) :: d_phys(nnod,ntot_comp)
!
!
      call subtract_tensors_smp(np_smp, nnod, inod_smp_stack,           &
     &    d_phys(1,i_v1), d_phys(1,i_v2), d_phys(1,i_r))
!
      end subroutine subtract_tensor_array_smp
!
!-----------------------------------------------------------------------
!
      end module copy_of_fields_smp
