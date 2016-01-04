!
!     module divide_phys_by_delta_t
!
!      Written by H. Matsui on Nov., 2006
!
!      subroutine s_divide_phys_by_delta_t(node, nod_fld)
!      subroutine s_divide_phys_by_num_udt(icou, node, nod_fld)
!        type(node_data), intent(in) :: node
!        type(phys_data), intent(inout) :: nod_fld
!
      module divide_phys_by_delta_t
!
      use m_precision
!
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_divide_phys_by_delta_t(node, nod_fld)
!
      use m_phys_constants
      use m_t_int_parameter
      use m_ctl_params_4_diff_udt
      use products_nodal_fields_smp
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, ist
!
!
!$omp parallel private(i,ist)
      do i = 1, nod_fld%num_phys
        ist = nod_fld%istack_component(i-1) + 1
!
        if      ( nod_fld%num_component(i) .eq. n_scalar) then
          call multi_by_const_nod_scalar                                &
     &       (node, nod_fld, ddt, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_vector) then
          call multi_by_const_nod_vector                                &
     &       (node, nod_fld, ddt, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_sym_tensor) then
          call multi_by_const_nod_tensor                                &
     &       (node, nod_fld, ddt, ist, ist)
        end if
      end do
!$omp end parallel
!
      end subroutine s_divide_phys_by_delta_t
!
!-----------------------------------------------------------------------
!
      subroutine s_divide_phys_by_num_udt(icou, node, nod_fld)
!
      use m_constants
      use m_phys_constants
      use products_nodal_fields_smp
!
      integer(kind = kint), intent(in) :: icou
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, ist
      real(kind = kreal) :: dnum
!
!
      dnum = one / dble(icou)
!$omp parallel private(i,ist)
      do i = 1, nod_fld%num_phys
        ist = nod_fld%istack_component(i-1) + 1
!
        if      ( nod_fld%num_component(i) .eq. n_scalar) then
          call multi_by_const_nod_scalar                                &
     &       (node, nod_fld, dnum, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_vector) then
          call multi_by_const_nod_vector                                &
     &       (node, nod_fld, dnum, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_sym_tensor) then
          call multi_by_const_nod_tensor                                &
     &       (node, nod_fld, dnum, ist, ist)
        end if
      end do
!$omp end parallel
!
      end subroutine s_divide_phys_by_num_udt
!
!-----------------------------------------------------------------------
!
      end module divide_phys_by_delta_t
