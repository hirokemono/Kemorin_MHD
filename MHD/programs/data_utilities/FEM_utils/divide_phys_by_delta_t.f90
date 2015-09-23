!
!     module divide_phys_by_delta_t
!
!      Written by H. Matsui on Nov., 2006
!
!      subroutine s_divide_phys_by_delta_t
!      subroutine s_divide_phys_by_num_udt(icou)
!
      module divide_phys_by_delta_t
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_divide_phys_by_delta_t
!
      use m_phys_constants
      use m_t_int_parameter
      use m_ctl_params_4_diff_udt
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use products_nodal_fields_smp
!
      integer(kind = kint) :: i, ist
!
!
!$omp parallel private(i,ist)
      do i = 1, nod_fld1%num_phys
        ist = nod_fld1%istack_component(i-1) + 1
!
        if      ( nod_fld1%num_component(i) .eq. n_scalar) then
          call multi_by_const_nod_scalar                                &
     &       (node1, nod_fld1, ddt, ist, ist)
        else if ( nod_fld1%num_component(i) .eq. n_vector) then
          call multi_by_const_nod_vector                                &
     &       (node1, nod_fld1, ddt, ist, ist)
        else if ( nod_fld1%num_component(i) .eq. n_sym_tensor) then
          call multi_by_const_nod_tensor                                &
     &       (node1, nod_fld1, ddt, ist, ist)
        end if
      end do
!$omp end parallel
!
      end subroutine s_divide_phys_by_delta_t
!
!-----------------------------------------------------------------------
!
      subroutine s_divide_phys_by_num_udt(icou)
!
      use m_constants
      use m_phys_constants
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use products_nodal_fields_smp
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint) :: i, ist
      real(kind = kreal) :: dnum
!
!
      dnum = one / dble(icou)
!$omp parallel private(i,ist)
      do i = 1, nod_fld1%num_phys
        ist = nod_fld1%istack_component(i-1) + 1
!
        if      ( nod_fld1%num_component(i) .eq. n_scalar) then
          call multi_by_const_nod_scalar                                &
     &       (node1, nod_fld1, dnum, ist, ist)
        else if ( nod_fld1%num_component(i) .eq. n_vector) then
          call multi_by_const_nod_vector                                &
     &       (node1, nod_fld1, dnum, ist, ist)
        else if ( nod_fld1%num_component(i) .eq. n_sym_tensor) then
          call multi_by_const_nod_tensor                                &
     &       (node1, nod_fld1, dnum, ist, ist)
        end if
      end do
!$omp end parallel
!
      end subroutine s_divide_phys_by_num_udt
!
!-----------------------------------------------------------------------
!
      end module divide_phys_by_delta_t
