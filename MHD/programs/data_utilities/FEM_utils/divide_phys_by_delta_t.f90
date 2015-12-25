!
!     module divide_phys_by_delta_t
!
!      Written by H. Matsui on Nov., 2006
!
!      subroutine s_divide_phys_by_delta_t(nod_fld)
!      subroutine s_divide_phys_by_num_udt(icou, nod_fld)
!        type(phys_data), intent(inout) :: nod_fld
!
      module divide_phys_by_delta_t
!
      use m_precision
!
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
      subroutine s_divide_phys_by_delta_t(nod_fld)
!
      use m_phys_constants
      use m_t_int_parameter
      use m_ctl_params_4_diff_udt
      use m_geometry_data
      use products_nodal_fields_smp
!
      type(phys_data), intent(inout) :: nod_fld
      integer(kind = kint) :: i, ist
!
!
!$omp parallel private(i,ist)
      do i = 1, nod_fld%num_phys
        ist = nod_fld%istack_component(i-1) + 1
!
        if      ( nod_fld%num_component(i) .eq. n_scalar) then
          call multi_by_const_nod_scalar                                &
     &       (node1, nod_fld, ddt, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_vector) then
          call multi_by_const_nod_vector                                &
     &       (node1, nod_fld, ddt, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_sym_tensor) then
          call multi_by_const_nod_tensor                                &
     &       (node1, nod_fld, ddt, ist, ist)
        end if
      end do
!$omp end parallel
!
      end subroutine s_divide_phys_by_delta_t
!
!-----------------------------------------------------------------------
!
      subroutine s_divide_phys_by_num_udt(icou, nod_fld)
!
      use m_constants
      use m_phys_constants
      use m_geometry_data
      use products_nodal_fields_smp
!
      integer(kind = kint), intent(in) :: icou
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
     &       (node1, nod_fld, dnum, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_vector) then
          call multi_by_const_nod_vector                                &
     &       (node1, nod_fld, dnum, ist, ist)
        else if ( nod_fld%num_component(i) .eq. n_sym_tensor) then
          call multi_by_const_nod_tensor                                &
     &       (node1, nod_fld, dnum, ist, ist)
        end if
      end do
!$omp end parallel
!
      end subroutine s_divide_phys_by_num_udt
!
!-----------------------------------------------------------------------
!
      end module divide_phys_by_delta_t
