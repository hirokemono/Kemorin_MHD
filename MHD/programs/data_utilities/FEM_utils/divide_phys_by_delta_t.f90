!
!     module divide_phys_by_delta_t
!
      module divide_phys_by_delta_t
!
!      Written by H. Matsui on Nov., 2006
!
      use m_precision
!
      implicit none
!
!      subroutine s_divide_phys_by_delta_t
!      subroutine s_divide_phys_by_num_udt(icou)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_divide_phys_by_delta_t
!
      use m_t_int_parameter
      use m_ctl_params_4_diff_udt
      use m_node_phys_address
      use m_node_phys_data
      use multi_by_const_fields
!
      integer(kind = kint) :: i, ist
!
!
      do i = 1, num_nod_phys
        ist = istack_nod_component(i-1) + 1
!
        if      ( num_nod_component(i) .eq. 1) then
          call multi_by_const_nod_scalar(ddt, ist, ist)
        else if ( num_nod_component(i) .eq. 3) then
          call multi_by_const_nod_vector(ddt, ist, ist)
        else if ( num_nod_component(i) .eq. 6) then
          call multi_by_const_nod_tensor(ddt, ist, ist)
        end if
!
      end do
!
      end subroutine s_divide_phys_by_delta_t
!
!-----------------------------------------------------------------------
!
      subroutine s_divide_phys_by_num_udt(icou)
!
      use m_constants
      use m_node_phys_address
      use m_node_phys_data
      use multi_by_const_fields
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint) :: i, ist
      real(kind = kreal) :: dnum
!
!
      dnum = one / dble(icou)
      do i = 1, num_nod_phys
        ist = istack_nod_component(i-1) + 1
!
        if      ( num_nod_component(i) .eq. 1) then
          call multi_by_const_nod_scalar(dnum, ist, ist)
        else if ( num_nod_component(i) .eq. 3) then
          call multi_by_const_nod_vector(dnum, ist, ist)
        else if ( num_nod_component(i) .eq. 6) then
          call multi_by_const_nod_tensor(dnum, ist, ist)
        end if
!
      end do
!
      end subroutine s_divide_phys_by_num_udt
!
!-----------------------------------------------------------------------
!
      end module divide_phys_by_delta_t
